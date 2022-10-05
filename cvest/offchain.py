import os
import pickle

from datetime import datetime, timedelta
from typing import List

import cbor2

from pycardano import (
    Address,
    AlonzoMetadata,
    AuxiliaryData,
    BlockFrostChainContext,
    ChainContext,
    Metadata,
    MultiAsset,
    Network,
    PlutusData,
    Redeemer,
    RedeemerTag,
    PaymentSigningKey,
    PaymentVerificationKey,
    Transaction,
    TransactionBuilder,
    TransactionOutput,
    UTxO,
    Value,
    script_hash,
    min_lovelace_post_alonzo
)

from cvest.datum import VestingDatum, new_vesting_datum


def get_env_var(key):
    val = os.environ.get(key, None)
    if val is None:
        raise Exception(f"Couldn't find env variable: {key}!")
    return val


with open(get_env_var("VEST_UTXO_PATH"), "rb") as f:
    vest_utxo = pickle.load(f)

with open(get_env_var("MINT_UTXO_PATH"), "rb") as f:
    mint_utxo = pickle.load(f)

NETWORK = Network.TESTNET
MINT_FEE = 10000000
MAX_MINT_SPAN = 3650

fee_address = Address.from_primitive(get_env_var("FEE_ADDRESS"))

payment_pkh = script_hash(vest_utxo.output.script)

owner_skey = PaymentSigningKey.load("keys/payment.skey")

owner_vkey = PaymentVerificationKey.from_signing_key(owner_skey)

script_address = Address(
    payment_part=payment_pkh,
    network=NETWORK
)

context = BlockFrostChainContext(
    get_env_var("BLOCKFROST_PROJECT_ID"),
    network=NETWORK,
    base_url="https://cardano-preprod.blockfrost.io/api"
)


def create_grant_tx(
    sender: Address,
    beneficiary: Address,
    vals: List[tuple[datetime, int]],
    cancellable: bool = False,
    pay_fee_from_vest: bool = True,
    mint: bool = True,
) -> Transaction:
    """
    Create an unsigned transaction that generates UTxOs that can be spent by the beneficiary after specific deadlines.
    """
    tx_builder = TransactionBuilder(context)
    tx_builder.execution_memory_buffer = 0.5
    tx_builder.execution_step_buffer = 0.5
    tx_builder.add_input_address(sender)

    ttl = 1200  # 1200 seconds

    tx_builder.ttl = context.last_block_slot + ttl

    mint_amount = 0

    for deadline, amount in vals:
        if pay_fee_from_vest:
            min_vest_amount = amount - 500000
            datum = new_vesting_datum(beneficiary, sender, cancellable, deadline, min_vest_amount)
        else:
            datum = new_vesting_datum(beneficiary, sender, cancellable, deadline, amount)
        tx_output = TransactionOutput(script_address, amount, datum=datum)
        tx_builder.add_output(tx_output)

        delta = deadline - datetime.now() - timedelta(seconds=ttl)
        mint_amount += min(max(delta.days, 0), MAX_MINT_SPAN) * amount

    if mint and mint_amount > 0 and not cancellable:
        tx_builder.add_minting_script(mint_utxo, Redeemer(RedeemerTag.MINT, data=1))
        tokens = MultiAsset.from_primitive({
            script_hash(mint_utxo.output.script).payload: {b"LOCK": mint_amount}
        })

        tx_builder.mint = tokens

        mint_output = TransactionOutput(sender, Value(0, tokens))
        mint_output.amount.coin = min_lovelace_post_alonzo(mint_output, context)
        tx_builder.add_output(mint_output)

        mint_fee_output = TransactionOutput(fee_address, MINT_FEE)
        tx_builder.add_output(mint_fee_output)
        tx_builder.required_signers = [owner_vkey.hash()]

    metadata = {
        20: {
            script_hash(mint_utxo.output.script).payload.hex(): {
                b"LOCK".hex(): {
                    "decimals": 6,
                    "desc": "LOCK Token",
                    "ticker": "LOCK",
                    "version": "1.0"
                }
            }
        }
    }

    # Place metadata in AuxiliaryData, the format acceptable by a transaction.
    auxiliary_data = AuxiliaryData(AlonzoMetadata(metadata=Metadata(metadata)))
    tx_builder.auxiliary_data = auxiliary_data

    return tx_builder.build_and_sign([owner_skey], sender, merge_change=True)


def typed_datum(utxo: UTxO) -> UTxO:
    """
    Decorate a utxo with VestingDatum.
    """
    if utxo.output.datum is not None:
        datum = VestingDatum.from_primitive(cbor2.loads(utxo.output.datum.cbor))
        assert datum.to_cbor("bytes") == utxo.output.datum.cbor
        utxo.output.datum = datum
    return utxo


def get_pending(beneficiary: Address) -> List[UTxO]:
    """
    Get unvested utxos for a beneficiary.
    """
    utxo = context.utxos(str(script_address))
    results = []

    for u in utxo:
        typed_datum(u)
        if u.output.datum.beneficiary == beneficiary.payment_part.payload:
            results.append(u)

    return results


def vestable(utxo) -> bool:
    """
    Check if a utxo is vestable.
    """
    if utxo.output.datum is None:
        return False

    return utxo.output.datum.deadline_in_datetime() <= datetime.now()


def cancellable(utxo) -> bool:
    """
    Check if a utxo is cancellable.
    """
    if utxo.output.datum is None:
        return False

    return utxo.output.datum.deadline_in_datetime() > datetime.now() and utxo.output.datum.cancellable


def vest(beneficiary: Address, utxo: UTxO) -> Transaction:
    """
    Vest a utxo.
    """

    tx_builder = TransactionBuilder(context)

    tx_builder.execution_memory_buffer = 0.3
    tx_builder.execution_step_buffer = 0.3

    tx_builder.add_script_input(utxo, vest_utxo, None, Redeemer(RedeemerTag.SPEND, data=PlutusData()))

    tx_builder.ttl = context.last_block_slot + 1200
    tx_builder.validity_start = context.last_block_slot

    tx_builder.add_output(TransactionOutput(beneficiary, utxo.output.datum.min_vest_amount))

    # Sign with owner's signing key so the collateral can be spent.
    tx = tx_builder.build_and_sign([owner_skey], fee_address, merge_change=True)

    return tx


def cancel(granter: Address, utxos: List[UTxO]) -> Transaction:
    """
    Cancel a list of locked but cancellable funds.
    """
    tx_builder = TransactionBuilder(context)

    tx_builder.execution_memory_buffer = 0.3
    tx_builder.execution_step_buffer = 0.3

    for utxo in utxos:
        tx_builder.add_script_input(utxo, vest_utxo, None, Redeemer(RedeemerTag.SPEND, data=PlutusData()))

    tx_builder.ttl = context.last_block_slot + 1200
    tx_builder.validity_start = context.last_block_slot

    tx_builder.add_input_address(granter)

    tx_builder.add_output(TransactionOutput(granter, sum([u.output.amount.coin for u in utxos])))

    tx_builder.required_signers = [granter.payment_part.payload]

    # Sign with owner's signing key so the collateral can be spent.
    tx = tx_builder.build_and_sign([], fee_address, merge_change=True)

    return tx
