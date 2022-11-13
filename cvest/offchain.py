import os
import pickle
from datetime import datetime, timedelta
from typing import List, Union, Tuple

import cbor2
from cachetools import TTLCache, cached
from pycardano import (
    Address,
    AlonzoMetadata,
    AuxiliaryData,
    BlockFrostChainContext,
    Metadata,
    MultiAsset,
    Network,
    PaymentSigningKey,
    PaymentVerificationKey,
    PlutusData,
    Redeemer,
    RedeemerTag,
    Transaction,
    TransactionBuilder,
    TransactionOutput,
    UTxO,
    Value,
    min_lovelace_post_alonzo,
    script_hash,
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
VEST_FEE = 1500000
MINT_FEE = 10000000
MAX_MINT_SPAN = 3650

fee_address = Address.from_primitive(get_env_var("FEE_ADDRESS"))

payment_pkh = script_hash(vest_utxo.output.script)

owner_skey = PaymentSigningKey.load("keys/payment.skey")

owner_vkey = PaymentVerificationKey.from_signing_key(owner_skey)

owner_addr = Address(owner_vkey.hash(), network=NETWORK)

script_address = Address(payment_part=payment_pkh, network=NETWORK)

_network = "preview"

context = BlockFrostChainContext(
    get_env_var("BLOCKFROST_PREPROD")
    if _network == "preprod"
    else get_env_var("BLOCKFROST_PREVIEW"),
    network=NETWORK,
    base_url="https://cardano-preprod.blockfrost.io/api"
    if _network == "preprod"
    else "https://cardano-preview.blockfrost.io/api",
)


def create_grant_tx(
    senders: Union[Address, List[Address]],
    vals: List[tuple[Address, datetime, int]],
    can_cancel: bool = False,
    mint: bool = True,
    change_address: Address = None,
) -> Transaction:
    """
    Create an unsigned transaction that generates UTxOs that can be spent by the beneficiary after specific deadlines.
    """
    if isinstance(senders, list):
        sender = senders[0]
    else:
        sender = senders
        senders = [sender]

    tx_builder = TransactionBuilder(context)

    for s in senders:
        try:
            context.utxos(str(s))
            tx_builder.add_input_address(s)
        except Exception:
            pass

    ttl = 1200  # 1200 seconds

    tx_builder.ttl = context.last_block_slot + ttl

    mint_amount = 0

    tx_fee_estimate = 500000

    for beneficiary, deadline, amount in vals:
        total_amount = amount + tx_fee_estimate + VEST_FEE
        datum = new_vesting_datum(
            beneficiary, sender, can_cancel, deadline, amount
        )
        tx_output = TransactionOutput(script_address, total_amount, datum=datum)
        tx_builder.add_output(tx_output)

        delta = deadline - datetime.now() - timedelta(seconds=ttl)
        mint_amount += min(max(delta.days, 0), MAX_MINT_SPAN) * amount

    if mint and mint_amount > 0 and not can_cancel:
        tx_builder.add_minting_script(mint_utxo, Redeemer(RedeemerTag.MINT, data=1))
        tokens = MultiAsset.from_primitive(
            {script_hash(mint_utxo.output.script).payload: {b"LOCK": mint_amount}}
        )

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
                        "version": "1.0",
                    }
                }
            }
        }

        # Place metadata in AuxiliaryData, the format acceptable by a transaction.
        auxiliary_data = AuxiliaryData(AlonzoMetadata(metadata=Metadata(metadata)))
        tx_builder.auxiliary_data = auxiliary_data

    signers = [owner_skey] if mint and mint_amount > 0 else []

    return tx_builder.build_and_sign(signers, change_address or sender)


def typed_datum(utxo: UTxO) -> UTxO:
    """
    Decorate a utxo with VestingDatum.
    """
    if utxo.output.datum is not None:
        try:
            datum = VestingDatum.from_primitive(cbor2.loads(utxo.output.datum.cbor))
        except Exception:
            try:
                datum = VestingDatum.from_primitive(cbor2.loads(utxo.output.datum.to_cbor("bytes")))
            except Exception as e:
                print(e)
                datum = None
        utxo.output.datum = datum
    return utxo


@cached(cache=TTLCache(maxsize=1, ttl=20))
def get_script_utxos() -> List[UTxO]:
    """
    Get all utxos sitting in the script address.
    """
    utxo = context.utxos(str(script_address))
    results = []

    for u in utxo:
        typed_datum(u)
        if u.output.datum is not None:
            results.append(u)

    return results


def _to_payment_part_set(addresses: Union[Address, List[Address]]) -> set[bytes]:
    if isinstance(addresses, Address):
        addresses = [addresses]

    return {a.payment_part.payload for a in addresses}


def get_pending(beneficiaries: Union[Address, List[Address]]) -> List[UTxO]:
    """
    Get unvested utxos for a beneficiary.
    """
    payment_part_set = _to_payment_part_set(beneficiaries)

    utxos = get_script_utxos()
    results = []

    for u in utxos:
        if (
            u.output.datum.beneficiary in payment_part_set
            or u.output.datum.beneficiary_script in payment_part_set
        ):
            results.append(u)

    return results


def get_grants(granters: Union[Address, List[Address]]) -> List[UTxO]:
    """
    Get unvested utxos granted by a granter.
    """
    payment_part_set = _to_payment_part_set(granters)

    utxos = get_script_utxos()
    results = []

    for u in utxos:
        if u.output.datum.granter in payment_part_set:
            results.append(u)

    return results


def vestable(utxo) -> bool:
    """
    Check if a utxo is vestable.
    """
    if utxo.output.datum is None:
        return False

    return utxo.output.datum.deadline_in_datetime() <= datetime.utcnow()


def cancellable(utxo) -> bool:
    """
    Check if a utxo is cancellable.
    """
    if utxo.output.datum is None:
        return False

    return (
        utxo.output.datum.deadline_in_datetime() > datetime.utcnow()
        and utxo.output.datum.cancellable
    )


def vest(beneficiary: Address, utxo: UTxO) -> Transaction:
    """
    Vest a utxo.
    """

    tx_builder = TransactionBuilder(context)

    tx_builder.add_script_input(
        utxo, vest_utxo, None, Redeemer(RedeemerTag.SPEND, data=PlutusData())
    )

    tx_builder.ttl = context.last_block_slot + 1200
    tx_builder.validity_start = context.last_block_slot

    tx_builder.add_output(
        TransactionOutput(beneficiary, utxo.output.datum.min_vest_amount)
    )
    tx_builder.add_output(TransactionOutput(fee_address, VEST_FEE))

    # Sign with owner's signing key so the collateral can be spent.
    tx = tx_builder.build_and_sign(
        [owner_skey],
        beneficiary,
        merge_change=True,
        collateral_change_address=owner_addr,
    )

    return tx


def cancel(granter: Address, utxos: List[UTxO]) -> Transaction:
    """
    Cancel a list of locked but cancellable funds.
    """
    tx_builder = TransactionBuilder(context)

    # tx_builder.execution_memory_buffer = 0.5
    # tx_builder.execution_step_buffer = 0.5

    for utxo in utxos:
        tx_builder.add_script_input(
            utxo, vest_utxo, None, Redeemer(RedeemerTag.SPEND, data=PlutusData())
        )

    tx_builder.ttl = context.last_block_slot + 1200
    tx_builder.validity_start = context.last_block_slot

    tx_builder.add_input_address(granter)

    tx_builder.add_output(
        TransactionOutput(granter, sum([u.output.amount.coin for u in utxos]))
    )

    # Require granter to sign this transaction.
    tx_builder.required_signers = [granter.payment_part.payload]

    tx = tx_builder.build_and_sign(
        [], granter, merge_change=True, collateral_change_address=granter
    )

    return tx
