import os
import pickle

from datetime import datetime, timedelta
from typing import List

from pycardano import (
    Address,
    BlockFrostChainContext,
    ChainContext,
    MultiAsset,
    Network,
    PlutusData,
    Redeemer,
    RedeemerTag,
    Transaction,
    TransactionBuilder,
    TransactionOutput,
    UTxO,
    Value,
    script_hash,
    min_lovelace_post_alonzo
)

from cvest.datum import new_vesting_datum


def get_env_var(key):
    val = os.environ.get(key, None)
    if val is None:
        raise Exception(f"Couldn't find env variable: {key}!")
    return val


fee_address = Address.from_primitive(get_env_var("FEE_ADDRESS"))

with open(get_env_var("VEST_UTXO_PATH"), "rb") as f:
    vest_utxo = pickle.load(f)

with open(get_env_var("MINT_UTXO_PATH"), "rb") as f:
    mint_utxo = pickle.load(f)

NETWORK = Network.TESTNET

context = BlockFrostChainContext(
    get_env_var("BLOCKFROST_PROJECT_ID"),
    network=NETWORK,
    base_url="https://cardano-preview.blockfrost.io/api"
)


def create_grant_tx(
    sender: Address,
    beneficiary: Address,
    vals: List[tuple[datetime, int]],
    mint: bool = True,
) -> Transaction:
    """
    Create an unsigned transaction that generates UTxOs that can be spent by the beneficiary after specific deadlines.
    """
    tx_builder = TransactionBuilder(context)
    tx_builder.add_input_address(sender)

    payment_pkh = script_hash(vest_utxo.output.script)

    print(payment_pkh)

    output_address = Address(
        payment_part=payment_pkh,
        staking_part=beneficiary.staking_part or sender.staking_part,
        network=sender.network,
    )

    ttl = 1200  # 1200 seconds

    tx_builder.ttl = context.last_block_slot + ttl

    mint_amount = 0

    for deadline, amount in vals:
        datum = new_vesting_datum(beneficiary, deadline)
        tx_output = TransactionOutput(output_address, amount, datum=datum)
        tx_builder.add_output(tx_output)

        delta = deadline - datetime.now() - timedelta(seconds=ttl)
        mint_amount += delta.days * amount

    if mint:
        print(script_hash(mint_utxo.output.script))
        tx_builder.add_minting_script(mint_utxo, Redeemer(RedeemerTag.MINT, data=1))
        tokens = MultiAsset.from_primitive({
            script_hash(mint_utxo.output.script).payload: {b"": mint_amount}
        })

        tx_builder.mint = tokens

        mint_output = TransactionOutput(sender, Value(1000000, tokens))
        mint_output.amount.coin = min_lovelace_post_alonzo(mint_output, context)
        tx_builder.add_output(mint_output)

        mint_fee_output = TransactionOutput(fee_address, 10000000-1)
        tx_builder.add_output(mint_fee_output)

    return tx_builder.build_and_sign([])


def vest(beneficiary: Address, script_utxo: UTxO, context: ChainContext):
    """
    Spend vestable utxos
    """
    pass