import json
import os
import pickle

import cbor2
from pycardano import (
    Address,
    AlonzoMetadata,
    AuxiliaryData,
    BlockFrostChainContext,
    Metadata,
    Network,
    PaymentSigningKey,
    PaymentVerificationKey,
    PlutusV2Script,
    TransactionBuilder,
    TransactionInput,
    TransactionOutput,
    UTxO,
    min_lovelace_post_alonzo,
)

def get_env_var(key):
    val = os.environ.get(key, None)
    if val is None:
        raise Exception(f"Couldn't find env variable: {key}!")
    return val


network = Network.TESTNET

_network = "preview"

context = BlockFrostChainContext(
    get_env_var("BLOCKFROST_PREPROD")
    if _network == "preprod"
    else get_env_var("BLOCKFROST_PREVIEW"),
    network=network,
    base_url="https://cardano-preprod.blockfrost.io/api"
    if _network == "preprod"
    else "https://cardano-preview.blockfrost.io/api",
)


owner_skey = PaymentSigningKey.load("keys/payment.skey")
owner_vkey = PaymentVerificationKey.from_signing_key(owner_skey)

owner_address = Address(owner_vkey.hash(), network=network)

tx_builder = TransactionBuilder(context)

tx_builder.add_input_address(owner_address)

with open("contract/vesting-plutusV2.plutus", "r") as f:
    val = json.load(f)
    vest_script = PlutusV2Script(cbor2.loads(bytes.fromhex(val["cborHex"])))

vest_script_output = TransactionOutput(
    address=owner_address, amount=1000000, script=vest_script
)
vest_script_output.amount = min_lovelace_post_alonzo(vest_script_output, context)

tx_builder.add_output(vest_script_output)


with open("contract/mint-plutusV2.plutus", "r") as f:
    val = json.load(f)
    mint_script = PlutusV2Script(cbor2.loads(bytes.fromhex(val["cborHex"])))

mint_script_output = TransactionOutput(
    address=owner_address, amount=1000000, script=mint_script
)
mint_script_output.amount = min_lovelace_post_alonzo(mint_script_output, context)

tx_builder.add_output(mint_script_output)

tx_builder.auxiliary_data = AuxiliaryData(
    AlonzoMetadata(
        metadata=Metadata(
            {
                674: {
                    "msg": [
                        "ADA Vest contracts",
                        "https://github.com/cffls/cardano-vesting",
                    ]
                }
            }
        )
    )
)


tx = tx_builder.build_and_sign([owner_skey], change_address=owner_address)

print(tx)

context.submit_tx(tx.to_cbor())

print("Transaction submitted successfully")

vest_script_utxo = UTxO(TransactionInput(tx.id, 0), vest_script_output)

with open("plutus_vest_script_utxo.pickle", "wb") as f:
    pickle.dump(vest_script_utxo, f)

mint_script_utxo = UTxO(TransactionInput(tx.id, 1), mint_script_output)

with open("plutus_mint_script_utxo.pickle", "wb") as f:
    pickle.dump(mint_script_utxo, f)

print(
    "Plutus script UTxO is saved to plutus_vest_script_utxo.pickle and plutus_mint_script_utxo.pickle"
    "They could be used as reference script for other transactions."
)
