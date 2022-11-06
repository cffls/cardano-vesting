from datetime import datetime

import pycardano
from flask import Flask, request
from flask_cors import CORS, cross_origin
from pycardano import (
    Address,
    Transaction,
    TransactionWitnessSet,
    UTxO,
    VerificationKeyHash,
)

import cvest.offchain as oc

app = Flask(__name__, template_folder="../frontend", static_folder="../frontend/static")

cors = CORS(app)
# app.config['CORS_HEADERS'] = 'Content-Type'


def format_utxo(utxo: UTxO):
    return {
        "tx_hash": utxo.input.transaction_id.payload.hex(),
        "tx_index": utxo.input.index,
        "amount": utxo.output.amount.coin,
        "deadline": utxo.output.datum.deadline_in_datetime().strftime(
            "%Y-%m-%dT%H:%M:%S"
        ),
        "cancellable": utxo.output.datum.cancellable == 1,
        "granter": str(
            Address(VerificationKeyHash(utxo.output.datum.granter), network=oc.NETWORK)
        ),
        "beneficiary": str(
            Address(
                VerificationKeyHash(utxo.output.datum.beneficiary), network=oc.NETWORK
            )
            if utxo.output.datum.beneficiary
            else ""
        ),
        "beneficiary_script": str(
            Address(
                VerificationKeyHash(utxo.output.datum.beneficiary_script.hex()),
                network=oc.NETWORK,
            )
            if utxo.output.datum.beneficiary_script
            else ""
        ),
        "min_vest_amount": utxo.output.datum.min_vest_amount,
    }


def get_addr_from_hex(addr):
    try:
        return Address(VerificationKeyHash(bytes.fromhex(addr)), network=oc.NETWORK)
    except Exception as e:
        return pycardano.Address.decode(bytes.fromhex(addr))


def clean_addresses(addresses):
    clean_addrs = []
    for addr in addresses:
        if "," in addr:
            clean_addrs.extend(addr.split(","))
        else:
            clean_addrs.append(addr)

    return [get_addr_from_hex(sender) for sender in clean_addrs if len(sender) <= 114]


@app.route("/get_grants")
@cross_origin()
def get_grants():
    addresses = clean_addresses(request.args.getlist("address"))

    grants = oc.get_grants(addresses)

    return {"results": [format_utxo(grant) for grant in grants]}


@app.route("/get_vests")
@cross_origin()
def get_vests():
    addresses = clean_addresses(request.args.getlist("address"))

    vests = oc.get_pending(addresses)

    return {"results": [format_utxo(vest) for vest in vests]}


@app.route("/create_grants", methods=["POST"])
@cross_origin()
def create_grants():
    data = request.json
    senders = [get_addr_from_hex(sender) for sender in data["senders"]]

    grants = [
        (
            Address.from_primitive(grant["address"]),
            datetime.fromtimestamp(grant["deadline"] / 1000),
            int(float(grant["amount"]) * 1000000),
        )
        for grant in data["grants"]
    ]

    tx = oc.create_grant_tx(
        senders,
        grants,
        can_cancel=True,
        change_address=get_addr_from_hex(data["change_address"]),
    )

    return {"tx": tx.to_cbor()}


def compose_tx_and_witness(data):
    tx = Transaction.from_cbor(data["tx"])
    witness = TransactionWitnessSet.from_cbor(data["witness"])
    tx.transaction_witness_set = witness
    return tx


@app.route("/submit_tx", methods=["POST"])
@cross_origin()
def submit_tx():
    tx = compose_tx_and_witness(request.json)
    tx_id = tx.transaction_body.hash().hex()
    print(f"Transaction: \n {tx}")
    print(f"Transaction cbor: {tx.to_cbor()}")
    print(f"Transaction ID: {tx_id}")
    oc.context.submit_tx(tx.to_cbor())
    return {"tx_id": tx_id}
