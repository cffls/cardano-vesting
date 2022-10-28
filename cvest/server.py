from datetime import datetime

from flask import Flask, render_template, request

from pycardano import (
    Address,
    UTxO,
    VerificationKeyHash,
)

import cvest.offchain as oc

app = Flask(__name__, template_folder='../frontend', static_folder='../frontend/static')


def format_utxo(utxo: UTxO):
    return {
        "tx_hash": utxo.input.transaction_id.payload.hex(),
        "tx_index": utxo.input.index,
        "amount": utxo.output.amount.coin,
        "deadline": datetime.utcfromtimestamp(int(utxo.output.datum.deadline/1000)).strftime('%Y-%m-%d %H:%M:%S'),
        "cancellable": utxo.output.datum.cancellable == 1,
        "granter": str(Address(VerificationKeyHash(utxo.output.datum.granter), network=oc.NETWORK)),
        "beneficiary": str(Address(VerificationKeyHash(utxo.output.datum.beneficiary), network=oc.NETWORK) if utxo.output.datum.beneficiary else ""),
        "beneficiary_script": str(Address(VerificationKeyHash(utxo.output.datum.beneficiary_script.hex()), network=oc.NETWORK) if utxo.output.datum.beneficiary_script else ""),
        "min_vest_amount": utxo.output.datum.min_vest_amount,
    }


@app.route("/get_grants")
def get_grants():
    addresses = [Address.from_primitive(bytes.fromhex(sender)) for sender in request.args.getlist("address")]

    grants = oc.get_grants(addresses)

    return {"results": [format_utxo(grant) for grant in grants]}


@app.route("/get_vests")
def get_vests():
    addresses = [Address.from_primitive(bytes.fromhex(sender)) for sender in request.args.getlist("address")]

    vests = oc.get_pending(addresses)

    return {"results": [format_utxo(vest) for vest in vests]}


@app.route("/")
def home_page():
    return render_template("index.html")