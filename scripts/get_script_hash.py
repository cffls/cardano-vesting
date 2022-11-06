import json
import os

import cbor2
from pycardano import PlutusV1Script, PlutusV2Script, script_hash


def get_env_val(key):
    val = os.environ.get(key)
    if not val:
        raise Exception(f"Environment variable {key} is not set!")
    return val


with open(get_env_val("SCRIPT_PATH"), "r") as f:
    script_json = json.load(f)
    script = cbor2.loads(bytes.fromhex(script_json["cborHex"]))
    if script_json["type"] == "PlutusScriptV1":
        print(script_hash(PlutusV1Script(script)))
    elif script_json["type"] == "PlutusScriptV2":
        print(script_hash(PlutusV2Script(script)))
    else:
        raise Exception(f"Unknown script type: {script_json['type']}")
