import os

from pycardano import Address, PaymentKeyPair, Network

key_dir = "keys"

if not os.path.isdir(key_dir):
    os.mkdir(key_dir)
else:
    print(f"Directory {key_dir} already exists")
    exit(1)

key_pair = PaymentKeyPair.generate()

key_pair.signing_key.save(os.path.join(key_dir, "payment.skey"))
key_pair.verification_key.save(os.path.join(key_dir, "payment.vkey"))

mainnet_address = Address(payment_part=key_pair.verification_key.hash(), network=Network.MAINNET)
with open(os.path.join(key_dir, "payment.addr"), "w") as f:
    f.write(str(mainnet_address))

with open(os.path.join(key_dir, "payment.pkh"), "w") as f:
    f.write(mainnet_address.payment_part.payload.hex())

testnet_address = Address(payment_part=key_pair.verification_key.hash(), network=Network.TESTNET)
with open(os.path.join(key_dir, "testnet.addr"), "w") as f:
    f.write(str(testnet_address))
