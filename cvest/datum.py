from dataclasses import dataclass
from datetime import datetime

from pycardano import Address, PlutusData


@dataclass
class VestingDatum(PlutusData):
    beneficiary: bytes
    deadline: int


def new_vesting_datum(address: Address, deadline: datetime) -> VestingDatum:
    return VestingDatum(address.payment_part.payload, int(deadline.timestamp() * 1000))
