from dataclasses import dataclass
from datetime import datetime

from pycardano import Address, AddressType, PlutusData


@dataclass
class VestingDatum(PlutusData):
    beneficiary: bytes
    beneficiary_script: bytes
    granter: bytes
    cancellable: int
    deadline: int
    min_vest_amount: int

    def deadline_in_datetime(self) -> datetime:
        return datetime.utcfromtimestamp(self.deadline // 1000)


def new_vesting_datum(beneficiary: Address,
                      granter: Address,
                      cancellable: bool,
                      deadline: datetime,
                      min_vest_amount: int) -> VestingDatum:
    if beneficiary.address_type.name.startswith("SCRIPT"):
        return VestingDatum(b"",
                            beneficiary.payment_part.payload,
                            granter.payment_part.payload,
                            int(cancellable),
                            int(deadline.timestamp() * 1000),
                            min_vest_amount)
    else:
        return VestingDatum(beneficiary.payment_part.payload,
                            b"",
                            granter.payment_part.payload,
                            int(cancellable),
                            int(deadline.timestamp() * 1000),
                            min_vest_amount)
