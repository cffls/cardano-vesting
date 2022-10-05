from dataclasses import dataclass
from datetime import datetime

from pycardano import Address, PlutusData


@dataclass
class VestingDatum(PlutusData):
    beneficiary: bytes
    granter: bytes
    cancellable: int
    deadline: int
    min_vest_amount: int

    def deadline_in_datetime(self) -> datetime:
        return datetime.fromtimestamp(self.deadline // 1000)


def new_vesting_datum(beneficiary: Address,
                      granter: Address,
                      cancellable: bool,
                      deadline: datetime,
                      min_vest_amount: int) -> VestingDatum:
    return VestingDatum(beneficiary.payment_part.payload,
                        granter.payment_part.payload,
                        int(cancellable),
                        int(deadline.timestamp() * 1000),
                        min_vest_amount)
