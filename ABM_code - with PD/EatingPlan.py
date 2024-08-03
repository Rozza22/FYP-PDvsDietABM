from enum import IntEnum

class EatingSchema(IntEnum):
    NONE=-1
    VERY_LOW=0
    LOW=1
    MED=2
    HIGH=3
    VERY_HIGH=4

class EatingPlan:
    def __init__(self):
        self.schema = EatingSchema.NONE
        self.probability = 0.0
