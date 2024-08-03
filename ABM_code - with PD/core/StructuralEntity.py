
from __future__ import annotations
from typing import Dict, Tuple, List
from abc import abstractmethod, ABCMeta

import repast4py

from .Regulator import Regulator

# ABCMeta is how to define abstract base classes in python
class StructuralEntity(metaclass=ABCMeta):

    def __init__(self, regulatorList, powerList, transformationInterval):
        self.mpRegulatorList: List[Regulator] = regulatorList
        self.mpPowerList = powerList
        self.mpPTansformational_interval = transformationInterval
        
        # These are not used in the food model
        self.mTransformationalInterval = 1
        self.mTransformationalTriggerCount = 1

    # Python does not support overloaded constructors, classmethod is a nice alternative
    @classmethod # allows for class funtion to be carried out before an object/agent is created
    def ctor(cls, regulator_list: List[Regulator], transformational_interval: int) -> StructuralEntity:
        instance = cls()
        instance.mpRegulatorList = regulator_list
        instance.mpTransformationalInterval =  transformational_interval
        return instance

    @abstractmethod
    def do_transformation(self, tick):
        pass
