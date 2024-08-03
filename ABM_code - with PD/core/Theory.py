import math
from core.Globals import *
from abc import abstractmethod, ABCMeta

class Theory(metaclass=ABCMeta):
    def __init__(self):
        self._agent = None

    def setAgent(self, agent):
        self._agent = agent
    
    def generateCorrectedMeanSD(self, desiredMean, desiredSd):
        hashKey = None
        localPair = (0.0, 0.0)
    
        if (desiredMean < 0):
            std.cerr << "drink mean must be >= 0. " << desiredMean << " , " << desiredSd << std.endl
            desiredMean = 0
        if (desiredSd < 0):
            std.cerr << "drink sd must be >= 0. " << desiredMean << " , " << desiredSd << std.endl
            desiredSd = 0
    
        #quick fix when sd too small ~0
        if (desiredSd < .05):
            return (desiredMean, 0)
    
        #quick fix for lookup table: desireMean not exist for [0..1]
        if (desiredMean < 1):
            return (desiredMean, desiredSd)
    
        #quick fix for lookup table: mean in range but sd out of range
        if (round(desiredMean) < 10 and desiredMean >=1 and desiredSd>3.15):
            return (desiredMean, desiredSd)
    
        if ((round(desiredMean) < 10 and desiredMean >=1 and desiredSd<=3.15 and desiredSd > 0.05) or (desiredMean > 10 and desiredSd <= 0.7 and desiredSd > 0.05)):
            localPair = self.doLookup(desiredMean, desiredSd)
            return localPair
        else:
            localPair = self.doFunctionLookup(desiredMean, desiredSd)
            return localPair
    
    def doLookup(self, mean, sd):
        roundedMean = round(mean *10) #x10 then rounded. eg 2.1 => 21
        roundedSD = ((round(sd*100) + 5/2) / 5) * 5 #x100 then round to the nearest 5. eg 0.11 => 10 or 0.14 => $
        #if (roundedSD == 0) roundedSD = 5
        hashKey = roundedMean * 1000 + roundedSD
        try:
            return Globals.MEAN_SD_LOOKUP_TABLE.at(hashKey)
        except(oor):
            print("cerr")
            #std::cerr << mean <<" "<< sd << " => " << roundedMean <<" "<< roundedSD <<
            #    " => "<< hashKey << " hashkey not in MEAN_SD_LOOKUP_TABLE map. " << std::endl

    def doFunctionLookup(self, desiredMean, desiredSd):
        intercept = .9931*desiredMean - 0.7776
        quadCoeff = .3613 + 0.66*exp(-0.66*(desiredMean - 6.54))
        linearCoeff = .3957-exp(-7.43*(desiredMean - 0.623))
        newMean = quadCoeff*desiredSd*desiredSd + linearCoeff*desiredSd + intercept
        newSD = 1.7245*desiredSd - 0.592
        return (newMean, newSD)
    
    def safeOdds(self, numerator, denominator):
        smallNumber = 1e-100
        if (denominator<smallNumber):
            return (numerator + smallNumber) / (denominator + smallNumber)
        else:
            return numerator / denominator
    
    def safeLog(self, value):
        smallNumber = 1e-100
        if (value<0):  #throw error if negative (because of log)
            fprintf(stderr, "Input value for log function is negative: %.2f.\n", value)
        retVal = math.log(smallNumber) if value == 0 else math.log(value)
        return retVal