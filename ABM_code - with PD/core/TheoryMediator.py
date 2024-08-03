import math
from re import I

from core.Agent import Agent
from core.Theory import Theory
from core import Globals
from abc import abstractmethod, ABCMeta
from EatingPlan import *
import sys
import repast4py

class TheoryMediator(metaclass=ABCMeta):

    def __init__(self, theoryList):
        self.theoryList = theoryList
        self.habitProbability = []
        self.intentionProb = []
        for i in range(0, Globals.NUM_SCHEMA):
            self.intentionProb.append(0.0)
    
    def linkAgent(self, agent):
        #link this mediator to agent
        self.agent = agent
    
        #link each theory to agent
        for theory in self.theoryList:
            theory.setAgent(agent)
       
    # /**
    #  * @details Calculate log odds for each schema, then calculate probability for each schema.
    #  */
    def updateIntentionProbabilities(self):
        #calculate logodds
        logOdds = [] 
        totalLogOdds = 0.0
        for i in range(0, Globals.NUM_SCHEMA):
            temp = Globals.BETA_ATTITUDE * self.attitude[i] + Globals.BETA_NORM * self.norm[i] + Globals.BETA_PBC * self.pbc[i]

            #deal with exp(temp) is out of range for double
            try:
                logOdds.append(math.exp(temp))
            except OverflowError:
                logOdds.append(sys.float_info.max)
    
            totalLogOdds += logOdds[i]
    
        #calculate probabilities from logodds
        total = 0
        for i in range(0, Globals.NUM_SCHEMA):
            self.intentionProb[i] = 0 if (totalLogOdds==0) else logOdds[i] / totalLogOdds
        
    
    
    # /**
    #  * @brief Do propotional selection to choose a schema.
    #  * 
    #  * @return The EatingPlan with schema and its associated probability
    #  */
    def getProportionalSelectionIntentionPlan(self):
        #random a number from 0 -> total
        total = 0
        for i in range(0, Globals.NUM_SCHEMA):
            total += self.intentionProb[i]
        if (total==0):
            plan = EatingPlan()
            plan.schema = EatingSchema.ABSTAIN
            plan.probability = 1
            return plan
        rand = repast4py.random.default_rng.random() * total
        
        #do the propotional selection
        plan = EatingPlan()
        cummulativeProb = 0
        for i in range(0, Globals.NUM_SCHEMA):
            if (self.intentionProb[i] > 0): 
                cummulativeProb += self.intentionProb[i]
                if (rand <= cummulativeProb):
                    plan.probability = self.intentionProb[i]
                    plan.schema = EatingSchema(i)
                    return plan
                    if (plan.schema==EatingSchema.NONE):
                        print("Error")
        
        return plan

    # /**
    #  * @brief Choose a schema with max probability.
    #  * 
    #  * @return The EatingPlan with schema and its associated probability
    #  */
    def getMaxIntentionPlan(self):
        #search for max prob
        maxIntentionProb = -1
        maxIntentionIndex = -1
        for i in range(0, Globals.NUM_SCHEMA):
            if (self.intentionProb[i] > maxIntentionProb):
                maxIntentionProb = self.intentionProb[i]
                maxIntentionIndex = i
    
        plan = EatingPlan()
        plan.schema = EatingSchema(maxIntentionIndex)
        plan.probability = maxIntentionProb
    
        return plan
    
    def setIntentionProbability(self, i, value):
        # Ensure array is long enough
        while (len(self.habitProbability) < i + 1):
            self.habitProbability.append(0.0)
        self.habitProbability[i] = value

    def getTheory(self, derivedTheory):
        for theory in self.theoryList:
            if (isinstance(theory, derivedTheory)):
                return theory