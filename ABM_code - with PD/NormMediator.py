from core.TheoryMediator import TheoryMediator
from core import Globals
from EatingPlan import EatingSchema
from EatingPlan import EatingPlan
import NormGlobals
from mpi4py import MPI
import repast4py
from repast4py import context, schedule



class NormMediator(TheoryMediator):
    def __init__(self, theoryList): 
        super().__init__(theoryList)
        self.normTheory = theoryList[0]
        #self.contagionTheory = theoryList[1]
        self.attitude = []
        self.norm = []
        self.pbc = []

    
    def mediateSituation(self):
        for theory in self.theoryList:
            theory.doSituation()
    
    def mediateAction(self):
        # mediate thought pathway: calc TPB
        self.mediateThoughtPathway()
        self.updateIntentionProbabilities()
    
    def mediateNonEatingActions(self):
        pass
    
    # def mediateThoughtPathway(self, agent):
    def mediateThoughtPathway(self):
        if not self.attitude:
            for i in range(0, Globals.NUM_SCHEMA):
                self.attitude.append(None)
                self.norm.append(0.0)
                self.pbc.append(0)

        # self.attitude = self.normTheory.getAttitude(EatingSchema)
        for i in range(0, Globals.NUM_SCHEMA):
            schema = EatingSchema(i)
            # eatingPlan = EatingPlan()
            self.attitude[i] = self.normTheory.getAttitude(schema)
            #self.norm[i] = NormGlobals.NORM_THEORY_WEIGHT*self.normTheory.getNorm(schema) + (1-NormGlobals.NORM_THEORY_WEIGHT)*self.contagionTheory.getNorm(schema)
            self.norm[i] = self.normTheory.getNorm(schema)
            # could add in function here for changing PD based off what agents are eating
            self.pbc[i] = self.normTheory.getPerceivedBehaviourControl(schema)
            # self.bmiValue[i] = self.normTheory.getbmiValue(schema)

    def updateBMIpd(self, eatingPlan):
        # schema = EatingSchema(i)
        self.bmival = self.normTheory.updateBMIval(eatingPlan)
        self.PD = self.normTheory.updatePDval()
        # self.normTheory.PDintervention() # TURN OFF FOR CALIBRATION
        return self.bmival, self.PD
