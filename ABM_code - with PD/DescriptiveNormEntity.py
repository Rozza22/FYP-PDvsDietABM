from core.StructuralEntity import StructuralEntity
from core import Globals
import NormGlobals

class DescriptiveNormEntity(StructuralEntity):

    def __init__(self, regulatorList, powerList, transformationalInterval, context):
        super().__init__(regulatorList, powerList, transformationalInterval)
        self.mpContext = context
    
        #init norm arrays
        size = Globals.P_REFERENCE_GROUP.size()
        self.avgPrevalence = []
        for i in range(0, size):
            self.avgPrevalence.append({})
    
    def updateDescriptiveGroupEating(self):
        size = Globals.P_REFERENCE_GROUP.size()
    
        #reset avg arrays
        for i in range(0, size):
            for j in range(0, Globals.NUM_SCHEMA):
                self.avgPrevalence[i][j] = 0
    
        #create arrays to count agents in a group
        pCountInGroup = []
        for i in range(0, size):
            pCountInGroup.append(0)
    
        #loop through agents to calculate average
        # Sort and shuffle the vector to address reproducibillity issues
        #self.sortAndShuffleAgentPointers(agents)
        # TODO: Don't see why this needed to be randomised?
        for agent in self.mpContext.agents():
            sex = agent.getSex()
            # PD = agent.getPD() # added in
            ageGroup = agent.findAgeGroup()
            ses = agent.getSes()
            groupId = Globals.P_REFERENCE_GROUP.getId(sex, ageGroup, ses)
    
            countSchemas = agent.getSchemaCountOverNDays(NormGlobals.N_DAYS_DESCRIPTIVE)
            for schemaId in range(0, Globals.NUM_SCHEMA):
                self.avgPrevalence[groupId][schemaId] += countSchemas[schemaId] / NormGlobals.N_DAYS_DESCRIPTIVE
    
            pCountInGroup[groupId] += 1
    
        #caculate aggregate info
        #printf("Tick %.1f - Des norm entity\n", repast::RepastProcess::instance()->getScheduleRunner().currentTick());
        for i in range(0, size):
            for j in range(0, Globals.NUM_SCHEMA):
                if (pCountInGroup[i] == 0):
                    self.avgPrevalence[i][j] = 0
                else:
                    self.avgPrevalence[i][j] /= pCountInGroup[i]
                #printf("%.2f\t",avgPrevalence[i][j]);
            #printf("\n");
    
    def do_transformation(self, tick):
        currentTick = tick
        if (currentTick % self.mTransformationalInterval == 0):
            self.mTransformationalTriggerCount += 1
            self.updateDescriptiveGroupEating() #update avg eating values
    
    def getAvgPrevalence(self, groupId):
        return self.avgPrevalence[groupId]
