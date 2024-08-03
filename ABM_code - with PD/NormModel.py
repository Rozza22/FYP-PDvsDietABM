from NormTheory import NormTheory
import repast4py

from NormMediator import NormMediator
from core.Model import Model
from core import Globals
from DescriptiveNormEntity import DescriptiveNormEntity

from EatingPlan import EatingSchema

class NormModel(Model):
    def __init__(self, comm, props):
        super().__init__(comm, props)
        strNDaysDes = props["norms.n.days.descriptive"]
        strPerceptionBias = props["bias.factor"]
        self.info = []
        self.mIndexId = 0
        self.countSpawnPerYear = 0
    
        if (strNDaysDes):
            N_DAYS_DESCRIPTIVE = float(strNDaysDes); 
        if (strPerceptionBias):
            PERCEPTION_BIAS = float(strPerceptionBias)
    
        #read parameters from file and store in a table for init agents function (used by Model as well)
        #rank = repast4py.RepastProcess.instance().rank()
        rankFileNameProperty = "file.rank0"
        rankFileName = props[rankFileNameProperty]
        self.readRankFileForTheory(rankFileName)
    
        #dummy list for Des Norm
        regulatorListDes = []
        powerListDes = []
    
        #create structural entities
        mIntervalDesNorm = int(props["transformational.interval.descriptive.norm"])
        pDesNorm = DescriptiveNormEntity(regulatorListDes, powerListDes, mIntervalDesNorm, self.context)
        self.structuralEntityList.append(pDesNorm)
        self.countDiePerYear = 0
    
        #Norm outputs (1 core)
        if (self.THEORY_SPECIFIC_OUTPUT):
            annualFileName = addUniqueSuffix("outputs/annual_norm_output.csv")
            self.mpAnnualNormOutputFile = fopen(annualFileName.c_str(),"w")
            fprintf(self.mpAnnualNormOutputFile,"%s,%s,","Year","Tick")
            for i in range(0, Globals.NUM_SEX):
                for j in range(0, Globals.NUM_AGE_GROUPS):
                    for k in range(0, Globals.NUM_SCHEMA):
                        fprintf(self.mpAnnualNormOutputFile,"%s%d%d%d,","DesPrev", i, j, k)
            for i in range(0, Globals.NUM_SEX):
                for j in range(0, Globals.NUM_AGE_GROUPS):
                    fprintf(self.mpAnnualNormOutputFile,"%s%d%d,","PopulationCount", i, j)
            fprintf(self.mpAnnualNormOutputFile,"\n")
    
    def destructor(self): 
        if (Globals.THEORY_SPECIFIC_OUTPUT and self.mpAnnualNormOutputFilenot == NULL):
             fclose(self.mpAnnualNormOutputFile)
    
    #read parameters from file and store in a table
    def readRankFileForTheory(self, rankFileName): 
        try:
            with open(rankFileName, "r") as myfile:
                #read the csv file
                self.infoTable = self.readCSV(myfile)
                myfile.close()
        except IOError:
            print("Unable to open file: " + rankFileName)
    
    def initMediatorAndTheoryFromFile(self, agent, info):
        autonomy = agent.getAutonomy()
        emotional = agent.getEmotional()
        restraint = agent.getRestraint()
        PD = agent.getPD()
        bmival = agent.getbmival()
        age = agent.getAge()
        sex = agent.getSex()

        #create theory(ies) and a mediator for each agent
        theoryList = []
        # normTheory = NormTheory(self.context, self.structuralEntityList[0], autonomy, emotional, restraint, PD, bmival, age, sex)
        normTheory = NormTheory(self.context, self.structuralEntityList[0], autonomy, emotional, restraint, age, sex, PD, bmival, id)
        theoryList.append(normTheory)
        mediator = NormMediator(theoryList)

        #link agent with the mediator
        agent.setMediator(mediator)
        normTheory.initDesires()
    
    def initForTheory(self, runner):
        #update avg eating values before 1st situational mechanism
        self.structuralEntityList[0].updateDescriptiveGroupEating()
    #PLAY AROUND WITH THESE IF STATEMENTS RUN SIMULATION WITH THIS SWITCHED ON AND OFF
        #If low impulsivity (<0.3) and high autonomy (>0.7), desire = habit.
        #Otherwise, randomly copy another agents within reference group.
        #(pick 100 random agents, if one is with the same reference group, copy desire arrays)
        for agent in self.context.agents():
            #get norm theory to get autonomy
            pNormTheory1 = NormTheory
            if (not agent.getTheory(pNormTheory1)):
                print("initForTheory. Agent has no norm theory.")
            else:
                pNormTheory1 = agent.getTheory(NormTheory)
    
    def addUniqueSuffix(self, fileName):
    
        if (not boost.filesystem.exists(fileName)): #check if the file doesn't exist.
            return fileName                       #if not, return the filename as is.
    
        else:
            #I will assume that if the file basename doesn't exists, none of the numbered versions do either.
            #If numbered versions exist while the base fileName file doens't exist, self will overwrite them.
            i = 1
            noExtensionFileName = fileName
            for count in range(0, 4):
                noExtensionFileName.pop_back()
            testFileName = noExtensionFileName + "_" + to_string(i) + ".csv"
            while (boost.filesystem.exists(testFileName)):
                i += 1
                testFileName = noExtensionFileName + "_" + to_string(i) + ".csv"
            return testFileName
    
    def writeAnnualTheoryDataToFile(self):
        #if (Globals.THEORY_SPECIFIC_OUTPUT and annualNormOutput.is_open()){
        if (Globals.THEORY_SPECIFIC_OUTPUT and self.mpAnnualNormOutputFilenot ==NULL):
            size = P_REFERENCE_GROUP.size()
    
            fprintf(self.mpAnnualNormOutputFile,"%d,",simYear)
            fprintf(self.mpAnnualNormOutputFile,"%d,",floor(repast4py.RepastProcess.instance().getScheduleRunner().currentTick()))
            pDesNormEntity = structuralEntityList[0]
            for i in range(0, Globals.NUM_SEX):
                for j in range(0, Globals.NUM_AGE_GROUPS):
                    tempPrevArr = pDesNormEntity.getAvgPrevalence(P_REFERENCE_GROUP.getId(i,j))
                    for k in range(0, Globals.NUM_SCHEMA):
                        fprintf(self.mpAnnualNormOutputFile,"%.5f,",tempPrevArr[k])
            
            count[size] = {0}
            for agent in context:
                count[P_REFERENCE_GROUP.getId(agent.getSex(), agent.findAgeGroup())] += 1
            
            for i in range(0, size):
                fprintf(self.mpAnnualNormOutputFile,"%d,",count[i])
            fprintf(self.mpAnnualNormOutputFile,"\n")
    
            pDesNormEntity.mTransformationalTriggerCount = 0
        else:
            print("Error: Can't write to annual_norm_output.csv, file not open.")
