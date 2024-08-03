from re import L
import os.path
from AdvertisingScalingNormModifier import AdvertisingScalingNormModifier
from core import Globals
from core.StatisticsCollector import StatisticsCollector
from enum import Enum
from datetime import datetime
import repast4py
from repast4py import context, schedule
from core.Agent import Agent
from .SVDataSet import SVDataSet

#states of a character in CSV file
class CSVState(Enum):
    UnquotedField = 1
    QuotedField = 2
    QuotedQuote = 3

class Model:
    
    def __init__(self, comm, params):
        self.props = params
        self.stopAt = self.props["stop.at"]
        self.countOfAgents = self.props["count.of.agents"]
        self.startYear = self.props["start.year"]
        self.simYear = 0
        self.schemaProfilesTable = {}
        self.context = repast4py.context.SharedContext(comm) # Encapsulates a population of agents on a single process rank.
        self.structuralEntityList = []
        self.infoTable = []
        
        self.__runner: repast4py.schedule.SharedScheduleRunner = repast4py.schedule.init_schedule_runner(comm) # Initializes the default schedule runner, a dynamic schedule of executable events shared and synchronized across processes.
    
        #self.provider = AgentPackageProvider(context)
        #self.receiver = AgentPackageReceiver(context)
        self.startTime = datetime.now()
    
        #read flags and custom run settings
        self.MECHANISM_START_TICK = 1
        self.MECHANISM_INTERVAL_TICK = 1
        self.YEARLY_START_TICK = 0
        self.YEARLY_INTERVAL_TICK = 365
        self.THEORY_SPECIFIC_OUTPUT = self.props["theory.specific.output"]
        self.AGENT_LEVEL_OUTPUT = self.props["agent.level.output"]
        self.SPAWNING_ON = self.props["spawning.on"]
        self.PRINT_POPULATION_STAT_YEARLY = self.props["print.population.stat.yearly"]
        self.SINGLE_RUN_LIMIT_MIN = self.props["single.run.limit.minute"]
        self.FAIL_FAST = self.props["fail.fast.on"]
            
        #Read intervals for mechanisms
        self.ACTION_MECHANISM_INTERVAL_TICK = self.props["action.interval"]
        self.SITUATIONAL_MECHANISM_INTERVAL_TICK = self.props["situational.interval"]
    
        self.AGENT_INIT_OUTPUT = False
        
        # read in policy parameters
        Globals.ADVERTISING_POLICY_ENABLED = bool(self.props["advertising.policy.enabled"])
        Globals.ADVERTISING_POLICY_METHOD = self.props["advertising.policy.method"]
        Globals.ADVERTISING_POLICY_SCALING_FACTOR = float(self.props["advertising.policy.scaling.factor"])
        Globals.ADVERTISING_POLICY_SCALING_CATEGORIES_AFFECTED = int(self.props["advertising.policy.scaling.categories-affected"])
        Globals.ADVERTISING_POLICY_SCALING_ENABLE_AT_TICK = int(self.props["advertising.policy.scaling.enable.at.tick"])

        self.annualValues = None
    
        #read in ERFC lookup table
        # lookupFilename = self.props["compressed.lookup.file"]
        # self.readMeanSDLookupTable(lookupFilename)
    
        schemaProfilesFilename = self.props["schema.profiles.file"]
        self.readSchemaProfilesTable(schemaProfilesFilename)
        
        #custom-logging flag
        CUSTOM_LOG = 0 if not "custom.log" in self.props else self.props["custom.log"]
        if (CUSTOM_LOG):
            Logger("Starting...")
    
        #full path of annual data file
        self.ANNUAL_DATA_FILE = "annual_data" if not "annual.data.file" in self.props else self.props["annual.data.file"]
    
        #beta for calculating intention
        Globals.BETA_ATTITUDE = 1/3 if not "beta.attitude" in self.props else self.props["beta.attitude"]
        Globals.BETA_NORM = 1/3 if not "beta.norm" in self.props else self.props["beta.norm"]
        Globals.BETA_PBC = 1/3 if not "beta.pbc" in self.props else self.props["beta.pbc"]

        #beta for weighting the contribution of emotional eating and restraint and PD
        Globals.BETA_RESTRAINT = 1 if not "beta.restraint" in self.props else self.props["beta.restraint"]
        Globals.BETA_EMOTIONAL = 1 if not "beta.emotional" in self.props else self.props["beta.emotional"]
        Globals.BETA_PD = 1 if not "beta.PD" in self.props else self.props["beta.PD"]
        Globals.BETA_HFSSBMIVAL = 1 if not "beta.HFSSbmival" in self.props else self.props["beta.HFSSbmival"]
        Globals.BETA_BMIVALPD = 1 if not "beta.bmivalPD" in self.props else self.props["beta.bmivalPD"]
        Globals.BETA_BMIVALEMO = 1 if not "beta.bmivalEmo" in self.props else self.props["beta.bmivalEmo"]
        Globals.BETA_INTERVENTIONEFFECT = 1 if not "beta.interventionEffect" in self.props else self.props["beta.interventionEffect"]
        Globals.BETA_INTERVENTIONSCALE = 1 if not "beta.interventionScale" in self.props else self.props["beta.interventionScale"]
        Globals.ALPHA_ATTITUDE = 1 if not "alpha.attitude" in self.props else self.props["alpha.attitude"]
        Globals.ALPHA_BMI = 1 if not "alpha.PD" in self.props else self.props["alpha.PD"]
        Globals.ALPHA_PD = 1 if not "alpha.bmi" in self.props else self.props["alpha.bmi"]
        #Will have to put some PD stuff here, I think we call the props value from here for it - have added above
    
    def run(self):
        self.__runner.execute()


    def readMeanSDLookupTable(self, lookupFilename):
        try:
            with open(lookupFilename, "r") as myfile:
                #read the csv file
                localTable = self.readCSV(myfile)
                localTable
                myfile.close()

        except IOError:
            print("Unable to open lookup file: " + lookupFilename)
    
        #find index on header line.
        headerLine = localTable[0]
        localTable.pop()
    
        indexKey = self.findIndexInHeader("hashkey", headerLine)
        indexMean = self.findIndexInHeader("ERFC.mean", headerLine)
        indexSd = self.findIndexInHeader("ERFC.SD", headerLine)
        
        #read in variables and create map stored in globals.
        
        #read each line and put into a global table
        while (localTable): 
            localMap = localTable[0]
            localTable.pop(0)
    
            #read the variables from a row of localTable
            localKey = localMap[indexKey]
            localMean = localMap[indexMean]
            localSd = localMap[indexSd]
            localMeanSd = (localMean, localSd)
            #create a key/pair map from row.
            
            # Globals.MEAN_SD_LOOKUP_TABLE[localKey] = localMeanSd
    
    def destructor(self):
        if (self.CUSTOM_LOG):
            Logger("Maximum memory used: " + std.to_string(getMaxMemoryUsed()/1024) + "MB");#logging maximum memory use
        
        if (self.CUSTOM_LOG):
            elapsed = std.chrono.duration_cast<std.chrono.seconds>( std.chrono.steady_clock.now() - startTime )
            Logger("End - runtime: " + std.to_string(elapsed.count()) + "sec");#logging runtime
    
    def findIndexInHeader(self, string, headerLine): 
        if not string in headerLine:
            print("Index Not Found: {}", string)
            return -1
    
        # Found: return index of element from iterator
        return headerLine.index(string)
    
    def initialize(self):
        self.normaliseBMIColumn() # this is to normalise BMI
        self.initAgents() #Generate Agents (core)
        self.initStatisticCollector() #init (either here or overwrite by theory-specific model class)
        self.addStatistics() #add annual data collection
        if (self.AGENT_INIT_OUTPUT):
            self.writeInitAgentDataToFile()
        self.initSchedule(); #manange schedule (core)
        self.initForTheory(self.__runner); #manage and schedule data collection (theory specific)
    
    
    # Generating agents, theories, structural entities
    def initAgents(self):
        monthlyCaloriesSDPct = self.props["microsim.init.monthly.sd.pct"]
    
        #find index from the header line, ASSUMING the infoTable was already be read
        headerLine = self.infoTable[0]
        self.infoTable.pop(0)
    
        self.indexId = self.findIndexInHeader("ID", headerLine)
        self.indexSex = self.findIndexInHeader("Sex", headerLine)
        self.indexAge = self.findIndexInHeader("age", headerLine)
        self.indexRace = self.findIndexInHeader("eth_new", headerLine)
        self.indexBmival = self.findIndexInHeader("bmival", headerLine)
        self.indexEating = self.findIndexInHeader("microsim.init.eatingstatus", headerLine)
        self.indexMeanKcalNonHfss = self.findIndexInHeader("TotalKcal_M_0", headerLine)
        self.indexSDKcalNonHfss = self.findIndexInHeader("TotalKcal_SD_0", headerLine)
        self.indexMeanKcalHfss = self.findIndexInHeader("TotalKcal_M_1", headerLine)
        self.indexSDKcalHfss = self.findIndexInHeader("TotalKcal_SD_1", headerLine)
        self.indexTraitImpulsivity = self.findIndexInHeader("automaticity", headerLine)
        self.indexHabitUpdateInterval = self.findIndexInHeader("habit.update.interval", headerLine)
        self.indexSes = self.findIndexInHeader("SES", headerLine)
        self.indexHistCat1 = self.findIndexInHeader("history_0.200", headerLine)
        self.indexHistCat2 = self.findIndexInHeader("history_201.500", headerLine)
        self.indexHistCat3 = self.findIndexInHeader("history_501.750", headerLine)
        self.indexHistCat4 = self.findIndexInHeader("history_751.1000", headerLine)
        self.indexHistCat5 = self.findIndexInHeader("history_1001.", headerLine)

        self.indexDesireCat1 = self.findIndexInHeader("desire_0.200", headerLine)
        self.indexDesireCat2 = self.findIndexInHeader("desire_201.500", headerLine)
        self.indexDesireCat3 = self.findIndexInHeader("desire_501.750", headerLine)
        self.indexDesireCat4 = self.findIndexInHeader("desire_751.1000", headerLine)
        self.indexDesireCat5 = self.findIndexInHeader("desire_1001.", headerLine)
        
        self.indexAutonomy = self.findIndexInHeader("autonomy", headerLine)
        self.indexRestraint = self.findIndexInHeader("restraint", headerLine)
        self.indexEmotional = self.findIndexInHeader("emotional", headerLine)
        self.indexPD = self.findIndexInHeader("PD", headerLine)

        self.simDay = 0
    
        #ignore first line, and assume the rest of lines = countOfAgents in model.props
        sizes = self.context.size()
        while (self.infoTable and self.context.size()[-1] < self.countOfAgents): 
            info = self.infoTable[0]
            self.infoTable.pop(0)
    
            #read the variables from a row of infoTable
            tempId = int(info[self.indexId])
            sex = info[self.indexSex]
            age = int(info[self.indexAge])
            race = info[self.indexRace]
            bmival = float(info[self.indexBmival])
            eating = info[self.indexEating]
            traitImpulsivity = info[self.indexTraitImpulsivity]
            habitUpdateInterval = info[self.indexHabitUpdateInterval]
            ses = info[self.indexSes]
            year = self.simYear + self.startYear
            meanKcalNonHfss = float(info[self.indexMeanKcalNonHfss])
            sdKcalNonHfss = float(info[self.indexSDKcalNonHfss])
            meanKcalHfss = float(info[self.indexMeanKcalHfss])
            sdKcalHfss = float(info[self.indexSDKcalHfss])
            simDay = self.simDay
            #THIS IS WHERE THE HISTORY IS READ IN
            histCat1 = info[self.indexHistCat1]
            histCat2 = info[self.indexHistCat2]
            histCat3 = info[self.indexHistCat3]
            histCat4 = info[self.indexHistCat4]
            histCat5 = info[self.indexHistCat5]
            schemaProfiles = list(map(float, [histCat1, histCat2, histCat3, histCat4, histCat5]))
    
            desireCat1 = info[self.indexDesireCat1]
            desireCat2 = info[self.indexDesireCat2]
            desireCat3 = info[self.indexDesireCat3]
            desireCat4 = info[self.indexDesireCat4]
            desireCat5 = info[self.indexDesireCat5]
            desireProfiles = list(map(float, [desireCat1, desireCat2, desireCat3, desireCat4, desireCat5]))
            
            autonomy = float(info[self.indexAutonomy])
            emotional = float(info[self.indexEmotional])
            restraint = float(info[self.indexRestraint])
            PD = float(info[self.indexPD])

            #create an agent using a constructor with variables from file
            agent = Agent(tempId, sex, age, race, meanKcalNonHfss, sdKcalNonHfss, meanKcalHfss, sdKcalHfss, 
                    year, traitImpulsivity, habitUpdateInterval, ses, schemaProfiles, autonomy, emotional, restraint, simDay, PD, bmival)
            self.context.add(agent)
    
            #init mediators and theory
            self.initMediatorAndTheoryFromFile(agent, info)
            # TODO: This is a bit of a hack until code is restructured
            agent.mediator.theoryList[0].setDesires(desireProfiles)

            # Apply advertising reduction if enabled
            #if self.ADVERTISING_POLICY_ENABLED:
                #if self.ADVERTISING_POLICY_METHOD == "scaling":
                    #currentTick = self.__runner.schedule.tick 
                    #self.advertisingScalingNormModifier = AdvertisingScalingNormModifier(self.ADVERTISING_POLICY_SCALING_FACTOR, self.ADVERTISING_POLICY_SCALING_CATEGORIES_AFFECTED, self.ADVERTISING_POLICY_ENABLED, self.ADVERTISING_POLICY_SCALING_ENABLE_AT_TICK, currentTick)
                    #agent.mediator.theoryList[0].normModifiers.append(self.advertisingScalingNormModifier)
                    #self.__runner.schedule_event(self.ADVERTISING_POLICY_SCALING_ENABLE_AT_TICK, self.activateAdvertisingScalingPolicy)

          #to implement a new policy ->
          #make a new thing in the model
          #activate automaticity scaling
    
        #Init agents complete. Overwrite infoTable with spawn file data
        self.getReadyToSpawn()
        
    def activateAdvertisingScalingPolicy(self):
        self.advertisingScalingNormModifier.setActive(True)
    
    def initStatisticCollector(self):
        self.statisticsCollector = StatisticsCollector(self.context)

    def sanitizeFilename(self, filename):
        dotsRemoved = filename.replace('.', '_')
        slashesToUnderscores = dotsRemoved.replace('/', '_')
        return slashesToUnderscores
    
    def addStatistics(self): 
        # Counter to append unique id to end of duplicate file names
        counter = 1
        fileOutputName = self.ANNUAL_DATA_FILE + '_' + self.props["parameters_file"] + '_' + str(counter) + '_' + str(self.props["random.seed"]) 
        # fileOutputName = 'outputs/calibration/' + self.sanitizeFilename(fileOutputName) + '.csv' # This is path for calibration part
        # fileOutputName = 'outputs/calibrationNew/' + self.sanitizeFilename(fileOutputName) + '.csv' # This is path for calibration part
        # fileOutputName = 'outputs/interventionVariation/' + self.sanitizeFilename(fileOutputName) + '.csv' # This is path for interventionVariation part
        # fileOutputName = 'outputs/validation/' + self.sanitizeFilename(fileOutputName) + '.csv' # This is path for validation part
        fileOutputName = 'outputs/' + self.sanitizeFilename(fileOutputName) + '.csv' # This is path for optimal model or just random test

        # Search for a filename that doesn't exist already
        while(os.path.isfile(fileOutputName)):
            fileOutputName = fileOutputName.replace('_' + str(counter) + '_', '_' + str(counter + 1) + '_')
            counter += 1
        
        # Construct the dataset with the selected filename
        data_set = SVDataSet(fileOutputName, ",", self.__runner.schedule)
    
        outSumPopulation = self.statisticsCollector.getDataSource("Population")
        data_set.addDataSource(outSumPopulation)
       
        # outMeanPD = self.statisticsCollector.getDataSource("meanPD") # added in to try and print mean PD into output
        # data_set.addDataSource(outMeanPD)

        outMeanPDmale = self.statisticsCollector.getDataSource("meanPDmale") # added in to try and print mean PD into output
        data_set.addDataSource(outMeanPDmale)

        outMeanPDfemale = self.statisticsCollector.getDataSource("meanPDfemale") # added in to try and print mean PD into output
        data_set.addDataSource(outMeanPDfemale)

        # outPDyes = self.statisticsCollector.getDataSource("PDyes") # added in to try and print mean PD into output
        # data_set.addDataSource(outPDyes)

        outPDyesMale = self.statisticsCollector.getDataSource("PDyesMale") # added in to try and print mean PD into output
        data_set.addDataSource(outPDyesMale)

        outPDyesFemale = self.statisticsCollector.getDataSource("PDyesFemale") # added in to try and print mean PD into output
        data_set.addDataSource(outPDyesFemale)

        # outPDno = self.statisticsCollector.getDataSource("PDno") # added in to try and print mean PD into output
        # data_set.addDataSource(outPDno)

        outPDnoMale = self.statisticsCollector.getDataSource("PDnoMale") # added in to try and print mean PD into output
        data_set.addDataSource(outPDnoMale)

        outPDnoFemale = self.statisticsCollector.getDataSource("PDnoFemale") # added in to try and print mean PD into output
        data_set.addDataSource(outPDnoFemale)

        # outMeanBMI = self.statisticsCollector.getDataSource("meanBMI") # added in to try and print mean PD into output
        # data_set.addDataSource(outMeanPD)

        outMeanBMImale = self.statisticsCollector.getDataSource("meanBMImale") # added in to try and print mean PD into output
        data_set.addDataSource(outMeanBMImale)

        outMeanBMIfemale = self.statisticsCollector.getDataSource("meanBMIfemale") # added in to try and print mean PD into output
        data_set.addDataSource(outMeanBMIfemale)

        # outBMIgrp1 = self.statisticsCollector.getDataSource("bmiGrp1") # added in to try and print number of people in each bmi group
        # data_set.addDataSource(outBMIgrp1)

        outBMIgrp1Male = self.statisticsCollector.getDataSource("bmiGrp1Male") # added in to try and print number of people in each bmi group
        data_set.addDataSource(outBMIgrp1Male)

        outBMIgrp1Female = self.statisticsCollector.getDataSource("bmiGrp1Female") # added in to try and print number of people in each bmi group
        data_set.addDataSource(outBMIgrp1Female)

        # outBMIgrp2 = self.statisticsCollector.getDataSource("bmiGrp2")
        # data_set.addDataSource(outBMIgrp2)

        outBMIgrp2Male = self.statisticsCollector.getDataSource("bmiGrp2Male") # added in to try and print number of people in each bmi group
        data_set.addDataSource(outBMIgrp2Male)

        outBMIgrp2Female = self.statisticsCollector.getDataSource("bmiGrp2Female") # added in to try and print number of people in each bmi group
        data_set.addDataSource(outBMIgrp2Female)

        # outBMIgrp3 = self.statisticsCollector.getDataSource("bmiGrp3")
        # data_set.addDataSource(outBMIgrp3)

        outBMIgrp3Male = self.statisticsCollector.getDataSource("bmiGrp3Male") # added in to try and print number of people in each bmi group
        data_set.addDataSource(outBMIgrp3Male)

        outBMIgrp3Female = self.statisticsCollector.getDataSource("bmiGrp3Female") # added in to try and print number of people in each bmi group
        data_set.addDataSource(outBMIgrp3Female)

        # outBMIgrp4 = self.statisticsCollector.getDataSource("bmiGrp4")
        # data_set.addDataSource(outBMIgrp4)

        outBMIgrp4Male = self.statisticsCollector.getDataSource("bmiGrp4Male") # added in to try and print number of people in each bmi group
        data_set.addDataSource(outBMIgrp4Male)

        outBMIgrp4Female = self.statisticsCollector.getDataSource("bmiGrp4Female") # added in to try and print number of people in each bmi group
        data_set.addDataSource(outBMIgrp4Female)

        outSumMale = self.statisticsCollector.getDataSource("Male")
        data_set.addDataSource(outSumMale)
    
        outSumFemale = self.statisticsCollector.getDataSource("Female")
        data_set.addDataSource(outSumFemale)

        outSumQuantMale = self.statisticsCollector.getDataSource("QuantMale")
        data_set.addDataSource(outSumQuantMale)
    
        outSumQuantFemale = self.statisticsCollector.getDataSource("QuantFemale")
        data_set.addDataSource(outSumQuantFemale)

        outSumHFSSQuantMale = self.statisticsCollector.getDataSource("HFSSQuantMale")
        data_set.addDataSource(outSumHFSSQuantMale)
        
        outSumHFSSQuantFemale = self.statisticsCollector.getDataSource("HFSSQuantFemale")
        data_set.addDataSource(outSumHFSSQuantFemale)

        outSumSESlowMale = self.statisticsCollector.getDataSource("SESlowMale")
        data_set.addDataSource(outSumSESlowMale)

        outSumSESmedMale = self.statisticsCollector.getDataSource("SESmedMale")
        data_set.addDataSource(outSumSESmedMale)

        outSumSEShighMale = self.statisticsCollector.getDataSource("SEShighMale")
        data_set.addDataSource(outSumSEShighMale)

        outSumHFSSQuantSESLowMale = self.statisticsCollector.getDataSource("HFSSQuantSESLowMale")
        data_set.addDataSource(outSumHFSSQuantSESLowMale)

        outSumHFSSQuantSESMedMale = self.statisticsCollector.getDataSource("HFSSQuantSESMedMale")
        data_set.addDataSource(outSumHFSSQuantSESMedMale)

        outSumHFSSQuantSESHighMale = self.statisticsCollector.getDataSource("HFSSQuantSESHighMale")
        data_set.addDataSource(outSumHFSSQuantSESHighMale)

        outSumSESlowFemale = self.statisticsCollector.getDataSource("SESlowFemale")
        data_set.addDataSource(outSumSESlowFemale)

        outSumSESmedFemale = self.statisticsCollector.getDataSource("SESmedFemale")
        data_set.addDataSource(outSumSESmedFemale)

        outSumSEShighFemale = self.statisticsCollector.getDataSource("SEShighFemale")
        data_set.addDataSource(outSumSEShighFemale)

        outSumHFSSQuantSESLowFemale = self.statisticsCollector.getDataSource("HFSSQuantSESLowFemale")
        data_set.addDataSource(outSumHFSSQuantSESLowFemale)

        outSumHFSSQuantSESMedFemale = self.statisticsCollector.getDataSource("HFSSQuantSESMedFemale")
        data_set.addDataSource(outSumHFSSQuantSESMedFemale)

        outSumHFSSQuantSESHighFemale = self.statisticsCollector.getDataSource("HFSSQuantSESHighFemale")
        data_set.addDataSource(outSumHFSSQuantSESHighFemale)

        outcountschema1male = self.statisticsCollector.getDataSource("countschema1Male")
        data_set.addDataSource(outcountschema1male)

        outcountschema2male = self.statisticsCollector.getDataSource("countschema2Male")
        data_set.addDataSource(outcountschema2male)

        outcountschema3male = self.statisticsCollector.getDataSource("countschema3Male")
        data_set.addDataSource(outcountschema3male)

        outcountschema4male = self.statisticsCollector.getDataSource("countschema4Male")
        data_set.addDataSource(outcountschema4male)

        outcountschema5male = self.statisticsCollector.getDataSource("countschema5Male")
        data_set.addDataSource(outcountschema5male)

        outcountschema1female = self.statisticsCollector.getDataSource("countschema1Female")
        data_set.addDataSource(outcountschema1female)

        outcountschema2female = self.statisticsCollector.getDataSource("countschema2Female")
        data_set.addDataSource(outcountschema2female)

        outcountschema3female = self.statisticsCollector.getDataSource("countschema3Female")
        data_set.addDataSource(outcountschema3female)

        outcountschema4female = self.statisticsCollector.getDataSource("countschema4Female")
        data_set.addDataSource(outcountschema4female)

        outcountschema5female = self.statisticsCollector.getDataSource("countschema5Female")
        data_set.addDataSource(outcountschema5female)


        # Creates an object with record method which grabs the value of each added data source, and a write method which writes the data set using
        # the specified file name and separator. Also holds a const reference to a schedule which is used to obtain the current tick when record is
        # called
        self.annualValues = data_set
    
    def getReadyToSpawn(self):
        pass
    
    def doSituationalMechanisms(self):
        #Rank 0 checks elapsed duration and throws an error if needed
        if (self.SINGLE_RUN_LIMIT_MIN > 0):
            if (repast4py.RepastProcess.instance().rank() == 0):
                elapsed = std.chrono.duration_cast<std.chrono.minutes>( std.chrono.steady_clock.now() - startTime )
                if (elapsed.count() > self.SINGLE_RUN_LIMIT_MIN):
                    std.cerr << "Model. The simulation has run for more than " << self.SINGLE_RUN_LIMIT_MIN << " minutes." << std.endl
    
        for agent in self.context.agents():
            #Here is where the agents are looped, agent memories can be revised here
            #Both situational mechanisms and the action mechanism are called
            #Agents update in a random order
    
            agent.doSituation()
            

        # TODO: Should be able to skip these as we are single threaded, but best to check 
        #repast4py.RepastProcess.instance().synchronizeAgentStates<AgentPackage, AgentPackageProvider, AgentPackageReceiver>(*provider, *receiver)
    
    
    def doActionMechanisms(self):
        for agent in self.context.agents():
            agent.doAction(self.__runner.schedule.tick)
        
        #repast4py.RepastProcess.instance().synchronizeAgentStates<AgentPackage, AgentPackageProvider, AgentPackageReceiver>(*provider, *receiver)
    
    def doTransformationalMechanisms(self): 
        for structuralEntity in self.structuralEntityList:
            structuralEntity.do_transformation(self.__runner.schedule.tick)
    
        #repast4py.RepastProcess.instance().synchronizeAgentStates<AgentPackage, AgentPackageProvider, AgentPackageReceiver>(*provider, *receiver)
    
    def ageAgents(self):
        for agent in self.context.agents():
            # agent.updateBMIval(eatingPlan, EatingSchema)
            # agent.updatePDval()
            # agent.PDintervention()
            agent.ageAgent()
            agent.reset12MonthDrinker()
            agent.resetTotalCaloriesPerAnnum()    
        #repast4py.RepastProcess.instance().synchronizeAgentStates<AgentPackage, AgentPackageProvider, AgentPackageReceiver>(*provider, *receiver)
    
    def killAgents(self):
        pass
    
    def incrementSimYear(self):
        self.simYear += 1
        self.ageAgents()
    
    def countSimDay(self):
        self.simDay += 1; 
        #print("Year: " + str(self.simYear) + ", Day: " + str(self.simDay))
        if (self.simDay==self.YEARLY_INTERVAL_TICK):
            self.simDay = 0
        #std.cout<<"Day = "<<simDay<<std.endl

    def getSimDay(self):
        currentTick = self.__runner.schedule.tick 
        return(currentTick)
    
    def spawnAgents(self, tick):
        pass
    
    def doDailyActions(self):
        self.countSimDay()
        currentTick = self.__runner.schedule.tick 
        if (currentTick==self.MECHANISM_START_TICK or currentTick % self.SITUATIONAL_MECHANISM_INTERVAL_TICK == 0):
            self.doSituationalMechanisms()
        if (currentTick % self.ACTION_MECHANISM_INTERVAL_TICK == 0):
            self.doActionMechanisms()
            intTick = int(currentTick)
            self.currentTick = intTick
            #print(self.currentTick)
        self.doTransformationalMechanisms()
        if (self.THEORY_SPECIFIC_OUTPUT):
            self.writeDailyAgentDataToFile()
        # Agent death currently disabled for food model
        # self.killAgents()
        if (self.SPAWNING_ON):
            self.spawnAgents(currentTick)
            
    def doYearlyTheoryActions(self):
        pass
    
    def doYearlyActions(self):
        dblCurrentTick = self.__runner.schedule.tick 
        
        self.statisticsCollector.collectAgentStatistics()
        self.annualValues.record()
        self.doYearlyTheoryActions()
    
        if (dblCurrentTick > 0): #the last date of each year
            #if (THEORY_SPECIFIC_OUTPUT) writeAnnualTheoryDataToFile()
            self.incrementSimYear()
    
        if (self.PRINT_POPULATION_STAT_YEARLY):
            print("To implement")
            # std.cout << "Year " << startYear+simYear << "\t" << countOfAgents
            #         << "\tDie " << countDiePerYear
            #         << "\tMigrate-Out " << countMigrateOutPerYear
            #         << "\tSpawn " << countSpawnPerYear
            #         << std.endl
    
        #reset pop stat count
        countDiePerYear = 0
        countMigrateOutPerYear = 0
        countSpawnPerYear = 0
    
        #logging memory use
        #if (self.CUSTOM_LOG):
        #    memoryUsed = self.getUsedMemoryValue()
        #    if (memoryUsed > maxMemoryUsed):
        #        maxMemoryUsed = memoryUsed
        #    Logger("Physical memory in use: " + std.to_string(memoryUsed/1024)+"MB")
    
    def initSchedule(self):
        self.__runner.schedule_repeating_event(self.MECHANISM_START_TICK, self.MECHANISM_INTERVAL_TICK, self.doDailyActions)
        self.__runner.schedule_repeating_event(self.YEARLY_START_TICK, self.YEARLY_INTERVAL_TICK, self.doYearlyActions)
        self.__runner.schedule_stop(self.stopAt)
        self.__runner.schedule_end_event(self.annualValues.write)
    
    #read on row in the CSV file
    def readCSVRow(self, row): 
        state = CSVState.UnquotedField
        fields = [""]
        i = 0; # index of the current field
        for c in row:
                if state == CSVState.UnquotedField:
                    if c == ',': # end of field
                        fields.append("")
                        i += 1
                    elif c == '"': 
                        state = CSVState.QuotedField
                    else:  
                        fields[i] += c.strip()
                elif state == CSVState.QuotedField:
                    if c == '"': 
                        state = CSVState.QuotedQuote
                    else:  
                        fields[i] += c.strip()
                elif state == CSVState.QuotedQuote:
                    if c ==  ',': # , after closing quote
                        fields.append("")
                        i += 1
                        state = CSVState.UnquotedField
                    elif c == '"': # "" . "
                        fields[i] += '"'
                        state = CSVState.QuotedField
                    else:  # end of quote
                        state = CSVState.UnquotedField
        return fields
    
    # Read CSV file, Excel dialect. Accept "quoted fields ""with quotes"""
    def readCSV(self, inFile):
        table = [] 
        for row in inFile:
            fields = self.readCSVRow(row)
            table.append(fields)
        return table
    
    def readSchemaProfilesTable(self, filename): 
        #read from csv and store in localTable
        localTable = None
        try: 
            with open(filename, "r") as myfile:
                #read the csv file
                localTable = self.readCSV(myfile)
                myfile.close()
        except IOError:
            print("Unable to open file: " + filename)
    
        #find index on header line.
        headerLine = localTable[0]
        localTable.pop(0)
    
        indexSex = self.findIndexInHeader("Sex", headerLine)
        indexAgeCat = self.findIndexInHeader("agecat", headerLine)
        indexSes = self.findIndexInHeader("SES", headerLine)
        indexSchema1 = self.findIndexInHeader("0-200", headerLine)
        indexSchema2 = self.findIndexInHeader("201-500", headerLine)
        indexSchema3 = self.findIndexInHeader("501-750", headerLine)
        indexSchema4 = self.findIndexInHeader("750-1000", headerLine)
        indexSchema5 = self.findIndexInHeader("1001+", headerLine)

        #read each line and put into a global table
        while (localTable):
            localMap = localTable[0]
            localTable.pop(0)
    
            #read the variables from a row of localTable and make hashkey
            localKey = self.makeSchemaProfilesHashKey(localMap[indexSex], localMap[indexAgeCat], localMap[indexSes])
            localVector = [
                float(localMap[indexSchema1]),
                float(localMap[indexSchema2]),
                float(localMap[indexSchema3]),
                float(localMap[indexSchema4]),
                float(localMap[indexSchema5])
            ]
    
            #create a key/pair map
            self.schemaProfilesTable[localKey] = localVector

    
    def makeSchemaProfilesHashKey(self, sex, ageCategory, ses):
        return sex + "-" + ageCategory + "-" + ses

    def normaliseBMIColumn(self): # this is to normalise BMI column and use max value so that we can change output file accordingly
        maxValue = None
        normalized_column = []

        headerLine = self.infoTable[0]
    
        self.indexBmival = self.findIndexInHeader("bmival", headerLine)

        # Step 1: Find the max value in the column
        for row in self.infoTable[1:]:  # Skip header row
            value = float(row[self.indexBmival])
            if maxValue is None or value > maxValue:
                maxValue = value

        # Step 2: Normalize all values in the "bmival" column
        for row in self.infoTable[1:]:  # Skip header row
            value = float(row[self.indexBmival])
            # print(value)
            normalizedValue = value / maxValue
            row[self.indexBmival] = normalizedValue  # Update the original value with the normalized value
