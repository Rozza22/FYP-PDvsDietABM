# Agent.py

from re import L
from random import randint
from numpy import number
from numpy.random import normal
from sympy import ordered, rem
from repast4py import core, random
import repast4py
from core import Globals
from EatingPlan import EatingSchema
from EatingPlan import EatingPlan
import math
import scipy.stats
from enum import Enum
from datetime import datetime
import repast4py
from repast4py import context, schedule
from .SVDataSet import SVDataSet
from mpi4py import MPI
# from NormTheory import updateBMIval, updatePDval, PDintervention

# from updateAgentCharacteristics import updateBMIval, updatePDval, PDintervention
from maxBMIval import maxBMIvalue

csv_filename = "props/GLondon_basepop_1000.csv"
# csv_filename = "props/GLondon_basepop_1000 - agent38.csv"
maxValue = maxBMIvalue(csv_filename)



class Agent(repast4py.core.Agent):
    
    def __init__(self, id:int, sex, age:int, race, meanKcalNonHfss:float, sdKcalNonHfss:float, meanKcalHfss:float, sdKcalHfss:float, year, traitImpulsivity, habitUpdateInterval, ses, schemaProfiles, autonomy, emotional, restraint, simDay, PD, bmival):
        super().__init__(id, 0, 0)
        
        self._id:int = id
        self.sex = sex
        self.age:int = age
        self.ses = ses # socio economic status
        self.race = race
        self.bmival = bmival
        self.habitBag = []
        self.habitStock = [] 
        self.habitProbability = [] 
        self.habitUpdateInterval = habitUpdateInterval # each person takes a different amount of time for habits to update
        self.nonHfssCalsToday = 0
        self.hfssCalsToday = 0
        self.numberCaloriesToday = 0
        self.totalCaloriesPerAnnum = 0

        self.currentTick = 0
        
        # Mean and standard deviation for daily calories from non-hfss foods
        self.TotalKcal_M_0 = meanKcalNonHfss
        self.TotalKcal_SD_0 = sdKcalNonHfss
    
        # Mean and standard deviation for daily calories from hfss foods
        self.TotalKcal_M_1 = meanKcalHfss
        self.TotalKcal_SD_1 = sdKcalHfss

        self.pastYearSchemas = []
        self.pastYearCalories = []
        self.pastYearHFSSCalories = []
        
        self.eatingBuddies = []
        self.mediator = None
        self.year = year
        self.traitImpulsivity = traitImpulsivity
        self.autonomy = autonomy
        self.emotional = emotional
        self.restraint = restraint
        self.PD = PD
        self.schemaProfiles = schemaProfiles
        self.initPastYearSchemasCalories(self.schemaProfiles)

        
    def setMediator(self, mediator):  
        self.mediator = mediator
        self.mediator.linkAgent(self)
        while (len(self.habitProbability) < Globals.NUM_SCHEMA):
            self.habitProbability.append(0.0)
        for i in range(0, Globals.NUM_SCHEMA):
            self.mediator.setIntentionProbability(i, self.habitProbability[i])
    
    # Apply situational mechanisms
    def doSituation(self) :
        self.mediator.mediateSituation()
    
    # Apply action mechanisms
    def doAction(self, tick) :
        intTick = int(tick)
        self.currentTick = intTick
        # self.updateBMIval(self.habitUpdateInterval) # calling function added to update bmi
        # print(self.currentTick)
        if (intTick != 0 and intTick % int(self.habitUpdateInterval) == 0):
            self.updateHabitProbabilities(self.habitUpdateInterval)
            # Update the habit bag, so it has self.habitUpdateInterval elements
            self.updateHabitBag(self.habitUpdateInterval)

    
        chosenPath = ""
        eatingPlan = EatingPlan()
        # if repast4py.random.default_rng.random() < float(self.traitImpulsivity) : 
        #     chosenPath="habit"
        #     eatingPlan.schema = self.drawSchemaFromHabitBag()
        #     eatingPlan.probability = 1
        # else :
        chosenPath="intention"
        self.mediator.mediateAction() # calls all behaviour functions from Mediator
        # print(["before proportional selection: ", eatingPlan.schema])
        eatingPlan = self.mediator.getProportionalSelectionIntentionPlan()
        # print(["after proportional selection: ", eatingPlan.schema])
        
        self.doEating(eatingPlan.schema)        

        if (intTick != 0 and intTick % int(self.habitUpdateInterval) == 0):
            self.bmival, self.PD = self.mediator.updateBMIpd(eatingPlan) # update PD and BMI of agent at each agent's own habit update interval
    
        self.updatePastYearCalories()
    
    # Age the agent by one year (assume all agents age at the same time)
    def ageAgent(self):
        self.age += 1
        self.year += 1

    def reset12MonthDrinker(self): 
        self.is12MonthDrinker = False

    def resetTotalCaloriesPerAnnum(self):
        self.totalCaloriesPerAnnum = 0

    # method to identify age group of the agent ranging from 0 to Globals.NUM_AGE_GROUPS -1
    def findAgeGroup(self) -> int:
        myAgeGroup = 0

        if (int(self.age) <= int(Globals.MAX_AGE)):
            while (int(self.age) > Globals.AGE_GROUPS[myAgeGroup]):
                myAgeGroup = myAgeGroup + 1
        else:
            myAgeGroup = Globals.NUM_AGE_GROUPS - 1
    
        return myAgeGroup # Age group 0 is the youngest age groups
    
    #returns the average number of calories consumed in the past n days (counting back from today).
    def getAvgCaloriesNDays(self, numberOfDays:int) -> float:
        averageCalories = 0.0
        totalDays = numberOfDays
        index = len(self.pastYearCalories) - 1
        while (numberOfDays > 0): 
            averageCalories = averageCalories + float(self.pastYearCalories[index])
            index = index - 1
            numberOfDays = numberOfDays - 1
        return averageCalories/totalDays

    def getAvgHFSSCaloriesNDays(self, numberOfDays:int) -> float:
        averageHFSSCalories = 0.0
        totalDays = numberOfDays
        index = len(self.pastYearHFSSCalories) - 1
        while (numberOfDays > 0): 
            averageHFSSCalories = averageHFSSCalories + float(self.pastYearHFSSCalories[index])
            index = index - 1
            numberOfDays = numberOfDays - 1
        return averageHFSSCalories/totalDays
    
    def getAvgCaloriesNDaysWithOccasion(self, numberOfDays, perOccasion):
        averageCalories = 0
        index = self.pastYearCalories.size()-1
        totalDays = 0  
        if (perOccasion == False): #avg over all days
            totalDays = numberOfDays
            while (numberOfDays > 0):
                averageCalories = averageCalories + self.pastYearCalories[index]
                index -= 1
                numberOfDays -= 1

        else: #avg over only eating days
            totalDays = 0
            while (numberOfDays > 0):
                if (self.pastYearCalories[index] > 0):
                    averageCalories = averageCalories + self.pastYearCalories[index]
                    totalDays += 1

                index -= 1
                numberOfDays -= 1

        if (totalDays == 0):
            averageCalories = 0
        else:
            averageCalories = averageCalories/totalDays
    
        return averageCalories

    #forward update past-year mean and variance using Welford method
    def updateForwardMeanVariance(self, addedValue):
        self.pastYearN += 1
        if (self.pastYearN != 0):
            oldMean = self.pastYearMeanCalories
            self.pastYearMeanCalories = float(self.pastYearMeanCalories) + (float(addedValue) - float(self.pastYearMeanCalories)) / float(self.pastYearN)
            self.pastYearSquaredDistanceDrink = float(self.pastYearSquaredDistanceDrink) + (float(addedValue) - float(oldMean))*(float(addedValue) - float(self.pastYearMeanCalories))
        else:
            self.pastYearMeanCalories = 0
            self.pastYearSquaredDistanceDrink = 0
    
        #limit estimated-mean between 0 and MAX_CALORIES
        if (self.pastYearMeanCalories < 0):
            self.pastYearMeanCalories = 0
        # if (self.pastYearMeanCalories > Globals.MAX_CALORIES):
        #     self.pastYearMeanCalories = Globals.MAX_CALORIES
    

    #backward update past-year mean and variance using Welford method
    def updateBackwardMeanVariance(self, removedValue):
        removedValue = float(removedValue)
        self.pastYearN -= 1
        if (self.pastYearN != 0):
            oldMean = self.pastYearMeanCalories
            self.pastYearMeanCalories = ((self.pastYearN+1)*self.pastYearMeanCalories - removedValue) / self.pastYearN
            self.pastYearSquaredDistanceDrink = self.pastYearSquaredDistanceDrink - (removedValue - oldMean)*(removedValue - self.pastYearMeanCalories)
        else:
            self.pastYearMeanCalories = 0
            self.pastYearSquaredDistanceDrink = 0

        #limit estimated-mean between 0 and MAX_CALORIES

    
    def selectIndexFromProbabilityVector(self, probVector):
        chooser = repast4py.random.default_rng.random()
        # Forward iterate through the probability vector, until the RNG value is in the window
        for idx in range(0, probVector.size()):
            if(chooser < probVector[idx]): 
                return idx
        # This will only ever be reached if the final element in probVector is < 1.0, in which case return the 0th element.
        return 0
    
    def getProportionalSelectionHabitualEatingPlan(self):
        #random a number from 0 . total prob
        totalProb = 0
        for i in range(0, Globals.NUM_SCHEMA):
            totalProb += self.habitProbability[i]
        rand = repast4py.random.default_rng.random() * totalProb
    
        #do the proportional selection
        selectedIndex = -1
        cumulativeProb = 0
        for i in range(0, Globals.NUM_SCHEMA):
            if (self.habitProbability[i] > 0):
                cumulativeProb += self.habitProbability[i]
                if (rand <= cumulativeProb):
                    selectedIndex = i
                    break

        if (selectedIndex==-1):
            print("getProportionalSelectionHabitualEatingPlan. Proportional selection failed.")
    
        plan = EatingPlan()
        plan.schema = EatingSchema(selectedIndex)
        plan.probability = self.habitProbability[selectedIndex]
        return plan

    # /**
    #  * @brief Perform eating
    #  * @details Get drink from schema, update related variables. This method replaces doEatingEngine().
    #  * 
    #  * @param schema A selected schema
    #  */
    def doEating(self, schema):
        if (schema==EatingSchema.NONE):
            print("Eating schema must NOT be NONE.")

        if (schema<EatingSchema.NONE or schema>EatingSchema.VERY_HIGH):
            print("Eating schema out of range.")
            
        # Sample non-hfss foods based on normal distribution
        nonHfssCals = max(0, normal(self.TotalKcal_M_0, self.TotalKcal_SD_0))

        # Sample hfss foods from schema
        self.todaySchema = schema
        hfssCals = self.getCaloriesFromSchema(schema)

        numberOfCalories = nonHfssCals + hfssCals 
        self.isEatingToday = (numberOfCalories > 0)
        self.nonHfssCalsToday = nonHfssCals
        self.hfssCalsToday = hfssCals
        self.numberCaloriesToday = numberOfCalories
        self.totalCaloriesPerAnnum += numberOfCalories

        # if (self.totalCaloriesPerAnnum > 750*365): # this is in the high or very high schema
        #     self.bmival = self.bmival + 1
    
    # /**
    #  * @brief Return number of drinks from a schema
    #  */
    def getCaloriesFromSchema(self, schema):
        
        # TODO: Check upper limit on very high category
        calorieRanges = {
            EatingSchema.VERY_LOW: (0, 200),
            EatingSchema.LOW: (201, 500),
            EatingSchema.MED: (501, 750),
            EatingSchema.HIGH: (751, 1000),
            EatingSchema.VERY_HIGH: (1001, 1100)
        }
        
        (low, high) = calorieRanges[schema]
        calories = randint(low, high)
        return calories
    
    def getEatingBuddies(self):
        return []
    
    def initPastYearSchemasCalories(self, schemaProfile):
        self.pastYearSchemas.clear()
        self.pastYearCalories.clear()

        tempTotalCalories = 0
        totalProb = 0.0

        for i in range(0, Globals.NUM_SCHEMA):
            totalProb += schemaProfile[i]
    
        #propotional select schema every day
        for day in range(0, 365):

            #random a number from 0 . total prob
            rand = repast4py.random.default_rng.random() * totalProb
    
            #do the proportional selection
            selectedIndex = -1
            cumulativeProb = 0.0
            for i in range(0, Globals.NUM_SCHEMA):
                if (schemaProfile[i] > 0):
                    cumulativeProb += schemaProfile[i]
                    if (rand <= cumulativeProb):
                        selectedIndex = i
                        break

            if (selectedIndex==-1):
                print("initPastYearSchemasCalories. Proportional selection failed.")
    
            #Add schema to past year vector
            schema = EatingSchema(selectedIndex)
            self.pastYearSchemas.append(schema)
            
            # Sample non-hfss foods based on normal distribution
            nonHfssCals = max(0, normal(self.TotalKcal_M_0, self.TotalKcal_SD_0))

            # hfss calories from schema
            hfssCals = self.getCaloriesFromSchema(schema)
            calories = nonHfssCals + hfssCals
            self.pastYearCalories.append(calories)
            self.pastYearHFSSCalories.append(hfssCals)

    
            tempTotalCalories += calories

        #calculate mean and variance from history using Welford method
        self.pastYearN = 0
        self.pastYearMeanCalories = 0
        self.pastYearSquaredDistanceDrink = 0
        oldMean = 0

        for k in range(0, 365):
            x = self.pastYearCalories[k-1]
            if (x!=0): #only account for eating days (number of drinks > 0)
                self.pastYearN += 1
                oldMean = self.pastYearMeanCalories
                self.pastYearMeanCalories = self.pastYearMeanCalories + (x - self.pastYearMeanCalories) / self.pastYearN
                self.pastYearSquaredDistanceDrink = self.pastYearSquaredDistanceDrink + (x - self.pastYearMeanCalories)*(x-oldMean)
    
        self.MeanCaloriesToday = self.pastYearMeanCalories
        self.SDCaloriesToday = self.pastYearSquaredDistanceDrink
    
        # Set todaySchema to the most recent schema, adefing UB.
        self.todaySchema = self.pastYearSchemas[-1]
    
        #reset habitual arrays to 0: stock and proportion
        for i in range(0, Globals.NUM_SCHEMA):
            self.habitProbability.append(0)
            self.habitStock.append(0)

    
    def updateHabitProbabilities(self, numberOfDays):
        numberOfDays = float(numberOfDays)
        schemaCount = self.getSchemaCountOverNDays(numberOfDays)
        for i in range(0, Globals.NUM_SCHEMA):
            self.habitProbability[i] = schemaCount[i] / numberOfDays
    
    def drawSchemaFromHabitBag(self):
        schema = EatingSchema.NONE
        if len(self.habitBag) == 0:
            self.updateHabitBag(self.habitUpdateInterval)
        if len(self.habitBag) != 0:
            schema = self.habitBag.pop()
        return schema
    
    def updateHabitBag(self, numberOfDays):
        self.habitBag.clear()
        for i in range(365-1, 365-int(numberOfDays), -1):
            self.habitBag.append(self.pastYearSchemas[i])
            
    def getDrinkFrequencyLevel(self):
        return self.drinkFrequencyLevel
    
    def getPastYearMeanCalories(self):
        return self.pastYearMeanCalories
        
    def getPastYearSdCalories(self):
        return math.sqrt(self.getPastYearVarianceCalories())
        
    def getPastYearVarianceCalories(self):
        return (self.pastYearSquaredDistanceCalories / self.pastYearN) if self.pastYearN <= 1 else 0.0
        
    def getSes(self):
        return self.ses
        
    def getSchemaCountOverNDays(self, numberOfDays:int):
        countSchemas = []
        while (len(countSchemas) < Globals.NUM_SCHEMA):
            countSchemas.append(0.0)
        
        index = len(self.pastYearCalories) - 1
        while (numberOfDays > 0):
            countSchemas[self.pastYearSchemas[index]] += 1
            index -= 1
            numberOfDays -= 1

        return countSchemas

    def countSimDay(self):
        self.simDay += 1; 
        #print("Year: " + str(self.simYear) + ", Day: " + str(self.simDay))

    def getSimDay(self):
        return(self.currentTick)
    
    def getSex(self):
        return self.sex

    def getAge(self):
        return int(self.age)

    def getbmival(self):
        return float(self.bmival)
        
    def getTheory(self, derivedTheory):
        return self.mediator.getTheory(derivedTheory) 
        
    def getTraitImpulsivity(self):
        return float(self.traitImpulsivity)

    def getAutonomy(self):
        return self.autonomy

    def getEmotional(self):
        return self.emotional

    def getRestraint(self):
        return self.restraint

    def getPD(self):
        if self.PD >= 1:
            self.PD = 1
            
        return self.PD

    def getTick(self):
        return self.simDay
        
    def isHaveKCaloriesOverNDays(self, numberOfDays, kNumberCalories):
        index = len(self.pastYearCalories) - 1
        while (numberOfDays > 0): 
            if (int(self.pastYearCalories[index]) >= kNumberCalories):
                # early exit as soon as a matching day has been found
                return True
            index -= 1
            numberOfDays -= 1
        return False
        
    def getNumDaysHavingKCaloriesOverNDays(self, numberOfDays, kNumberCalories):
        countDays = 0
        index = len(self.pastYearCalories) - 1
        while (numberOfDays > 0): 
            if (int(self.pastYearCalories[index]) >= kNumberCalories):
                countDays += 1
            index -= 1
            numberOfDays -= 1
        return countDays
        
    def updatePastYearCalories(self):
        self.pastYearCalories.append(self.numberCaloriesToday)
        self.pastYearHFSSCalories.append(self.hfssCalsToday)

        self.updateForwardMeanVariance(self.numberCaloriesToday)

        if ( self.pastYearCalories[0] != 0 ):
            self.updateBackwardMeanVariance(self.pastYearCalories[0])

        self.pastYearCalories = self.pastYearCalories[1:]

        # update schema vector 
        self.pastYearSchemas.append(self.todaySchema)
        self.pastYearSchemas = self.pastYearSchemas[1:]
        
    def getMortalityRate(self):
        return self.mortality
        
    def getMigrationOutRate(self):
        return self.migrationOutRate
