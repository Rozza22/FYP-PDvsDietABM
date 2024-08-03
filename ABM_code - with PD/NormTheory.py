from core.Theory import Theory
import repast4py
import NormGlobals
from AdvertisingScalingNormModifier import AdvertisingScalingNormModifier
from repast4py import context, schedule
from core import Globals
from re import L
import os.path
from core.Model import Model
from core.SVDataSet import SVDataSet
from mpi4py import MPI
import numpy as np
import math
from scipy.optimize import minimize
import random
# from core.Agent import 

from EatingPlan import EatingSchema
from EatingPlan import EatingPlan

from maxBMIval import maxBMIvalue

csv_filename = "props/GLondon_basepop_1000.csv"
# csv_filename = "props/GLondon_basepop_1000 - agent38.csv"
maxValue = maxBMIvalue(csv_filename)

class NormTheory(Theory): # To rename this IntentionTheory because that's what we're actually looking at
    
    # def __init__(self, pPopulation, pDesNormEntity, autonomy, emotional, restraint, PD, bmival, age, sex):
    # def __init__(self, pPopulation, pDesNormEntity, autonomy, emotional, restraint, age, sex, agent.PD, agent.bmival):
    def __init__(self, pPopulation, pDesNormEntity, autonomy, emotional, restraint, age, sex, PD, bmival, id):
        self.autonomy = autonomy
        self.emotional = emotional
        self.restraint = restraint
        self.PD = PD
        self.bmival = bmival
        self.age = age
        self.sex = sex
        self._id = id
        
        self.mpPopulation = pPopulation
        self.mpDesNormEntity = pDesNormEntity
    
        self.desires = {}
        self.descriptiveNormPrev = []
        for i in range(0, Globals.NUM_SCHEMA):
            self.descriptiveNormPrev.append(0.0)
        self.normModifiers = []
    
    def initDesires(self):
        self.desires = [] 
        countSchemas = self._agent.getSchemaCountOverNDays(365)
        while (len(self.desires) < Globals.NUM_SCHEMA):
            self.desires.append(0.0)
        for schemaId in range(0, Globals.NUM_SCHEMA):
            self.desires[schemaId] += countSchemas[schemaId] / 365.0
    
    def doSituation(self):
        self.updateDescriptiveNorm()
    
    def updateDescriptiveNorm(self):
        weights = 0
        weightedEating = []
        for i in range(0, Globals.NUM_SCHEMA):
            weightedEating.append(0.0)
        groupId = Globals.P_REFERENCE_GROUP.getId(self._agent.getSex(), self._agent.findAgeGroup(), self._agent.getSes())
        tick = self._agent.getSimDay()
        #print(tick)
        for i in range(0, Globals.P_REFERENCE_GROUP.size()):
            w = Globals.P_REFERENCE_GROUP.compare(groupId,i)
            weights += w
            for j in range(0, Globals.NUM_SCHEMA):
                weightedEating[j] += w * self.mpDesNormEntity.getAvgPrevalence(i)[j]
    
        if (weights != 0):
            countSchemas = self._agent.getSchemaCountOverNDays(NormGlobals.N_DAYS_DESCRIPTIVE)
            for i in range(0, Globals.NUM_SCHEMA):
                self.descriptiveNormPrev[i] = weightedEating[i]/weights * NormGlobals.PERCEPTION_BIAS + \
                        countSchemas[i]/NormGlobals.N_DAYS_DESCRIPTIVE * (1-NormGlobals.PERCEPTION_BIAS)

                total = sum(self.descriptiveNormPrev) 
                scaleFactor = (1.0 / total)
                self.descriptiveNormPrev = np.array(self.descriptiveNormPrev) * scaleFactor

                if tick>Globals.ADVERTISING_POLICY_SCALING_ENABLE_AT_TICK and Globals.ADVERTISING_POLICY_ENABLED:
                        self.categoriesAffected = Globals.ADVERTISING_POLICY_SCALING_CATEGORIES_AFFECTED
                        self.scalingFactor = Globals.ADVERTISING_POLICY_SCALING_FACTOR
                    #for i in range(1, self.categoriesAffected+1):
                       # self.descriptiveNormPrev[-i] *= (1.0 - self.scalingFactor)
                        self.descriptiveNormPrev[2] *= (1.0 - self.scalingFactor)
                        self.descriptiveNormPrev[3] *= (1.0 - self.scalingFactor)
                        self.descriptiveNormPrev[4] *= (1.0 - self.scalingFactor)
                        # print(profile)
            
        # Normalise weights
                        total = sum(self.descriptiveNormPrev) 
                        scaleFactor = (1.0 / total)
                        self.descriptiveNormPrev = np.array(self.descriptiveNormPrev) * scaleFactor
        else:
            print("Descriptive Norm: weights should NOT be 0.")
    
    def getAttitude(self, schema):

        schemaId = schema
        # schemaId = EatingSchema
        # desire = 0.0
        if self.desires:
            def softmaxSchemaChoice(x):
                expX = np.exp(x - np.max(x))
                return expX / np.sum(expX, axis = 0)
            
            def logitFunctionSchemaChoice(params, schemaId, PD, emotional, BMI):
                alpha, betaEmotional, betaPD, betaBMI = params
                HealthyPDmultiplier = 2
                UnhealthyPDmultiplier = 10
                # [VERY_LOW, LOW, MED, HIGH, VERY_HIGH]
                if (self.PD < 4/12): # if low/good PD, higher chance of choosing better diet
                    lowerSchema = HealthyPDmultiplier*(alpha + (betaEmotional * emotional) + (betaPD * PD) + (betaBMI * BMI))
                    return softmaxSchemaChoice([lowerSchema, lowerSchema, 0, 0, 0])
                elif (self.PD >= 4/12):
                    higherSchema = UnhealthyPDmultiplier*(alpha + (betaEmotional * emotional) + (betaPD * PD) + (betaBMI * BMI))
                    return softmaxSchemaChoice([0, 0, 0, higherSchema, higherSchema])
                
            alpha = Globals.ALPHA_ATTITUDE
            betaBMI = Globals.BETA_BMIVALEMO
            betaPD = Globals.BETA_PD
            betaEmotional = Globals.BETA_EMOTIONAL
            params = [alpha, betaEmotional, betaPD, betaBMI]

            if (self.PD >= 4/12): # this is needed to make sure strength of PD in the above equation is consistent
                PD = ((self.PD*12) - 4)/8 # if it wasn't here, importance/weight of high PD would be greater than low PD
            elif (self.PD < 4/12):
                PD = 1 - (self.PD*12)/4

            newDesires = np.array(logitFunctionSchemaChoice(params, schemaId, PD, self.emotional, self.bmival))
        # Rescale the rest of the schemas to sum to 1
        
        # combinedDesires = self.desires + newDesires
        self.desires = self.desires + newDesires
        # desireSum = sum(combinedDesires)
        desireSum = sum(self.desires)

        # print(["NewDesires: ", [newDesires]])
        # print(["CombinedDesires: ", [combinedDesires]])

        if desireSum != 0:
            self.desires = [d/desireSum for d in self.desires]
            desire = self.safeOdds(self.desires[schemaId], self.desires[0])
            # desire = self.desires[schemaId]
        else:
            print("WARNING: Agent has no desires")
        if desire == 0:
            desire = 1e-30 # small number for log

        # print(desire)
        # print([self.desires])
        return self.safeLog(desire)
        # return desire

        
    
    def getNorm(self, schema):
        groupId = Globals.P_REFERENCE_GROUP.getId(self._agent.getSex(), self._agent.findAgeGroup(), self._agent.getSes())
        schemaId = int(schema)

        tick = self._agent.getSimDay()
        #print(tick)
        #print(self.descriptiveNormPrev)

        descriptive = self.safeOdds(self.descriptiveNormPrev[schemaId], self.descriptiveNormPrev[0])
        return (1-self.autonomy)*(self.safeLog(descriptive))
    
    def getPerceivedBehaviourControl(self, schema):
        schemaId = schema
        # print(schemaId)
        desire = 0.0
        if self.desires:
        # Apply modifier to schemaId 4 and 5
            # if schemaId == 0:
            if schemaId == 4 or schemaId == 5: # changing this makes almost no difference
            #weight restrained eating according to global parameter
                self.restraint = self.restraint*Globals.BETA_RESTRAINT
                self.desires[schemaId] = self.desires[schemaId] + (self.desires[schemaId] * self.restraint) # bmival and PD have no direct impact on desires (restrained eating)
                #self.desires[schemaId] = self.desires[schemaId]*Globals.BETA_RESTRAINT

        # Rescale the rest of the schemas to sum to 1
        desires_sum = sum(self.desires)
        if desires_sum != 0:
            self.desires = [d/desires_sum for d in self.desires]
            desire = self.safeOdds(self.desires[schemaId], self.desires[0])
        else:
            print("WARNING: Agent has no desires")
        if desire == 0:
            desire = 1e-30 # small number for log
        return self.safeLog(desire)


    def updateBMIval(self, eatingPlan):
        schemaId = eatingPlan.schema

        def softmax(x):
            expX = np.exp(x - np.max(x))
            return expX / np.sum(expX, axis = 0)

        def logit_function(params, schemaId, BMI): # now goes too high
            # print(schemaId)
            alpha, beta = params 

            if schemaId == EatingSchema.VERY_HIGH or schemaId == EatingSchema.HIGH:
                increaseBMI = alpha + beta * schemaId
                return softmax([increaseBMI, 0, 0])
            elif schemaId == EatingSchema.VERY_LOW or schemaId == EatingSchema.LOW:
                # print([schemaId, "is very low"])
                if ((BMI < 25/maxValue)): # if healthy weight or underweight, it is completely random whether BMI changes and which way
                    decreaseBMI = alpha + beta * schemaId
                    return softmax([0, 0, decreaseBMI])
                else:
                    decreaseBMI = alpha + beta * schemaId
                    return softmax([0, decreaseBMI, 0])
                    # return softmax([0, 0, 0])
            else:
                sameBMI = alpha + beta * schemaId # if med or low, will most likely stay the same weight
                return softmax([0, 0, sameBMI])

        # alpha = 0.1
        alpha = Globals.ALPHA_BMI
        beta = Globals.BETA_HFSSBMIVAL
        params = [alpha, beta]

        probPDchange = np.array(logit_function(params, schemaId, self.bmival)) # will output something like [0.2, 0.7, 0.1] for BMI = 22 where its [gain, loss, same]

        randGainOrLoss = repast4py.random.default_rng.random() # once in a bmi category, this decides whether weight is gained or lost

        if self.age>60:
            if (self.bmival < 25/maxValue):
                randGainAmount = repast4py.random.default_rng.random() * 0.01 * 0.5 # For healthy or underweight, 2% change at a time is fine
                if (randGainOrLoss < probPDchange[0]):
                    self.bmival = self.bmival + self.bmival * randGainAmount # Weight gain
                elif (randGainOrLoss > probPDchange[0] and (randGainOrLoss < (probPDchange[1] + probPDchange[0]))):
                    self.bmival = self.bmival - self.bmival * randGainAmount # weight loss
            else:
                randGainAmount = repast4py.random.default_rng.random() * 0.02 * 0.5 # anything larger than 5% weight gain is unrealistic over time frame this is being applied on
                if (randGainOrLoss < probPDchange[0]):
                    self.bmival = self.bmival + self.bmival * randGainAmount # Weight gain
                elif (randGainOrLoss > probPDchange[0] and (randGainOrLoss < (probPDchange[1] + probPDchange[0]))):
                    self.bmival = self.bmival - self.bmival * randGainAmount # weight loss
        elif self.age<=60:
            if (self.bmival < 25/maxValue):
                randGainAmount = repast4py.random.default_rng.random() * 0.01 # For healthy or underweight, 2% change at a time is fine
                if (randGainOrLoss < probPDchange[0]):
                    self.bmival = self.bmival + self.bmival * randGainAmount # Weight gain
                elif (randGainOrLoss > probPDchange[0] and (randGainOrLoss < (probPDchange[1] + probPDchange[0]))):
                    self.bmival = self.bmival - self.bmival * randGainAmount # weight loss
            else:
                randGainAmount = repast4py.random.default_rng.random() * 0.02  # anything larger than 5% weight gain is unrealistic over time frame this is being applied on
                if (randGainOrLoss < probPDchange[0]):
                    self.bmival = self.bmival + self.bmival * randGainAmount # Weight gain
                elif (randGainOrLoss > probPDchange[0] and (randGainOrLoss < (probPDchange[1] + probPDchange[0]))):
                    self.bmival = self.bmival - self.bmival * randGainAmount # weight loss

        if self.bmival < 13/maxValue or self.bmival > maxValue: # BMI of 13 is considered fatal so have made a floor here
            self.bmival == 13/maxValue

        return self.bmival


    def updatePDval(self):

        # Making a cumulative logit equation to see whether PD will [increase, decrease, stay the same]
        def softmax(x):
            expX = np.exp(x - np.max(x))
            return expX / np.sum(expX, axis = 0)

        def logit_function(params, BMI):
            alpha, beta = params 
            if BMI > 25/maxValue: # if overweight, you have a higher chance of worse PD
                increasePD = alpha + beta * BMI
                return softmax([increasePD, 0, 0])
            elif (BMI < 25/maxValue) and (BMI > 18.5/maxValue): # if healthy weight, higher chance of improving PD
                decreasePD = alpha + beta * BMI
                return softmax([0, decreasePD, 0])
                # return softmax([0, 0, 0])
            else:
                samePD = alpha + beta * BMI
                return softmax([0, 0, samePD])

        # alpha = 1.1
        alpha = Globals.ALPHA_PD # threshold
        beta = Globals.BETA_BMIVALPD # strength of relationship between BMI and PD
        params = [alpha, beta]

        probPDchange = np.array(logit_function(params, self.bmival)) # will output something like [0.2, 0.7, 0.1] for BMI = 22, # [gain, loss, stay]
        # print(probPDchange)
        # print("done")
        randGainOrLoss = repast4py.random.default_rng.random() # random number to see which path will be taken
        randGainAmount = repast4py.random.default_rng.random() * 0.1 # anything larger than 10% PD change is unrealistic over time frame this is being applied on

        if (self.sex == "f"): # BMI value affects women's mental health differently compared to men
            if ((randGainOrLoss < probPDchange[0]) and (self.bmival >= 30/maxValue)): # more extreme impact as a result of being obese compared to overweight
                self.PD = self.PD + self.PD * randGainAmount * 1.2 # PD increased by 12% and below is 3%, this represents theory 
            elif (randGainOrLoss < probPDchange[0] and ((self.bmival >= 25/maxValue) and (self.bmival < 30/maxValue))):
                self.PD = self.PD + self.PD * randGainAmount * 0.3 # PD increased
            elif ((randGainOrLoss > probPDchange[0]) and (randGainOrLoss < probPDchange[0] + probPDchange[1])):
                self.PD = self.PD - self.PD * randGainAmount * 1.2
                # print("reduced PD")
        elif (self.sex == "m"):
            if ((randGainOrLoss < probPDchange[0]) and (self.bmival >= 30/maxValue)):
                self.PD = self.PD + self.PD * randGainAmount * 0.6 # PD increased half as much as females 
            elif (randGainOrLoss < probPDchange[0] and (self.bmival >= 25/maxValue) and (self.bmival < 30/maxValue)):
                self.PD = self.PD + self.PD * randGainAmount * 0.15 # PD increased
            elif ((randGainOrLoss > probPDchange[0]) and (randGainOrLoss < probPDchange[0] + probPDchange[1])):
                self.PD = self.PD - self.PD * randGainAmount * 0.6

        # INTERVENTION - TURN OFF FOR CALIBRATION
        def intervention():
            # [gain, loss, stay]
            probAppointmentEffective = [0.75, 0.05, 0.2] # [decrease, increase, same] # These are the probabilities based off research on how effective treatment is on average for people

            randAppointment = repast4py.random.default_rng.random() # chance that this patient gets seen to - based off level of improvement in services atm
            randEffectiveOrNot = repast4py.random.default_rng.random() # choosing whether the treatment will be effective or not
            randEffectiveness = repast4py.random.default_rng.random() # The size of the impact that treatment has on people

            if randAppointment < Globals.BETA_INTERVENTIONSCALE: # the global value here represents the level at which the UK mental health services improve
                if (randEffectiveOrNot < probAppointmentEffective[0]):
                    # self.PD = self.PD - self.PD * Globals.BETA_INTERVENTIONEFFECT * randEffectiveness
                    self.PD = self.PD - self.PD * randEffectiveness
                elif (randEffectiveOrNot > probAppointmentEffective[0] + probAppointmentEffective[1]):
                    # self.PD = self.PD + self.PD * Globals.BETA_INTERVENTIONEFFECT * randEffectiveness
                    self.PD = self.PD + self.PD * randEffectiveness
            
            return self.PD

        # if intervention is called:
        self.PD = intervention() # turn this off for calibration

        if self.PD >= 1: # GHQ-12 score cannot go above 12 and it has been normalised so 1 represents 12.
            self.PD = 1

        return self.PD
    
    def getAutonomy(self):
        return self.autonomy

    def getEmotional(self):
        return self.emotional

    def getRestraint(self):
        return self.restraint
    
    def getDesires(self):
        return self.desires
    
    def setDesires(self, desires):
        self.desires = []
        for i in range(0, Globals.NUM_SCHEMA):
            self.desires.append(desires[i])
