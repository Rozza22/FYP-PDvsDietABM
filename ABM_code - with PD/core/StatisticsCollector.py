from .Globals import *
from .DataSource import DataSource
from core import Globals
from maxBMIval import maxBMIvalue

csv_filename = "props/GLondon_basepop_1000.csv"
# csv_filename = "props/GLondon_basepop_1000 - agent38.csv"
maxValue = maxBMIvalue(csv_filename)

class StatisticsCollector:
    def __init__(self, population):
        self.population = population
        self.map =dict() 
    
    def collectAgentStatistics(self):
        vPopulation = 0
        vTotalPD = 0 # added
        vTotalMalePD = 0
        vTotalFemalePD = 0
        vMeanPD = 0 #added
        vMeanMalePD = 0
        vMeanFemalePD = 0
        vPDyes = 0 #added
        vPDyesMale = 0
        vPDyesFemale = 0
        vPDno = 0 #added
        vPDnoMale = 0
        vPDnoFemale = 0

        vTotalBMI = 0
        vTotalFemaleBMI = 0
        vTotalMaleBMI = 0
        vMeanBMI = 0
        vMeanMaleBMI = 0
        vMeanFemaleBMI = 0
        vBMIGrp1 = 0 #added
        vBMIGrp1Male = 0 #added
        vBMIGrp1Female = 0 #added
        vBMIGrp2 = 0 #added
        vBMIGrp2Male = 0 #added
        vBMIGrp2Female = 0 #added
        vBMIGrp3 = 0 #added
        vBMIGrp3Male = 0 #added
        vBMIGrp3Female = 0 #added
        vBMIGrp4 = 0 #added
        vBMIGrp4Male = 0 #added
        vBMIGrp4Female = 0 #added

        vMale = 0
        vFemale = 0
        vAgeGroup1 = 0
        vAgeGroup2 = 0
        vAgeGroup3 = 0
        vAgeGroup4 = 0
        v12MonthEaters = 0
        v12MonthEatersMale = 0
        v12MonthEatersFemale = 0
        v12MonthEatersAgeGroup1 = 0
        v12MonthEatersAgeGroup2 = 0
        v12MonthEatersAgeGroup3 = 0
        v12MonthEatersAgeGroup4 = 0
        vQuantMale = 0.0
        vQuantFemale = 0.0
        vQuantAgeGroup1 = 0.0
        vQuantAgeGroup2 = 0.0
        vQuantAgeGroup3 = 0.0
        vQuantAgeGroup4 = 0.0
        vFreqMale = 0
        vFreqFemale = 0
        vFreqAgeGroup1 = 0
        vFreqAgeGroup2 = 0
        vFreqAgeGroup3 = 0
        vFreqAgeGroup4 = 0

        vSESlowMale = 0
        vSESmedMale = 0
        vSEShighMale = 0

        vSESlowFemale = 0
        vSESmedFemale = 0
        vSEShighFemale = 0


        vHFSSQuantMale = 0
        vHFSSQuantFemale = 0

        vHFSSQuantSESlowMale = 0
        vHFSSQuantSESmedMale = 0
        vHFSSQuantSEShighMale = 0

        vHFSSQuantSESlowFemale = 0
        vHFSSQuantSESmedFemale = 0
        vHFSSQuantSEShighFemale = 0

        vcountschemasMale = 0
        vcountschemasFemale = 0

        vcountschema1Male = 0
        vcountschema2Male = 0
        vcountschema3Male = 0
        vcountschema4Male = 0
        vcountschema5Male = 0

        vcountschema1Female = 0
        vcountschema2Female = 0
        vcountschema3Female = 0
        vcountschema4Female = 0
        vcountschema5Female = 0

        for individual in self.population.agents():
            # Cache if this agent had more than one drink on atleast the last year, to avoid recomputing it many times. This isnt' super helpful here due to elseifs.
            moreThanOneCalorieThisYear = individual.isHaveKCaloriesOverNDays(365, 1)
    
            vPopulation += 1
            vTotalPD += 12*individual.getPD()
            vTotalBMI += maxValue*individual.getbmival()

            # Track PD values
            if(12*individual.getPD() >= 4):
                vPDyes += 1
            elif(12*individual.getPD() < 4):
                vPDno += 1
            # elif(12*individual.getPD()>12):
            #     print(individual.getPD())

            # added this to try and track BMI values
            if (maxValue*individual.getbmival() < 18.5):
                vBMIGrp1 += 1
            elif (maxValue*individual.getbmival() >= 18.5 and maxValue*individual.getbmival() < 25):
                vBMIGrp2 += 1
            elif (maxValue*individual.getbmival() >= 25 and maxValue*individual.getbmival() < 30):
                vBMIGrp3 += 1
            elif (maxValue*individual.getbmival() >= 30):
                vBMIGrp4 += 1
    
            # By gender
            if (individual.getSex() == MALE): 
                vMale += 1
                vTotalMalePD += 12*individual.getPD()
                vTotalMaleBMI += maxValue*individual.getbmival()
                if(12*individual.getPD() >= 4):
                    vPDyesMale += 1
                elif(12*individual.getPD() < 4):
                    vPDnoMale += 1

                if (maxValue*individual.getbmival() < 18.5):
                    vBMIGrp1Male += 1
                elif (maxValue*individual.getbmival() >= 18.5 and maxValue*individual.getbmival() < 25):
                    vBMIGrp2Male += 1
                elif (maxValue*individual.getbmival() >= 25 and maxValue*individual.getbmival() < 30):
                    vBMIGrp3Male += 1
                elif (maxValue*individual.getbmival() >= 30):
                    vBMIGrp4Male += 1

                if (moreThanOneCalorieThisYear): 
                    v12MonthEatersMale += 1
                    vQuantMale += individual.getAvgCaloriesNDays(365)
                    vFreqMale += individual.getNumDaysHavingKCaloriesOverNDays(30,1)
                    vHFSSQuantMale += individual.getAvgHFSSCaloriesNDays(365)
                    vcountschemasMale = individual.getSchemaCountOverNDays(365)
                    vcountschema1Male += vcountschemasMale[0]
                    vcountschema2Male += vcountschemasMale[1]
                    vcountschema3Male += vcountschemasMale[2]
                    vcountschema4Male += vcountschemasMale[3]
                    vcountschema5Male += vcountschemasMale[4]

                    if (individual.getSes() == "low"):
                        vSESlowMale +=1
                        if (moreThanOneCalorieThisYear):
                            vHFSSQuantSESlowMale += individual.getAvgHFSSCaloriesNDays(365)

                    elif (individual.getSes() == "medium"):
                        vSESmedMale +=1
                        if (moreThanOneCalorieThisYear):
                            vHFSSQuantSESmedMale += individual.getAvgHFSSCaloriesNDays(365)

                    elif (individual.getSes() == "high"):
                        vSEShighMale +=1
                        if (moreThanOneCalorieThisYear):
                            vHFSSQuantSEShighMale += individual.getAvgHFSSCaloriesNDays(365)

            elif (individual.getSex() == FEMALE):
                vFemale += 1
                vTotalFemalePD += 12*individual.getPD()
                vTotalFemaleBMI += maxValue*individual.getbmival()
                if(12*individual.getPD() >= 4):
                    vPDyesFemale += 1
                elif(12*individual.getPD() < 4):
                    vPDnoFemale += 1

                if (maxValue*individual.getbmival() < 18.5):
                    vBMIGrp1Female += 1
                elif (maxValue*individual.getbmival() >= 18.5 and maxValue*individual.getbmival() < 25):
                    vBMIGrp2Female += 1
                elif (maxValue*individual.getbmival() >= 25 and maxValue*individual.getbmival() < 30):
                    vBMIGrp3Female += 1
                elif (maxValue*individual.getbmival() >= 30):
                    vBMIGrp4Female += 1

                if (moreThanOneCalorieThisYear):
                    v12MonthEatersFemale += 1
                    vQuantFemale += individual.getAvgCaloriesNDays(365)
                    vFreqFemale += individual.getNumDaysHavingKCaloriesOverNDays(30,1)
                    vHFSSQuantFemale += individual.getAvgHFSSCaloriesNDays(365)
                    vcountschemasFemale = individual.getSchemaCountOverNDays(365)
                    vcountschema1Female += vcountschemasFemale[0]
                    vcountschema2Female += vcountschemasFemale[1]
                    vcountschema3Female += vcountschemasFemale[2]
                    vcountschema4Female += vcountschemasFemale[3]
                    vcountschema5Female += vcountschemasFemale[4]


                    if (individual.getSes() == "low"):
                        vSESlowFemale +=1
                        if (moreThanOneCalorieThisYear):
                            vHFSSQuantSESlowFemale += individual.getAvgHFSSCaloriesNDays(365)

                    elif (individual.getSes() == "medium"):
                        vSESmedFemale +=1
                        if (moreThanOneCalorieThisYear):
                            vHFSSQuantSESmedFemale += individual.getAvgHFSSCaloriesNDays(365)

                    elif (individual.getSes() == "high"):
                        vSEShighFemale +=1
                        if (moreThanOneCalorieThisYear):
                            vHFSSQuantSEShighFemale += individual.getAvgHFSSCaloriesNDays(365)

                
            # By age
            if (individual.getAge() >= Globals.AGE_GROUPS[0] and individual.getAge() <= Globals.AGE_GROUPS[1]):
                vAgeGroup1 += 1
                if (moreThanOneCalorieThisYear):
                    v12MonthEatersAgeGroup1 += 1
                    vQuantAgeGroup1 += individual.getAvgCaloriesNDays(30)
                    vFreqAgeGroup1 += individual.getNumDaysHavingKCaloriesOverNDays(30,1)
            elif (individual.getAge() >= Globals.AGE_GROUPS[1] and individual.getAge() <= Globals.AGE_GROUPS[2]):
                vAgeGroup2 += 1
                if (moreThanOneCalorieThisYear):
                    v12MonthEatersAgeGroup2 += 1
                    vQuantAgeGroup2 += individual.getAvgCaloriesNDays(30)
                    vFreqAgeGroup2 += individual.getNumDaysHavingKCaloriesOverNDays(30,1)
            elif (individual.getAge() >= Globals.AGE_GROUPS[2] and individual.getAge() <= Globals.AGE_GROUPS[3]):
                vAgeGroup3 += 1
                if (moreThanOneCalorieThisYear):
                    v12MonthEatersAgeGroup3 += 1
                    vQuantAgeGroup3 += individual.getAvgCaloriesNDays(30)
                    vFreqAgeGroup3 += individual.getNumDaysHavingKCaloriesOverNDays(30,1)
            elif (individual.getAge() >= Globals.AGE_GROUPS[4]):
                vAgeGroup4 += 1
                if (moreThanOneCalorieThisYear):
                    v12MonthEatersAgeGroup4 += 1
                    vQuantAgeGroup4 += individual.getAvgCaloriesNDays(30)
                    vFreqAgeGroup4 += individual.getNumDaysHavingKCaloriesOverNDays(30,1)

        vMeanPD = vTotalPD/vPopulation    
        vMeanMalePD = vTotalMalePD/vMale
        vMeanFemalePD = vTotalFemalePD/vFemale

        vMeanBMI = vTotalBMI/vPopulation
        vMeanMaleBMI = vTotalMaleBMI/vMale
        vMeanFemaleBMI = vTotalFemaleBMI/vFemale

        if vMeanPD > 12:
            print("mean PD is over the limit")
            # print(vPopulation)

        self.map["Population"] = vPopulation
        self.map["meanPD"] = vMeanPD
        self.map["meanPDmale"] = vMeanMalePD
        self.map["meanPDfemale"] = vMeanFemalePD
        self.map["PDyes"] = vPDyes
        self.map["PDyesMale"] = vPDyesMale
        self.map["PDyesFemale"] = vPDyesFemale
        self.map["PDno"] = vPDno
        self.map["PDnoMale"] = vPDnoMale
        self.map["PDnoFemale"] = vPDnoFemale

        self.map["meanBMI"] = vMeanBMI
        self.map["meanBMImale"] = vMeanMaleBMI
        self.map["meanBMIfemale"] = vMeanFemaleBMI
        self.map["bmiGrp1"] = vBMIGrp1
        self.map["bmiGrp1Male"] = vBMIGrp1Male
        self.map["bmiGrp1Female"] = vBMIGrp1Female
        self.map["bmiGrp2"] = vBMIGrp2
        self.map["bmiGrp2Male"] = vBMIGrp2Male
        self.map["bmiGrp2Female"] = vBMIGrp2Female
        self.map["bmiGrp3"] = vBMIGrp3
        self.map["bmiGrp3Male"] = vBMIGrp3Male
        self.map["bmiGrp3Female"] = vBMIGrp3Female
        self.map["bmiGrp4"] = vBMIGrp4
        self.map["bmiGrp4Male"] = vBMIGrp4Male
        self.map["bmiGrp4Female"] = vBMIGrp4Female

        self.map["Male"] = vMale
        self.map["Female"] = vFemale
        self.map["AgeGroup1"] = vAgeGroup1
        self.map["AgeGroup2"] = vAgeGroup2
        self.map["AgeGroup3"] = vAgeGroup3
        self.map["AgeGroup4"] = vAgeGroup4
        self.map["12MonthEaters"] = v12MonthEaters
        self.map["12MonthEatersMale"] = v12MonthEatersMale
        self.map["12MonthEatersFemale"] = v12MonthEatersFemale
        self.map["12MonthEatersAgeGroup1"] = v12MonthEatersAgeGroup1
        self.map["12MonthEatersAgeGroup2"] = v12MonthEatersAgeGroup2
        self.map["12MonthEatersAgeGroup3"] = v12MonthEatersAgeGroup3
        self.map["12MonthEatersAgeGroup4"] = v12MonthEatersAgeGroup4
        self.map["QuantMale"] = vQuantMale
        self.map["QuantFemale"] = vQuantFemale
        self.map["QuantAgeGroup1"] = vQuantAgeGroup1
        self.map["QuantAgeGroup2"] = vQuantAgeGroup2
        self.map["QuantAgeGroup3"] = vQuantAgeGroup3
        self.map["QuantAgeGroup4"] = vQuantAgeGroup4
        self.map["FreqMale"] = vFreqMale
        self.map["FreqFemale"] = vFreqFemale
        self.map["FreqAgeGroup1"] = vFreqAgeGroup1
        self.map["FreqAgeGroup2"] = vFreqAgeGroup2
        self.map["FreqAgeGroup3"] = vFreqAgeGroup3
        self.map["FreqAgeGroup4"] = vFreqAgeGroup4

        self.map["HFSSQuantMale"] = vHFSSQuantMale
        self.map["HFSSQuantFemale"] = vHFSSQuantFemale
        self.map["SESlowMale"] = vSESlowMale
        self.map["SESmedMale"] = vSESmedMale
        self.map["SEShighMale"] = vSEShighMale
        self.map["HFSSQuantSESLowMale"] = vHFSSQuantSESlowMale
        self.map["HFSSQuantSESMedMale"] = vHFSSQuantSESmedMale
        self.map["HFSSQuantSESHighMale"] = vHFSSQuantSEShighMale

        self.map["SESlowFemale"] = vSESlowFemale
        self.map["SESmedFemale"] = vSESmedFemale
        self.map["SEShighFemale"] = vSEShighFemale
        self.map["HFSSQuantSESLowFemale"] = vHFSSQuantSESlowFemale
        self.map["HFSSQuantSESMedFemale"] = vHFSSQuantSESmedFemale
        self.map["HFSSQuantSESHighFemale"] = vHFSSQuantSEShighFemale

        self.map["countschema1Female"] = vcountschema1Female
        self.map["countschema2Female"] = vcountschema2Female
        self.map["countschema3Female"] = vcountschema3Female
        self.map["countschema4Female"] = vcountschema4Female
        self.map["countschema5Female"] = vcountschema5Female

        self.map["countschema1Male"] = vcountschema1Male
        self.map["countschema2Male"] = vcountschema2Male
        self.map["countschema3Male"] = vcountschema3Male
        self.map["countschema4Male"] = vcountschema4Male
        self.map["countschema5Male"] = vcountschema5Male

        self.collectTheoryAgentStatistics()
        
    def collectTheoryAgentStatistics(self):
        pass
    
    def getDataSource(self, name):
        return DataSource(self, name)
