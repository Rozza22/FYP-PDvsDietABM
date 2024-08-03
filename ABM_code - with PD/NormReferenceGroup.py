#include "NormReferenceGroup.h"
#include "globals.h"

#include <iostream>
 
import NormGlobals
from core import Globals

class NormReferenceGroup:
    
    def __init__(self):
        # for loop, init all ref groups
        self.mNumberOfReferenceGroups = Globals.NUM_SEX*Globals.NUM_AGE_GROUPS*Globals.NUM_SES
        self.mReferenceGroups = [] 
        for sex in ["m", "f"]:
            for j in range(0, Globals.NUM_AGE_GROUPS):
                for ses in ["low", "medium", "high"]:
                    self.mReferenceGroups.append((sex, j, ses))
    
    def size(self):
        return self.mNumberOfReferenceGroups
    
    def getId(self, sex, ageGroup, ses):
        refGroup = (sex, int(ageGroup), ses)
        for i in range(0, self.mNumberOfReferenceGroups):
            if (self.mReferenceGroups[i] == refGroup):
                return i
    
        print("Error: Do not find any reference group for [" + sex + " " + str(ageGroup) + " " + str(ses) + "].")
        return -1; #do not find any matching reference group
    
    def compare(self, id1, id2):
        # declare local integer as counter
        numberShared = 0
    
        # compare and count number of shared attributes
        (s1, a1, ses1) = self.mReferenceGroups[id1]
        (s2, a2, ses2) = self.mReferenceGroups[id2]
    
        # sum the difference (note: cannot loop the index for tuple)
        numberShared += 1 if s1 == s2 else 0
        numberShared += 1 if a1 == a2 else 0
        numberShared += 1 if ses1 == ses2 else 0
    
        # return number of shared attributes
        return numberShared