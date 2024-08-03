class AdvertisingScalingNormModifier:
    def __init__(self, scalingFactor, categoriesAffected, active, advertisingtick, currenttick):
        self.scalingFactor = scalingFactor
        self.categoriesAffected = categoriesAffected
        self.active = active
        self.advertisingtick = advertisingtick
        self.currenttick = currenttick
    
    # Scales the highest categoriesAffected categories by scalingFactor across all norm schema profiles. A scalingFactor of 0.05 = 5% reduction BEFORE renormalising etc.
    def modify(self, profile, advertisingtick, currenttick):
        # If not enabled, return the unmodified profile

        if not self.active and currentick < advertisingtick:
            return profile

        else:

        # Scale down top categoriesAffected categories
            for i in range(1, self.categoriesAffected+1):
                profile[-i] *= (1.0 - self.scalingFactor)
                #profile[3] *= (1.0 - self.scalingFactor)
                #profile[4] *= (1.0 - self.scalingFactor)
                #print(profile)
            
        # Normalise weights
                total = sum(profile) 
                scaleFactor = (1.0 / total)
                return [x * scaleFactor for x in profile]
    
    def setActive(self, active):
        self.active = active