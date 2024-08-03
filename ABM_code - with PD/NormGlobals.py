INJUNCTIVE_THRESHOLD = 999 # this is read in from the model.props
INJUNCTIVE_PROPORTION = 99.9 # this is read in from the model.props
INJUNCTIVE_ADJUSTMENT = 99.9 # this is read in from the model.props
INJ_RELAXATION_PREV_ADJUSTMENT = 0.8 # this is read in from the model.props
INJ_PUNISHMENT_PREV_ADJUSTMENT = 0.95 # this is read in from the model.props

# Todo: this needs to be specified for the descriptive entity
DESCRIPTIVE_INCUBATION_PERIOD = 100
DESCRIPTIVE_INCUBATION_PERCENT = 0.85

# This is the bias in perceiving quantity
PERCEPTION_BIAS = 0.5 # this is read in from model.props

# Time periods over which eating behaviour it is evaluated
N_DAYS_DESCRIPTIVE = 30 # is used to generate descriptive norms on prevalence and average quantity
COMP_DAYS_PUNISH = 90 # Used in the binge punishment (average drinks in comp days > injunctive threshold)
COMP_DAYS_RELAX = 90 # Used in the injunctive relaxation (prevalence of one drink over comp days)

# CP Edit: add the discounting factor to decrease payoff with increasing drinks
DISCOUNT_MALE = 0.1
DISCOUNT_FEMALE = 0.1

DESIRE_MULTIPLIER_ABSTAINER = 0.5
DESIRE_MULTIPLIER_DRINKER = 1.5

P_REFERENCE_GROUP = None


NORM_THEORY_WEIGHT = 0.5