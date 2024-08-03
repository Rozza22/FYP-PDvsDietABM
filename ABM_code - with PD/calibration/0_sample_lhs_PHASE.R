sample_lhs <- function(N_SAMPLES){
  prior <- list(    #INDIVIDUAL LEVEL PARAMETERS 
    c("qunif", 0, 3), #BAUTONOMY 1 Sex = m
    c("qunif", 0, 3), #B AUTONOMY 2 Sex = m 
    c("qunif", 0, 3), #B AUTONOMY 1 Sex = f
    c("qunif", 0, 3), #B AUTONOMY 2 Sex = f
    c("qunif", 39, 102), #HABIT UPDATE INTERVAL MEAN 
    c("qunif", 20, 60), #HABIT UPDATE INTERVAL SD 
    #POPULATION LEVEL PARAMETERS 
    c("qunif", 30, 90), #N DAYS DESCRIPTIVE CALC
    c("qunif", 30, 90), #N DAYS DESCRIPTIVE TRANS
    c("qunif", 0, 1), #BIAS FACTOR DESCRIPTIVE NORM
    c("qunif", 0, 1), #BETA ATTITUDE
    c("qunif", 0, 1), #BETA NORM
    c("qunif", 0, 1), #BETA PBC
    c("qunif", 0, 1), #BETA RESTRAINT
    c("qunif", 0, 1), #BETA EMOTIONAL
    c("qunif", 0, 1), # BETA PD added in 
    c("qunif", 0, 1), # BETA HFSSbmival added in 
    c("qunif", 0, 1), # BETA bmivalPD added in
    c("qunif", 0, 1), # BETA bmivalEmo added in
    c("qunif", 0, 1), # BETA INTERVENTIONEFFECT added in
    c("qunif", 0, 1), # BETA INTERVENTIONSCALE added in
    c("qunif", 0, 1), # ALPHA ATTITUDE added in
    c("qunif", 0, 1), # ALPHA PD added in
    c("qunif", 0, 1) # ALPHA BMI added in
  )
  
  names(prior) <- c("BAUTONOMY1_SEXM","BAUTONOMY2_SEXM","BAUTONOMY1_SEXF","BAUTONOMY2_SEXF",
                    "HABIT_UPDATE_MEAN","HABIT_UPDATE_SD",
                    "DESCRIPTIVE_CALC_N", "DESCRIPTIVE_TRANS_N", "BIAS_FACTOR","BETA_ATTITUDE","BETA_NORM",
                    "BETA_PBC","BETA_RESTRAINT","BETA_EMOTIONAL",
                    "BETA_PD", "BETA_HFSSBMIVAL", "BETA_BMIVALPD", 
                    "BETA_BMIVALEMO", "BETA_INTERVENTIONEFFECT", "BETA_INTERVENTIONSCALE", 
                    "ALPHA_ATTITUDE", "ALPHA_PD", "ALPHA_BMI"
  )
  
  N_PRIORS <- length(prior)
  # print(N_PRIORS)
  set.seed(as.numeric(Sys.time()))
  lhsSampleUniforms <- maximinLHS(N_SAMPLES, N_PRIORS)
  lhsSample <- matrix(nrow = N_SAMPLES, ncol = N_PRIORS)
  
  for(i in 1:N_PRIORS) {
    lhsSample[, i] <- eval(call(prior[[i]][1], lhsSampleUniforms[, i],
                                as.numeric(prior[[i]][2]), as.numeric(prior[[i]][3])))
  }
  
  lhsSample <- as.data.frame(lhsSample)
  version <- c(1:N_SAMPLES)
  lhsSample <- cbind(version, lhsSample)
  names(lhsSample) <- c("SampleNum",names(prior))
  return(lhsSample)
}
