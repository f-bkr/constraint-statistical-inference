###-----------------------------------------------------------------------------
# simdesign: Simulation study
###-----------------------------------------------------------------------------
library(SimDesign)

source("Master Skript.R")
source("Data Generation.R")
source("elr_function.R")
source("EDR_function.R")
source("goric_preference function.R")
source("bain_function.R")
source("bain_preference function.R")
source("p_value_decision function.R")

###-----------------------------------------------------------------------------
# Design
###-----------------------------------------------------------------------------
Design <- createDesign(
  N = c(24, 60, 120, 240, 480, 960),
  true_hypothesis = c(0, 1),
  small.effect = c(0, 1),
  cutoff = c("regular", "unusual"),
  hypothesis = "adjmean0 < adjmean1; adjmean1 < adjmean2"
)


###-----------------------------------------------------------------------------
# Generate
###-----------------------------------------------------------------------------
Generate <- function(condition, fixed_objects) {
  data <- generateData(
    N = condition$N,
    hypothesis = condition$true_hypothesis,
    small.effect = condition$small.effect
  )
  return(data)
}


###-----------------------------------------------------------------------------
# Analyse
###-----------------------------------------------------------------------------
Analyse <- function(condition, dat, fixed_objects) {
  
  ## NHST in ELR
  elrmod <- elr_function(data = dat)
  p_nhst <- p_value_decision(p_value = elrmod@results@hypotheses[1, "p-value"],
                             true_hypothesis = condition$true_hypothesis,
                             cutoff = condition$cutoff)
  
  ## IHT in ELR
  p_iht <- p_value_decision(p_value = effectLite_iht(
                            constraints = condition$hypothesis, 
                            test = "Wald", 
                            object = elrmod)$pvalue,
                            true_hypothesis = condition$true_hypothesis,
                            cutoff = condition$cutoff)
  
  
  ## GORIC(A)
  parnames <- c("adjmean0","adjmean1","adjmean2")
  est_AdjMeans <- elrmod@results@est[parnames]
  VCOV_AdjMeans <- elrmod@results@vcov.def[parnames,parnames]
  gorica_AdjMeans <- goric(est_AdjMeans, 
                           VCOV=VCOV_AdjMeans, 
                           hypotheses=list(H1=condition$hypothesis))
  
  goric_decision <- goric_preference(gorica_AdjMeans, condition$true_hypothesis)
  
  
  ## bain
  bain_AdjMeans <- bain_function(est_AdjMeans = est_AdjMeans, 
                                 VCOV_AdjMeans=VCOV_AdjMeans, 
                                 hypothesis = condition$hypothesis, 
                                 n = condition$N)
  
  bain_decision <- bain_preference(bain_AdjMeans, condition$true_hypothesis)
  
  
  #returning a named vector:
  return(c(
    p_nhst = p_nhst,
    p_iht = p_iht,
    goric_iht = goric_decision,
    bain_iht = bain_decision))
}


###-----------------------------------------------------------------------------
# Summarise
###-----------------------------------------------------------------------------
Summarise <- function(condition, results, fixed_objects) {
  
  edr_nhst  <- mean(results[, "p_nhst"]  == 1, na.rm = TRUE)
  edr_iht  <- mean(results[, "p_iht"]  == 1, na.rm = TRUE)
  edr_goric  <- mean(results[, "goric_iht"]  == 1, na.rm = TRUE)
  edr_bain  <- mean(results[, "bain_iht"]  == 1, na.rm = TRUE)
  
  return(c(
    EDR_NHST = edr_nhst,
    EDR_IHT = edr_iht,
    EDR_GORIC = edr_goric,
    EDR_BAIN = edr_bain))
}


###-----------------------------------------------------------------------------
# Simulation
###-----------------------------------------------------------------------------
res <- runSimulation(design = Design,
                     replications = 1000,
                     generate = Generate,
                     analyse = Analyse,
                     summarise = Summarise,
                     parallel = TRUE)

write.csv(res, "Simulation_2")
