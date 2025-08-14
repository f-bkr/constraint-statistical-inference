###-----------------------------------------------------------------------------
# simdesign: Simulation study
###-----------------------------------------------------------------------------
library(SimDesign)
source("Master Skript.R")


###-----------------------------------------------------------------------------
# Design
###-----------------------------------------------------------------------------
Design <- createDesign(
  N = c(24, 60, 120, 240, 480),
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
  
  
  # You can add more analyses here, returning a named vector:
  return(c(
    p_nhst = p_nhst,
    p_iht = p_iht))
}


###-----------------------------------------------------------------------------
# Summarise
###-----------------------------------------------------------------------------
Summarise <- function(condition, results, fixed_objects) {
  
  edr_nhst  <- mean(results[, "p_nhst"]  == 1, na.rm = TRUE)
  edr_iht  <- mean(results[, "p_iht"]  == 1, na.rm = TRUE)
  
  return(c(
    EDR_NHST = edr_nhst,
    EDR_IHT = edr_iht))
}


###-----------------------------------------------------------------------------
# Simulation
###-----------------------------------------------------------------------------
res <- runSimulation(design = Design,
                     replications = 5,
                     generate = Generate,
                     analyse = Analyse,
                     summarise = Summarise)

write.csv(res, "Test_1_res")
