### library ####
library(EffectLiteR)
library(restriktor)
library(bain)
library(tidyverse)

source("Data Generation.R")
source("elr_function.R")
source("EDR_function.R")
source("goric_preference function.R")
source("bain_function.R")
source("bain_preference function.R")
source("p_value_decision function.R")
#------------------------------------------------------------------------------------------------------------------------


start_simulation <- function (repetitions=5, N, true_hypothesis=0, conditions, hypothesis, p_threshold=0.05, small.effect=0){
  
results <- array(0, dim = c(repetitions, length(N), length(conditions)),
                 dimnames = list(NULL, as.character(N), conditions))
# A 3d Array with [dim 1: repitition number, dim 2: N, dim 3: condition]


for (n in N) {
  for (r in 1:repetitions){
    print(r) #to know status of simulation process
    
    data <- generateData(N=n, hypothesis=true_hypothesis, small.effect=small.effect0)
    
    ## NHST in ELR
    elrmod <- elr_function(data=data)
    
    p_nhst <- p_value_decision(p_value = elrmod@results@hypotheses[1,"p-value"],
                               true_hypothesis = true_hypothesis,
                               threshold = p_threshold)
    
    ## IHT in ELR
    p_iht <- p_value_decision(p_value = effectLite_iht(constraints = hypothesis, test = "Wald", object = elrmod)$pvalue,
                              true_hypothesis = true_hypothesis,
                              threshold = p_threshold)
    
    
    ## GORIC(A)
    parnames <- c("adjmean0","adjmean1","adjmean2")
    est_AdjMeans <- elrmod@results@est[parnames]
    VCOV_AdjMeans <- elrmod@results@vcov.def[parnames,parnames]
    gorica_AdjMeans <- goric(est_AdjMeans, VCOV=VCOV_AdjMeans, hypotheses=list(H1=hypothesis))
    
    goric_decision <- goric_preference(gorica_AdjMeans, true_hypothesis) # EDR with maximum goric weight

    
    ## bain
    
    bain_AdjMeans <- bain_function(est_AdjMeans = est_AdjMeans, VCOV_AdjMeans=VCOV_AdjMeans, hypothesis = hypothesis, true_hypothesis=true_hypothesis, n = n)
    
    bain_decision <- bain_preference(bain_AdjMeans, true_hypothesis)
    
    
    # to catch random "Fehler in results[r, as.character(n), "bain"] <- bain_decision : 
    #Ersetzung hat Länge 0" error
    
    if (length(bain_decision) == 0 || is.na(bain_decision)) {
      bain_decision <- NA  # or 1 if you want to treat invalid as error
    }
    
    ## results: 0 is correct decision, 1 is an erroneous decision
    results[r, as.character(n), "iht"] <- p_iht
    results[r, as.character(n), "nhst"] <- p_nhst
    results[r, as.character(n), "goric"] <- goric_decision
    results[r, as.character(n), "bain"] <- bain_decision
  }
}

return(empirical_detection_rate(results))
}

# type I error rate
# 100 reps
#        50  100
#nhst  0.05 0.05
#iht   0.11 0.04
#goric 0.20 0.24

# warning for 100 reps:
#lavaan->lav_samplestats_icov():  
#  sample covariance matrix in group: 4 is not positive-definite

