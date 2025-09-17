source("Master Skript.R")

# seed for reproducibility
set.seed(313)



####
# Type I error rate
#
# - H0 is always true
# - Vary decision threshold
####

start_simulation(repetitions=1000, N=c(24, 60, 120, 240, 480), true_hypothesis=0, conditions = c("nhst", "iht", "goric", "bain"),
                 hypothesis = "adjmean0 < adjmean1; adjmean1 < adjmean2", p_threshold = 0.05, small.effect=0)



####
# Type II error rate
#
# - H1 is always true
# - vary effect sizes
# - Vary decision threshold
####



#-------------------------------------------------------------------------------
# Profiling for runtime
#-------------------------------------------------------------------------------
Rprof("test.out")
start_simulation(repetitions=10, N=c(24, 60), true_hypothesis=0, conditions = c("nhst", "iht", "goric", "bain"),
                 hypothesis = "adjmean0 < adjmean1; adjmean1 < adjmean2", p_threshold = 0.05, small.effect=0)
Rprof(NULL)

#-------------------------------------------------------------------------------
### Errors
#-------------------------------------------------------------------------------

## Not computing PMPc

#Running bain with:
#  est_AdjMeans: 0.1446, -0.2559, -0.1567
#hypothesis: adjmean0 < adjmean1; adjmean1 < adjmean2
#n: 24
#VCOV_AdjMeans: 0.5351, 0, 0, 0, 0.3261, 0, 0, 0, 0.3963
#Warning: bain_obj is invalid, returning NA
#Bayesian informative hypothesis testing for an object of class numeric:
  
#  Fit   Com   BF.u  BF.c  PMPa PMPb
#H1 0.333 0.500 0.666 0.499          
#H2 0.546                            
#Hu                                  

#Hypotheses:
#  H1: adjmean0<adjmean1
#H2: adjmean1<adjmean2

#Note: BF.u denotes the Bayes factor of the hypothesis at hand versus the unconstrained hypothesis Hu. BF.c denotes the Bayes factor of the hypothesis at hand versus its complement. PMPa contains the posterior model probabilities of the hypotheses specified. PMPb adds Hu, the unconstrained hypothesis. PMPc adds Hc, the complement of the union of the hypotheses specified.


#-------------------------------------------------------------------------------

##Not computing PMP at all ?!?

#Running bain with:
#  est_AdjMeans: 0.1446, -0.2559, -0.1567
#hypothesis: adjmean0 < adjmean1; adjmean1 < adjmean2
#n: 24
#VCOV_AdjMeans: 0.5351, 0, 0, 0, 0.3261, 0, 0, 0, 0.3963
#Warning: bain_obj is invalid, returning NA
#Bayesian informative hypothesis testing for an object of class numeric:
  
#  Fit   Com   BF.u  BF.c  PMPa PMPb
#H1 0.333 0.500 0.666 0.499          
#H2 0.546                            
#Hu                                  

#Hypotheses:
#  H1: adjmean0<adjmean1
#H2: adjmean1<adjmean2

#Note: BF.u denotes the Bayes factor of the hypothesis at hand versus the unconstrained hypothesis Hu. BF.c denotes the Bayes factor of the hypothesis at hand versus its complement. PMPa contains the posterior model probabilities of the hypotheses specified. PMPb adds Hu, the unconstrained hypothesis. PMPc adds Hc, the complement of the union of the hypotheses specified.


#-------------------------------------------------------------------------------
### test simulations
#-------------------------------------------------------------------------------
# call:
#> start_simulation(repetitions=300, N=c(100, 200), true_hypothesis=1, conditions = c("nhst", "iht", "goric"), hypothesis = "adjmean0 < adjmean1; adjmean1 < adjmean2", effect.magnitude = 1, effect.ratio = 1)
#
#        100       200
#nhst  0.8933333 0.8166667
#iht   0.7500000 0.6400000
#goric 0.9300000 0.9200000
#
#Warnmeldungen:                   ???
#1: lavaan->lav_samplestats_icov():  
#  sample covariance matrix in group: 4 is not positive-definite 
#2: lavaan->lav_samplestats_icov():  
#  sample covariance matrix in group: 4 is not positive-definite 
#3: lavaan->lav_samplestats_icov():  
#  sample covariance matrix in group: 1 is not positive-definite 
#4: lavaan->lav_samplestats_icov():  
#  sample covariance matrix in group: 1 is not positive-definite 


# call (runtime ~35 Min):
#> start_simulation(repetitions=300, N=c(100, 200), true_hypothesis=0, conditions = c("nhst", "iht", "goric"), hypothesis = "adjmean0 < adjmean1; adjmean1 < adjmean2", effect.magnitude = 1, effect.ratio = 1)
#           100        200
#nhst  0.04666667 0.05333333
#iht   0.04333333 0.05333333
#goric 0.19666667 0.24666667


# wanna add bayes.strong & bayes.weak conditions

# r^2 = d^2 / (4 + d^2)   [Cohen, 1969].
#https://goodcalculators.com/effect-size-calculator/
#  © 2015-2025 goodcalculators.com