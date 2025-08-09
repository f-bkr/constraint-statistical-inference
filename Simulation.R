source("Master Skript.R")

set.seed(313)
test <- start_simulation(repetitions=2, N=c(100, 200), true_hypothesis=0, conditions = c("nhst", "iht", "goric", "bain"),
                         hypothesis = "adjmean0 < adjmean1; adjmean1 < adjmean2", p_threshold = 0.05, small.effect=0)


### test simulation

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