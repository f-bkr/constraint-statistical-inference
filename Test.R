### library ####
library(EffectLiteR)
library(restriktor)
library(tidyverse)
library(bain)

source("Data Generation.R")
source("EDR_function.R")
source("goric_preference function.R")
#----------------------------------------------------------------------------

data <- generateData(N=100, hypothesis=0)

elrmod <- effectLite(
  data = tets,
  y = "y",
  x = "x",
  k = "k",
  z = "z",
  method = "sem",
  mimic="lm",             # since we use sem but mimic lm, we need to change other arguments default setting according to lm function
  fixed.cell=TRUE,        # Mit sem Methode wäre FALSE default
  fixed.z=TRUE,           # Mit sem Methode wäre FALSE deafult
  homoscedasticity=TRUE,  # Mit sem Methode wäre FALSE deafult
  test.stat = "Wald"
)


parnames <- c("adjmean0","adjmean1","adjmean2")
est_AdjMeans <- elrmod@results@est[parnames]
VCOV_AdjMeans <- elrmod@results@vcov.def[parnames,parnames]
hypothesis = "adjmean0 <  adjmean1; adjmean1 < adjmean2"

test <- bain(x = est_AdjMeans,
     hypothesis = hypothesis,
     n = 100,
     Sigma = list(VCOV_AdjMeans),
     group_parameters = 3, joint_parameters = 0)

test
test$fit$PMPc


bain_decision <-  #is 1 if Ha is preferred, is 0 if H0 is preferred

  
  
t <- goric(est_AdjMeans, VCOV=VCOV_AdjMeans, hypotheses=list(H1=hypothesis))
t <- effectLite_iht(constraints = "adjmean0 < adjmean1; adjmean1 < adjmean2",
                    test="Wald",
                    object=elrmod)



p <- p_value_decision(p_value=effectLite_iht(constraints = "adjmean0 < adjmean1; adjmean1 < adjmean2",
               test="Wald",
               object=elrmod)$pvalue,
                true_hypothesis = 0,
               threshold = 0.05)
t
p



#-----------------------------------------------------------------------------
# aus mayer 2016 supplements:
# z:
# xi <- 1 + 0.5*k + 0.3*Ix1 + 0.4*Ix2 + rnorm(N,0,2)
#
#y :
#  eta <- (0.1+0.8*xi) + (0.3+0.5*k+0.2*xi)*Ix1 +
# (0.3+0.5*k+0.2*xi+0.4*k*xi)*Ix2 + rnorm(N,0,0.7)
