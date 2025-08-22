### library ####
library(EffectLiteR)
library(restriktor)
library(tidyverse)
library(bain)

source("Data Generation.R")
source("EDR_function.R")
source("goric_preference function.R")
#----------------------------------------------------------------------------

data <- generateData(N=600, hypothesis=1, small.effect = 0)

elrmod <- effectLite(
  data = data,
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
hypothesis2 = "adjmean0 < adjmean1 < adjmean2"

effectLite_iht(constraints = "adjmean0 < adjmean1 < adjmean2",
               test=,
               object=elrmod)


test <- bain(x = est_AdjMeans,
     hypothesis = hypothesis,
     n = 24,
     Sigma = list(VCOV_AdjMeans),
     group_parameters = 3, joint_parameters = 0)

test

bain_function(est_AdjMeans = est_AdjMeans, n=24, hypothesis = hypothesis, VCOV_AdjMeans = VCOV_AdjMeans)

bain_preference(test, true_hypothesis = 1, cutoff = "unusual")
bain_preference_old(test, true_hypothesis = 1)


test_g <- goric(est_AdjMeans, VCOV=VCOV_AdjMeans, hypotheses=list(H1=hypothesis))
selected <- which.max(test_g$result$gorica.weights)
selected
  
  
t <- goric(est_AdjMeans, VCOV=VCOV_AdjMeans, hypotheses=list(H1=hypothesis))
t <- effectLite_iht(constraints = "adjmean0 adjmean1 < adjmean2",
                    test="Wald",
                    object=elrmod)



p <- p_value_decision(p_value=effectLite_iht(constraints = "adjmean0 < adjmean1; adjmean1 < adjmean2",
               test="Wald",
               object=elrmod)$pvalue,
                true_hypothesis = 0,
               threshold = 0.05)
t
p





p_nhst <- p_value_decision(p_value = elrmod@results@hypotheses[1, "p-value"],
                           true_hypothesis = 0,
                           threshold = 0.05)


a <- (c(
  p_nhst = p_nhst))
a

mean(a[, "p_nhst"]  == 1, na.rm = TRUE)

#------------------------------------------------------------------------------
# effect sizes

dat <- generateData(N=9999996, hypothesis = 1, small.effect = 0)
dat_small <- generateData(N=9999996, hypothesis = 1, small.effect = 1)

dat_x0 <- dat[dat$x == 0, ]
dat_x1 <- dat[dat$x == 1, ]
dat_x2 <- dat[dat$x == 2, ]

cohen.d(dat_x2$y, dat_x0$y)
#d estimate: 0.6956135 (medium)

cohen.d(dat_x1$y, dat_x0$y)
#d estimate: 0.5547329 (medium)


dat_small_x0 <- dat_small[dat_small$x==0, ]
dat_small_x1 <- dat_small[dat_small$x==1, ]
dat_small_x2 <- dat_small[dat_small$x==2, ]

cohen.d(dat_small_x2$y, dat_small_x0$y)
#d estimate: 0.6658335 (medium)

cohen.d(dat_small_x1$y, dat_small_x0$y)
#d estimate: 0.348134 (small)

#-----------------------------------------------------------------------------
# aus mayer 2016 supplements:
# z:
# xi <- 1 + 0.5*k + 0.3*Ix1 + 0.4*Ix2 + rnorm(N,0,2)
#
#y :
#  eta <- (0.1+0.8*xi) + (0.3+0.5*k+0.2*xi)*Ix1 +
# (0.3+0.5*k+0.2*xi+0.4*k*xi)*Ix2 + rnorm(N,0,0.7)
