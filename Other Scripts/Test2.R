source("Data Generation.R")

library(restriktor)
library(EffectLiteR)

d <- generateData(N=120, hypothesis = 1, small.effect = 1)

generateMeasurementModel(data = d)

m <- lm(data = d, y ~ x + z + k)
summary(m)

# extract unconstrained estimates
est <- coef(m)
# unconstrained variance-covariance matrix
VCOV <- vcov(m)


goric(object = m, hypotheses = list(h1 = 'b2 > b1 > b0'))
