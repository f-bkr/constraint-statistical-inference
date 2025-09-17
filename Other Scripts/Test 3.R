dat <- generateData(N=960, hypothesis = 0)

elrmod <- elr_function(data = dat)
p_nhst <- p_value_decision(p_value = elrmod@results@hypotheses[1, "p-value"],
                           true_hypothesis = "adjmean0 < adjmean1; adjmean1 < adjmean2",
                           cutoff = "regular")

## IHT in ELR
p_iht <- p_value_decision(p_value = effectLite_iht(
  constraints = "adjmean0 < adjmean1; adjmean1 < adjmean2", 
  test = "Wald", 
  object = elrmod)$pvalue,
  true_hypothesis = 0,
  cutoff = "regular")


## GORIC(A)
parnames <- c("adjmean0","adjmean1","adjmean2")
est_AdjMeans <- elrmod@results@est[parnames]
VCOV_AdjMeans <- elrmod@results@vcov.def[parnames,parnames]
gorica_AdjMeans <- goric(est_AdjMeans, 
                         VCOV=VCOV_AdjMeans, 
                         hypotheses=list(H1="adjmean0 < adjmean1; adjmean1 < adjmean2"),
                         comparison = "unconstrained")

gorica_AdjMeans
