
cope_data <- readRDS("cope_data_for_bhs_shs_3_months.rds")



effectLiteModel <- effectLite(
  data = cope_data,
  y = "f1_cdi_mean",
  x = "condition",
  z = "b_cdi_mean",
  method = "sem",
  missing = "fiml",
  test.stat = "Wald"
)

effectLiteModel


effectLite_iht(
  constraints = "adjmean1 < adjmean2", 
  test = "Wald", 
  object = effectLiteModel)
