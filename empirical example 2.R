################################################################################
# Libary and data
################################################################################
library(EffectLiteR)
library(readxl)
library(mice)


S1Dataset <- read_excel("S1Dataset.xlsx")
S1Dataset <- as.data.frame(S1Dataset)



S1Dataset$Gender <- factor(S1Dataset$Gender, levels=c(0,1), labels=c("male","female"))
S1Dataset$Use_of_antidepressants <- factor(S1Dataset$Use_of_antidepressants, levels=c(0,1), labels=c("no","yes"))
S1Dataset$Group <- factor(S1Dataset$Group, levels=c(0,1), labels=c("control","treatment"))


################################################################################
# effectLiteR model
################################################################################
categorical_covariates <- c("Gender", "Use_of_antidepressants")

continuous_covariates <- c("BDI_Sum_T1")

effectLiteModel <- effectLite(
  data = S1Dataset,
  y = "BDI_Sum_T3",
  x = "Group",
  k = categorical_covariates,
  z = continuous_covariates,
  interactions = "no",
  method = "sem",
  missing = "fiml",
  test.stat = "Wald"
)

effectLiteModel

################################################################################
# Tests
################################################################################


# gucken ob Zellen mit y besetzt sind
with(S1Dataset, table(Group, Gender, Use_of_antidepressants, !is.na(BDI_Sum_T3)))

#gucken ob Zellen mit z besetzt sind
with(S1Dataset, tapply(Sick_leave, list(Group, Gender, Use_of_antidepressants), function(x) sum(!is.na(x))))
with(S1Dataset, tapply(BDI_Sum_T1, list(Group, Gender, Use_of_antidepressants), function(x) sum(!is.na(x))))

