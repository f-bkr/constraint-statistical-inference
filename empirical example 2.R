library(EffectLiteR)
library(readxl)
library(mice)


S1Dataset <- read_excel("S1Dataset.xlsx")


S1Dataset$Gender <- factor(S1Dataset$Gender, levels=c(0,1), labels=c("male","female"))
S1Dataset$Use_of_antidepressants <- factor(S1Dataset$Use_of_antidepressants, levels=c(0,1), labels=c("no","yes"))
S1Dataset$Group <- factor(S1Dataset$Group, levels=c(0,1), labels=c("control","treatment"))


#md.pattern(S1Dataset)

categorical_covariates <- c("Gender", "Use_of_antidepressants")

continuous_covariates <- c("Sick_leave", "BDI_Sum_T1")

effectLiteModel <- effectLite(
  data = S1Dataset,
  y = "BDI_Sum_T3",
  x = "Group",
  k = categorical_covariates,
  z = continuous_covariates,
  method = "sem",
  missing = "fiml",
  test.stat = "Wald"
)

effectLiteModel


with(S1Dataset, table(Group, Gender, Use_of_antidepressants, !is.na(BDI_Sum_T3)))
