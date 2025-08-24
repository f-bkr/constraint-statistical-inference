library(tidyverse)

# Read data
df <- read.csv("Simulation_2")

# Keep only relevant columns
df_clean <- df %>%
  select(N, true_hypothesis, small.effect, cutoff, hypothesis,
         EDR_NHST, EDR_IHT, EDR_GORIC, EDR_BAIN)

# Pivot longer
df_long <- df_clean %>%
  pivot_longer(cols = starts_with("EDR"),
               names_to = "Method",
               values_to = "EDR")

# Relabel methods
df_long$Method <- recode(df_long$Method,
                         "EDR_NHST" = "NHST",
                         "EDR_IHT" = "IHT",
                         "EDR_GORIC" = "GORIC",
                         "EDR_BAIN" = "BAIN")

# Relabel true_hypothesis for readability
df_long$true_hypothesis <- factor(df_long$true_hypothesis,
                                  levels = c(0, 1),
                                  labels = c("H0 True", "H1 True"))





ggplot(df_long, aes(x = factor(N), y = EDR, 
                    color = Method, group = interaction(Method, small.effect),
                    linetype = factor(small.effect))) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_grid(true_hypothesis ~ cutoff) +
  labs(title = "Empirical Discovery Rates across Methods",
       subtitle = "Faceted by true hypothesis and cutoff type",
       x = "Sample Size (N)",
       y = "EDR",
       color = "Method",
       linetype = "Effect Size (0=big, 1=small)") +
  theme_minimal(base_size = 14)
