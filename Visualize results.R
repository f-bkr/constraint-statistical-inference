library(tidyverse)

sim_results <- res

# Step 1 — Convert to long format
sim_long <- sim_results %>%
  select(N, true_hypothesis, small.effect, cutoff, hypothesis,
         EDR_NHST, EDR_IHT, EDR_GORIC, EDR_BAIN) %>%
  pivot_longer(
    cols = starts_with("EDR_"),
    names_to = "Method",
    values_to = "EDR"
  ) %>%
  mutate(
    Method = recode(Method,
                    EDR_NHST = "NHST",
                    EDR_IHT = "IHT",
                    EDR_GORIC = "GORIC",
                    EDR_BAIN = "BAIN"),
    true_hypothesis = factor(true_hypothesis,
                             levels = c(0,1),
                             labels = c("Null is True", "Alternative is True")),
    small.effect = factor(small.effect,
                          levels = c(0,1),
                          labels = c("Large Effect", "Small Effect")),
    hypothesis = factor(hypothesis,
                        levels = c(0,1),
                        labels = c("Regular Cutoff", "Unusual Cutoff"))
  )

# Step 2 — Plot with facets for all three conditions
ggplot(sim_long, aes(x = N, y = EDR, color = Method, group = Method)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_grid(true_hypothesis + hypothesis ~ small.effect) +
  labs(
    title = "Estimated Discovery Rates (EDR) Across Methods",
    subtitle = "Faceted by Hypothesis Truth, Cutoff Type, and Effect Size",
    x = "Sample Size (N)",
    y = "EDR",
    color = "Method"
  ) +
  theme_minimal(base_size = 14)
