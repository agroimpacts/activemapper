# Label risk combined with iteration metrics
# Note: consider moving label_risk.rda creation to here from
#   data_from_instances.R

library(activemapper)
library(dplyr)

data("label_risk")
data("iteration_metrics")

# extract risk for training sites only
risk <- label_risk$stats %>%
  filter(usage == "Train") %>%
  filter(aoi %in% 1:16) %>%
  select(-usage)

# Combine with various metrics
mets <- c("Accuracy", "AUC", "F1", "TSS", "Precision", "Recall", "FPR")
metrics <- iteration_metrics %>%
  filter(Metric %in% mets) %>%
  mutate(AOI = as.factor(gsub("labeller", "", AOI))) %>%
  filter(AOI %in% 1:16) %>%
  # pivot_wider(names_from = Metric, values_from = Score) %>%
  group_by(AOI) %>% filter(Iteration == max(Iteration)) %>%
  select(-Iteration)

label_risk_metrics <- left_join(risk, metrics, by = c("aoi" = "AOI"))

usethis::use_data(label_risk_metrics)
