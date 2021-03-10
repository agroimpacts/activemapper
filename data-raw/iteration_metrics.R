# Iteration performance metrics -
# Analyze and reshape for figure making, including calculating percentage
# change per iteration

library(dplyr)
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

# function to calculate change over time
delta_func <- function(x, rnd = 4) round((x - lag(x)) / lag(x), rnd)

# process individual AOIs
metrics <- c("Accuracy", "AUC", "F1", "TSS", "Precision", "Recall", "TPR",
             "FPR")
aoinms <- paste0("labeller", c(1:16, "1r", "8r", "15r", "8v2", "82019"))
iteration_metrics <- instance_tbls$iteration_metrics %>%
  filter(aoi %in% aoinms) %>%
  filter(iteration > 0) %>%
  filter(!(aoi == "labeller3" & run == 0)) %>% # remove run 0 labeller3
  filter(!(aoi %in% paste0("labeller", c("1r", "8r")) & run == 1)) %>% # no run1
  mutate(iteration = iteration - 1) %>% # rebase iteration counter on 0
  mutate(F1 = 2 * (precision * recall) / (precision + recall)) %>%
  rename(AOI = aoi, Iteration = iteration, Accuracy = accuracy, AUC = auc,
         TSS = tss, Precision = precision, Recall = recall, TPR = tpr,
         FPR = fpr) %>%
  select(AOI, Iteration, !!metrics) %>%
  group_by(AOI) %>%  # calculate deltas
  mutate_at(metrics, list("change" = delta_func)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Accuracy:FPR_change, names_to = "Metric",
                      values_to = "Score")

# process across AOI means and combine with AOIs
aois <- paste0("labeller", 1:16)  # calculate summary across just AOIs 1-16
iteration_metrics %>%
  filter(AOI %in% aois) %>%
  filter(Iteration < 4) %>%
  group_by(Iteration, Metric) %>%
  summarize(Score = mean(Score)) %>%
  mutate(AOI = "All") %>%
  ungroup %>%
  select(AOI, !!names(.)) %>%
  bind_rows(iteration_metrics, .) -> iteration_metrics

usethis::use_data(iteration_metrics, overwrite = TRUE)

