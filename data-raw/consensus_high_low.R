# Iteration metrics from models trained with consensus and highest and lowest
# accuracy individual labels

library(dplyr)
library(aws.s3)
library(glue)
data("iteration_metrics")

fnames <- unlist(lapply(paste0(c('low', 'high'), ".csv"), function(x) {
  paste0(glue("planet/incoming_metrics_{c(1, 2, 8, 15)}_"), x)
}))
high_low_metrics <- lapply(fnames, function(x) {
  dat <- s3read_using(FUN = readr::read_csv, object = x,
                      bucket = "activemapper")
  highlow <- gsub("planet/incoming_metrics_[0-9]+_|.csv", "", x)
  dat %>% mutate(aoi = paste0(aoi, "_", highlow)) %>% rename(AOI = aoi) %>%
    select(AOI, !!names(.))
}) %>% do.call(rbind, .)

# Process and reshape
metrics <- c("Accuracy", "AUC", "F1", "TSS", "Precision", "Recall", "TPR",
             "FPR")
high_low_metrics <- high_low_metrics %>%
  mutate(iteration = iteration - 1) %>% # rebase iteration counter on 0
  mutate(F1 = 2 * (precision * recall) / (precision + recall)) %>%
  rename(Iteration = iteration, Accuracy = accuracy, TSS = tss,
         Precision = precision, Recall = recall, TPR = tpr,
         FPR = fpr) %>%
  select(AOI, Iteration, !!metrics) %>%
  tidyr::pivot_longer(cols = Accuracy:FPR, names_to = "Metric",
                      values_to = "Score")

# save out for keeping
# usethis::use_data(high_low_metrics, overwrite = TRUE)

# bring in iteration metrics and join with consensus values from those
consensus <- iteration_metrics %>%
  filter(AOI %in% paste0("labeller", c(1, 2, 8, 15))) %>%
  group_by(AOI) %>%
  filter(Iteration == max(Iteration) & !grepl("change", Metric)) %>%
  ungroup() %>%
  mutate(Strategy = "Consensus") %>%
  mutate(AOI = gsub("labeller", "", AOI))

# join
consensus_high_low <- high_low_metrics %>%
  mutate(Strategy = tools::toTitleCase(gsub("[0-9]+_", "", AOI)),
         AOI = gsub("_.*", "", AOI)) %>%
  bind_rows(consensus, .) %>%
  mutate(AOI = as.numeric(AOI)) %>%
  arrange(AOI, Metric) %>%
  mutate(ord = group_indices(., AOI)) %>%  # define order (for plotting)
  mutate(AOI = as.character(AOI))

usethis::use_data(consensus_high_low, overwrite = TRUE)

#------------------------------------------------------------------------------#
