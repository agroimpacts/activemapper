library(dplyr)

# Results from all instances: Average N labels and label accuracy
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

# instance_tbls$hit_data %>%
#   filter(aoi == "labeller3" & delete_time > "2019-12-05") %>%
#   left_join(., instance_tbls$assignment_data %>% filter(aoi == "labeller3"),
#             by = c("aoi", "hit_id")) %>%
#   filter(!is.na(score))
# instance_tbls$assignment_data %>%
#   filter(!(aoi == "labeller3" & completion_time < "2019-12-05" & !is.na(score)))

# select scores from Q assignments, dropping Q sites in labeller3 done during
# run 0. Filter by completion_time earlier than 5/12/2019, which was a few days
# before run 1 started. Anything after that is a Q assignment done during run 1.
# Also including Q sites done during labeller, labellertrainsouth,
# labellertrainsouth, labellertrainsouthnl, and labellertrainsw. labellernl not
# included because not used for map-making

# aoinms <- paste0("labeller", 1:16)  # original
aoinms <- c(
  "labeller",
  paste0("labeller", c("trainsouth", "trainsouthnl", "trainsw", 1:16))
)
quality_scores <- instance_tbls$assignment_data %>%
  select(-one_of("comment", "status")) %>%
  filter(!is.na(score)) %>% rename(id = worker_id) %>%
  filter(aoi %in% aoinms) %>%
  filter(!(aoi == "labeller3" & completion_time < "2019-12-05")) %>%
  left_join(., instance_tbls$users, by = c("aoi", "id")) %>%
  left_join(., instance_tbls$hit_data %>% select(aoi, hit_id, name)) %>%
  select(-contains("time")) %>% select(aoi, name, !!names(.))

# summary statistics for label quality (from Q sites)
quality_sum <- quality_scores %>% group_by(uuid) %>%
  summarize(N = n(), Mean = mean(score), StDev = sd(score)) %>%
  arrange(-Mean)

# overall number of labels
nlabels <- instance_tbls$assignment_data %>%
  filter(aoi %in% aoinms) %>%
  left_join(., instance_tbls$hit_data, by = c("aoi", "hit_id")) %>%
  left_join(., instance_tbls$kml_data, by = c("aoi", "name")) %>%
  filter(!status %in% c("Returned", "Abandoned")) %>%
  select(aoi, worker_id, status, kml_type) %>%
  rename(id = worker_id) %>%
  left_join(., instance_tbls$users %>% select(aoi, uuid, id),
            by = c("aoi", "id")) %>%
  filter(uuid %in% quality_sum$uuid) %>%
  mutate(uuid = substr(uuid, 1, 5)) %>%
  group_by(uuid, kml_type) %>% count() %>% arrange(-n) %>%
  rename(ID = uuid) %>% ungroup() %>%
  mutate(kml_type = case_when(
    kml_type == "F" ~ "Training",
    kml_type == "Q" ~ "Training reference"
  ))

# output list
label_summary <- list("scores" = quality_scores, "score_stats" = quality_sum,
                      "nlabels" = nlabels)

usethis::use_data(label_summary, overwrite = TRUE)
