# top label data: maps and statistics from the most accurate labeller to map
# validation sites
library(dplyr)
library(sf)

#------------------------------------------------------------------------------#
# Extract maps for validation sites of most accurate labeller for each AOI
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

# main grid
mgrid <- data.table::fread(
  system.file("extdata/ghana_grid.csv", package = "activemapper")
)

aoinms <- paste0("labeller", 1:16)

# calculate worker scores
worker_scores <- instance_tbls$assignment_data %>%
  rename(id = worker_id) %>%
  filter(!is.na(score)) %>%
  filter(aoi %in% aoinms) %>%
  group_by(aoi, id) %>%
  summarize(score = mean(score), n = n()) %>%
  left_join(., instance_tbls$users) %>%
  rename(wid = id)
# worker_scores %>% filter(n < 10)

# identify validation sites
validation_sites <- instance_tbls$incoming_names %>%
  filter(aoi %in% aoinms) %>%
  filter(usage == "validate") %>%
  filter(!(aoi == "labeller3" & run == 0)) %>%
  select(aoi, name)

# pull out assignments for those sites
validation_assignments <- instance_tbls$hit_data %>%
  left_join(validation_sites, .) %>%
  select(aoi, name, hit_id, delete_time) %>%
  left_join(., instance_tbls$assignment_data) %>%
  select(aoi, name, hit_id, assignment_id, worker_id, status, delete_time)

# which assignment was done by most accurate worker to map that assignment
top_validation_assignments <- validation_assignments %>%
  rename(wid = worker_id) %>%
  filter(status %in% c("Approved")) %>% # only approved assignments
  group_by(aoi, hit_id) %>% # count()
  left_join(., worker_scores) %>% #select(aoi, name, hit_id, id, score)
  filter(score == max(score)) %>%
  select(aoi, name, hit_id, assignment_id, wid, score) %>%
  ungroup()

# get the associated maps
top_validation_maps <- left_join(
  top_validation_assignments, instance_tbls$user_maps,
  by = c("aoi", "assignment_id")
) %>%
  rename(name = name.x, field = name.y) %>%
  select(aoi, name, field, fieldnum, hit_id, assignment_id, wid, score,
         category, geom_clean)

# calculate their areas
top_validation_maps <- top_validation_maps %>%
  st_as_sf() %>%
  mutate(area = as.numeric(units::set_units(st_area(.), "ha"))) %>%
  as_tibble()

# and summary statistics
# for fields
top_validation_flds <- top_validation_maps %>%
  filter(!is.na(field)) %>%
  group_by(name) %>%
  summarize(aoi = unique(aoi), wid = unique(wid), hit_id = unique(hit_id),
            assignment_id = unique(assignment_id), n = n(), Mean = mean(area),
            Median = median(area), StDev = sd(area)) %>%
  left_join(., mgrid, by = "name") %>%
  select(-fwts) %>%
  select(aoi, name, x, y, hit_id, assignment_id, wid, n, Mean, Median, StDev)

# non fields
top_validation_noflds <- top_validation_maps %>%
  filter(is.na(field)) %>%
  mutate(n = 0, Mean = 0, Median = 0, StDev = NA) %>%
  left_join(., mgrid, by = "name") %>%
  select(-fwts) %>%
  select(aoi, name, x, y, hit_id, assignment_id, wid, n, Mean, Median, StDev)

# bind
top_validation_stats <- bind_rows(top_validation_flds, top_validation_noflds)

# combine into list
top_label_data <- list(
  "maps" = top_validation_maps, "stats" = top_validation_stats,
  "scores" = worker_scores
)

# usethis::use_data(top_label_data, overwrite = TRUE)
save(top_label_data, file = here::here("inst/extdata/top_label_data.rda"))

