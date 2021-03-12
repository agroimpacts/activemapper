#------------------------------------------------------------------------------#
# Training and validation sites for AOIs 1-16

#
# Collect training and validation sites for AOIs 1-16
library(dplyr)
library(sf)

# inputs
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))
mgrid <- data.table::fread(
  system.file("extdata/ghana_grid.csv", package = "activemapper")
)
train_val <- readr::read_csv(here::here("inst/extdata/train_val_sites.csv"))

# Initial random training draw
initial_train_pts <- train_val %>% filter(grepl("static", file)) %>%
  filter(!grepl("spatial|sub", file)) %>% distinct() %>%
  rename(Cluster = file) %>%
  mutate(Cluster = case_when(
    grepl("cluster1", Cluster) ~ "1",
    grepl("cluster2", Cluster) ~ "2",
    grepl("cluster3", Cluster) ~ "3",
  )) %>% select(-id) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# Active learning points
activelearn_pts <- instance_tbls$incoming_names %>%
  filter(aoi %in% paste0("labeller", 1:16)) %>% # distinct(aoi)
  filter(!(aoi == "labeller3" & run == 0)) %>%
  filter(processed == TRUE) %>%
  select(-one_of("processed", "label")) %>%
  left_join(., mgrid, by = "name") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  select(aoi, name, run, iteration, usage) %>%
  mutate(Type = case_when(
    usage == "validate" ~ "Validation",
    usage == "train" ~ paste("Iteration", iteration)
  ))

# pull out retraining sites for AOI3 and combine with initial train sites
initial_train_pts <- activelearn_pts %>%
  filter(aoi == "labeller3" & iteration == 0 & usage =="train") %>%
  rename(Cluster = aoi) %>% mutate(Cluster = "AOI 3 retrain") %>%
  select(Cluster, name, iteration) %>%
  rbind(initial_train_pts, .)  # bind original to to AOI3 new points

# Drop AOI3 retraining points from run 1
activelearn_pts <- activelearn_pts %>%
  filter(!(aoi == "labeller3" & iteration == 0 & usage == "train"))

# list and put in data/
train_val_sites <- list("initial" = initial_train_pts,
                        "active" = activelearn_pts)
usethis::use_data(train_val_sites, overwrite = TRUE)

