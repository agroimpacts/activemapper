# Summarize labeller versus segmentation data from grid, tile, aoi scales
library(dplyr)
library(activemapper)

# data
datnms <- c("top_label_data", "field_validation_stats",
            "tile_aoi_validation_stats")
for(i in datnms) {
  load(system.file(glue::glue("extdata/{i}.rda"), package = "activemapper"))
}

# Summary statistics at each scale, per AOI and across AOIs:
# 1) mean and 2) median area, and 3) mean and 4) median field count
#------------------------------------------------------------------------------#
# grid scale
human <- top_label_data$stats %>%
  mutate(mu_prop = NA, type = "Validation") %>%
  select(-contains("id"))
machine <- field_validation_stats %>%
  mutate(type = "Segmentation")
humachine_grid <- bind_rows(human, machine) %>%
  mutate(aoi = as.numeric(gsub("labeller", "", aoi))) %>%
  mutate(scale = "grid") %>% arrange(aoi) %>%
  select(scale, aoi, type, n, Mean, Median, StDev, mu_prop, x, y)

# humachine_grid %>% filter(aoi == 1 & type == "Segmentation") %>%
#   summarize(WMed = median(rep(Median, times = n)),
#             Med = median(Median),
#             Med2 = mean(Median),
#             WMed2 = spatstat::weighted.median(Median, w = n))

# Summarize AOI-wise averages (field size)
humachine_grid_sum <- humachine_grid %>% group_by(aoi, type) %>%
  summarize(Median = median(Mean), Mean = mean(Mean)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "grid", measure = "area") %>%
  select(scale, aoi, type, measure, !!names(.))

# set up summarizing function
sum_func <- function(x, scale, measure) {
  x %>% group_by(type, Stat) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    mutate(scale = scale, aoi = "All", measure = measure) %>%
    select(scale, aoi, type, measure, Stat, value) %>%
    rbind(x, .)
}

# combine with grid-wise averages
humachine_grid_sum <- sum_func(humachine_grid_sum, "grid", "area")

# field number
humachine_grid_sum_n <- humachine_grid %>% group_by(aoi, type) %>%
  summarize(Median = median(n), Mean = mean(n)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "grid", measure = "n") %>%
  select(scale, aoi, type, measure, !!names(.))

# combine with grid-wise averages
humachine_grid_sum_n <- sum_func(humachine_grid_sum_n, "grid", "n")

# # mean across AOIs - evaluated against 1600 validation sites
# # area
# humachine_grid_all <- humachine_grid %>% group_by(type) %>%
#   summarize(Median = median(Mean), Mean = mean(Mean)) %>%
#   tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
#   mutate(scale = "grid", aoi = "All", measure = "area") %>%
#   select(scale, aoi, type, measure, !!names(.))
#
# # field number
# humachine_grid_all_n <- humachine_grid %>% group_by(type) %>%
#   summarize(Median = median(n), Mean = mean(n)) %>%
#   tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
#   mutate(scale = "grid", aoi = "All", measure = "n") %>%
#   select(scale, aoi, type, measure, !!names(.))
#
#
#
# # Combine grids
# segstats_grid <- rbind(humachine_grid_sum, humachine_grid_all,
#                        humachine_grid_sum_n, humachine_grid_all_n)
#
# aoi_wide <- tile_aoi_validation_stats$aoi %>% group_by(type) %>%
#   summarize(Mean = weighted.mean(area_mu, w = n),
#             Median = weighted.mean(area_med, w = n)) %>%
#   tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
#   # tidyr::pivot_wider(names_from = type, values_from = value) %>%
#   mutate(aoi = "All", scale = "aoi", measure = "area") %>%
#   select(scale, aoi, type, measure, !!names(.))

#------------------------------------------------------------------------------#
# Tile scale - averages of validation polygons and segments within tiles that
# they fall in

# [double-check] rationale for these statistics

## tile scale
humachine_tile <- tile_aoi_validation_stats$tile %>%
  mutate(type = ifelse(grepl("segments", type), "Segmentation", type),
         type = ifelse(grepl("validation", type), "Validation", type)) %>%
  select(type, aoi, tile, area_mu, area_med, n_mu) %>%
  rename(Mean = area_mu, Median = area_med)

# humachine_tile %>% group_by(aoi, type) %>%
#   summarize(Median = median(Mean), Mean = mean(Mean))
# area
humachine_tile_sum <- humachine_tile %>% group_by(aoi, type) %>%
  summarize(Median = median(Mean), Mean = mean(Mean)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "tile", measure = "area") %>%
  select(scale, aoi, type, measure, !!names(.))

# combine with tile-wise averages
humachine_tile_sum <- sum_func(humachine_grid_sum, "tile", "area")

# field count
humachine_tile_sum_n <- humachine_tile %>% group_by(aoi, type) %>%
  summarize(Median = median(n_mu), Mean = mean(n_mu)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "tile", measure = "n") %>%
  select(scale, aoi, type, measure, !!names(.))

# combine with tile-wise averages
humachine_tile_sum_n <- sum_func(humachine_grid_sum, "tile", "n")

# # median and means across AOIs - evaluated against validation tiles
# # area
# humachine_tile_all <- humachine_tile %>% group_by(type) %>%
#   summarize(Median = median(Mean), Mean = mean(Mean)) %>%
#   tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
#   mutate(scale = "tile", aoi = "All", measure = "area") %>%
#   select(scale, aoi, type, measure, !!names(.))
#
# # field number
# humachine_tile_all_n <- humachine_tile %>% group_by(type) %>%
#   summarize(Median = median(n_mu), Mean = mean(n_mu)) %>%
#   tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
#   mutate(scale = "grid", aoi = "All", measure = "n") %>%
#   select(scale, aoi, type, measure, !!names(.))

# # Combine grids
# segstats_tiles <- rbind(humachine_tile_sum, humachine_tile_all,
#                         humachine_tile_sum_n, humachine_tile_all_n)

#------------------------------------------------------------------------------#
# AOI scale - averages of validation polygons and segments within AOIs

## AOI scale
humachine_aoi <- tile_aoi_validation_stats$aoi %>%
  mutate(type = ifelse(grepl("segments", type), "Segmentation", type),
         type = ifelse(grepl("validation", type), "Validation", type)) %>%
  select(type, aoi, area_mu, area_med, n) %>%
  rename(Mean = area_mu, Median = area_med)

## aoi scale
humachine_aoi_sum <- humachine_aoi %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "aoi", measure = "area") %>%
  select(scale, aoi, type, measure, Stat, value)

# combine with AOI-wise averages
humachine_aoi_sum <- sum_func(humachine_aoi_sum, "tile", "area")

# humachine_tile_sum_n %>% group_by(type, Stat) %>%
#   summarize(value = mean(value))

# humachine_aoi %>% group_by(type) %>%
#   summarize(Median = median(Mean),
#             Mean = mean(Mean))

# ## across AOI
# humachine_aoi_all <- humachine_aoi %>% group_by(type) %>%
#   summarize(Mean = weighted.mean(Mean, w = n),
#             Median = weighted.mean(Median, w = n)) %>%
#   tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
#   mutate(aoi = "All", scale = "aoi", measure = "area") %>%
#   select(scale, aoi, type, measure, !!names(.))

# Combine grids
# segstats_aoi <- rbind(humachine_aoi_sum, humachine_aoi_all)

# # Country-wide mean
# segment_files <- dir(here::here("external/data/results/segments/merged"),
#                      pattern = "boundarymerge", full.names = TRUE)
# iid <- as.numeric(gsub("[[:alpha:]]|_|\\.", "", basename(segment_files)))
# segment_files <- tibble(file = segment_files, order = iid) %>% arrange(iid)
# segment_files <- segment_files %>% filter(!grepl("aoi3_*.*merge.geojson", file))
#
# # area and quiet func
# ha <- function(x) as.numeric(units::set_units(st_area(x), "ha"))
#
# registerDoMC(cores = 7)
# seg_areas <- foreach(x = 1:length(segment_files), .combine = rbind) %dopar% {
#   # x <- 8 #7 #3 #7 #5
#   flds <- segment_files %>% filter(order == x) %>% pull(file) %>%
#     read_sf() %>% select(-tile) %>%
#     filter(!duplicated(.)) %>%
#     mutate(area = ha(.), aoi = x) %>%
#     as_tibble() %>%
#     select(aoi, id, area)
# }
#
# top_label_data$maps %>% summarize(Mean = mean(area), Median = median(area))
# seg_areas %>% summarize(Mean = mean(area), Median = median(area))


#------------------------------------------------------------------------------#
# Combine
segment_quality_stats <- rbind(humachine_grid_sum,
                               humachine_grid_sum_n,
                               humachine_tile_sum,
                               humachine_tile_sum_n,
                               humachine_aoi_sum)
usethis::use_data(segment_quality_stats, overwrite = TRUE)


