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

# field number
humachine_grid_sum_n <- humachine_grid %>% group_by(aoi, type) %>%
  summarize(Median = median(n), Mean = mean(n)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "grid", measure = "n") %>%
  select(scale, aoi, type, measure, !!names(.))

# mean across AOIs - evaluated against 1600 validation sites
# area
humachine_grid_all <- humachine_grid %>% group_by(type) %>%
  summarize(Median = median(Mean), Mean = mean(Mean)) %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "grid", aoi = "All", measure = "area") %>%
  select(scale, aoi, type, measure, !!names(.))

# field number
humachine_grid_all_n <- humachine_grid %>% group_by(type) %>%
  summarize(Median = median(n), Mean = mean(n)) %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "grid", aoi = "All", measure = "n") %>%
  select(scale, aoi, type, measure, !!names(.))

# Combine grids
segstats_grid <- rbind(humachine_grid_sum, humachine_grid_all,
                       humachine_grid_sum_n, humachine_grid_all_n)

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

humachine_tile_sum <- humachine_tile %>% group_by(aoi, type) %>%
  summarize(Median = median(Mean), Mean = mean(Mean)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "tile", measure = "area") %>%
  select(scale, aoi, type, measure, !!names(.))

humachine_tile_sum_n <- humachine_tile %>% group_by(aoi, type) %>%
  summarize(Median = median(n_mu), Mean = mean(n_mu)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(scale = "tile", measure = "n") %>%
  select(scale, aoi, type, measure, !!names(.))

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



aoi_wide <- tile_aoi_validation_stats$aoi %>% group_by(type) %>%
  summarize(Mean = weighted.mean(area_mu, w = n),
            Median = weighted.mean(area_med, w = n)) %>%
  tidyr::pivot_longer(cols = Median:Mean, names_to = "Stat") %>%
  mutate(aoi = "All", scale = "aoi", measure = "area") %>%
  select(scale, aoi, type, measure, !!names(.))

