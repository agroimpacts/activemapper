
# calculate map accuracy
# Following Stehman and Foody (2019)
library(activemapper)
library(raster)
library(dplyr)
library(sf)

# load data
data("mapped_areas")
load(system.file("extdata/ref_int_probs.rda", package = "activemapper"))
ref_int_segments <- read_sf(
  system.file("extdata/ref_int_segments.geojson", package = "activemapper")
)

# load aez
aez <- read_sf(
  system.file("extdata/ghana_aez.geojson", package = "activemapper")
) %>% select(ID, Name) %>% rename(aezid = ID, aez = Name)

# Pick up AEZ intersections
ref_int_segments <- st_join(ref_int_segments, aez) %>%
  select(name, tile, aoi, aezid, aez, category, afrac)
# ref_int_segments %>% as_tibble() %>% group_by(aoi) %>% count()

# set up data for error matrix
error_matrix_data <- as_tibble(ref_int_segments) %>%
  # select(name, tile, aoi, aezid, aez, category, afrac) %>%
  left_join(., ref_int_probs, by = c("name", "tile", "aoi")) %>%
  select(name, tile, aoi, aezid, aez, category, afrac, mean) %>%
  rename(seg_pct = afrac, rf_prob = mean) %>%
  mutate(seg_pct = seg_pct * 100) %>%
  mutate(ref = case_when(
    category == "annualcropland" ~ 1,
    category == "noncropland" ~ 0,
    category == "unsure1" ~ 1,
    category == "unsure2" ~ 0
  )) %>%
  mutate(seg_class = ifelse(seg_pct < 0.5, 0, 1),
         prob_class = ifelse(rf_prob < 50, 0, 1)) %>%
  arrange(aoi)

# Create error matrix based on probability images
# helper function for error matrix
error_matrix <- function(pred, ref) {
  x <- table("Predicted" = pred, "Reference" = ref)
  rbind(cbind(x, Total = rowSums(x)), Total = c(colSums(x), sum(x)))
}

# plot(st_geometry(aez))
# plot(st_geometry(st_centroid(ref_int_segments)), add = TRUE)

# calculate error matrices by AOI

error_matrices_aoi <- lapply(unique(error_matrix_data$aoi), function(x) {
  dat <- error_matrix_data %>% filter(aoi == x)
  error_matrix_rf <- error_matrix(dat$prob_class, dat$ref)
  error_matrix_seg <- error_matrix(dat$seg_class, dat$ref)
  list("rf" = error_matrix_rf, "seg" = error_matrix_seg)
})
names(error_matrices_aoi) <- paste0("aoi", unique(error_matrix_data$aoi))

# # error matrices by zones (1-6, 7-9 & 12 + 15; 10-11+13+14+16)
# zones <- list(1:6, c(7:9, 12, 15), c(10:11, 13, 14, 16))
# error_matrices_zones <- lapply(zones, function(x) {
#   dat <- error_matrix_data %>% filter(aoi %in% x)
#   error_matrix_rf <- error_matrix(dat$prob_class, dat$ref)
#   error_matrix_seg <- error_matrix(dat$seg_class, dat$ref)
#   list("rf" = error_matrix_rf, "seg" = error_matrix_seg)
# })


# error matrices by AEZ
aezids <- sort(unique(error_matrix_data$aezid))
error_matrices_aez <- lapply(aezids, function(x) {
  dat <- error_matrix_data %>% filter(aezid == x)
  error_matrix_rf <- error_matrix(dat$prob_class, dat$ref)
  error_matrix_seg <- error_matrix(dat$seg_class, dat$ref)
  list("rf" = error_matrix_rf, "seg" = error_matrix_seg)
})
names(error_matrices_aez) <- paste0("aez", aezids)

# Full error matrix
error_matrix_rf <- error_matrix(error_matrix_data$prob_class,
                                error_matrix_data$ref)
error_matrix_seg <- error_matrix(error_matrix_data$seg_class,
                                 error_matrix_data$ref)
error_matrix_ghana <- list("rf" = error_matrix_rf, "seg" = error_matrix_seg)

#------------------------------------------------------------------------------#
# Outputs
error_matrices <- list("aoi" = error_matrices_aoi, "aez" = error_matrices_aez,
                       "ghana" = error_matrix_ghana)
usethis::use_data(error_matrices, overwrite = TRUE)
