# Calculate cropland area from cropland fraction and segments, for accuracy
# assessment and area estimation.
library(sf)
library(dplyr)
library(here)

# Datasets
aois <- st_read(
  system.file("extdata/aois.geojson", package = "activemapper")
) # %>% st_transform(crs = st_crs("ESRI:102022")$wkt)
# sum(aois_proj$area) == sum(as.numeric(st_area(aois_proj)) / 10^6)
africa <- st_read(
  system.file("extdata/africa.geojson", package = "activemapper")
)
aez <- st_read(
  system.file("extdata/ghana_aez.geojson", package = "activemapper")
)

# Cropland fractions from probability images
cropfrac <- raster(
  system.file("extdata/cropfrac_005.tif", package = "activemapper")
)

# Read in segments, drop original AOI3
segment_files <- dir(here("external/data/results/segments/merged"),
                     pattern = "boundarymerge", full.names = TRUE)
iid <- as.numeric(gsub("[[:alpha:]]|_|\\.", "", basename(segment_files)))
segment_files <- tibble(file = segment_files, order = iid) %>% arrange(iid)
segment_files <- segment_files %>% filter(!grepl("aoi3.*.merge.geo", file))

#------------------------------------------------------------------------------#
# Cropland fractions - in both AOIs and AEZ
# First mask out bad pixels on coasts

# mask twice
cropfrac <- raster(terra::mask(terra::rast(cropfrac), terra::vect(aois)))
cropfrac <- africa %>%
  filter(name %in% c("Ghana", "Togo", "Cte d'Ivoire", "Burkina Faso")) %>%
  mask(cropfrac, .)

# rasterize aois
aoisr <- terra::vect(aois) %>%
  terra::rasterize(., y = terra::rast(cropfrac)) %>%
  terra::mask(., terra::rast(cropfrac)) %>%
  raster(.)
# plot(aoisr); plot(cropfrac)

# rasterize aez
aezr <- terra::vect(aez) %>%
  terra::rasterize(., y = terra::rast(cropfrac)) %>%
  terra::mask(., terra::rast(cropfrac)) %>%
  raster(.)

# project
cropfrac_prj <- projectRaster(stack(cropfrac, aoisr),
                              crs = st_crs("ESRI:102022")$wkt, res = 500)
# freq(cropfrac_prj < 0)  # only 1
cropfrac_prj[cropfrac_prj < 0] <- 0
# areaproj <- cellStats(Which(!is.na(cropfrac_prj)), sum) * (500^2 * 10^-6)
# areagcs <- cellStats(mask(area(cropfrac), cropfrac), sum)
# (areaproj - areagcs) / areaproj * 100  #  projected area 0.29% larger
# cellStats(cropfrac_prj, "mean")  # 0.2133
# cellStats(cropfrac, "mean")  # 0.2134
# tiles_prj <- st_transform(tiles, crs = st_crs("ESRI:102022")$wkt)
# plot(cropfrac_prj)
# plot(tiles_prj$geometry, add = TRUE)

# calculate frequencies of each class by AOI (with checks)
crop_freq <- zonal((cropfrac_prj[[1]] > 0.5) * 1, cropfrac_prj[[2]], fun = sum)
nocrop_freq <- zonal((cropfrac_prj[[1]] <= 0.5) * 1, cropfrac_prj[[2]],
                     fun = sum)
class_freq <- merge(nocrop_freq, crop_freq, by = "zone")
colnames(class_freq) <- c("aoi", "nocrop", "crop")
# all(rowSums(class_freq[, -1]) == freq(cropfrac_prj[[2]])[-17, 2])
# sum(rowSums(class_freq[, -1]))
# all(colSums(class_freq[, -1]) == freq(cropfrac_prj[[1]])[-3, 2])
# cropfrac_aoi <- cropfrac_aoi[names(cropfrac_aoi) != "fun"]
# class_freq_aoi <- #

# AEZ
cropfrac_prj_aez <- projectRaster(stack(cropfrac, aezr),
                                  crs = st_crs("ESRI:102022")$wkt, res = 500)
# freq(cropfrac_prj < 0)  # only 1
cropfrac_prj_aez[cropfrac_prj_aez < 0] <- 0

crop_freq_aez <- zonal((cropfrac_prj_aez[[1]] > 0.5) * 1, cropfrac_prj_aez[[2]],
                       fun = sum)
nocrop_freq_aez <- zonal((cropfrac_prj_aez[[1]] <= 0.5) * 1,
                         cropfrac_prj_aez[[2]], fun = sum)
class_freq_aez <- merge(nocrop_freq_aez, crop_freq_aez, by = "zone")
colnames(class_freq_aez) <- c("aez", "nocrop", "crop")

# area and cropland fractions
# aoi
classarea_rf_aoi <- as_tibble(class_freq) %>%
  mutate(nocrop_area = nocrop * (res(cropfrac_prj)[1]^2) * 10^-6) %>%
  mutate(crop_area = crop * (res(cropfrac_prj)[1]^2) * 10^-6)
# sum(classarea_rf_aoi$crop_area + classarea_rf_aoi$nocrop_area)
# sum(aois$area)

# aez
classarea_rf_aez <- as_tibble(class_freq_aez) %>%
  mutate(nocrop_area = nocrop * (res(cropfrac_prj)[1]^2) * 10^-6) %>%
  mutate(crop_area = crop * (res(cropfrac_prj)[1]^2) * 10^-6) %>% na.omit()
# sum(classarea_rf_aez$crop_area + classarea_rf_aez$nocrop_area)
# sum(units::set_units(st_area(aez), "km2"))

#------------------------------------------------------------------------------#
# Segments (AOIs and Ghana only)
classarea_segments_aoi <- lapply(1:16, function(x) {  # x <- 1
  # i <- segment_files$file[1]
  f <- segment_files$file[x]
  aoi_id <- gsub("[[:alpha:]]|_|\\.", "", basename(f))
  print(glue::glue("Running AOI {aoi_id}"))

  # read in fields and calculate area
  flds <- read_sf(f) %>% rename(fid = id)  # rename id field
  fld_area <- flds %>% st_transform(crs = st_crs("ESRI:102022")$wkt) %>%
    st_area(.) %>% units::set_units(., "km2") %>% as.numeric(.)

  # aoi area
  aoi_area <- aois %>% filter(aois == aoi_id) %>% pull(area)

  # output
  areas <- tibble("aoi" = aoi_id, "crop_area" = sum(fld_area),
                  "nocrop_area" = aoi_area - sum(fld_area))
  return(areas)
}) %>% do.call(rbind, .)


#------------------------------------------------------------------------------#
# save out
mapped_areas <- list("rf_areas_aoi" = classarea_rf_aoi,
                     "rf_areas_aez" = classarea_rf_aez,
                     "seg_areas_aoi" = classarea_segments_aoi)
usethis::use_data(mapped_areas, overwrite = TRUE)
