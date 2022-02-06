
# Calculate average field size at 0.005 degree resolution
library(raster)
library(sf)
library(dplyr)
library(here)
library(doMC)

# tile grid
tiles <- st_read(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)

r <- raster(extent(tiles), res = 0.05)
crs(r) <- crs(tiles)

# for field areas
tilesr <- fasterize::fasterize(sf = tiles, raster = r, field = "aoi1")
tilesr005 <- disaggregate(tilesr, fact = 10)
tilesr005_sf <- stars::st_as_stars(tilesr005) %>% st_as_sf() %>%
  rename(aoi1 = layer)

# Read in segments
segment_files <- dir(here("external/data/results/segments/merged"),
                     pattern = "boundarymerge", full.names = TRUE)
iid <- as.numeric(gsub("[[:alpha:]]|_|\\.", "", basename(segment_files)))
segment_files <- tibble(file = segment_files, order = iid) %>% arrange(iid)
segment_files <- segment_files %>% filter(!grepl("aoi3_*.*merge.geojson", file))

# area and quiet func
ha <- function(x) as.numeric(units::set_units(st_area(x), "ha"))
quiet <- function(x) suppressWarnings(suppressMessages(x))

logf <- here("external/logs/field_size_005.log")
registerDoMC(cores = 7)
fsizes <- foreach(i = segment_files$file) %dopar% {
  # i <- segment_files$file[1]
  aoi_id <- gsub("[[:alpha:]]|_|\\.", "", basename(i))

  cat(glue::glue("Starting {aoi_id} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)
  print(aoi_id)

  # read in fields
  cat(glue::glue("...aoi {aoi_id} reading in fields and calculating areas"),
      file = logf, sep = "\n", append = TRUE)
  flds <- read_sf(i) %>% mutate(area = ha(.))

  # select out AOI grid and rasterize
  aoi_grids <- tilesr005_sf %>% filter(aoi1 == aoi_id) %>%
    mutate(cid = 1:n(), area = as.numeric(st_area(.) / 10000))
  aoir <- crop(tilesr005, aoi_grids)

  cat(glue::glue("...aoi {aoi_id} intersecting fields and grids"),
      file = logf, sep = "\n", append = TRUE)
  # intersect fields with grids
  fintersect <- st_intersection(
    aoi_grids %>% select(cid), flds %>% select(id, area)
  ) %>% mutate(iarea = ha(.))

  # calculate average area of fields intersecting each cell, selecting fields
  # that fall primarily (>50%) in each cell, and calculate the average area
  cat(glue::glue("...aoi {aoi_id} calculating mean area of fields per grid"),
      file = logf, sep = "\n", append = TRUE)
  mu_areas <- as_tibble(fintersect) %>% select(-geometry) %>%
    group_by(cid) %>% filter(iarea / area > 0.5) %>%
    summarize(farea = mean(area))
  mu_iareas <- as_tibble(fintersect) %>% select(-geometry) %>%
    group_by(cid) %>% summarize(farea = mean(iarea))
  # plot(mu_iareas$area, mu_areas$area)

  # equivalent
  # as_tibble(fintersect) %>%
  #   select(-geometry) %>%
  #   group_by(cid) %>%
  #   summarize(area = mean(iarea))
  # mu_areas <- as_tibble(fintersect) %>% group_by(cid) %>%
  #   summarize(farea = mean(area))

  # join with grids and rasterize
  cat(glue::glue("...aoi {aoi_id} joining areas with tiles and rasterizing"),
      file = logf, sep = "\n", append = TRUE)
  aoi_grids_ha <- left_join(aoi_grids, mu_areas) %>%
    select(aoi1, cid, area, farea) %>%
    mutate(farea = ifelse(is.na(farea), 0, farea))
  # aoi_grids_hai <- left_join(aoi_tiles, mu_iareas) %>%
  #   select(aoi1, cid, area, farea) %>%
  #   mutate(farea = ifelse(is.na(farea), 0, farea))
  # mean(aoi_grids_hai$farea)
  # mean(aoi_grids_ha$farea)
  # plot(aoi_grids_hai$farea, aoi_grids_ha$farea)

  aoi_grids_hav <- terra::vect(aoi_grids_ha %>% select(farea))
  aoi_grids_har <- terra::rasterize(aoi_grids_hav, terra::rast(aoir),
                                    field = "farea")

  # plot(terra::rast(aoir))
  # terra::plot(aoi_grids_har)

  cat(glue::glue("Finished aoi {aoi_id} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)
  cat("", file = logf, sep = "\n", append = TRUE)
  return(raster::raster(aoi_grids_har))
}
fsizes$fun <- mean
fsizes_005 <- do.call(terra::mosaic, fsizes)

# plot(log10(fsizes_005))
# fdensity <- raster(here("external/data/field_density005.tif"))
writeRaster(fsizes_005, filename = here("inst/extdata/mean_field_size005.tif"))


