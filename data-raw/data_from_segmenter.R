## Processing field segment for:
## 1. Stratifying validation sample
## 2. Analyzing field statistics

library(raster)
library(sf)
library(dplyr)
library(here)

# Datasets
# grid, tiles, etc
mgrid <- raster(
  system.file("extdata/ghana_grid.tif", package = "activemapper")
)
tiles <- st_read(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)

# Read in segments
segment_files <- dir(here("external/data/results/segments/merged"),
                     pattern = "boundarymerge", full.names = TRUE)
iid <- as.numeric(gsub("[[:alpha:]]|_|\\.", "", basename(segment_files)))
segment_files <- tibble(file = segment_files, order = iid) %>% arrange(iid)

r <- raster(extent(tiles), res = 0.05)
crs(r) <- crs(tiles)

# for field areas
tilesr <- fasterize::fasterize(tiles, r, field = "aoi1")
tilesr005 <- disaggregate(tilesr, fact = 10)
tilesr005_sf <- stars::st_as_stars(tilesr005) %>% st_as_sf() %>%
  rename(aoi1 = layer)

#------------------------------------------------------------------------------#
# Point based stratification for map reference sample selection

##!! Replace with csv grid
# Create point grid for stratifying model results
# mask main grid and convert to points
mgrid <- mask(mgrid, tiles)
mgrid_xy <- as.data.frame(mgrid, xy = TRUE) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% rename(id = ghana_grid) %>%
  tidyr::drop_na(id)

# join with tiles
# system.time(extract(tilesr, master_grid_xy))  # 11.81
system.time(mgrid_xytiles <- st_join(mgrid_xy, tilesr005_sf))  # 26

#------------------------------------------------------------------------------#
# Intersect points with grids
logf <- here("external/logs/field_points_005.log")
registerDoMC(cores = 7)
fpoints <- foreach(i = segment_files$file) %dopar% {
  # i <- segment_files$file[1]
  aoi_id <- gsub("[[:alpha:]]|_|\\.", "", basename(i))

  cat(glue::glue("Starting {aoi_id} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

  # read in fields
  cat(glue::glue("...aoi {aoi_id} reading in fields and calculating tile area"),
      file = logf, sep = "\n", append = TRUE)
  flds <- read_sf(i) %>% rename(fid = id)  # rename id field

  # intersect fields with main grid
  cat(glue::glue("...aoi {aoi_id} intersecting fields and calculating area"),
      file = logf, sep = "\n", append = TRUE)
  mgrid_ss <- mgrid_xytiles %>% filter(aoi1 == aoi_id)  # subset mgrid
  fld_int <- st_join(mgrid_ss, flds)
  fld_int <- fld_int %>% mutate(class = ifelse(is.na(fid), 0, 1)) %>%
    select(id, fid, aoi1, class) %>% rename(aoi = aoi1)

  cat(glue::glue("aoi {aoi_id} complete"),
      file = logf, sep = "\n", append = TRUE)
  return(fld_int)
}

# Rename one of two AOI3 results, and then bind
fpoints[[3]] <- fpoints[[3]] %>% mutate(aoi = "3N")
fpoints <- do.call(rbind, fpoints)
xy <- fpoints %>% st_coordinates() %>% as_tibble() %>% rename(x = X, y = Y)
fpoints_tb <- bind_cols(fpoints %>% as_tibble() %>% select(-geometry), xy)

# write to intermediate output
data.table::fwrite(
  fpoints_tb, file = here("external/data/field_points005.csv")
)

#------------------------------------------------------------------------------#
# Analyze field segments

# Coverage per cell
logf <- here("external/logs/gridding_005.log")
registerDoMC(cores = 7)
fareas <- foreach(i = segment_files$order, .combine = stack) %dopar% {
  # i <- segment_files[11]
  aoi_id <- gsub("[[:alpha:]]|_|\\.", "", basename(i))

  cat(glue::glue("Starting {aoi_id} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)
  print(aoi_id)

  # read in fields
  cat(glue::glue("...aoi {aoi_id} reading in fields and calculating tile area"),
      file = logf, sep = "\n", append = TRUE)
  flds <- read_sf(i)
  # select out AOI grid and
  aoi_tiles <- tilesr005_sf %>% filter(aoi1 == aoi_id) %>%
    mutate(cid = 1:n(), area = as.numeric(st_area(.) / 10000))

  # intersect fields
  cat(glue::glue("...aoi {aoi_id} intersecting fields and calculating area"),
      file = logf, sep = "\n", append = TRUE)
  # fld_int <- st_intersection(aoi_tiles, flds)
  fld_int <- st_join(aoi_tiles_xy, flds)
  fld_int %>% mutate(class = )

  drops <- fld_int %>%
    st_is(c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) %>%
    which()
  polys <- fld_int %>% st_is("POLYGON") %>% which()
  fld_int <- fld_int %>% slice(-drops)
  fld_int <- fld_int %>% mutate(farea = as.numeric(st_area(.) / 10000))
  fld_areas <- fld_int %>% as_tibble() %>% group_by(cid) %>%
    summarize(farea = sum(farea))

  cat(glue::glue("...aoi {aoi_id} joining areas with tiles"), file = logf,
      sep = "\n", append = TRUE)
  aoi_tiles <- left_join(aoi_tiles, fld_areas) %>%
    mutate(farea = ifelse(is.na(farea), 0, farea)) %>%
    mutate(fprop = round(farea / area, 4))

  # crop down to just
  cat(glue::glue("...aoi {aoi_id} rasterizing"), file = logf, sep = "\n",
      append = TRUE)
  aoir <- tilesr005 == as.numeric(aoi_id)
  arear <- area(aoir) * 100
  # aoir[aoir == 0] <- NA
  farea <- fasterize::fasterize(aoi_tiles, aoir, field = "fprop",
                                fun = "sum", background = 0)

  cat(glue::glue("Finished aoi {aoi_id} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)
  cat("", file = logf, sep = "\n", append = TRUE)
  return(farea)
}

# plot(fdensity)

writeRaster(fdensity, filename = here("external/data/field_density005.tif"))

# usethis::use_data("data_from_segmenter")

#------------------------------------------------------------------------------#
# extract and summarize field stats for validation sites
library(dplyr)
library(ggplot2)
library(activemapper)
library(doMC)

data("top_label_data")

gcs = "+proj=longlat +datum=WGS84 +no_defs"
top_label_grids <- top_label_data$stats %>% select(name, x, y) %>%
  data.table::as.data.table() %>%
  rmapaccuracy::point_to_gridpoly(xy = ., 0.005 / 2, gcs, gcs)
top_label_grids <- top_label_data$stats %>% select(aoi, name) %>%
  left_join(., top_label_grids)
top_label_grids <- top_label_grids %>%
  mutate(aoi = as.numeric(gsub("labeller", "", aoi))) %>%
  arrange(aoi)

# Read in segments, drop old AOI3
segment_files <- dir(here("external/data/results/segments/merged"),
                     pattern = "boundarymerge", full.names = TRUE)
iid <- as.numeric(gsub("[[:alpha:]]|_|\\.", "", basename(segment_files)))
segment_files <- tibble(file = segment_files, order = iid) %>% arrange(iid)
segment_files <- segment_files %>% filter(!grepl("aoi3_*.*merge.geojson", file))


logf <- here("external/logs/field_stats.log")
aois <- unique(top_label_grids$aoi)

registerDoMC(cores = 7)
fld_stats <- foreach(x = 1:length(aois)) %dopar% {
# fld_stats <- lapply(1:length(aois), function(x) {  # x <- 3

  cat(glue::glue("Starting labeller{x} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

  # pull grids
  grids <- top_label_grids %>% filter(aoi == !!aois[x]) %>% st_as_sf()
  flds <- segment_files %>% filter(order == x) %>% pull(file) %>%
    read_sf()

  # intersect segments with grids (to get fields for validation sites with
  # fields), and then grids with fields filtering for NA to get empty validation
  # sites
  fld_int <- st_join(flds, grids, left = FALSE)
  grid_int <- st_join(grids, flds) %>% filter(is.na(id))

  # calculate area stats
  fld_stats <- fld_int %>%
    mutate(area = as.numeric(units::set_units(st_area(.), "ha"))) %>%
    as_tibble() %>%
    group_by(name) %>%
    summarize(n = n(), Mean = mean(area), StDev = sd(area)) %>%
    mutate(aoi = x) %>%
    select(aoi, !!names(.))
  grid_stats <- grid_int %>% as_tibble() %>%
    select(aoi, name) %>%
    mutate(aoi = x, n = n(), Mean = 0, StDev = NA) %>%
    select(aoi, !!names(.))

  grids_xy <- grids %>% st_centroid() %>%
    mutate(x = st_coordinates(.)[, 1],
           y = st_coordinates(.)[, 2]) %>%
    as_tibble() %>%
    select(aoi, name, x, y)

  # combine and get grid center coordinates back
  segment_stats <- grids_xy %>%
    left_join(., bind_rows(fld_stats, grid_stats))

  cat(glue::glue("aoi {x} complete"),
      file = logf, sep = "\n", append = TRUE)
  cat(glue::glue(""), file = logf, sep = "\n", append = TRUE)

  return(segment_stats)
}

field_validation_stats <- do.call(rbind, fld_stats) %>%
  mutate(aoi = paste0("labeller", aoi))

usethis::use_data(field_validation_stats)
#
#   i <- 50
#   nm <- unique(fld_int$name)[i]
#   fld_int %>% filter(name == nm) %>%
#     ggplot() + geom_sf() +
#     geom_sf(data = grids %>% filter(name == nm), fill = "transparent") +
#     geom_sf(data = fld_int %>% filter(name == nm) %>% slice(1), fill = "red")



