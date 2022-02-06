## Processing field segment for stratifying validation sample

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

