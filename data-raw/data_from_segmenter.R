## Processing field segment for:
## 1. Stratifying validation sample
## 2. Analyzing field statistics

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
