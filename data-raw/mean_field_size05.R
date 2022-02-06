
# Calculate average field size at 0.05 degree resolution
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
# tilesr <- fasterize::fasterize(sf = tiles, raster = r, field = "aoi1")
# tilesr005 <- disaggregate(tilesr, fact = 10)
# tilesr005_sf <- stars::st_as_stars(tilesr005) %>% st_as_sf() %>%
  # rename(aoi1 = layer)

# Read in segments
segment_files <- dir(here("external/data/results/segments/merged"),
                     pattern = "boundarymerge", full.names = TRUE)
iid <- as.numeric(gsub("[[:alpha:]]|_|\\.", "", basename(segment_files)))
segment_files <- tibble(file = segment_files, order = iid) %>% arrange(iid)
segment_files <- segment_files %>% filter(!grepl("aoi3_*.*merge.geojson", file))

# area and quiet func
ha <- function(x) as.numeric(units::set_units(st_area(x), "ha"))
quiet <- function(x) suppressWarnings(suppressMessages(x))

logf <- here("external/logs/field_size_05.log")
registerDoMC(cores = 7)
fsizes <- foreach(i = segment_files$file) %dopar% {
  # i <- segment_files$file[3]
  aoi_id <- gsub("[[:alpha:]]|_|\\.", "", basename(i))

  cat(glue::glue("Starting {aoi_id} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)
  print(aoi_id)

  # read in fields
  cat(glue::glue("...aoi {aoi_id} reading in fields and calculating areas"),
      file = logf, sep = "\n", append = TRUE)
  flds <- read_sf(i)
  if(any(duplicated(flds))) {
    # cat(glue::glue("......aoi {aoi_id} removing duplicates"),
    #     file = logf, sep = "\n", append = TRUE)
    flds <- flds %>% distinct(.keep_all = TRUE)
  }
  fldsc <- flds %>% st_cast("POLYGON") %>% mutate(area = ha(.)) %>% quiet(.)

  # field mean size and count
  tile_areas_n <- fldsc %>% as_tibble() %>% group_by(tile) %>%
    summarize(muarea = mean(area), nflds = n(), aoi = aoi_id) %>%
    select(aoi, !!names(.))

  # tile_ids <- unique(flds$tile)
  # tile_areas_n2 <- lapply(tile_ids, function(y) { # y <- tile_ids[1]
  #   flds %>% filter(tile == y) %>% st_cast("POLYGON") %>%
  #     quiet(.) %>% mutate(area = ha(.)) %>% as_tibble() %>%
  #     summarize(area = mean(area), nflds = n()) %>%
  #     mutate(tile = y)
  # })
  # tile_areas_n2 <- do.call(rbind, tile_areas_n2) %>% select(tile, !!names(.))
  # plot(tile_areas_n %>% arrange(tile) %>% pull(nflds),
  #      tile_areas_n2 %>% arrange(tile) %>% pull(nflds))
  # plot(tile_areas_n %>% arrange(tile) %>% pull(muarea),
  #      tile_areas_n2 %>% arrange(tile) %>% pull(area))
  #   summarize(farea = mean(area))
  cat(glue::glue("...aoi {aoi_id} completed"),
      file = logf, sep = "\n", append = TRUE)
  return(tile_areas_n)
}
fsizes_05 <- do.call(rbind, fsizes) %>%
  mutate(tile = as.numeric(tile), aoi = as.numeric(aoi))

fsizes_05 <- tiles %>% rename(aoi = aoi1) %>% select(-aoi2) %>%
  left_join(., fsizes_05) %>% select(aoi, tile, !!names(.))

st_write(fsizes_05, dsn = here("inst/extdata/field_size_n_05.geojson"))
# writeRaster(fsizes_005, filename = here("inst/extdata/mean_field_size005.tif"))


