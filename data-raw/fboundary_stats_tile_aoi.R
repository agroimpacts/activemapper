# Extract and summarize digitized and segmented field boundary statistics at
# tile and AOI scales
library(dplyr)
library(sf)
library(ggplot2)
library(activemapper)
library(doMC)
library(here)

data("top_label_data")

tiles <- st_read(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)

load(here("inst/extdata/segment_validation_data.rda"))

# Read in segments, drop old AOI3
segment_files <- dir(here("external/data/results/segments/merged"),
                     pattern = "boundarymerge", full.names = TRUE)
iid <- as.numeric(gsub("[[:alpha:]]|_|\\.", "", basename(segment_files)))
segment_files <- tibble(file = segment_files, order = iid) %>% arrange(iid)
segment_files <- segment_files %>% filter(!grepl("aoi3_*.*merge.geojson", file))

# get grids from validation data and intersect with tiles
segment_val_grids <- lapply(segment_validation_data, function(x) x$grids) %>%
  do.call(rbind, .)
segment_val_tiles <- tiles %>% dplyr::select(tile) %>%
  st_join(segment_val_grids, ., left = FALSE, largest = TRUE)

# get tile ids for validation grids
val_tiles <- top_label_data$stats %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_join(., tiles %>% select(tile)) %>%
  as_tibble() %>%
  select(aoi, tile, name) %>%
  mutate(aoi = gsub("labeller", "", aoi)) %>%
  arrange(aoi, tile)

# use validation tiles to get segment and validation field stats
logf <- here("external/logs/tile_aoi_stats.log")
aois <- unique(tiles$aoi1)

registerDoMC(cores = 7)
val_stats <- foreach(x = 1:length(aois)) %dopar% { # x <- 8 #7 #3 #7 #5
# tile_aoi_fld_stats <- lapply(1:length(aois), function(x) { # x <- 7

  cat(glue::glue("Starting labeller{x} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

  # read in segments for aoi
  aoi_segments <- segment_files %>% filter(order == x) %>% pull(file) %>%
    read_sf() %>% filter(!duplicated(.))
  aoi_segments <- aoi_segments %>%
    mutate(tile = as.numeric(tile)) %>%
    mutate(area = as.numeric(units::set_units(st_area(.), "ha")))

  # get tiles intersecting validation sites for this aoi
  aoi_val_tiles <- val_tiles %>% filter(aoi == x)
  tile_ids <- unique(aoi_val_tiles$tile)

  # cut down segments to those validation intersecting tiles
  val_segments <- aoi_segments %>% filter(tile %in% tile_ids)

  # get validation fields (labels)
  # first cross-check names
  fld_aoi_val_tiles <- val_tiles %>% filter(tile %in% tile_ids)
  if(!all(fld_aoi_val_tiles$name %in% aoi_val_tiles$name)) {
    cat(glue::glue("Missing val grids in labeller{x} at {Sys.time()}"),
        file = logf, sep = "\n", append = TRUE)
    stop("Missing validation grids in one of the datasets", call. = FALSE)
  }

  # get validation field areas for the aoi
  val_flds <- top_label_data$maps %>% filter(aoi == paste0("labeller", x)) %>%
    left_join(., fld_aoi_val_tiles %>% select(name, tile)) %>%
    select(name, tile, area)

  # other checks
  # area check
  tile_area_chk <- tiles %>% filter(tile %in% !!aoi_val_tiles$tile) %>%
    st_area() %>% as.numeric(.) %>% sum(.) / 10000
  segment_area_chk <- val_segments %>% pull(area) %>% sum(.)
  if(segment_area_chk > tile_area_chk) {
    cat(glue::glue("Fld area > tile area in labeller{x} at {Sys.time()}"),
        file = logf, sep = "\n", append = TRUE)
    stop("Fld area shouldn't be larger", call. = FALSE)
  }

  # intersection checks
  tid <- sample(unique(val_segments$tile), size = 5)
  int_lengths <- sapply(tid, function(y) {  # y <- tid[1]
    flds <- val_segments %>% filter(tile == y)
    tile <- tiles %>% filter(tile == y)
    length(unlist(suppressMessages(st_intersects(tile, flds))))
  })
  if(any(int_lengths == 0)) {
    cat(glue::glue("Fields don't intersect tiles: labeller{x} at {Sys.time()}"),
        file = logf, sep = "\n", append = TRUE)
    stop("Fields don't intersect tiles", call. = FALSE)
  }

  # statistics
  # tile-scale
  # segment stats, tile-scale
  tile_segment_stats <- aoi_segments %>%
    filter(tile %in% !!unique(aoi_val_tiles$tile)) %>%
    as_tibble() %>% select(-geometry) %>%
    group_by(tile) %>%
    summarize(area_sum = sum(area), area_mu = mean(area),
              area_med = median(area), area_sd = sd(area), n = n(),
              n_mu = n() / 100) %>%
    mutate(type = "segments", aoi = x) %>%
    select(type, aoi, names(.))

  # validation field stats, tile-scale
  tile_val_stats <- val_flds %>% group_by(tile) %>%
    summarize(area_sum = sum(area), area_mu = mean(area),
              area_med = median(area), area_sd = sd(area),
              n = n(), n_grids = length(unique(name)),
              n_mu = n() / length(unique(name))) %>%
    mutate(type = "validation", aoi = x) %>%
    select(type, aoi, names(.))

  # segments stats, aoi scale
  aoi_segment_stats <- aoi_segments %>% as_tibble() %>%
    summarize(area_sum = sum(area), area_mu = mean(area),
              area_med = median(area), area_sd = sd(area),
              n = n()) %>%
    mutate(type = "segments", aoi = x) %>%
    select(type, aoi, names(.))

  # validation stats, aoi scale
  aoi_val_stats <- val_flds %>%
    summarize(area_sum = sum(area), area_mu = mean(area),
              area_med = median(area), area_sd = sd(area),
              n = n()) %>%
    mutate(type = "validation", aoi = x) %>%
    select(type, aoi, names(.))

  # bind rows
  tile_stats <- bind_rows(tile_segment_stats, tile_val_stats)
  aoi_stats <- bind_rows(aoi_segment_stats, aoi_val_stats)

  # out list
  stat_list <- list("tile" = tile_stats, "aoi" = aoi_stats)

  cat(glue::glue("Stats complete for labeller{x} at {Sys.time()}"),
      file = logf, sep = "\n", append = TRUE)
  return(stat_list)
}

# combined into output list
tile_aoi_validation_stats <- list(
  "tile" = do.call(rbind, lapply(val_stats, function(x) x$tile)),
  "aoi" = do.call(rbind, lapply(val_stats, function(x) x$aoi))
)

usethis::use_data(tile_aoi_validation_stats, overwrite = TRUE)

