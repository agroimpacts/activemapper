# Extract and summarize digitized and segmented field boundary statistics over
# validation sites

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

# area and quiet func
ha <- function(x) as.numeric(units::set_units(st_area(x), "ha"))
quiet <- function(x) suppressWarnings(suppressMessages(x))

# variables
logf <- here("external/logs/field_stats.log")
aois <- unique(top_label_grids$aoi)

# loop over aois to process field/grid intersections
registerDoMC(cores = 7)
fld_stats <- foreach(x = 1:length(aois)) %dopar% { # x <- 8 #7 #3 #7 #5
  # fld_stats <- lapply(1:length(aois), function(x) {  # x <- 13

  cat(glue::glue("Starting labeller{x} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

  # pull grids
  grids <- top_label_grids %>% filter(aoi == !!aois[x]) %>% st_as_sf()
  flds <- segment_files %>% filter(order == x) %>% pull(file) %>%
    read_sf() %>% select(-tile) %>%
    filter(!duplicated(.))

  # intersect segments with grids (to get fields for validation sites with
  # fields), and then grids with fields filtering for NA to get empty validation
  # sites
  # intersecting fields and grids
  fld_int <- quiet(st_join(flds, grids, left = FALSE)) %>% arrange(name) %>%
    group_by(name, id) %>% mutate(n = n()) %>%
    summarize(aoi = unique(aoi), n = mean(n)) %>%  # hack to reduce doubles
    ungroup()

  # field intersections by grid
  fld_isect <- lapply(unique(grids$name), function(x) {
    # x <- unique(grids$name)[1]
    # x <- "GH0024641"# "GH0237163"
    fld <- fld_int %>% filter(name == x)
    grd <- grids %>% filter(name == x)
    int <- quiet(st_intersection(fld, grd))  # intersect
    int <- quiet(st_buffer(int, dist = 0))  # buffer width 0 for lines
    int <- int %>% mutate(iarea = ha(.)) %>% select(id, name, iarea) %>%
      group_by(name, id) %>%
      summarize(iarea = sum(iarea)) %>% ungroup() %>%
      filter(!st_is_empty(.))
  }) %>% do.call(rbind, .)

  # field differences by grid
  fld_dif <- lapply(unique(grids$name), function(x) {
    # x <- unique(grids$name)[1]
    # x <- "GH0188837"
    fld <- fld_int %>% filter(name == x)
    grd <- grids %>% filter(name == x)
    dif <- quiet(st_difference(fld, grd))  # difference
    dif <- quiet(st_buffer(dif, dist = 0))  # buffer width 0 for lines
    dif <- dif %>% mutate(darea = ha(.)) %>% select(id, name, darea) %>%
      group_by(name, id) %>%
      summarize(darea = sum(darea)) %>% ungroup() %>%
      filter(!st_is_empty(.))
  }) %>% do.call(rbind, .)

  # full join of intersects and differences
  fld_int_dif <- full_join(fld_isect %>% as_tibble() %>% select(-geometry),
                           fld_dif %>% as_tibble() %>% select(-geometry))
  fld_int_dif <- fld_int_dif %>%
    mutate(iarea = ifelse(is.na(iarea), 0, iarea),
           darea = ifelse(is.na(darea), 0, darea)) %>%  # set NA to 0
    mutate(area = iarea + darea, prop_in = iarea / (iarea + darea))

  # bring area and proportion into fld_int
  fld_int_areas <- left_join(fld_int, fld_int_dif) %>%
    select(aoi, id, name, iarea, darea, area, prop_in) %>% arrange(name)
  # plot(fld_int_areas$area, ha(fld_int))  # check that areas are the same

  # check
  # plot(fld_int_areas$area, ha(fld_int))
  chk <- sum(round((fld_int_areas$area - ha(fld_int)), 3))
  if(chk > 0) {
    cat(glue::glue("Error in labeller{x} at {Sys.time()}"), file = logf,
        sep = "\n", append = TRUE)
    cat(glue::glue("Difference in area of {chk} hectares."), file = logf,
        sep = "\n", append = TRUE)
    stop("Areas don't match", call. = FALSE)
  }

  # calculate area stats
  fld_stats <- fld_int_areas %>% as_tibble() %>%
    group_by(name) %>%
    summarize(aoi = unique(aoi), n = n(), Mean = mean(area), StDev = sd(area),
              mu_prop = mean(prop_in)) %>%
    select(aoi, !!names(.))

  grid_stats <- grids %>% as_tibble() %>% select(-geom) %>%
    filter(!name %in% unique(fld_int_areas$name)) %>%
    mutate(n = 0, Mean = 0, StDev = NA, mu_prop = NA)

  all_stats <- bind_rows(fld_stats, grid_stats) %>% arrange(name)

  grids_xy <- grids %>% st_centroid(.) %>%
    mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>%
    as_tibble() %>% select(aoi, name, x, y)

  # combine and get grid center coordinates back
  segment_stats <- left_join(grids_xy, all_stats)
  # segment_stats %>% filter(name == nm)

  # Check plot
  # nm <- sample(unique(segment_stats$name), 1)
  # fld_int %>% filter(name == nm) %>%
  #   ggplot() + geom_sf() +
  #   geom_sf(data = grids %>% filter(name == nm), fill = "transparent") +
  #   geom_sf(data = fld_int %>% filter(name == nm), fill = "red") +
  #   geom_sf(data = fld_istion %>% filter(name == nm), fill = "blue") +
  #   geom_sf(data = fld_dif %>% filter(name == nm), fill = "orange")

  cat(glue::glue("aoi {x} complete"),
      file = logf, sep = "\n", append = TRUE)
  cat(glue::glue(""), file = logf, sep = "\n", append = TRUE)

  segment_list <- list("grids" = grids, "ints" = fld_int, "int" = fld_isect,
                       "dif" = fld_dif, "stats" = segment_stats)
  return(segment_list)
}

# extract stats
field_validation_stats <- lapply(fld_stats, function(x) x$stats) %>%
  do.call(rbind, .) %>% mutate(aoi = paste0("labeller", aoi))

# to data
usethis::use_data(field_validation_stats, overwrite = TRUE)

# save out rest to inst/extdata
segment_validation_data <- lapply(fld_stats, function(x) x[1:4])
save(segment_validation_data,
     file = here::here("inst/extdata/segment_validation_data.rda"))

