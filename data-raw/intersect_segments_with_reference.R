# intersect reference labels with segmentations

library(sf)
library(raster)
library(dplyr)
library(activemapper)
library(here)
library(doMC)

# reference polygons
ref_labels <- read_sf(
  system.file("extdata/map_reference_labels.geojson", package = "activemapper")
)

# segmentations
segment_files <- dir(here("external/data/results/segments/merged"),
                     pattern = "boundarymerge", full.names = TRUE)
iid <- as.numeric(gsub("[[:alpha:]]|_|\\.", "", basename(segment_files)))
segment_files <- tibble(file = segment_files, order = iid) %>% arrange(iid)
segment_files <- segment_files %>% filter(!grepl("aoi3_*.*merge.geojson", file))


# area and quiet func
ha <- function(x) as.numeric(units::set_units(st_area(x), "ha"))
quiet <- function(x) suppressWarnings(suppressMessages(x))

# variables
logf <- here("external/logs/intersect_segments_with_reference.log")
aois <- unique(ref_labels$aoi)

# loop over aois to process field/grid intersections
registerDoMC(cores = 7)
ref_int_segments <- foreach(x = 1:length(aois), .combine = rbind) %dopar% {
  # x <- 3 #8 #7 #3 #7 #5

  cat(glue::glue("Starting labeller{x} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

  # pull grids
  ref_labels_ss <- ref_labels %>% filter(aoi == !!aois[x]) %>%
    mutate(ref_area = as.numeric(ha(.))) %>%
    select(-geometry)

  # get segmentation
  flds <- segment_files %>% filter(order == x) %>% pull(file) %>%
    read_sf() %>% select(-tile) %>%
    filter(!duplicated(.))

  # intersect ref labels with segments
  ref_fld_int <- quiet(st_intersection(ref_labels_ss, flds)) %>%
    mutate(int_area = as.numeric(ha(.))) %>%
    mutate(afrac = int_area / ref_area) %>%
    select(name, tile, col, row, aoi, category, afrac)

  # find the ones that have no intersection
  ref_fld_noint <- ref_labels_ss %>%
    filter(!name %in% ref_fld_int$name) %>%
    mutate(afrac = 0) %>%
    select(name, tile, col, row, aoi, category, afrac)

  # bind them
  ref_fld_isected <- rbind(ref_fld_int, ref_fld_noint)

  cat(glue::glue("aoi {x} complete"),
      file = logf, sep = "\n", append = TRUE)
  cat(glue::glue(""), file = logf, sep = "\n", append = TRUE)

  return(ref_fld_isected)
}

# union separated segments to produce correct numbers of rows
# ref_int_segments <- st_read("inst/extdata/ref_int_segments.geojson")
ref_int_segments <- ref_int_segments %>% group_by(name) %>%
  summarize(tile = unique(tile), aoi = unique(aoi), category = unique(category),
            afrac = sum(afrac)) %>% arrange(aoi, tile, name, tile)
# as_tibble(ref_int_segments) %>% group_by(name) %>% count() %>% filter(n > 1)
# ref_int_segments %>% filter(name == "GH0005059") %>% plot()

# write out to inst/extdata
st_write(ref_int_segments, dsn = here("inst/extdata/ref_int_segments.geojson"),
         delete_dsn = TRUE)

