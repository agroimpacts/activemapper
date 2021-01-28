# Collect data from probability images for accuracy assessment
# Uses polygons collected from validator
#

## Map reference site selection
library(aws.s3)
library(raster)
library(sf)
library(dplyr)
library(rmapaccuracy)

# get validation samples
load(system.file("extdata/mapref_tbls.rda", package = "activemapper"))
# load(system.file("extdata/instance_tbls.rda", package = "activemapper"))

ref_sample <- readr::read_csv(
  system.file("extdata/map_reference_sample.csv", package = "activemapper")
)

tiles <- read_sf(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)
gcs <- unname(unlist(st_crs(4326))[2])

# create master_grid extent, get list of probability images, download fpool
# (for checking)

# collect polygons with worker_id
val_sample <- left_join(mapref_tbls$user_maps, mapref_tbls$assignment_data) %>%
  select(name, fieldnum, assignment_id, category, worker_id, comment) %>%
  left_join(., mapref_tbls$categories) %>%
  filter(name %in% ref_sample$name)  # filter out several not in ref_sample

# val_nms <- unique(val_sample$name)
# nms_not_in <- val_nms[!val_nms %in% ref_sample$name]
# mapref_tbls$kml_data %>% filter(name %in% nms_not_in)
# mapref_tbls$kml_data %>% filter(name %in% ref_sample$name) %>%
#   group_by(kml_type) %>% count()

# mapref_tbls$kml_data %>% filter(name %in% val_nms) %>%
#   group_by(kml_type) %>% count()
# mapref_tbls$kml_data$kml_type


# val_sample %>% filter(name == "GH0639733") %>% select(worker_id) %>%
#   plot()
# val_sample %>% filter(category == "unsure1")
# val_sample_1 <- val_sample %>% filter(worker_id == 134)

# separate by validator
val_sample1 <- val_sample %>% filter(worker_id == 90)
val_sample2 <- val_sample %>% filter(worker_id == 134)
overlap_samples <- unique(
  val_sample1$name[val_sample1$name %in% val_sample2$name]
)

# remove overlap sites
val_samples <- rbind(val_sample1,
                     val_sample2 %>% filter(!name %in% overlap_samples))
# val_samples$name[which(!val_samples$name %in% ref_sample$name)]

# run through and extract points closest to grid, and convert points to polygons
# unique(val_samples$name)[length(unique(val_samples$name))]

val_sample_1r <- lapply(unique(val_samples$name), function(x) {
  # x <- unique(val_sample_1$name)[1]
  # x <- unique(val_sample_1$name)[9]
  # x <- "GH0393434"
  # x <- "GH0022229"
  # x <- "GH0082965"
  dat <- val_samples %>% filter(name == x) %>% rename(geom = geom_clean)
  nm <- unique(dat$name)
  print(nm)

  # create original reference geometry
  orig_pt <- ref_sample %>% filter(name == nm) %>%
    select(-aoi, -class) %>% data.table::data.table()
  orig_geom <- point_to_gridpoly(orig_pt, 0.0001433, gcs, gcs)

  # dat %>% filter(st_intersects(dat, orig_geom, sparse = FALSE))
  # pull out the one nearest to the original grid
  out <- dat %>% slice(st_nearest_feature(orig_geom, .)) %>%
    mutate(int_grid = any(st_intersects(., orig_geom, sparse = FALSE)))

  # convert points to polygons
  if(st_is(out$geom, "POINT")) {
    out <- left_join(as_tibble(out) %>% select(-geom), orig_geom, by = "name")
  }
  return(as_tibble(out))
}) %>% do.call(rbind, .)
# val_sample_1r <- do.call(rbind, val_sample_1r)
all(sapply(val_sample_1r, function(x) st_is(x$geom, "POLYGON")))

# sum of category
# val_cats <- val_sample_1r %>% as_tibble() %>%
#   group_by(category, categ_description) %>% count()
# sum(val_cats[3:4, ][["n"]]) / sum(val_cats[["n"]])

# get list of row_col identifiers
val_sample_1r_sf <- val_sample_1r %>% st_as_sf()
ints <- sort(unlist(st_intersects(val_sample_1r_sf, tiles)))
val_tiles <- tiles %>% slice(ints)
# ggplot2::ggplot(val_tiles) + ggplot2::geom_sf() +
#   ggplot2::geom_sf(data = val_sample_1r_sf)

xys <- val_tiles %>% st_centroid() %>% st_coordinates()
val_tile_rcs <- rowcol_from_xy(xys[, 1], xys[, 2], res = 0.05)

# Combine with labels
cbind(as_tibble(val_sample_1r_sf), tile = val_tiles$tile, val_tile_rcs) %>%
  select(name, assignment_id, worker_id, category, categ_description, tile,
         row, col, geom) %>% st_as_sf() -> ref_labels


# write out
st_write(ref_labels, here::here("inst/extdata/map_reference_labels.geojson"))

# # test
# i <- 2
# # tst <- val_sample_1r_sf %>% filter(int_grid == FALSE) %>%
# #   slice(i)
# tst <- val_sample_1r_sf %>% filter(grepl("unsure", category)) %>%
#   slice(i)
# orig_pt <- ref_sample %>% filter(name == tst$name) %>%
#   select(-aoi, -class) %>% data.table::data.table()
# orig_geom <- point_to_gridpoly(orig_pt, 0.0001433, gcs, gcs)
# ggplot2::ggplot(orig_geom) + ggplot2::geom_sf() +
#   ggplot2::geom_sf(data = tst)


