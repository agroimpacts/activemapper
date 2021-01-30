# extract cropland probabilities from prediction images using reference polygons
# Ran on rstudio instance on EC2, to avoid download into S3

library(sf)
library(raster)
library(dplyr)
library(aws.s3)
library(activemapper)

# reference polygons
ref_labels <- read_sf(
  system.file("extdata/map_reference_labels.geojson", package = "activemapper")
)
ref_labels <- ref_labels %>%
  mutate(image = paste0("image_c", col, "_r", row, ".tif")) %>%
  select(-geometry)

# pull in tiles and AOIS for filtering
data("tile_key")

tiles <- read_sf(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)
aois <- read_sf(
  system.file("extdata/aois.geojson", package = "activemapper")
)
# tiles_aois <- st_join(tiles, aois, largest = TRUE)

# tiles_aois %>% filter(aoi1 == 8) %>% st_geometry() %>% plot()


# ref_labels %>% filter(name == "GH0391094")

# collect image catalog
bucket_prefixes <- paste0(1:16, "_whole/")
bucket_prefixes[bucket_prefixes == "3_whole/"] <- "3_whole_retrain/"

img_catalog <- lapply(bucket_prefixes, function(x) { # x <- bucket_prefixes[1]
  img_root <- glue::glue("/vsis3/activemapper/classified-images/{x}")
  # aws_string <- glue::glue("aws s3 ls s3://activemapper/classified-images/{x}")
  # imgs <- system(aws_string, intern = TRUE) %>% as_tibble() %>%
  #   tidyr::separate(value, sep = c(20, 31),
  #                   into = c("date", "size", "image")) %>%
  #   mutate_all(trimws) %>% mutate(prefix = x)
  aoi_id <- gsub("_.*", "", x)

  print(glue::glue("Processing AOI {aoi_id}"))
  # plot(tiles %>% filter(tile %in% tile_ids) %>% st_geometry())

  ref_labels_aoi <- ref_labels %>% filter(aoi %in% aoi_id)

  # plot(ref_labels_aoi$geometry)

  # imgs %>% filter(image == y)
  tile_ids <- unique(ref_labels_aoi$tile)
  # which(tile_ids == 486300)
  # ref_labels_aoi %>% as_tibble() %>% group_by(tile) %>% count()
  probs <- lapply(1:nrow(ref_labels_aoi), function(y) { # y <- 1
    d <- ref_labels_aoi %>% slice(y)
    print(glue::glue("..{d$name}"))
    img_nm <- glue::glue("image_c{d$col}_r{d$row}.tif")
    img <- raster(glue::glue("{img_root}{img_nm}"))
    prob <- extract(img, d)[[1]]
    return(prob)
  })
  # sapply(probs, function(x) mean(x[[1]]))
  return(probs)
})

save(img_catalog, file = here::here("inst/extdata/image_probs.rda"))
