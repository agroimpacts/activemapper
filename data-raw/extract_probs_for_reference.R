# extract cropland probabilities from prediction images using reference polygons
# Ran on rstudio instance on EC2, to avoid download into S3

library(sf)
library(raster)
library(dplyr)
library(aws.s3)
library(activemapper)
library(doMC)

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

# collect image catalog
bucket_prefixes <- paste0(1:16, "_whole/")
bucket_prefixes[bucket_prefixes == "3_whole/"] <- "3_whole_retrain/"

registerDoMC(cores = 4)
ref_int_probs <- lapply(bucket_prefixes, function(x) { # x <- bucket_prefixes[1]
  img_root <- glue::glue("/vsis3/activemapper/classified-images/{x}")
  aoi_id <- gsub("_.*", "", x)
  print(glue::glue("Processing AOI {aoi_id}"))

  ref_labels_aoi <- ref_labels %>% filter(aoi %in% aoi_id)
  tile_ids <- unique(ref_labels_aoi$tile)
  # which(tile_ids == 486300)

  # ref_labels_aoi %>% as_tibble() %>% group_by(tile) %>% count()
  # probs <- lapply(1:nrow(ref_labels_aoi), function(y) { # y <- 20

  # # check on tile with 2 labels
  # d <- ref_labels_aoi %>% slice(21)
  # rc <- c("c" = unique(d$col), "r" = unique(d$row))
  # img_nm <- glue::glue("image_c{rc['c']}_r{rc['r']}.tif")
  # img <- raster(glue::glue("{img_root}{img_nm}"))
  # prob1 <- extract(img, d)[[1]]
  # d2 <- ref_labels_aoi %>% slice(22)
  # prob2 <- extract(img, d2)[[1]]
  # tst_probs <- list(prob1, prob2)

  probs <- foreach(y = tile_ids, .combine = rbind) %dopar% {
    # y <- tile_ids[20]
    # d <- ref_labels_aoi %>% slice(y)
    d <- ref_labels_aoi %>% filter(tile == y)
    print(glue::glue("..{d$name}"))
    rc <- c("c" = unique(d$col), "r" = unique(d$row))
    # img_nm <- glue::glue("image_c{d$col}_r{d$row}.tif")
    img_nm <- glue::glue("image_c{rc['c']}_r{rc['r']}.tif")
    img <- raster(glue::glue("{img_root}{img_nm}"))
    prob <- extract(img, d)

    # compile stats
    prob_stats <- t(sapply(prob, function(x) summary(x)))
    prob_tb <- tibble(name = d$name, tile = d$tile, aoi = d$aoi,
                      data.frame(prob_stats))
    # extract(img, d)

    # test with other double tile
    # all(sapply(prob, mean) == sapply(tst_probs, mean))
    # names(prob) <- d$name
    # return(prob)
    return(prob_tb)
  }
  return(probs)
})

ref_int_probs <- do.call(rbind, ref_int_probs)
ref_int_probs <- ref_int_probs %>%
  rename(min = Min., q1 = X1st.Qu., median = Median, mean = Mean, q3 = X3rd.Qu.,
         max = Max.)

save(ref_int_probs, file = here::here("inst/extdata/ref_int_probs.rda"))
