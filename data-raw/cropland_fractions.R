# Create grid scale fractional map from RF 3 m probability images
# Ran on rstudio instance on EC2, to avoid download into S3

library(sf)
# library(raster)
library(terra)
library(dplyr)
library(aws.s3)
# library(activemapper)
library(glue)
library(parallel)
# library(doMC)

# pull in tiles and AOIs for filtering
data("tile_key")

# bucket prefixes
bucket_prefixes <- paste0(1:16, "_whole/")
bucket_prefixes[bucket_prefixes == "3_whole/"] <- "3_whole_retrain/"

# function for estimating proportions of pixels > 0.5
ctfun <- function(x, na.rm = na.rm) {
  v <- x[!is.na(x)]
  length(which(v > 50)) / length(v)
}

# logfile
logf <- here::here("external/logs/grid_probs.log")
cropfrac_aoi <- lapply(bucket_prefixes, function(x) {
  # x <- bucket_prefixes[1]
  aoi_prefix <- glue("classified-images/{x}")
  aoi_id <- gsub("_.*", "", x)

  cat(glue::glue("Starting {aoi_id} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

    # get list of images for AOI and filter using tile_key down to list to read in
  img_bucket <- get_bucket_df(bucket = "activemapper", prefix = aoi_prefix) %>%
    select(Key)
  tile_filter <- tile_key %>% filter(aoi == aoi_id) %>%
    mutate(image = glue("image_c{col}_r{row}.tif")) %>%
    mutate(Key = glue("{aoi_prefix}{image}"))
  # images <- img_bucket %>% filter(Key %in% tile_filter$Key)

  tile_ids <- unique(tile_filter$tile)
  cropfracs <- mclapply(tile_ids, function(y) {
  # cropfracs <- mclapply(tile_ids[1:10], function(y) { # y <- tile_ids[1]
    # cat(glue::glue("...aggregating tile {y}"),
    #     file = logf, sep = "\n", append = TRUE)
    img_nm <- tile_filter %>% filter(tile == y) %>% pull(Key)
    img <- rast(raster::raster(glue("/vsis3/activemapper/{img_nm}")) * 1)
    cropfrac <- raster::raster(terra::aggregate(img, fact = 200, fun = ctfun))
    return(cropfrac)
  })
  cropfracs$fun <- mean
  aoi_frac <- do.call(terra::mosaic, cropfracs)
  # plot(aoi_frac)
  # plot(tiles %>% filter(aoi1 == 1) %>% st_geometry(), add = TRUE)
  return(aoi_frac)
})
cropfrac_aoi$fun <- mean

cropfrac_005 <- do.call(terra::mosaic, cropfrac_aoi)
# plot(cropfrac_005)
# probs_005[probs_005 > 100] <- NA


# save(cropfrac_aoi, file = here::here("inst/extdata/aoi_probs.rda"))
save(cropfrac_aoi, file = here::here("inst/extdata/cropfrac_aoi.rda"))
writeRaster(cropfrac_005,
            # filename = here::here("inst/extdata/probs_005.tif"))
            filename = here::here("inst/extdata/cropfrac_005.tif"))

