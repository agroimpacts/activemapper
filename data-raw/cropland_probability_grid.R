# Create probability map (grid scale) from RF 3 m probability images
# Ran on rstudio instance on EC2, to avoid download into S3

library(sf)
# library(raster)
library(terra)
library(dplyr)
library(aws.s3)
library(activemapper)
library(glue)
library(doMC)

# pull in tiles and AOIs for filtering
data("tile_key")

bucket_prefixes <- paste0(1:16, "_whole/")
bucket_prefixes[bucket_prefixes == "3_whole/"] <- "3_whole_retrain/"

# tiles <- vect(
#   system.file("extdata/ghana_tiles.geojson", package = "activemapper")
# )
tiles <- st_read(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)

# r <- rast(ext(tiles), res = 0.05)
# crs(r) <- crs(tiles)
# tilesr <- rasterize(tiles, y = r, field = "tile")

# tilesr <- fasterize::fasterize(tiles, r, field = "tile")
# tilesr005 <- disaggregate(tilesr, fact = 10)
# tilesr005_sf <- stars::st_as_stars(raster::raster(tilesr005)) %>% st_as_sf()
# tilesr005_v <- vect(tilesr005)
# tilesr005_sf <- stars::st_as_stars(tilesr005) %>% st_as_sf() %>%
#   rename(tile = layer)
# plot(tilesr)
# plot(tilesr005_sf$geometry, add = TRUE)

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
  # cropfracs <- mclapply(tile_ids[1:10], function(y) {
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
# aoi_probs$fun <- mean

# plot(aoi_probs[[16]])
# cropfrac_005 <- do.call(terra::mosaic, aoi_probs)
# probs_005[probs_005 > 100] <- NA
# save(aoi_probs, file = here::here("external/data/results/probs/aoi_probs.rda"))
# writeRaster(probs_005,
#             filename = here::here("inst/extdata/probs_005.tif"))

