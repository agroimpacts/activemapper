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

logf <- here::here("external/logs/grid_probs.log")

registerDoMC(4)
aoi_probs <- lapply(bucket_prefixes, function(x) {
  # x <- bucket_prefixes[1]

  aoi_prefix <- glue("classified-images/{x}")
  aoi_id <- gsub("_.*", "", x)

  cat(glue::glue("Starting {aoi_id} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

  # print(glue::glue("Processing AOI {aoi_id}"))

  # get list of images for AOI and filter using tile_key down to list to read in
  img_bucket <- get_bucket_df(bucket = "activemapper", prefix = aoi_prefix) %>%
    select(Key)
  tile_filter <- tile_key %>% filter(aoi == aoi_id) %>%
    mutate(image = glue("image_c{col}_r{row}.tif")) %>%
    mutate(Key = glue("{aoi_prefix}{image}"))
  # images <- img_bucket %>% filter(Key %in% tile_filter$Key)

  tile_ids <- unique(tile_filter$tile)
  # gridded_probs <- foreach(y = tile_ids[1:10]) %dopar% {
  gridded_probs <- foreach(y = tile_ids) %dopar% {
    # y <- tile_ids[1]
    cat(glue::glue("...aggregating tile {y}"),
        file = logf, sep = "\n", append = TRUE)

    # grids <- tilesr005_sf %>% filter(tile == y) %>%
    #   mutate(id = 1:nrow(.)) %>% select(id) %>% vect(.)
    img_nm <- tile_filter %>% filter(tile == y) %>% pull(Key)

    # img <- rast(raster::raster(glue("/vsis3/activemapper/{img_nm}")) * 1)
    img <- raster::raster(glue("/vsis3/activemapper/{img_nm}"))
    # prob <- aggregate(img, fact = 200, fun = "mean")
    prob <- raster::aggregate(img, fact = 200, fun = "mean")
    # plot(prob)
    # plot(grids, add = TRUE)
    return(prob)
  }
  gridded_probs$fun <- mean
  aoi_prob <- do.call(mosaic, gridded_probs)
  # plot(aoi_probs)
  # plot(tiles %>% filter(aoi1 == 1) %>% st_geometry(), add = TRUE)

  return(aoi_prob)
})
aoi_probs$fun <- mean

ghana <- read_sf(system.file("extdata/"))

# plot(aoi_probs[[16]])
probs_005 <- do.call(mosaic, aoi_probs)
probs_005[probs_005 > 100] <- NA
save(aoi_probs, file = here::here("external/data/results/probs/aoi_probs.rda"))
writeRaster(probs_005,
            filename = here::here("external/data/results/probs/probs_005.tif"))

