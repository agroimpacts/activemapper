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

tiles <- vect(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)

r <- rast(ext(tiles), res = 0.05)
crs(r) <- crs(tiles)
tilesr <- rasterize(tiles, y = r, field = "tile")
# tilesr <- fasterize::fasterize(tiles, r, field = "tile")
tilesr005 <- disaggregate(tilesr, fact = 10)
tilesr005_sf <- stars::st_as_stars(raster::raster(tilesr005)) %>% st_as_sf()
# tilesr005_v <- vect(tilesr005)
# tilesr005_sf <- stars::st_as_stars(tilesr005) %>% st_as_sf() %>%
#   rename(tile = layer)

lapply(bucket_prefixes, function(x) {
  # x <- bucket_prefixes[1]

  aoi_prefix <- glue("classified-images/{x}")
  aoi_id <- gsub("_.*", "", x)

  print(glue::glue("Processing AOI {aoi_id}"))

  # get list of images for AOI and filter using tile_key down to list to read in
  img_bucket <- get_bucket_df(bucket = "activemapper", prefix = aoi_prefix) %>%
    select(Key)
  tile_filter <- tile_key %>% filter(aoi == aoi_id) %>%
    mutate(image = glue("image_c{col}_r{row}.tif")) %>%
    mutate(Key = glue("{aoi_prefix}{image}"))
  # images <- img_bucket %>% filter(Key %in% tile_filter$Key)

  tile_ids <- unique(tile_filter$tile)
  gridded_probs <- foreach(y = tile_ids, .combine = rbind) %dopar% {
    # y <- tile_ids[1]

    grids <- tilesr005_sf %>% filter(tile == y) %>%
      mutate(ID = 1:nrow(.)) %>% select(ID) %>% vect(.)
    img_nm <- tile_filter %>% filter(tile == y) %>% pull(Key)

    img <- rast(glue("/vsis3/activemapper/{img_nm}"))


    # gridsr <- terra::rasterize(grids, y = img, field = "tile")
    # gridsr[] <- 1:ncell(gridsr)
    v <- terra::extract(img, grids, fun = mean)

    # gridsr <- fasterize::fasterize(grids, raster = img, field = "tile")
    # gridsr[] <- 1:ncell(gridsr)
    prob <- terra::zonal(x = img, z = gridsr)

  }



})


