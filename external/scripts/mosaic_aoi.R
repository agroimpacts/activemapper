# Run mosaicking job on AOI5
library(activemapper)
library(dplyr)
library(sf)
library(aws.s3)

# settings here
selected_aoi <- "5_whole"  # select the prefix for the AOI containing the images
output_path <- "activemapper/classified-images/aois/5/"
bucket <- "activemapper/classified-images/"
aws_string <- glue::glue("aws s3 ls s3://{bucket}")
ofile <- "aoi5_probabilities.tif"

# Filter tiles to just AOI5
tiles <- st_read(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)

# get paths and names of probability images
# get list of probability images from bucket
# system("aws s3 ls s3://activemapper/classified-images/")
# get upper level list of bucket
aois <- system(aws_string, intern = TRUE) %>% as_tibble() %>%
  tidyr::separate(value, sep = c(20, 31), into = c("date", "prefix", "aoi")) %>%
  mutate_all(trimws) %>% mutate(aoi = gsub("/", "", aoi))

# run this if you don't know available AOI names. There are currently a number
# of files at the top level, so this yields more than sub-bucket prefixes
# unique(aois$aoi)

# select specific objects from aoi you want.
# NOTE: this section could be put into a loop
aoi <- aois %>% filter(aoi == selected_aoi) %>% pull(aoi)
aoi_objects <- system(glue::glue("{aws_string}{aoi}/"), intern = TRUE) %>%
  as_tibble() %>%
  tidyr::separate(value, sep = c(20, 31), into = c("date", "size", "image")) %>%
  mutate_all(trimws)

#------------------------------------------------------------------------------#
# Get the column and row positions from the image names
rcs <- do.call(rbind,
               strsplit(gsub("image_|.tif|c|r", "", aoi_objects$image), "_"))
colnames(rcs) <- c("c", "r")
rcs <- apply(rcs, 2, as.numeric)
imgs <- cbind(aoi_objects, rcs) %>% as_tibble()
# rcs %>% View()

# reference them to our tiles
# use function to get xys from row col of probability images
xys <- xys_from_rowcol(r = imgs$r + 1, c = imgs$c + 1)
xys <- as_tibble(xys) %>% mutate(r = r - 1, c = c - 1) %>%
  dplyr::select(c, r, xs, ys) %>% left_join(imgs, .)
xys_sf <- st_as_sf(as_tibble(xys), coords = c("xs", "ys"), crs = 4326)
# plot(st_geometry(tiles), border = "transparent", col = "grey")
# plot(st_geometry(xys_sf %>% slice(1)), add = TRUE)
# plot(st_geometry(xys_sf), add = TRUE)

# intersect with tiles
selected_aoi2 <- gsub("_whole", "", selected_aoi)
xys_tiles <- st_join(xys_sf, tiles) %>%
  dplyr::select(image, c, r, tile, aoi1)

# filter to just images in AOI--note there are a lot of predictions for tiles in
# training images ouotside of AOI
xys_aoi <- xys_tiles %>% filter(aoi1 == selected_aoi2)

# checks
# plot(st_geometry(tiles), border = "transparent", col = "grey")
# plot(st_geometry(xys_aoi), add = TRUE, pch = 16, cex = 0.1)
# plot(st_geometry(tiles %>% filter(aoi1 == selected_aoi2) %>% slice(1)),
#      border = "transparent", col = "grey")
# plot(st_geometry(xys_aoi %>% slice(1)), add = TRUE)
# plot(st_geometry(xys_aoi))

# run mosaic
# path variables for all selected images in bucket
pths <- glue::glue("/vsis3/{bucket}{selected_aoi}/{xys_aoi$image}")
# testss <- xys_aoi5 %>% filter(c %in% 320:325, r %in% 560:565) %>% pull(fname)
# testss <- paste0(here::here("external/prob/aoi5/"), testss)
# gdalUtils::mosaic_rasters(gdalfile = testss, dst_dataset = ofile,
#                           verbose = TRUE)

system.time(gdalUtils::mosaic_rasters(gdalfile = pths, dst_dataset = ofile,
                                      verbose = TRUE))


