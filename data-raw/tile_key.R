
# RowCol <> Tile key for AOIs
library(activemapper)
library(rmapaccuracy)
library(sf)
library(dplyr)

tiles <- read_sf(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)
aois <- read_sf(
  system.file("extdata/aois.geojson", package = "activemapper")
)
tiles_aois <- st_join(tiles, aois, largest = TRUE)
tiles_aois <- tiles_aois %>% select(tile, aoi1) %>% rename(aoi = aoi1)

# extract row_col
xys <- tiles_aois %>% st_centroid() %>% st_coordinates()
tile_rcs <- rowcol_from_xy(xys[, 1], xys[, 2], res = 0.05)

# combine into tile, aoi, row, col key
tile_key <- tibble(
  as_tibble(tiles_aois), row = tile_rcs[, 1], col = tile_rcs[, 2]
) %>% select(aoi, tile, col, row, -geometry)

usethis::use_data(tile_key)

