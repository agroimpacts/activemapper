# Create one degree grid and tile grid for Africa and Ghana
library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(data.table)

# read in main grid
mgrid_path <- file.path("/vsis3/activemapper/grid/master_grid.tif")
mgrid <- raster(mgrid_path)

# read in Africa shape
africa <- st_read(here::here("external/data/africa_noisl_gcs.sqlite")) %>%
  st_set_crs(4326) %>% select(name, iso3)
africa_cont <- africa %>% st_union %>% st_sf
ghana <- africa %>% filter(name == "Ghana") %>% select(GEOMETRY)

# Make two aggregations for super cells and AOIs
# start with AOIs (1X1 degree)
e <- extent(mgrid)
e2 <- c(e@xmin, e@xmin + ceiling(e@xmax - e@xmin),
        e@ymax - ceiling(e@ymax - e@ymin), e@ymax)
aois <- raster(extent(e2), res = 1) %>% setValues(., 1)
crs(aois) <- st_crs(4326)$wkt
extract(aois, africa_cont, weights = TRUE, cellnumbers = TRUE) %>%
  as.data.frame %>% as_tibble %>% filter(weight > 0) %>%
  mutate(aoi = 1:nrow(.)) -> iwhichs
aois[which(!1:ncell(aois) %in% iwhichs$cell)] <- NA
aois[iwhichs$cell] <- iwhichs$aoi

# disggregate to tile scale
tiles <- disaggregate(aois, fact = 20)
tiles_aois <- tiles
tiles_dt <- tibble(cell = 1:ncell(tiles), aoi = values(tiles)) %>% na.omit() %>%
  mutate(tile = aoi) %>% as.data.table

# number tiles by row and column within each AOI. In data.table for speed
cnter <- 0
for(i in unique(tiles_dt$aoi)) {
  v <- ((cnter):(tiles_dt[aoi == i, .N] + cnter - 1) + 1)
  tiles_dt[aoi == i, tile := v]
  cnter <- tiles_dt[aoi == i, max(tile)]
}
tiles[tiles_dt$cell] <- tiles_dt$tile  # assign tile number to raster
tiles_stack <- stack(tiles_aois, tiles)
crs(tiles_stack) <- st_crs(4326)$wkt
names(tiles_stack) <- c("aoi", "tile")

# check
# j <- 1000
# tiles_dt %>% filter(aoi %in% j) %>% tail %>% slice(6)
# tiles_dt %>% filter(aoi == j + 1) %>% head %>% slice(1)
# tiles_dt %>% filter(aoi == j + 1) %>% tail %>% slice(6)
# tiles_dt %>% filter(aoi == j + 2) %>% head %>% slice(1)

# cut down tiles to Ghana
ghana_tiles <- crop(tiles_stack, ghana) %>% rasterToPolygons(.) %>%
  st_as_sf(.) %>% st_sf(crs = 4326)
st_intersects(ghana, ghana_tiles)[[1]] %>% slice(ghana_tiles, .) -> ghana_tilesr
# ghana_tilesr[1] %>% st_geometry %>% plot
# plot(ghana, add = TRUE, col = "red")

# write out
# Ghana tiles
st_write(ghana_tilesr, dsn = here::here("external/data/ghana_tiles.geojson"))

# Africa shape
st_write(africa, dsn = here::here("inst/extdata/africa.geojson"))

# Africa-wide degrees/AOIs
writeRaster(aois, filename = here::here("external/data/africa_aois.tif"),
            overwrite = TRUE)

# Africa-wide tiles
writeRaster(tiles_stack$tile,
            filename = here::here("external/data/africa_tiles.tif"),
            overwrite = TRUE)

# file.path(data_path, "processed/ghana_tiles.geojson") %>% st_read %>% st_as_sf

# # t1 <- st_read(system.file("extdata/ghana_tiles.geojson", package = "activemapper"))
# # all(sort(t1$tile) == sort(ghana_tilesr$tile))
# plot(t1["tile"])
# plot(ghana_tilesr["tile"])
# ghana_tilesr %>% filter(tile == "487345") %>% st_geometry %>% plot(add = TRUE)

