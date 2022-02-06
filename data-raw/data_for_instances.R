# Data set up
## Follows on from aoi_tiles.R

#------------------------------------------------------------------------------#
# Prepare main grid for Ghana
# Clip down labeller's primary grid to just Ghana tiles extent and ID layer for
# shipping with activemapper package data
library(raster)
library(sf)
library(dplyr)

# get data from labeller (get from S3)
mgrid_path <- file.path("/vsis3/activemapper/grid/master_grid.tif")
mgrid <- raster(mgrid_path)
# data_path <- file.path("/Users", unname(Sys.info()["user"]),
#                        "Dropbox/projects/activelearning/mapper/spatial/data")
# mgrid <- brick(file.path(data_path, "processed/master_grid.tif"))
# v <- values(mgrid[[1]])
# length(v[!is.na(v)])
# length(unique(v[!is.na(v)]))

# crop master_grid to ghana_grid
tiles <- st_read(here::here("external/data/ghana_tiles.geojson"))
# tiles <- tiles %>% rename(aoi = aoi1)
ghana_grid <- crop(mgrid[[1]], extent(tiles))
# length(ghana_grid[(!is.na(ghana_grid))])
# length(ghana_grid[!is.na(ghana_grid)])
# length(unique(ghana_grid[!is.na(ghana_grid)]))
# which(duplicated(ghana_grid[!is.na(ghana_grid)]))

writeRaster(ghana_grid, file = "inst/extdata/ghana_grid.tif",
            overwrite = TRUE, datatype = "INT4U")  # careful for datatype
# r <- raster("inst/extdata/ghana_grid.tif")
# length(r[!is.na(r)])
# length(unique(r[!is.na(r)]))

#------------------------------------------------------------------------------#
# Create AOIs, tiles, degrees
# slim down Africa data
africa <- st_read("inst/extdata/africa.geojson")

## Set up mapping AOIs
prod_groupings1 <- list(c(1216, 1217, 1283, 1284), c(1218, 1285), c(1219, 1286),
                        c(1348, 1349), 1350, c(1351, 1352), c(1413, 1414), 1415,
                        c(1416, 1417), c(1477, 1478), 1479, c(1480, 1481),
                        c(1539, 1540), 1541, c(1542, 1543), 1599:1602)
purrr::map(1:length(prod_groupings1), function(x) {
  ind <- prod_groupings1[[x]]
  tiles %>% filter(aoi %in% ind) %>% mutate(grp = x) %>% select(aoi, tile, grp)
}) %>% purrr::reduce(rbind) -> aois

aois <- aois %>% group_by(grp) %>%
  summarize(aois = mean(grp)) %>% ungroup
aois <- aois %>%
  mutate(area = st_area(.) %>% units::set_units("km2") %>% as.numeric) %>%
  select(grp, aois, area)
# mean(aois$area)
# range(aois$area)

st_write(aois, dsn = "inst/extdata/aois.geojson")

## Write out 1 degree grids
degrees <- raster(here::here("external/data/africa_aois.tif"))
degrees <- rasterToPolygons(degrees) %>% st_as_sf %>%
  rename(grid = africa_aois) %>% st_sf(crs = 4326)
st_write(degrees, "inst/extdata/degrees.geojson")

## Intersect AOIs with tiles to get AOI ID with tiles
# tiles <- read_sf(here::here("inst/extdata/ghana_tiles.geojson"))

# which tiles intersect which AOIs.
tile_pts <- st_intersection(tiles %>% st_centroid, aois %>% select(aois))
tiles2 <- left_join(tiles, tile_pts %>% as_tibble %>% select(tile, aois)) %>%
  arrange(aois) %>% select(tile, aois, aoi) %>% rename(aoi1 = aois, aoi2 = aoi)
# file.copy("inst/extdata/ghana_tiles.geojson",
#           "external/reference/ghana_tiles.geojson")
st_write(tiles2, "inst/extdata/ghana_tiles.geojson", delete_dsn = TRUE)

## get copy of primary grid in csv format
params <- yaml::yaml.load_file(here::here("common/config.yaml"))
dinfo <- params$mapper
host <- "labeller8.crowdmapper.org"
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = host,
                      dbname = "Africa", user = dinfo$db_username,
                      password = dinfo$db_password)
mgrid <- tbl(con, "master_grid") %>% collect()
mgrid %>% select(id, name, x, y, fwts) %>%
  readr::write_csv(here::here("inst/extdata/ghana_grid.csv"))
