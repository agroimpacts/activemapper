# Data set up

# Clip down labeller's master grid to just Ghana tiles extent and ID layer for
# shipping with activemapper package data
library(raster)
library(sf)
library(dplyr)

# get data from labeller
data_path <- file.path("/Users", unname(Sys.info()["user"]),
                       "Dropbox/projects/activelearning/mapper/spatial/data")
master_grid <- brick(file.path(data_path, "processed/master_grid.tif"))
v <- values(master_grid[[1]])
# length(v[!is.na(v)])
# length(unique(v[!is.na(v)]))

# crop master_grid to ghana_grid
tiles <- st_read("inst/extdata/ghana_tiles.geojson")
ghana_grid <- crop(master_grid[[1]], extent(tiles))
# length(ghana_grid[(!is.na(ghana_grid))])
# length(ghana_grid[!is.na(ghana_grid)])
# length(unique(ghana_grid[!is.na(ghana_grid)]))
# which(duplicated(ghana_grid[!is.na(ghana_grid)]))

writeRaster(ghana_grid , file = "inst/extdata/ghana_grid.tif",
            overwrite = TRUE, datatype = "INT4U")  # careful for datatype
r <- raster("inst/extdata/ghana_grid.tif")
# length(r[!is.na(r)])
# length(unique(r[!is.na(r)]))

# slim down Africa data
africa <- st_read(file.path(data_path, "external/africa_noisl_gcs.sqlite")) %>%
  st_sf(crs = 4326) %>% select(name, iso3) %>%
  st_write("inst/extdata/africa.geojson")

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
mean(aois$area)
range(aois$area)

st_write(aois, dsn = "inst/extdata/aois.geojson")

## Write out 1 degree grids
degrees <- raster(file.path(data_path, "processed/africa_aois.tif"))
degrees <- rasterToPolygons(degrees) %>% st_as_sf %>%
  rename(grid = africa_aois) %>% st_sf(crs = 4326)
st_write(degrees, "inst/extdata/degrees.geojson")

## Intersect AOIs with tiles to get AOI ID with tiles
tiles <- read_sf(here::here("inst/extdata/ghana_tiles.geojson"))

# which tiles intersect which AOIs.
tile_pts <- st_intersection(tiles %>% st_centroid, aois %>% select(aois))
tiles2 <- left_join(tiles, tile_pts %>% as_tibble %>% select(tile, aois)) %>%
  arrange(aois) %>% select(tile, aois, aoi) %>% rename(aoi1 = aois, aoi2 = aoi)
file.copy("inst/extdata/ghana_tiles.geojson",
          "external/reference/ghana_tiles.geojson")
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

################################################################################
# Train/validation/training reference sites

# read in incoming and outgoing names files
# get names of csv files holding sample sites
# also new Q sites from labeller 8
str <- glue::glue("aws s3 ls s3://activemapper/planet/")
planet_bucket <- system(str, intern = TRUE)
fnames <- sapply(strsplit(planet_bucket, split = " "), function(x) x[length(x)])
sample_files <- as_tibble(fnames) %>%
  filter(grepl("incoming_names|outgoing_names|q_sites", value)) %>%
  filter(!grepl("congo|BF|GH|tanzania|empty|test", value)) %>% pull()

# read in in loop
calval_pts <- lapply(sample_files, function(x) {  # x <- sample_files[1]
  print(x)
  sites <- s3read_using(readr::read_csv, bucket = "activemapper",
                        object = glue::glue("planet/{x}"))
  sites %>% mutate(file = gsub("\\.csv", "", x)) %>% select(file, !!names(.))
})

# write list for cleaning up data
# bind_cols(
#   file = sample_files,
#   nr = sapply(calval_pts, function(x) nrow(x))
# ) %>% readr::write_csv(here("external/misc/calval_sites.csv"))

# new Q sites from labeller8 (after turning on labeller8)
newqsites <- tbl(con, "kml_data_static") %>% collect() %>%
  mutate(file = "qsites_new", name) %>% select(file, name)

# combine with calval_pts
calval_pts <- append(calval_pts, list(newqsites))

# combine into single tibble
sapply(calval_pts, function(x) {  # x <- calval_pts[[62]]
  if(nrow(x) > 0) {
    # print(unique(x$file))
    if(substr(unique(x$file), 1, 1) == "q") {
      out <- x %>% mutate(usage = "qsite", iteration = 0)
      out %>% select(file, name, iteration, usage)
    } else {
      x %>% select(file, name, iteration, usage)
    }
  }
}) %>% reduce(rbind) -> calval_pts_tb

dup_names <- calval_pts_tb %>% distinct(name, usage) %>%
  group_by(name) %>% count() %>% filter(n > 1) %>% pull(name)
# calval_pts_tb %>% filter(usage == "qsite")
# calval_pts_tb %>% filter(name %in% dup_names) %>% filter(usage == "qsite")
  # arrange(name) %>%
  # filter(file == "incoming_names_3")
# calval_pts_tb %>% filter(file == "incoming_names_3") %>% arrange(name) %>%
#   group_by(name) %>% count() %>% filter(n > 1)

# join with main grid
mgrid <- data.table::fread(
  system.file("extdata/ghana_grid.csv", package = "activemapper")
)

calval_pts_tbf <- left_join(calval_pts_tb, mgrid) %>%
  select(file, id, name, x, y, iteration)

# data.table::fwrite(calval_pts_tbf,
#                    file = here("external/data/train_val_sites.csv"))
# file.copy(here::here("external/data/train_val_sites.csv"),
#           here::here("inst/extdata/train_val_sites.csv"))
data.table::fwrite(calval_pts_tbf,
                   file = here("inst/extdata/train_val_sites.csv"))


