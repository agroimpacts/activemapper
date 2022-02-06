#------------------------------------------------------------------------------#
# Image quality check data from image_checker
library(tidyr)
tiles <- st_read(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)

aws_qry <- glue::glue("aws ec2 describe-instances --filters ",
                      "'Name=tag:Name,Values=imagechecker' --output text ",
                      "--query 'Reservations[].Instances[][PublicIpAddress]'")
host <- system(aws_qry, intern = TRUE)
# browseURL(glue::glue("https://{host}/phpPgAdmin"))

# credentials
params <- yaml::yaml.load_file(here::here("common/config.yaml"))
dinfo <- params$mapper
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = host,
                      dbname = "Africa", user = dinfo$db_username,
                      password = dinfo$db_password)

# collect data
image_quality <- tbl(con, "assignment_data") %>%
  left_join(., tbl(con, "hit_data"), by = "hit_id") %>%
  left_join(., tbl(con, "kml_data"), by = "name") %>%
  left_join(., tbl(con, "master_grid"), by = "name") %>%
  select(id, name, assignment_id, hit_id, x, y, worker_id, kml_type,
         comment) %>%
  collect()
DBI::dbDisconnect(con)
save(image_quality, file = "external/data/aois/dbases/image_quality_raw.rda")
# load("external/data/aois/dbases/image_quality_raw.rda")

# pick up tile information
image_quality <- image_quality %>%
  select(name, x, y, worker_id, kml_type, comment) %>%
  tidyr::separate(comment, into = c(paste0("gs_", 1:4), paste0("os_", 1:4))) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_join(., tiles) %>% as_tibble() %>% select(-geometry, -aoi2) %>%
  select(name, tile, aoi1, worker_id, !!names(.))
# image_quality %>% filter(worker_id == 99) %>% filter(kml_type == "F")
# image_quality %>% filter(worker_id == 92) %>% filter(kml_type == "F")

# image quality
image_quality <- image_quality %>%
  pivot_longer(gs_1:os_4, names_to = "dimension", values_to = "score") %>%
  mutate(score = as.numeric(score),
         season = gsub("_.*", "", dimension),
         dimension = gsub("*.*_", "", dimension)) %>%
  rename(aoi = aoi1) %>%
  select(name, tile, aoi, worker_id, kml_type, season, dimension, score)

# tile quality
neworder <- c("Growing", "Dry")  # reorder for plotting
tile_quality <- image_quality %>% filter(kml_type == "F") %>%
  group_by(name, season, worker_id) %>%
  summarize(tile = min(tile), Score = sum(score) / 12) %>% ungroup() %>%
  group_by(tile, season) %>%
  summarize(Score = mean(Score)) %>%
  ungroup() %>%
  mutate(season = ifelse(season == "gs", "Growing", "Dry"))
tile_quality <- transform(
  tile_quality, season = factor(season, levels = neworder)
) %>% arrange(season) %>%
  inner_join(tiles, .)

# output list
image_quality <- list("img" = image_quality, "tile" = tile_quality)

usethis::use_data(image_quality, overwrite = TRUE)

