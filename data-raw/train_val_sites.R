#------------------------------------------------------------------------------#
# Training and validation sites for AOIs 1-16

# Collect training and validation sites for AOIs 1-16
library(aws.s3)
library(dplyr)
library(sf)

#------------------------------------------------------------------------------#
# Train/validation/training reference sites: collected from csvs on S3, as these
# included initial 500 sites used to train each model that were not on the
# instance databases

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

# new Q sites from labeller8 (after turning on labeller8). These were sites used
# after initial maps were completed, and created from best labels made during
# that process
params <- yaml::yaml.load_file(here::here("common/config.yaml"))
dinfo <- params$mapper
host <- "labeller8.crowdmapper.org"
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = host,
                      dbname = "Africa", user = dinfo$db_username,
                      password = dinfo$db_password)
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

#------------------------------------------------------------------------------#
# inputs
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))
mgrid <- data.table::fread(
  system.file("extdata/ghana_grid.csv", package = "activemapper")
)
train_val <- readr::read_csv(here::here("inst/extdata/train_val_sites.csv"))

# Initial random training draw
initial_train_pts <- train_val %>% filter(grepl("static", file)) %>%
  filter(!grepl("spatial|sub", file)) %>% distinct() %>%
  rename(Cluster = file) %>%
  mutate(Cluster = case_when(
    grepl("cluster1", Cluster) ~ "1",
    grepl("cluster2", Cluster) ~ "2",
    grepl("cluster3", Cluster) ~ "3",
  )) %>% select(-id) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# Active learning points
activelearn_pts <- instance_tbls$incoming_names %>%
  filter(aoi %in% paste0("labeller", 1:16)) %>% # distinct(aoi)
  filter(!(aoi == "labeller3" & run == 0)) %>%
  filter(processed == TRUE) %>%
  select(-one_of("processed", "label")) %>%
  left_join(., mgrid, by = "name") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  select(aoi, name, run, iteration, usage) %>%
  mutate(Type = case_when(
    usage == "validate" ~ "Validation",
    usage == "train" ~ paste("Iteration", iteration)
  ))

# pull out retraining sites for AOI3 and combine with initial train sites
initial_train_pts <- activelearn_pts %>%
  filter(aoi == "labeller3" & iteration == 0 & usage =="train") %>%
  rename(Cluster = aoi) %>% mutate(Cluster = "AOI 3 retrain") %>%
  select(Cluster, name, iteration) %>%
  rbind(initial_train_pts, .)  # bind original to to AOI3 new points

# Drop AOI3 retraining points from run 1
activelearn_pts <- activelearn_pts %>%
  filter(!(aoi == "labeller3" & iteration == 0 & usage == "train"))

# list and put in data/
train_val_sites <- list("initial" = initial_train_pts,
                        "active" = activelearn_pts)
usethis::use_data(train_val_sites, overwrite = TRUE)

