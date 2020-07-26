##  Preparing data outputs from instances

# Lightweight data go into data/
# Heavier data go into inst/extdata
# Intermediate (untracked) inputs go into external/..

# usethis::use_data_raw("data_from_instances")

# Packages
library(aws.s3)
library(dplyr)

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
# Instance databases
# Databases were all combined onto a single instance (labeller_all) and kept
# there. Downloading selected tables from them here

# Launch `labeller_all` with utility shell script
# $PROJECT_HOME/labeller/common/tools/start_instance.sh

# get IP of instance holding all databases
aws_qry <- glue::glue("aws ec2 describe-instances --filters ",
                      "'Name=tag:Name,Values=labeller_all' --output text ",
                      "--query 'Reservations[].Instances[][PublicIpAddress]'")
host <- system(aws_qry, intern = TRUE)
# browseURL(glue::glue("https://{host}/phpPgAdmin"))

# And then download the databases
dbnames <- paste0(
  "labeller",
  # keeping two variants of trainsouth and trainsw in first tiers to make sure
  # other data besides consensus_conflict wasn't compromised by age of backup
  c("", "trainsouth92019", "trainsouth",
    "trainsw102019", "trainsw",
    "trainsouthnl", "nl", "v2", "southv2", "test",
    "test2", 1:16, "1a2", "8a2", "15a2", "1r", "8r", "15r", "2nl",  "8v2",
    "82019")
)

nonspatial_tables <- c("accuracy_data", "assignment_data", "assignment_history",
                       "configuration", "hit_data", "incoming_names",
                       "iteration_metrics", "qual_accuracy_data",
                       "qual_assignment_data", "worker_data", "users")

# credentials
params <- yaml::yaml.load_file(here::here("common/config.yaml"))
dinfo <- params$mapper

db_tables <- lapply(dbnames, function(x) {  # x <- dbnames[7]
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = host,
                        dbname = x, user = dinfo$db_username,
                        password = dinfo$db_password)

  print(glue::glue("Fetching from {x}..."))
  tbls <- lapply(nonspatial_tables, function(y) {  # y <- nonspatial_tables[1]
    print(glue::glue("......table {y}"))
    tbl(con, y) %>% collect()
  })


  # kml_data
  kml_data <- tbl(con, "kml_data") %>%
    left_join(., tbl(con, "master_grid"), by = "name") %>%
    select(id, name, x, y, kml_type, mapped_count, mappers_needed,
           consensus_conflict) %>% collect()

  # scenes_data
  scenes_data <- tbl(con, "scenes_data") %>%
    filter(cell_id %in% !!unique(kml_data$id)) %>% collect()

  # user_maps
  user_sql <- glue::glue(
    "select name, category, assignment_id, geom_clean FROM user_maps ",
    "INNER JOIN categories USING (category)"
  )
  user_maps <- suppressWarnings(st_read(con, query = user_sql))
  if(nrow(user_maps) > 0) {
    user_maps <- user_maps %>%
      mutate(fieldnum = gsub("*.*_", "", name),
             name = gsub("_.*", "", name)) %>%
      select(name, fieldnum, assignment_id, category)
  } else {
    user_maps <- NA
  }

  DBI::dbDisconnect(con)

  # append to list
  for(i in 1:3) {
    tbls[length(tbls) + 1] <- list(list(kml_data, scenes_data, user_maps)[[i]])
  }
  names(tbls) <- c(nonspatial_tables, "kml_data", "scenes_data", "user_maps")

  return(tbls)
})
# dbnames[dbnames == "labellertrainsouth92019"] <- "labellertrainsouth"
names(db_tables) <- dbnames  # rename labellertrainsouth92019 to old name

# short-term save
save(db_tables, file = "external/data/aois/dbases/instance_dbases.rda")
# load("external/data/aois/dbases/instance_dbases.rda")

# check for drift in values across dates (check.R)

#------------------------------------------------------------------------------#
# reshape instance databases into combined tables across instances

load("external/data/aois/dbases/instance_dbases.rda")

tbl_names <- names(db_tables$labeller)
instance_tbls <- lapply(tbl_names, function(x) {  # x <- tbl_names[11]
  print(x)
  instance_tbl <- lapply(names(db_tables), function(yy) {
    # yy <- names(db_tables)[1]
    print(glue::glue("...{yy}"))
    dat <- db_tables[[yy]][[x]]
    if(is.data.frame(dat)) {
      dat <- as_tibble(dat) %>% mutate(aoi = yy) %>% select(aoi, !!names(.))
    }
  }) %>% do.call(rbind, .)
})
names(instance_tbls) <- tbl_names

# anonymize user names
instance_tbls$users <- instance_tbls$users %>% distinct(aoi, id, email) %>%
  group_by(email) %>%
  mutate(uuid = digest::digest(email, algo = "xxhash64")) %>%
  ungroup() %>%
  select(aoi, id, uuid)

# save(instance_tbls, file = "external/data/aois/dbases/instance_tables.rda")
save(instance_tbls, file = here::here("inst/extdata/instance_tbls.rda"))

#------------------------------------------------------------------------------#
# Results from labellertest: image composite quality analysis
# NOTE: 19/7/2020 - this still needs to be completed
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

library(tidyr)
mgrid <- readr::read_csv(
  system.file("extdata", "ghana_grid.csv", package = "activemapper")
)
tiles <- st_read(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)

image_quality <- left_join(
  instance_tbls$assignment_data %>% filter(aoi == "labellertest"),
  instance_tbls$hit_data %>% filter(aoi == "labellertest")
) %>% left_join(., mgrid, by = "name") %>%
  select(id, name, x, y, assignment_id, worker_id, comment)

image_quality <- image_quality %>% select(name, x, y, worker_id, comment) %>%
  filter(!(is.na(comment) | grepl("can't", comment))) %>%
  tidyr::separate(comment, into = c(paste0("gs_", 1:4), paste0("os_", 1:4))) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_join(., tiles) %>% as_tibble() %>% select(-geometry, -aoi2) %>%
  select(name, tile, aoi1, worker_id, !!names(.))

image_quality <- image_quality %>%
  pivot_longer(gs_1:os_4, names_to = "dimension", values_to = "score") %>%
  mutate(score = as.numeric(score),
         season = gsub("_.*", "", dimension),
         dimension = gsub("*.*_", "", dimension)) %>%
  rename(aoi = aoi1) %>%
  select(name, tile, aoi, worker_id, season, dimension, score)

usethis::use_data(image_quality, overwrite = TRUE)

#------------------------------------------------------------------------------#
# Training and validation sites for AOIs 1-16

# inputs
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))
mgrid <- data.table::fread(
  system.file("extdata/ghana_grid.csv", package = "activemapper")
)
train_val <- readr::read_csv(here("inst/extdata/train_val_sites.csv"))

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

#------------------------------------------------------------------------------#
# Results from all instances: Average N labels and label accuracy
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

# instance_tbls$hit_data %>%
#   filter(aoi == "labeller3" & delete_time > "2019-12-05") %>%
#   left_join(., instance_tbls$assignment_data %>% filter(aoi == "labeller3"),
#             by = c("aoi", "hit_id")) %>%
#   filter(!is.na(score))
# instance_tbls$assignment_data %>%
#   filter(!(aoi == "labeller3" & completion_time < "2019-12-05" & !is.na(score)))

# select scores from Q assignments, dropping Q sites in labeller3 done during
# run 0. Filter by completion_time earlier than 5/12/2019, which was a few days
# before run 1 started. Anything after that is a Q assignment done during run 1.
# Also including Q sites done during labeller, labellertrainsouth,
# labellertrainsouth, labellertrainsouthnl, and labellertrainsw. labellernl not
# included because not used for map-making

# aoinms <- paste0("labeller", 1:16)  # original
aoinms <- c(
  "labeller",
  paste0("labeller", c("trainsouth", "trainsouthnl", "trainsw", 1:16))
)
quality_scores <- instance_tbls$assignment_data %>%
  select(-one_of("comment", "status")) %>%
  filter(!is.na(score)) %>% rename(id = worker_id) %>%
  filter(aoi %in% aoinms) %>%
  filter(!(aoi == "labeller3" & completion_time < "2019-12-05")) %>%
  left_join(., instance_tbls$users, by = c("aoi", "id")) %>%
  left_join(., instance_tbls$hit_data %>% select(aoi, hit_id, name)) %>%
  select(-contains("time")) %>% select(aoi, name, !!names(.))

# summary statistics for label quality (from Q sites)
quality_sum <- quality_scores %>% group_by(uuid) %>%
  summarize(N = n(), Mean = mean(score), StDev = sd(score)) %>%
  arrange(-Mean)

# overall number of labels
nlabels <- instance_tbls$assignment_data %>%
  filter(aoi %in% aoinms) %>%
  left_join(., instance_tbls$hit_data, by = c("aoi", "hit_id")) %>%
  left_join(., instance_tbls$kml_data, by = c("aoi", "name")) %>%
  filter(!status %in% c("Returned", "Abandoned")) %>%
  select(aoi, worker_id, status, kml_type) %>%
  rename(id = worker_id) %>%
  left_join(., instance_tbls$users %>% select(aoi, uuid, id),
            by = c("aoi", "id")) %>%
  filter(uuid %in% quality_sum$uuid) %>%
  mutate(uuid = substr(uuid, 1, 5)) %>%
  group_by(uuid, kml_type) %>% count() %>% arrange(-n) %>%
  rename(ID = uuid) %>% ungroup() %>%
  mutate(kml_type = case_when(
    kml_type == "F" ~ "Training",
    kml_type == "Q" ~ "Training reference"
  ))

# output list
label_summary <- list("scores" = quality_scores, "score_stats" = quality_sum,
                      "nlabels" = nlabels)

usethis::use_data(label_summary, overwrite = TRUE)

#------------------------------------------------------------------------------#
# Consensus labelling conflicts (needs updating when consensus conflicts are
# fixed)

load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

# select AOIs and unique names:
aoinms <- c(
  "labeller",
  paste0("labeller", c("trainsouth", "trainsouthnl", "trainsw", 1:16))
)

nms <- unique(c(train_val_sites$initial$name, train_val_sites$active$name))
# any(!nms %in% unique(instance_tbls$kml_data$name))

# This selects out F sites from the mapped list
label_consensus <- instance_tbls$kml_data %>% filter(aoi %in% aoinms) %>%
  filter(name %in% nms) %>%
  select(-one_of("kml_type", "mapped_count", "mappers_needed")) %>%
  rename(Risk = consensus_conflict)

# label_consensus %>% filter(aoi == "labeller8") %>% filter(Risk > 0)
# label_consensus %>% filter(aoi == "labeller8") %>% filter(Risk > 0)
# label_consensus %>% group_by(aoi) %>%
#   summarize(zeros = sum(ifelse(Risk == 0, 1, 0)),
#             nonzeros = sum(ifelse(Risk > 0, 1, 0)))
usethis::use_data(label_consensus, overwrite = TRUE)

#------------------------------------------------------------------------------#
# Iteration metrics
# Analyze and reshape for figure making

load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

# function to calculate change over time
delta_func <- function(x, rnd = 4) round((x - lag(x)) / lag(x), rnd)

# process individual AOIs
metrics <- c("Accuracy", "AUC", "F1", "TSS", "Precision", "Recall", "TPR",
             "FPR")
aoinms <- paste0("labeller", 1:16)
iteration_metrics <- instance_tbls$iteration_metrics %>%
  filter(aoi %in% aoinms) %>%
  filter(iteration > 0) %>%
  filter(!(aoi == "labeller3" & run == 0))  %>% # remove run 0 labeller3
  mutate(iteration = iteration - 1) %>% # rebase iteration counter on 0
  mutate(F1 = 2 * (precision * recall) / (precision + recall)) %>%
  rename(AOI = aoi, Iteration = iteration, Accuracy = accuracy, AUC = auc,
         TSS = tss, Precision = precision, Recall = recall, TPR = tpr,
         FPR = fpr) %>%
  select(AOI, Iteration, !!metrics) %>%
  group_by(AOI) %>%  # calculate deltas
  mutate_at(metrics, funs("change" = delta_func)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Accuracy:FPR_change, names_to = "Metric",
                      values_to = "Score")

# process across AOI means and combine with AOIs
iteration_metrics %>%
  filter(Iteration < 4) %>%
  group_by(Iteration, Metric) %>% summarize(Score = mean(Score)) %>%
  mutate(AOI = "All") %>%
  ungroup %>%
  select(AOI, !!names(.)) %>%
  bind_rows(iteration_metrics, .) -> iteration_metrics

usethis::use_data(iteration_metrics)

