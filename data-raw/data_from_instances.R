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
                       "categories", "configuration", "hit_data",
                       "incoming_names", "iteration_metrics",
                       "qual_accuracy_data",
                       "qual_assignment_data", "worker_data", "users")

# credentials
params <- yaml::yaml.load_file(here::here("common/config.yaml"))
dinfo <- params$labeller

instance_dbases <- lapply(dbnames, function(x) {  # x <- dbnames[7]
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
names(instance_dbases) <- dbnames  # rename labellertrainsouth92019 to old name

# short-term save
save(instance_dbases, file = "external/data/aois/dbases/instance_dbases.rda")
# load("external/data/aois/dbases/instance_dbases.rda")

# check for drift in values across dates (check.R)

#------------------------------------------------------------------------------#
# reshape instance databases into combined tables across instances

load("external/data/aois/dbases/instance_dbases.rda")

tbl_names <- names(instance_dbases$labeller)
instance_tbls <- lapply(tbl_names, function(x) {  # x <- tbl_names[11]
  print(x)
  instance_tbl <- lapply(names(instance_dbases), function(yy) {
    # yy <- names(instance_dbases)[1]
    print(glue::glue("...{yy}"))
    dat <- instance_dbases[[yy]][[x]]
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
# Training and validation sites for AOIs 1-16

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

#------------------------------------------------------------------------------#
# Consensus labelling conflicts.
# Processed for all AOIs in consensus_risk.R
load(system.file("extdata/", "consensus_risk.rda", package = "activemapper"))
load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

mgrid <- data.table::fread(
  system.file("extdata/ghana_grid.csv", package = "activemapper")
)

# select AOIs and unique names:
aoinms <- c(
  "labeller",
  paste0("labeller", c("trainsouth", "trainsouthnl", "trainsw", 1:16))
)

# get usages
usages <- instance_tbls$incoming_names %>%
  filter(!(aoi == "labeller3" & run == 0)) %>%
  filter(label == TRUE) %>%
  filter(aoi %in% aoinms) %>%
  dplyr::select(aoi, name, usage) %>%
  left_join(., mgrid %>% dplyr::select(name, x, y), by = "name")

# checks on duplicates
# dupnms <- usages %>% group_by(name) %>% count() %>% filter(n > 1) %>% pull(name)
# length(dupnms)  # 503 duplicates
# dupnms2 <- usages %>% filter(name %in% dupnms) %>% arrange(name) %>%
#   filter(!aoi %in% c("labellertrainsouth", "labellertrainsouthnl"))
# nrow(dupnms2) # 7 of those in other AOIs besides 2 runs of labellertrainsouth
# usages %>% filter(name == "GH0328463")
# this site was validate in labeller7 and in both labellertrainsouth versions

label_risk_pts <- left_join(consensus_risk, usages, by = c("aoi", "name")) %>%
  dplyr::select(aoi, name, usage, !!names(.))
# check on duplicates
# nm <- label_risk %>% group_by(name) %>% count() %>% filter(n > 1) %>% pull(name)
# label_risk %>% filter(name %in% nm) %>% arrange(name) %>% View()
# label_risk %>%
#   filter(name %in% nm) %>%
#   filter(!aoi %in% c("labellertrainsouth", "labellertrainsouthnl")) %>%
#   dplyr::select(aoi, name, usage) #%>% distinct(name)
# label_risk %>% filter(name == "GH0328463")
# same as from usage

# Rename some columns and values
clusters <- paste0("labeller", c("", "trainsouth", "trainsouthnl", "trainsw"))
new_cluster_nms <- paste0("Cl", c("1", "2a", "2b", "3"))

label_risk_pts <- label_risk_pts %>%
  rename(Risk = meanrisk, Riskprop = prop_risky) %>%
  mutate(usage = stringr::str_to_title(usage)) %>%
  mutate(aoi = case_when(
    aoi == clusters[1] ~ "-3",
    aoi == clusters[2] ~ "-2",
    aoi == clusters[3] ~ "-1",
    aoi == clusters[4] ~ "0",
    TRUE ~ gsub("labeller", "", aoi)
  )) %>%
  mutate(ord = as.numeric(aoi)) %>% arrange(ord) %>%
  mutate(aoi = case_when(
    aoi == "-3" ~ new_cluster_nms[1],
    aoi == "-2" ~ new_cluster_nms[2],
    aoi == "-1" ~ new_cluster_nms[3],
    aoi %in% "0" ~ new_cluster_nms[4],
    TRUE ~ aoi
  )) %>%
  mutate(aoi = forcats::fct_reorder(aoi, ord)) %>%
  select(-ord)

# summarize
label_risk_stats <- label_risk_pts %>% group_by(aoi, usage) %>%
  summarize(Risk = mean(Risk, na.rm = TRUE)) %>%
  ungroup()

label_risk <- list(pts = label_risk_pts, stats = label_risk_stats)

usethis::use_data(label_risk, overwrite = TRUE)

#------------------------------------------------------------------------------#
# Iteration metrics
# Analyze and reshape for figure making

load(system.file("extdata/", "instance_tbls.rda", package = "activemapper"))

# function to calculate change over time
delta_func <- function(x, rnd = 4) round((x - lag(x)) / lag(x), rnd)

# process individual AOIs
metrics <- c("Accuracy", "AUC", "F1", "TSS", "Precision", "Recall", "TPR",
             "FPR")
aoinms <- paste0("labeller", c(1:16, "1r", "8r", "15r", "8v2", "82019"))
iteration_metrics <- instance_tbls$iteration_metrics %>%
  filter(aoi %in% aoinms) %>%
  filter(iteration > 0) %>%
  filter(!(aoi == "labeller3" & run == 0)) %>% # remove run 0 labeller3
  filter(!(aoi %in% paste0("labeller", c("1r", "8r")) & run == 1)) %>% # no run1
  mutate(iteration = iteration - 1) %>% # rebase iteration counter on 0
  mutate(F1 = 2 * (precision * recall) / (precision + recall)) %>%
  rename(AOI = aoi, Iteration = iteration, Accuracy = accuracy, AUC = auc,
         TSS = tss, Precision = precision, Recall = recall, TPR = tpr,
         FPR = fpr) %>%
  select(AOI, Iteration, !!metrics) %>%
  group_by(AOI) %>%  # calculate deltas
  mutate_at(metrics, list("change" = delta_func)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = Accuracy:FPR_change, names_to = "Metric",
                      values_to = "Score")

# process across AOI means and combine with AOIs
aois <- paste0("labeller", 1:16)  # calculate summary across just AOIs 1-16
iteration_metrics %>%
  filter(AOI %in% aois) %>%
  filter(Iteration < 4) %>%
  group_by(Iteration, Metric) %>%
  summarize(Score = mean(Score)) %>%
  mutate(AOI = "All") %>%
  ungroup %>%
  select(AOI, !!names(.)) %>%
  bind_rows(iteration_metrics, .) -> iteration_metrics

usethis::use_data(iteration_metrics, overwrite = TRUE)

#------------------------------------------------------------------------------#
# Metrics for highest, lowest, most accurate worker
library(aws.s3)
library(glue)

fnames <- unlist(lapply(paste0(c('low', 'high'), ".csv"), function(x) {
  paste0(glue("planet/incoming_metrics_{c(1, 2, 8, 15)}_"), x)
}))
high_low_metrics <- lapply(fnames, function(x) {
  dat <- s3read_using(FUN = readr::read_csv, object = x,
                      bucket = "activemapper")
  highlow <- gsub("planet/incoming_metrics_[0-9]+_|.csv", "", x)
  dat %>% mutate(aoi = paste0(aoi, "_", highlow)) %>% rename(AOI = aoi) %>%
    select(AOI, !!names(.))
}) %>% do.call(rbind, .)

# Process and reshape
metrics <- c("Accuracy", "AUC", "F1", "TSS", "Precision", "Recall", "TPR",
             "FPR")
high_low_metrics <- high_low_metrics %>%
  mutate(iteration = iteration - 1) %>% # rebase iteration counter on 0
  mutate(F1 = 2 * (precision * recall) / (precision + recall)) %>%
  rename(Iteration = iteration, Accuracy = accuracy, TSS = tss,
         Precision = precision, Recall = recall, TPR = tpr,
         FPR = fpr) %>%
  select(AOI, Iteration, !!metrics) %>%
  tidyr::pivot_longer(cols = Accuracy:FPR, names_to = "Metric",
                      values_to = "Score")

# save out for keeping
usethis::use_data(high_low_metrics, overwrite = TRUE)

# bring in iteration metrics and join with consensus values from those
consensus <- iteration_metrics %>%
  filter(AOI %in% paste0("labeller", c(1, 2, 8, 15))) %>%
  group_by(AOI) %>%
  filter(Iteration == max(Iteration) & !grepl("change", Metric)) %>%
  ungroup() %>%
  mutate(Strategy = "Consensus") %>%
  mutate(AOI = gsub("labeller", "", AOI))

# join
consensus_high_low <- high_low_metrics %>%
  mutate(Strategy = tools::toTitleCase(gsub("[0-9]+_", "", AOI)),
         AOI = gsub("_.*", "", AOI)) %>%
  bind_rows(consensus, .) %>%
  mutate(AOI = as.numeric(AOI)) %>%
  arrange(AOI, Metric) %>%
  mutate(ord = group_indices(., AOI)) %>%  # define order (for plotting)
  mutate(AOI = as.character(AOI))

usethis::use_data(consensus_high_low, overwrite = TRUE)

#------------------------------------------------------------------------------#




