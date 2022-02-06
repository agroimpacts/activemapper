##  Preparing data outputs from instances

# Lightweight data go into data/
# Heavier data go into inst/extdata
# Intermediate (untracked) inputs go into external/..

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

# load("external/data/aois/dbases/instance_dbases.rda")

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



