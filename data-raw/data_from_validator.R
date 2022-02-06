# Data from validator

library(dplyr)
library(sf)
library(here)
library(data.table)

nonspatial_tables <- c("accuracy_data", "assignment_data", "assignment_history",
                       "categories", "configuration", "hit_data",
                       "incoming_names", "iteration_metrics",
                       "qual_accuracy_data",
                       "qual_assignment_data", "worker_data", "users")

# credentials
params <- yaml::yaml.load_file(here::here("common/config.yaml"))
dinfo <- params$labeller

con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                      host = "validator.crowdmapper.org",
                      dbname = "Africa", user = dinfo$db_username,
                      password = dinfo$db_password)

print(glue::glue("Fetching from validator..."))
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

mapref_tbls <- tbls
save(mapref_tbls, file = here("external/reference/mapref_tbls.rda"))

mapref_tbls$users <- mapref_tbls$users %>% filter(id %in% c(90, 134)) %>%
  select(id, first_name, last_name)

save(mapref_tbls, file = here("inst/extdata/mapref_tbls.rda"))
# load(here("inst/extdata/mapref_tbls.rda"))
#
# # process results
# mapref_tbls$assignment_data %>% group_by(hit_id) %>% count %>% filter(n > 1)
# mapref_tbls$assignment_data %>% group_by(worker_id) %>% count
# mapref_tbls$assignment_data %>% distinct(hit_id) %>% count
#
# mapref_tbls$user_maps %>% group_by(assignment_id) %>% count() %>%
#   filter(n > 1)
#
# mapref <- left_join(as_tibble(mapref_tbls$user_maps),
#                     mapref_tbls$assignment_data) %>%
#   select(name, assignment_id, hit_id, worker_id, category, geom_clean) %>%
#   sf::st_as_sf(crs = 4326)
#
# pres <- mapref_tbls$configuration %>% filter(key == "KMLdlat") %>% pull(value)
# gcs <- mapref %>% st_crs %>% as.character(.) %>% .[2]
# ref_data <- mapref %>% st_centroid() %>%
#   mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>%
#   as_tibble() %>%
#   select(name, assignment_id, hit_id, worker_id, category, x, y) %>%
#   data.table()
# ref_polys <- ref_data %>%
#   rmapaccuracy::point_to_gridpoly(w = as.numeric(pres), gcs, gcs)
#
# # check against
