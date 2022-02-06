# Calculate Bayesian Risk for each labels
# Reprocessing to include measure on mean risk as well as proportion risky

library(rmapaccuracy)
library(dplyr)
library(data.table)
library(sf)
library(activemapper)
library(doMC)

load(here::here("external/data/aois/dbases/instance_dbases.rda"))
ref_grid <- fread(
  system.file("extdata", "ghana_grid.csv", package = "activemapper")
)

# select AOIs and unique names:
aoinms <- c(
  "labeller",
  paste0("labeller", c("trainsouth", "trainsouthnl", "trainsw", 1:16))
)
# lapply(aoinms, function(x) nrow(instance_dbases[[x]]$accuracy_data))

logf <- here::here("external/logs/consensus_risk.log")
if(file.exists(logf)) {
  file.copy(logf, paste0(logf, "bak"))
  file.remove(logf)
}
starttime <- Sys.time()
labeldir <- here::here("external/data/aois/labels/")

registerDoMC(cores = 6)
risk_statsl <- lapply(aoinms, function(x) {  # x <- "labellertrainsw"
  # x <- "labeller1"

  cat(glue::glue("Starting {x} at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

  db <- instance_dbases[[x]]  # select dbase

  # ggplot2::ggplot(db$assignment_data) +
  #   ggplot2::geom_histogram(ggplot2::aes(start_time))
  #
  # db$incoming_names %>% filter(run == 1)
  # db$assignment_data %>%

  # get tables
  kml_data <- db$kml_data
  assignment_data <- db$assignment_data
  accuracy_data <- db$accuracy_data
  qual_assignment_data <- db$qual_assignment_data
  qual_accuracy_data <- db$qual_accuracy_data
  user_maps <- db$user_maps
  hit_data <- db$hit_data
  categories <- db$categories

  # filter for labeller3
  if(x == "labeller3") {
    # run0nms <- db$incoming_names %>% filter(run == 0 & label == TRUE)
    # run1nms <- db$incoming_names %>% filter(run == 1 & label == TRUE)
    # run1nms$name[run0nms$name %in% run1nms$name]
    # hit_data %>% filter(name %in% run1nms$name)
    # db$iteration_metrics
    # hit_data %>%
    #   filter(name %in% run1nms$name[run0nms$name %in% run1nms$name]) %>%
    #   View()
    assignment_data <- assignment_data %>% filter(start_time > "2019-11-10")
    hit_data <- hit_data %>%
      filter(hit_id %in% !!unique(assignment_data$hit_id))
    kml_data <- kml_data %>% filter(name %in% !!hit_data$name)
    user_maps <- user_maps %>%
      filter(assignment_id %in% !!unique(assignment_data$assignment_id))

    cat(glue::glue("...filtered out run 0 results for  {x}"), file = logf,
        sep = "\n", append = TRUE)
  }

  # arguments
  category <- "field"
  unsure_category <- "unsure1"
  # params <- yaml::yaml.load_file(here::here("common/config.yaml"))
  risk_threshold <- db$configuration %>%
    filter(key == "Consensus_RiskyPixelThreshold") %>%
    pull(value) %>% as.numeric()
  mode <- "consensus"
  diam <- 0.005 / 2
  qsite <- FALSE

  # kml_names
  kmlnames <- kml_data %>% filter(kml_type == "F" & mapped_count >= 4) %>%
    distinct(name) %>% pull()

  cat(glue::glue("..with {length(kmlnames)} F sites"), file = logf,
      sep = "\n", append = TRUE)

  # consensusnl <- foreach(y = 1:length(kmlnames)) %dopar% {  # y <- 1
  name_counter <- seq(1, length(kmlnames), 20)  # write out every 20th label
  pkgs <- c("dplyr", "activemapper", "tibble", "raster", "magrittr")

  # parallelize on kmlnames
  risk_stats_out <- foreach(y = 1:length(kmlnames), .packages = pkgs,
                         .combine = rbind) %dopar% {  # y <- 1
  # risk_stats_out <- foreach(y = 1:14, .packages = pkgs,
  #                           .combine = rbind) %dopar% {  # y <- 14
  # risk_stats_out <- lapply(1:7, function(y) {  # y <- 1

  # consensusnl <- foreach(kmlnames) %dopar% {  # y <- kmlnames[1]
    kmlid <- kmlnames[y]
    # kmlid <- "GH0042987"
    # print(glue::glue("Processing {x}: {kmlid}"))

    cat(glue::glue("....processing {x}: {kmlid}"), file = logf,
        sep = "\n", append = TRUE)

    consensus <- consensus_map_creation2(
      kml_data = kml_data, hit_data = hit_data,
      assignment_data = assignment_data, accuracy_data = accuracy_data,
      qual_assignment_data = qual_assignment_data,
      qual_accuracy_data = qual_accuracy_data, user_maps = user_maps,
      ref_grid = ref_grid, kmlid = kmlid, mode = mode, diam = diam,
      risk_threshold = risk_threshold, categories = categories)

    aoiid <- gsub("labeller", "aoi", x)
    if(y %in% name_counter) {
      for(i in names(consensus$maps)) {
        suffix <- gsub("map", "", i)
        f <- paste0(labeldir,
                    glue::glue("{suffix}/{aoiid}_{kmlid}_{suffix}.tif"))
        raster::writeRaster(consensus$maps[[i]], filename = f, overwrite = TRUE)
      }
      cat(glue::glue("......saved maps at iteration {y} for {kmlid}"),
          file = logf, sep = "\n", append = TRUE)
    }

    # Risk stats
    risk_stats <- tibble::tibble(
      aoi = x, name = consensus$kmlid,
      meanrisk = consensus$stats["meanrisk"],
      prop_risky = consensus$stats["prop_risky"]
    )
    return(risk_stats)
  }
  # })
  cat(glue::glue("Finished processing {x} at at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)

  # clean up and complete
  # rm(list(label_maps, risk_maps, heat_maps, consensusnl))
  # cat(glue::glue("..removed raster data and list for {x}"), file = logf,
  #     sep = "\n", append = TRUE)
  cat(glue::glue("{x} completed at {Sys.time()}"), file = logf,
      sep = "\n", append = TRUE)
  cat("\n", file = logf, append = TRUE)

  f <- paste0(labeldir, glue::glue("{x}_risk_stats.rda"))
  save(risk_stats_out, file = f)

  return(risk_stats_out)
})
consensus_risk <- do.call(rbind, risk_statsl)

save(consensus_risk, file = here::here("inst/extdata/consensus_risk.rda"))

# run time
endtime <- Sys.time()
cat(glue::glue("{endtime - starttime}"), file = logf,
    sep = "\n", append = TRUE)



# raster::raster("external/data/aois/labels/heat/aoi3_BF0780993_heat.tif") %>%
#   raster::plot()
# raster::raster("external/data/aois/labels/label/aoi3_GH0171949_label.tif") %>%
#   raster::plot()
# raster::raster("external/data/aois/labels/risk/aoi3_GH0171949_risk.tif") %>%
#   raster::plot()


# instance_dbases$labeller$hit_data %>%
#   filter(name == !!consensusnl[[500]]$kmlid) %>%
#   left_join(., instance_dbases$labeller$kml_data) %>% View()
#   select(mappers_needed)
# instance_dbases$labeller$hit_data %>%
#   filter(name == !!consensusnl[[500]]$kmlid) %>%
#   left_join(., instance_dbases$labeller$assignment_data) %>%
#   inner_join(instance_dbases$labeller$user_maps, .) %>%
#   ggplot() + geom_sf(aes(color = as.factor(worker_id)), fill = "transparent")

# # check
# j <- sample(x = 1:length(consensusnl), size = 1)
# par(mfrow = c(1, 3))
# for(i in 1:3) plot(consensusnl[[j]]$maps[[i]])
#
# instance_dbases$labeller$hit_data %>%
#   filter(name == !!consensusnl[[j]]$kmlid) %>%
#   left_join(., instance_dbases$labeller$assignment_data) %>%
#   inner_join(instance_dbases$labeller$user_maps, .) %>%
#   ggplot() + geom_sf(aes(color = as.factor(worker_id)), fill = "transparent") +
#   ggtitle(consensusnl[[j]]$kmlid)


