# Map accuracy reports (as tables)

library(activemapper)
library(dplyr)
library(glue)

data("mapped_areas")
data("error_matrices")

# Accuracies for AOI and AEZ groups
# Variables
grps <- list("aoi" = list(1:3, 4:9, c(10:11, 13, 14, 16), c(12, 15)),
             "aez" = list(1, 2:4, 5, 6:7))
listkeys <- c("aoi", "aez")
grpnames <- list("aoi" = c("North", "Central", "SW", "SE"),
                 "aez" = c("Coast_Sav", "Forest", "Transition", "Guinea_Sav"))
classes <- c("Non-crop", "Crop")

# Process
zone_reports <- lapply(1:2, function(x) {  # x <- 2

  listkey <- listkeys[[x]]
  grp <- grps[[x]]
  grpname <- grpnames[[x]]

  reports <- lapply(grp, function(y) {  # y <- grp[[2]]

    # sum samples and areas into zones
    # rf
    rfl <- lapply(error_matrices[[listkey]][y], function(z) {
      m <- z$rf[1:2, 1:2]
      if(!"1" %in% rownames(m)) m[2, ] <- c(0, 0)
      m <- unname(m)
      m
    })
    rfmat <- Reduce(`+`, rfl)
    rfarea <- mapped_areas[[glue("rf_areas_{listkey}")]] %>%
      filter(get(listkey) %in% y) %>% select(nocrop_area, crop_area) %>%
      summarize_all(sum) %>% unlist()

    # accuracy and report
    rf_acc <- accuracy(rfmat[1:2, 1:2], areas = rfarea)
    rf_report_aoi <- accuracy_report(rfmat[1:2, 1:2], rf_acc, classes)


    # segs
    if(listkey == "aoi") {
      segl <- lapply(error_matrices[[listkey]][y], function(z) {
        m <- z$seg[1:2, 1:2]
        if(!"1" %in% rownames(m)) m[2, ] <- c(0, 0)
        m <- unname(m)
        m
      })
      segmat <- Reduce(`+`, segl)
      segarea <- mapped_areas[[glue("seg_areas_{listkey}")]] %>%
        filter(get(listkey) %in% y) %>% select(nocrop_area, crop_area) %>%
        summarize_all(sum) %>% unlist()

      # accuracy and report
      seg_acc <- accuracy(segmat[1:2, 1:2], areas = segarea)
      seg_report_aoi <- accuracy_report(segmat[1:2, 1:2], seg_acc, classes)

      reports <- list("rf" = rf_report_aoi, "seg" = seg_report_aoi)
    } else {
      reports <- list("rf" = rf_report_aoi)
    }
    return(reports)
  })
  names(reports) <- grpname
  return(reports)
})
names(zone_reports) <- names(grps)

# Ghana wide
ghana_mat <- error_matrices[["ghana"]]
rfarea <- mapped_areas[["rf_areas_aoi"]] %>%
  select(nocrop_area, crop_area) %>% summarize_all(sum) %>% unlist()
segarea <- mapped_areas[["seg_areas_aoi"]] %>%
  select(nocrop_area, crop_area) %>% summarize_all(sum) %>% unlist()
# accuracy
rf_acc <- accuracy(ghana_mat$rf[1:2, 1:2], areas = rfarea)
seg_acc <- accuracy(ghana_mat$seg[1:2, 1:2], areas = segarea)

# report
rf_report_aoi <- accuracy_report(ghana_mat$rf[1:2, 1:2], rf_acc, classes)
seg_report_aoi <- accuracy_report(ghana_mat$seg[1:2, 1:2], seg_acc, classes)

ghana_report <- list("rf" = rf_report_aoi, "seg" = seg_report_aoi)

# save output
accuracy_reports <- list("zones" = zone_reports, "ghana" = ghana_report)
usethis::use_data(accuracy_reports, overwrite = TRUE)


