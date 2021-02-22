# Kriging to get average field size and predicted probabilities.
# For calculating adjusted field statistics with calculating error metrics.

library(activemapper)
library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(gstat)
library(sp)

# load in data set
load(system.file("extdata/top_label_data.rda", package = "activemapper"))
load(system.file("extdata/field_validation_stats.rda",
                 package = "activemapper"))
load(system.file("extdata/segment_validation_data.rda",
                 package = "activemapper"))
tiles <- st_read(
  system.file("extdata/ghana_tiles.geojson", package = "activemapper")
)
fsizes <- raster(
  system.file("extdata/mean_field_size005.tif", package = "activemapper")
)

# albers ESRI:102022
aea <- glue::glue("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0",
                  " +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# label data from validation sites and segments
vlabs <- top_label_data$stats %>% select(aoi, name, x, y, Mean, n) %>%
  rename(vmu = Mean, vn = n)
segs <- field_validation_stats %>% select(aoi, name, Mean, n) %>%
  rename(smu = Mean, sn = n)

# hist(vlabs$vmu)
# hist(segs$smu)
val_segs <- left_join(vlabs, segs, by = c("name", "aoi")) %>%
  arrange(aoi, name) %>% mutate(dist = NA)

# val_segs %>% filter(vmu > 0 & smu > 0) %>%
#   st_as_sf(coords = c("x", "y")) %>% st_geometry() %>%
#   plot(pch = 16, cex = 0.5)

val_segs_replaced <- lapply(c("vmu", "smu"), function(x) {  # x <- "vmu"
  mu0 <- val_segs %>% filter((!!sym(x)) == 0) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  munot0 <- val_segs %>% filter((!!sym(x)) > 0) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)

  mu_replaced <- lapply(1:nrow(mu0), function(y) {  # y <- 1
    to_replace <- mu0[y, ]
    dists <- st_distance(munot0, to_replace)
    dists <- tibble(rown = 1:nrow(dists), d = as.numeric(dists)) %>%
      arrange(d) %>% slice(1:3)
    freplace <- munot0 %>% slice(dists$rown)
    replaced <- to_replace %>%
      mutate_at(vars(x), list(~mean(freplace[[x]]))) %>%
      mutate(dist = mean(dists$d))
    return(replaced)
  }) %>% do.call(rbind, .)
  mu_0filled <- rbind(mu_replaced, munot0)
  return(mu_0filled)
})

# combine
val_segs_replacedf <- full_join(
  as_tibble(val_segs_replaced[[1]]) %>% select(aoi, name, vmu, geometry) %>%
    arrange(aoi, name),
  as_tibble(val_segs_replaced[[2]]) %>% select(aoi, name, smu) %>%
      arrange(aoi, name),
  by = c("aoi", "name")
) %>% select(aoi, name, vmu, smu, geometry)

# as_tibble(mu_replaced) == as_tibble(vmu_0filled)
# plot(log(val_segs_replacedf$vmu), log(val_segs_replacedf$smu))

# regression and get residuals
szlm <- lm(log(vmu) ~ log(smu), data = val_segs_replacedf)
# plot(log(vmu) ~ log(smu), data = val_segs_replacedf)
# abline(szlm)
# summary(szlm)
val_segs_resid <- cbind(val_segs_replacedf, resid = residuals(szlm))

# Transform to projected system and fit variogram
val_segs_resid <- st_as_sf(val_segs_resid)
val_segs_residr <- st_transform(val_segs_resid, crs = aea)
v <- variogram(object = resid ~ 1, data = as_Spatial(val_segs_residr))  # c

# test models
# mods <- as.character(vgm()$short)
# png(here::here("external/test/vgm_test.png"), width = 7, height = 7, res = 300,
#     units = "in")
# par(mfrow = c(4, 5))
# for(i in mods) {  # i <- mods[5]
#   # m <- fit.variogram(object = v, model = vgm("Sph"))  # d
#   # m <- fit.variogram(object = v, model = vgm("Gau"))  # d
#   # m <- fit.variogram(object = v, model = vgm("Mat"))  # d
#   m <- fit.variogram(object = v, model = vgm(i))  # d
#   plot(variogramLine(m, max(v[, 2])), type = "l")  # e
#   points(v[, 2:3], pch = 20)  # f
#   legend("bottomright", legend = c("variogram fit", "variogram"),
#          lty = c(1, NA), pch = c(NA, 20), bty = "n") # g
#   mtext(text = i, side = 3, line = 0)
# }
# dev.off()

# exp looks closest
m <- fit.variogram(object = v, model = vgm("Exp"))  # d

# model
ordkrig <- gstat(id = "resid", formula = resid ~ 1,
                 data = as_Spatial(val_segs_residr)[, "resid"], model = m)

# interpolate
r <- !is.na(fsizes)
ralb <- projectRaster(r, crs = st_crs(aea)$wkt, res = 550)
names(ralb) <- "resid"
ordkrigr <- interpolate(object = ralb, model = ordkrig, progress = "text")
ordkrigrgcs <- mask(projectRaster(ordkrigr, fsizes), fsizes)
# plot(ordkrigrgcs)
# ordkrigrmsk <- mask(x = ordkrigr, mask = ralb)

# finish regression kriging
fsizes_adjusted <- exp(predsizes + ordkrigrgcs)  # add residuals
fsizes_adjusted[is.na(fsizes_adjusted)] <- 0
fsizes_adjusted <- mask(fsizes_adjusted, fsizes)
fsizes_adj <- stack(fsizes_adjusted, ordkrigrgcs)
writeRaster(fsizes_adj,
            filename = here::here("inst/extdata/mean_field_size005_adj.tif"))

# plot(fsizess[[2]], fsizess[[1]])
# avals <- extract(fsizess[[2]], val_segs_resid)
# plot(fsizess[[2]])
# plot(val_segs_resid %>% mutate(vmulog = log(vmu)) %>% select(vmulog),
#      pch = 16, cex = 0.5)
# plot(val_segs_resid["vmu"], pch = 16, cex = 0.5)
# plot(val_segs_replacedf$vmu, avals)
# plot(fsizess)

#------------------------------------------------------------------------------#
# other approaches tested and found wanting
# ordinary kriging of validation field sizes
# val_segs_replacedf <- val_segs_replacedf %>% mutate(vmuln = log10(vmu))
# val_segs_replacedfr <- st_as_sf(val_segs_replacedf) %>%
#   st_transform(crs = st_crs("ESRI:102022")$wkt)
#
# # test models
# v2 <- variogram(object = vmu ~ 1, data = as_Spatial(val_segs_replacedfr))  # c
# # mods <- as.character(vgm()$short)
# # png(here::here("external/test/vgm_test2.png"), width = 7, height = 7, res = 300,
# #     units = "in")
# # par(mfrow = c(4, 5))
# # for(i in mods) {  # i <- mods[5]
# #   # m <- fit.variogram(object = v, model = vgm(i))  # d
# #   m2 <- fit.variogram(object = v2, model = vgm(i))  # d
# #   plot(variogramLine(m2, max(v2[, 2])), type = "l")  # e
# #   points(v2[, 2:3], pch = 20)  # f
# #   legend("bottomright", legend = c("variogram fit", "variogram"),
# #          lty = c(1, NA), pch = c(NA, 20), bty = "n") # g
# #   mtext(text = i, side = 3, line = 0)
# # }
# # dev.off()
#
# m2 <- fit.variogram(object = v2, model = vgm("Exc"))  # d
# plot(variogramLine(m2, max(v2[, 2])), type = "l")  # e
# points(v2[, 2:3], pch = 20)  # f
# legend("bottomright", legend = c("variogram fit", "variogram"),
#        lty = c(1, NA), pch = c(NA, 20), bty = "n") # g
# mtext(text = i, side = 3, line = 0)
#
# ordkrig2 <- gstat(id = "vmu", formula = vmu ~ 1,
#                  data = as_Spatial(val_segs_replacedfr)[, "vmu"], model = m2)
#
# # interpolate
# r <- !is.na(fsizes)
# ralb <- projectRaster(r, crs = st_crs("ESRI:102022")$wkt, res = 550)
# ralb <- terra::rast(ralb)
# names(ralb) <- "vmu"
# # ordkrigr2 <- terra::interpolate(object = ralb, model = ordkrig2,
# #                                 xyNames=c("x", "y"))
# # ordkrigr2 <- terra::predict(ordkrig2, as(ralb, "SpatialGrid"))
# # plot(brick(ordkrigr2))  # not good
# ordkrigr2gcs <- mask(projectRaster(brick(ordkrigr2), fsizes), fsizes)
#
# ## [inverse distance weighted interpolation] - a little chunky
# m3 <- gstat(formula = vmu ~ 1, locations = as_Spatial(val_segs_replacedfr),
#             nmax = 7, set = list(idp = 0))
# nn <- interpolate(ralb, m3)
# # plot(nn)
#
# # plot(ordkrigr)
# m4 <- fields::Tps(coordinates(as_Spatial(val_segs_replacedfr)),
#                   val_segs_replacedfr$vmu)
# tps <- terra::interpolate(ralb, m4)
# tps <- mask(tps, idw)
# tpsgcs <- mask(projectRaster(tps, fsizes), fsizes)
# plot(tpsgcs)
# # plot(fsizess[[1]] + (ordkrigr2gcs - fsizess[[1]]))
#
# plot(tpsgcs)
# plot(ordkrigrgcs)
# plot(fsizess[[2]])
# plot(fsizess[[1]])
#
# ordkrigrgcs <- mask(projectRaster(ordkrigr, fsizes), fsizes)
#
