#' Get pointers to key database tables from labeller
#' @param connection postgresql connection to instance database
#' @param dbtables Vector of table names to get. Currently "default" brings up
#' names of tables needed for consensus labelling
#' @return Named list of lazy connections to labeller database tables
#' @export
get_db_tables <- function(connection, dbtables = "default") {
  if(dbtables == "default") {
    tblist <- c("kml_data", "hit_data", "assignment_data", "accuracy_data",
                "qual_assignment_data", "qual_accuracy_data",
                "master_grid", "categories", "configuration")
  } else {
    tblist <- dbtables
  }
  dbtbls <- lapply(tblist, function(x) {
    tab <- dplyr::tbl(connection, x)
  })
  names(dbtbls) <- tblist
  return(dbtbls)
}

#' Collection assignment history for a given worker
#' @param assignment_data Assignment data table
#' @param accuracy_data Accuracy data table
#' @param workerid Integer ID of worker
#' @return Tibble of measurements
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull select rename
#' @keywords internal
assignment_history <- function(assignment_data, accuracy_data, workerid) {
  historyassignmentid <- assignment_data %>%
    filter(worker_id == workerid) %>%
    filter(status == "Approved" | status == "Rejected") %>%
    filter(!(is.null(score) | is.na(score))) %>% pull(assignment_id)

  if(length(historyassignmentid) == 0) {
    measurements <- data.frame(ml.field = NA, ml.nofield = NA,
                               score.hist = NA)
  } else {
    measurements <- accuracy_data %>%
      filter(assignment_id %in% historyassignmentid) %>%
      dplyr::select(new_score, field_skill, nofield_skill) %>%
      rename(ml.field = field_skill, ml.nofield = nofield_skill,
             score.hist = new_score)
  }
  return(measurements)
}

#' Collection qualification assignment history for a given worker
#' @param qual_assignment_data Assignment data table
#' @param qual_accuracy_data Qualification accuracy table
#' @param workerid Integer ID of worker
#' @return Tibble of measurements
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull select rename group_by ungroup
#' @keywords internal
qual_assignment_history <- function(qual_assignment_data, qual_accuracy_data,
                                    workerid) {
  if(nrow(qual_assignment_data) == 0) {
    measurements <- data.frame(ml.field = NA, ml.nofield = NA,
                               score.hist = NA)
    return(measurements)
  }
  qual_historyassignmentid <- qual_assignment_data %>%
    filter(worker_id == workerid) %>%
    filter(status == "Approved" | status == "Rejected") %>%
    filter(!(is.null(score) | is.na(score))) %>% pull(assignment_id)

  if (length(qual_historyassignmentid) == 0) {
    measurements <- data.frame(ml.field = NA, ml.nofield = NA,
                               score.hist = NA)
  } else {
    measurements <- qual_accuracy_data %>%
      filter(assignment_id %in% qual_historyassignmentid) %>%
      select(assignment_id, new_score, field_skill, nofield_skill, try) %>%
      group_by(assignment_id) %>%
      filter(try == max(try, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(ml.field = field_skill, ml.nofield = nofield_skill,
             score.hist = new_score) %>%
      dplyr::select(ml.field, ml.nofield, score.hist)
  }
  return(measurements)
}

#' Get user_maps for generating consensus labels
#' @param assignmentid ID of assignment for which to get maps
#' @param categories Table holding class categories
#' @param category Single class category of interest
#' @param grid_poly Polygon of the reference grid corresponding to assignment
#' @param user_maps Either an sf object with fields or a postresql connection
#' @param qsite TRUE or FALSE, passed through from consensus_map_creation
#' @return An sf geometry object, empty or containing buffered/unioned
#' boundaries clipped to the grid
#' @importFrom magrittr %>%
#' @importFrom dplyr filter inner_join pull select rename group_by ungroup
#' @importFrom sf st_read st_intersection st_buffer st_union st_polygon
#' @keywords internal
get_user_maps <- function(assignmentid, categories, category, grid_poly,
                          user_maps, qsite) {
  if(class(user_maps)[1] == "PostgreSQLConnection") {
    con <- user_maps
    sql_str <- paste0(
      "select name, geom_clean FROM user_maps INNER JOIN categories ",
      "USING (category) where assignment_id='", assignmentid, "' AND ",
      ifelse(category == "field", "categ_group='field'",
             paste0("category='", category, "'"))
    )
    polys <- suppressWarnings(
      DBI::dbGetQuery(con, gsub(", geom_clean", "", sql_str))
    )
    # Read in if contains polygons
  } else if(is(user_maps, "sf")) {
    categ <- category
    polys <- user_maps %>%
      filter(assignment_id %in% assignmentid) %>%
      inner_join(., categories, by = "category") %>%
      {if(categ == "field") filter(., categ_group == "field") else .} %>%
      {if(categ != "field") filter(., category == categ) else .}
  } else {
    stop("user_maps is neither an sf object or database connection")
  }

  # check if polys has fields
  user_hasfields <- ifelse(nrow(polys) > 0, "Y", "N")

  # if user maps have field polygons
  if(user_hasfields == "Y") {
    if(class(user_maps)[1] == "PostgreSQLConnection") {  # if dbcon, read in
      polys <- suppressWarnings(st_read(con, query = sql_str))
    }

    # select only polygons
    polys <- polys %>% filter(st_is(. , "POLYGON"))

    # union user polygons
    poly <- suppressWarnings(suppressMessages(
      st_buffer(st_buffer(polys, 0.0001), -0.0001))
    )
    poly <- suppressWarnings(suppressMessages(st_buffer(
      st_buffer(st_union(poly), 0.0001), -0.0001))
    )

    # if for F sites, we need to first intersection user maps by grid
    # to retain those within-grid parts for calculation
    if(qsite == FALSE) {
      poly <- suppressWarnings(
        suppressMessages(st_intersection(poly, grid_poly))
      )
      poly <- suppressWarnings(
        suppressMessages(st_buffer(poly, 0))
      )
    }
    if(length(poly) == 0){
      geometry_user <- st_polygon()
    } else {
      geometry_user <- poly
    }
  }
  else {
    # if users do not map field, set geometry as empty polygon
    geometry_user <- st_polygon()
  }
  return(geometry_user)
}

#' Create consensus labels and calculated Bayesian measures of uncertainty
#' @param kml_data kml_data (see notes for type)
#' @param hit_data hit_data (see notes for type)
#' @param assignment_data assignment_data (see notes for type)
#' @param accuracy_data accuracy_data(see notes for type)
#' @param qual_assignment_data qual_assignment_data (see notes for type)
#' @param user_maps Either an sf object holding maps or postgresql connection
#' @param ref_grid Primary reference grid
#' @param kmlid Name of label site (must match name in ref_grid)
#' @param mode Label-making method: 'consensus', 'high' or 'low'. The first
#' creates a merged label, 'high' or 'low' uses the highest or the lowest
#' scoreing workers maps.
#' @param diam Half-width of cell in reference grid
#' @param risk_threshold Value from 0-1 indicating threhold for flagging pixels
#' with high label uncertainty
#' @param categories Table holding class categories
#' @param category Single class category to process. Default if "field"
#' @param unsure_category Name of class category indicating uncertainty
#' @param qsite Q ir F site?  Default is FALSE
#' @param crs Coordinate reference system, current 4326, otherwise proj4string
#' @return List containing rasters (label, heat map, risk map) and Bayesian
#' measures of uncertainty.
#' @details This
#' @import dplyr
#' @importFrom sf st_sf st_sfc st_is_empty st_union st_bbox
#' @importFrom data.table as.data.table
#' @importFrom raster ncell cellStats nrow ncol
#' @export
consensus_map_creation2 <- function(kml_data, hit_data, assignment_data,
                                    accuracy_data, qual_assignment_data,
                                    qual_accuracy_data, user_maps, ref_grid,
                                    kmlid, mode, diam, risk_threshold,
                                    categories, category = 'field',
                                    unsure_category = 'unsure1',
                                    qsite = FALSE, crs = "4326") {

  if(crs == 4326) {
    crsstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  } else {
    crsstr <- crs
  }

  # query hitid
  # query mappedcount
  mappedcount <- kml_data %>% filter(name == kmlid) %>% pull(mapped_count)

  # query assignmentid flagged as 'approved'
  assignmentid <- inner_join(assignment_data, hit_data, by = "hit_id") %>%
    filter(name == kmlid & status == "Approved") %>%
    arrange(assignment_id) %>% pull(assignment_id)
  # assignmentid <- unlist(assignmentid)

  # read grid polygon
  xy_tabs <- ref_grid %>% filter(name == kmlid) %>%
    dplyr::select(x, y, name) %>% as.data.table()
  # read grid geometry, and keep gcs
  grid_poly <- point_to_gridpoly(xy = xy_tabs, w = diam, NewCRSobj = crsstr,
                                 OldCRSobj = crsstr)
  grid_poly <- st_geometry(grid_poly)  # retain geometry only

  # lklh_field: p(user=field|groundtruth=field)
  # lklh_nofield: p(user=no field|groundtruth=no field)
  # Read user fields, field and no field likelihood from database into sf object
  bayes_polys <- lapply(assignmentid, function(x) {  # x <- assignmentid[1]
    # workerid
    workerid <- assignment_data %>% filter(assignment_id == x) %>%
      arrange(assignment_id) %>% pull(worker_id)

    # read all scored assignments including 'Approved' and 'Rejected'
    # for calculating history field and no field likelihood of worker i.
    # Get qualification assignments as well
    ############################# assignment history #########################
    userhistories <- data.frame(
      assignment_history(assignment_data, accuracy_data, workerid))
    # unique(assignment_data$worker_id)

    # check if any qual history assignmentid
    qual_userhistories <- data.frame(
      qual_assignment_history(qual_assignment_data, qual_accuracy_data,
                              workerid)
    )  # making these data.frames pulls them out of dbase if not tibble

    # combine regular assignment history for the user and delete NA values
    userhistories_all <- rbind(userhistories, qual_userhistories)
    scores <- userhistories_all %>% summarize_all(list(~ mean(., na.rm = TRUE)))

    ############################# user maps ####################################
    geometry_user <- get_user_maps(x, categories, category, grid_poly,
                                   user_maps, qsite)
    geometry_user_unsure <- get_user_maps(x, categories, unsure_category,
                                          grid_poly, user_maps, qsite)

    # we give 0.5 as posterior probability to unsure, meaning that the user
    # thinks it has only 50% chance of being a field
    # bayes_poly will consist two sf rows, the first is that the surely-labeled
    # fields, and the second is that unsure fields
    bayes_poly <- st_sf(
      'posterior.field' = c(1, 0.5),
      'max.field.lklh' = c(scores$ml.field, scores$ml.field),
      'max.nofield.lklh' = c(scores$ml.nofield, scores$ml.nofield),
      'prior'= c(scores$score.hist, scores$score.hist),
      geometry = c(st_sfc(geometry_user), st_sfc(geometry_user_unsure))
    )
    # set crs
    st_crs(bayes_poly) <- crsstr

    return(bayes_poly)
  })
  bayes_polys <- suppressWarnings(do.call(rbind, bayes_polys))

  if((nrow(bayes_polys) == 0) || (is.null(bayes_polys) == TRUE)) {
    stop("There are no valid assignments for creating consensus maps")
  }

  # count the number of user maps that has field polygons
  count_hasuserpolymap <- length(
    which(st_is_empty(bayes_polys[, "geometry"]) == FALSE)
  )

  # if there isn't any user map polygons for this grid or if for qsite,
  # use the grid extent as the raster extent
  if ((qsite == FALSE) || (count_hasuserpolymap == 0)) {
    rasterextent <- grid_poly
  } else {
    # for Q sites, use the maximum combined boundary of all polygons and master
    # grid as the raster extent
    bb_grid <- st_bbox(grid_poly)
    bb_polys <- st_bbox(st_union(bayes_polys))
    new_bbox <- st_bbox(c(xmin = min(bb_polys$xmin, bb_grid$xmin),
                          xmax = max(bb_polys$xmax, bb_grid$xmax),
                          ymax = max(bb_polys$ymax, bb_grid$ymax),
                          ymin = min(bb_polys$ymin, bb_grid$ymin)),
                        crs = crsstr)
    rasterextent <- st_as_sfc(new_bbox)
  }

  # Threshold here for determine field pixels in heat maps (not threshold for
  # risk pixels )
  if(mode == 'high') {
    tmp <- bayes_polys %>%
      filter(posterior.field == 1) %>%
      na.omit()
    bayes_polys <- tmp %>% filter(prior == max(tmp$prior))
  } else if (mode == 'low') {
    tmp <- bayes_polys %>%
      filter(posterior.field == 1) %>%
      na.omit()
    bayes_polys <- tmp %>% filter(prior == min(tmp$prior))
  }

  # using 0.50000001 can avoid identifying unsure polygon when only single user
  # or using mode to generate consensus maps
  bayesoutput <- bayes_model_averaging(
    bayes.polys = bayes_polys, rasterextent = rasterextent,
    threshold = 0.5000001
  )

  # calculate risk stats: mean risk and proportion of risky pixels
  meanrisk <- cellStats(bayesoutput$riskmap, mean)

  riskpixelpercentage <- round(
    ncell(bayesoutput$riskmap[bayesoutput$riskmap > risk_threshold]) /
      (nrow(bayesoutput$riskmap) * ncol(bayesoutput$riskmap)), 2
  )
  bayes_stats <- c("meanrisk" = meanrisk, "prop_risky" = riskpixelpercentage)
  return(list(kmlid = kmlid, maps = bayesoutput, stats = bayes_stats))
}

#' Write consensus results to instance database and s3
#' @param results Output list from consensus_map_creation
#' @param params Parameter file read in from common/
#' @param s3bucket Name of s3bucket, read from db
#' @param dbupdate Update the database with risk results or not. Default = TRUE
#' @param output_riskmap Save output risk map?
#' @param coninfo Database connection information from mapper_connect()
#' @details This function will not save a consensus map to s3 if s3bucket is
#' kept as null, useful for testing. The same applies for the risk map
#' @export

write_consensus_results <- function(results, params, output_riskmap,
                                    s3bucket = NULL, dbupdate = TRUE,
                                    coninfo = NULL) {

  # insert risk pixel percentage into kml_data table
  if(dbupdate & !is.null(connection)) {
    risk_sql <- paste0("update kml_data set consensus_conflict = '",
                       results$stats["meanrisk"], "' where name = '",
                       results$kmlid, "'")
    DBI::dbSendQuery(coninfo$con, risk_sql)
  }

  # writeout results to s3
  ###################### map output ###############
  rowcol <- rowcol_from_xy(xy_tabs$x, xy_tabs$y, offset = -1)

  if(!is.null(s3bucket)) {
    # upload consensus
    s3_filename <- paste0(kmlid, '_', rowcol[1, 'col'], '_', rowcol[1, 'row'])
    s3_upload(coninfo$dinfo["project_root"], s3bucket,
              bayesoutput$labelmap,
              params$labeller$consensus_directory,
              s3_filename)

    # upload riskmap, if wanted
    if(output_riskmap == TRUE) {
      s3_filename <- paste(kmlid + "_risk")
      s3_upload(coninfo$dinfo["project_root"], s3bucketname,
                bayesoutput$riskmap,
                params$labeller$consensus_riskmap_dir,
                s3_filename)
    }
  }
}
