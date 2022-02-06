#' Model performance metrics per iteration
#' @name accuracy_reports
#' @docType data
#' @description Results of accuracy assessment performed with map reference
#' sample, including margins of error for overall, producer's, and user's
#' accuracy, as well as class area estimates, with confidence intervals.
#' @keywords data
#' @examples
#' data(accuracy_reports)
#'
NULL

#' Model performance metrics per iteration
#' @name consensus_high_low
#' @docType data
#' @description Performance scores from final iteration for 4 AOIs (1, 2, 8, 15)
#' where label strategy experiments were run: models trained with consensus
#' labels versus those trained with most and least accurate individual labels
#' @keywords data
#' @examples
#' data(consensus_high_low)
NULL

#' Error matrices using map reference sample
#' @name error_matrices
#' @docType data
#' @description Error matrices are calculated for the per pixel and segmented
#' field boundary map, for particular zones consisted of grouped AOIs and AEZ,
#' and for all of Ghana.
#' @keywords data
#' @examples
#' data(error_matrices)
NULL

#' Model performance metrics per iteration
#' @name image_quality
#' @docType data
#' @description Quality scores for the seasonal image composites
#' @keywords data
#' @examples
#' data(image_quality)
NULL

#' Model performance metrics per iteration
#' @name iteration_metrics
#' @docType data
#' @description Performance scores per iteration for each of the AOIs, including
#' score changes measures between iterations
#' @keywords data
#' @examples
#' data(iteration_metrics)
NULL

#' Bayesian Risk metrics per site with summary statistics
#' @name label_risk
#' @docType data
#' @description Provides the mean Bayesian Risk and proportion of risky pixels
#' for each site per AOI, and summarized per AOI.
#' @keywords data
#' @examples
#' data(label_risk)
NULL

#' Summary Bayesian Risk metrics along with per AOI model performance metrics
#' @name label_risk_metrics
#' @docType data
#' @description Provides the mean Bayesian Risk per AOI and scores for various
#' Random Forests performance metrics assessed against validation sites.
#' @keywords data
#' @examples
#' data(label_risk_metrics)
NULL

#' Summary Bayesian Risk metrics along with per AOI model performance metrics
#' @name label_summary
#' @docType data
#' @description Provides the mean Bayesian Risk per AOI and scores for various
#' Random Forests performance metrics assessed against validation sites.
#' @keywords data
#' @examples
#' data(label_risk_metrics)
NULL

#' Croplands areas
#' @name mapped_areas
#' @docType data
#' @description Cropland areas calculated from the classified cropland
#' probabilities and from the segmented field boundaries per AOI. Areas
#' per AEZ are also calculated for the classified cropland
#' probabilities
#' @keywords data
#' @examples
#' data(mapped_areas)
NULL

#' Summary statistics for digitized versus machine-segmented fields
#' @name segment_quality_stats
#' @docType data
#' @description Mean, median areas and average counts of labeller-digitized
#' fields versus machine-segmented fields at the grid, tile, and AOI scales
#' @keywords data
#' @examples
#' data(segment_quality_stats)
NULL


#' Training and validation sites for AOIs 1-16
#' @name tile_key
#' @docType data
#' @description Tibble cross referencing AOIs, tile grid cell IDs, and col, row
#' numbers used for prediction tiles
#' @keywords data
#' @examples
#' data(tile_key)
NULL

#' Training and validation sites for AOIs 1-16
#' @name train_val_sites
#' @docType data
#' @description Centroids of all points used for training and validation,
#' excluding run 0 for AOI 3.
#' @keywords data
#' @examples
#' data(train_val_sites)
NULL


