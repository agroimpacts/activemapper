#' Calculate map accuracy and standard error
#' @param err_mat The error matrix, with reference sample and map sample in rows
#' @param areas The mapped number of pixels or area of each class (see details)
#' @param calculate_areas Whether to produce adjusted area estimates or not.
#' @details Follows the methods provided by Olofsson et al (2014a), producing
#' estimate of User's, Producer's, and Overall accuracy with standard errors for
#' each. The outputs can be used to estimate the areas and standard errors of
#' each class in the `area_estimator` function. If pixel counts are provided to
#' the areas argument, the resulting area estimates and their standard
#' errors are returned in pixels, and will require conversion. Note that there
#' are small differences in outputs here compared to published values due to
#' rounding issues.
#' @references
#' Olofsson, P., Foody, G.M., Stehman, S.V. & Woodcock, C.E. (2014a) Making
#' better use of accuracy data in land change studies: Estimating accuracy and
#' area and quantifying uncertainty using stratified estimation. Remote Sensing
#' of Environment, 129, 122–131.
#'
#' Olofsson, P., Foody, G.M., Herold, M., Stehman, S.V.,
#' Woodcock, C.E. & Wulder, M.A. (2014b) Good practices for estimating area and
#' assessing accuracy of land change. Remote Sensing of Environment, 148, 42–57.
#'
#' Olofsson, P., Kuemmerle, T., Griffiths, P., Knorn, J., Baccini, A.,
#' Gancz, V., Blujdea, V., Houghton, R.A., Abrudan, I.V. & Woodcock, C.E. (2011)
#' Carbon implications of forest restitution in post-socialist Romania.
#' Environmental Research Letters, 6, 045202.
#'
#' Stehman, S.V. & Foody, G.M. (2019) Key issues in rigorous accuracy assessment
#' of land cover products. Remote Sensing of Environment, 231, 111199.
#' @return A list containing the error matrix, the matrix containing the
#' estimated proportions of area for each cell, a matrix containing the
#' accuracy measures, and, optionally, a matrix containing the adjusted areas
#' estimates for each class with standard errors.
#' @export
#' @examples
#' # Example 1: Table 4 in Olofsson et al 2014a
#' err_df <- data.frame(class = paste(1:3), r1 = c(97, 3, 2), r2 = c(0, 279, 1),
#'                      r3 = c(3, 18, 97))
#' areas <- c(22353, 1122543, 610228)
#' acc_mat <- accuracy(err_df[, 2:4], areas)
#' acc_mat  # slight different areas and P
#'
#' # Example 2: Table 8 in Olofsson et al (2014b)
#' err_mat <- cbind(class = 1:4, r1 = c(66, 0, 1, 2), r2 = c(0, 55, 0, 1),
#'                  r3 = c(5, 8, 153, 9), r4 = c(4, 12, 11, 313))
#' areas <- c(200000, 150000, 3200000, 6450000)
#' accuracy(err_mat = err_mat[, 2:5], areas = areas)
#' areas <- (c(200000, 150000, 3200000, 6450000) * 30^2) / 10000
#' accuracy(err_mat = err_mat[, 2:5], areas = areas)
#'
#' # Example 3. Table 6 in Olofsson et al (2014a). Areas from Table 2 in
#' Olofsson et al (2011)
#' err_mat <- cbind(class = 1:3, logging = c(127, 2, 0),
#'                  forest = c(66, 322, 15), nonforest = c(54, 17, 540))
#' areas <- c(logging = 154159, forest = 6846562, nonforest = 16178659)
#' accuracy(err_mat[, 2:4], areas)

accuracy <- function(err_mat, areas, calculate_area = TRUE) {

  # convert to matrix if not
  if(!is.matrix(err_mat)) err_mat <- as.matrix(err_mat)

  Wi <- areas / sum(areas) # calculate weights

  ni <- rowSums(err_mat)  # Map class sample count
  Pij <- apply(err_mat, 2, function(x)  Wi * x / ni)  # p_ij
  colnames(Pij) <- paste0("Ref_", 1:ncol(Pij))
  rownames(Pij) <- paste0("Class_", 1:ncol(Pij))
  pi <- rowSums(Pij)  # p_i
  pj <- colSums(Pij)  # p_j
  U <- diag(Pij) / pi  # user's accuracy
  P <- diag(Pij) / pj  # producer's accuracy
  O <- sum(diag(Pij))  # overall

  # Std error and MOE
  # User's
  U_se <- sqrt(U * (1 - U) / (ni - 1))

  # Producer's
  # terms
  nj <- colSums(err_mat)
  Nplusj <- colSums(areas * err_mat / ni)  # nij / ni * aream
  exp1 <- areas^2 * (1 - P)^2 * U * (1 - U) / (nj - 1)
  exp2_1 <- areas^2 * err_mat / ni * (1 - err_mat / ni) / (ni - 1)
  exp2_2 <- sapply(1:nrow(exp2_1), function(x) P[x]^2 * sum(exp2_1[-x, x]))
  P_se <- sqrt(1 / Nplusj^2 * (exp1 + exp2_2))

  # Overall
  O_se <- sqrt(sum(Wi^2 * U * (1 - U) / (ni - 1)))

  # Combine outputs
  ua_pa <- cbind(U, U_se, P, P_se)
  oa_mat <- rbind(
    cbind(O, O_se),
    apply(cbind(NA, NA), 2, function(x) rep(x, nrow(ua_pa) - 1))
  )
  acc_mat <- cbind(ua_pa, oa_mat)

  if(calculate_area) {
    area_mat <- area_estimator(err_mat, areas, Pij, Wi)
    out <- list("error_matrix" = err_mat, "Pij" = Pij, "accuracy" = acc_mat,
                "area" = area_mat)
  } else {
    out <- list("error_matrix" = err_mat, "Pij" = Pij, "accuracy" = acc_mat)
  }
  return(out)
}


