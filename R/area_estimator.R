#' Calculate error-adjusted area for different map classes
#' @param err_mat The error matrix for each class (reference sample in columns)
#' @param areas Vector providing the areas for each mapped class
#' @param Pij The error matrix as proportions
#' @param Wi The vector of weights for each class
#' @details This function for calculating bias-adjusted areas from the error
#' matrix and the total area of each mapped class, is designed to be run or
#' internally within the accuracy function. If pixel counts are provided to
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
#' @return A matrix containing the estimated area of each class plus its
#' standard error.
#' @export
#' @examples
#' # Example 1: Table 4 in Olofsson et al 2014a. Area in ha
#' err_df <- data.frame(class = paste(1:3), r1 = c(97, 3, 2), r2 = c(0, 279, 1),
#'                      r3 = c(3, 18, 97))
#' areas <- c(22353, 1122543, 610228)
#' area_estimator(err_df[, 2:4], areas)
#'
#' # Example 2: Table 8 in Olofsson et al (2014b). Areas are pixel counts
#' err_mat <- cbind(class = 1:4, r1 = c(66, 0, 1, 2), r2 = c(0, 55, 0, 1),
#'                  r3 = c(5, 8, 153, 9), r4 = c(4, 12, 11, 313),
#'                  areas = c(200000, 150000, 3200000, 6450000))
#' area_estimator(err_mat[, 2:4], err_mat[, "areas"])
area_estimator <- function(err_mat, areas, Pij = NULL, Wi = NULL) {

  # convert to matrix if needed
  if(!is.matrix(err_mat)) err_mat <- as.matrix(err_mat)

  # calculate weights and Pij if NULL
  if(is.null(Wi)) Wi <- areas / sum(areas)  # W_i
  if(is.null(Pij)) {
    Pij <- apply(err_mat, 2, function(x)  Wi * x / rowSums(err_mat))  # p_ij
  }

  # Area estimator
  A_hat <- sum(areas) * colSums(Pij)

  # SE of area
  Sp <- sqrt(colSums((Wi * Pij - Pij^2) / (rowSums(err_mat) - 1)))  # SE
  A_hat_se <- sum(areas) * Sp  # SE of A

  area_mat <- cbind(A_hat, A_hat_se)
  rownames(area_mat) <- paste0("Class_", 1:nrow(area_mat))
  return(area_mat)
}
