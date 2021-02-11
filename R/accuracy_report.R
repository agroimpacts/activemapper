#' Produce table of accuracy and area measures
#' @param err_mat The error matrix, with reference sample and map sample in rows
#' @param acc_mat The output list produced by `accuracy`
#' @param classes Vector of names for the map/reference classes
#' @param digits Number of decimal values in outputs
#' @param z Width of confidence interval for reported standard errors
#' @details Produces a matrix for reporting that contains the
#' configuration for reporting map accuracy recommended by Stehman and Foody
#' (2019), with adjusted areas of each class provided.
#' @references
#' Stehman, S.V. & Foody, G.M. (2019) Key issues in rigorous accuracy assessment
#' of land cover products. Remote Sensing of Environment, 231, 111199.
#' @return A character matrix.
#' @export
#' @examples
#' # Example 1: Table 4 in Olofsson et al 2014a
#' err_mat <- cbind(r1 = c(97, 3, 2), r2 = c(0, 279, 1), r3 = c(3, 18, 97))
#' areas <- c(22353, 1122543, 610228)
#' acc_mat <- accuracy(err_mat, areas)
#' accuracy_report(err_mat, acc_mat, classes = paste0("Class", 1:3))

accuracy_report <- function(err_mat, acc_mat, classes, digits = 1, z = 1.96) {

  rnd <- function(tab, d = digits) sprintf(glue("%0.{d}f"), tab)
  a <- acc_mat$accuracy * 100

  # set up error matrix as percent
  Pij <- acc_mat$Pij * 100
  Pij <- round(cbind(Pij, rowSums(Pij)), digits)

  # Areas
  b <- acc_mat$area

  # U, P, O with SE
  U <- glue("{rnd(a[, 'U'])} ({rnd(a[, 'U_se'] * z)})")
  P <- glue("{rnd(a[, 'P'])} ({rnd(a[, 'U_se'] * z)})")

  O <- glue("{rnd(a[1, 'O'])} ({rnd(a[1, 'O_se'] * z)})")
  A <- glue("{rnd(b[, 1], 0)} ({rnd(b[, 2] * z, 0)})")

  # put together output matrix
  headers <- c("", classes, "Total", "U", "O", "n", "Area")
  outmat <- rbind(cbind(Pij, U, c(O, ""), rowSums(err_mat), A),
                  c(P, rep("", 5)),
                  c(colSums(err_mat), rep("", 5)))
  outmat <- cbind(c(classes, "P", "n"), outmat)
  rownames(outmat) <- NULL
  colnames(outmat) <- headers

  return(outmat)
}
