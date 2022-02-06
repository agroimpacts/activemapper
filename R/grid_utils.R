#' Get center coordinates from row column for learner's grid reference system
#' @param r Vector of column indices to refer to main grid
#' @param c Vector of row indices to refer to main grid
#' @param e Extent of main grid, defaults to current one used for Africa
#' @param res Resolution of main grid, defaults to 0.5
#' @return A matrix of
#' @export
xys_from_rowcol <- function(r, c,
                            e = c(-17.541, 51.459, -35.46, 37.54),
                            res = 0.05) {
  ext <- raster::extent(e)
  rext <- raster::raster(ext, res = res)
  xs <- raster::xFromCol(rext, c)
  ys <- raster::yFromRow(rext, r)
  xys <- cbind(r = r, c = c, xs, ys)
  return(xys)
}
