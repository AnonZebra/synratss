#' @title Calculate sum euclidean distances in color space
#'
#' @description Calculates the sum of euclidean or Manhattan/city block
#' distances, using a hex color code data frame with three rows (one for each
#' color axis - e. g. [R] [G] [B]) and three columns (one for each response).
#'
#' @param x a 3 row, 3 columns data frame with color space specifications
#' @param fmt specifies type of distance to compute ("euclidean"
#' for Euclidean distances, "manhattan" for Manhattan/city block distances
#' - "euclidean" is standard)
#'
#' @seealso \code{\link[stats]{dist}} \code{\link[base]{t}}
#' @export

dist_sum <- function(x, method = "euclidean") {
  item_score <- sum(dist(t(x), method = method))
  return(item_score)
}
