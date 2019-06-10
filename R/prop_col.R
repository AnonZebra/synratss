#' @title Calculate proportion of color codes within specified color range
#'
#' @description Calculates the proportion of color values in a data frame that
#' fall within a color range, specified using the col argument. Possible choices
#' are "black", "white", "blue" and "hazy", with "black" as default. Returns NA
#' if all values in the data frame are NA.
#'
#' @param hexdf data frame with hexadecimal color values
#' @param col 1-value character vector, specifying color range
#' @param byrow 1-value character vector, specifying if function should calculate
#' the proportion of rows (TRUE) where all values fall within color range,
#' or the proportion of elements (FALSE - default).
#'
#' @seealso \code{\link[synratss]{is_color}}
#'

prop_col <- function(hexdf, col = "black", byrow = FALSE) {
  if (all(is.na(hexdf))) {
    return(NA)
  }
  if (col %in% c("black", "white", "hazy", "red", "green", "blue")) {
    bool <- apply(hexdf, c(1, 2), function(x) is_color(x, col = col))
  }
  else {cat("unsupported color")}
  if (isTRUE(byrow)) {
    bool <- apply(bool, 1, all) # checks for each row in bool if all its elements are TRUE, and if so producing TRUE, otherwise FALSE, putting the values in "bool" (forming a logical vector)
  }
  propcol <- sum(bool, na.rm = TRUE)/length(bool[!is.na(bool)]) # calculates proportion of answers that are of the color being checked for
  return(propcol)
}
