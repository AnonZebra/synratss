#' @title Check if hex color code falls within specified color range
#'
#' @description Checks if a hexadecimal color value codes for a color
#' within a specified color range. Color range is specified using EITHER col OR
#' r/g/b arguments. Possible col specifications are: "blue", "red", "green",
#' "white", "black" or "hazy". r/g/b each take a
#' two-value numeric vector providing
#' specifications of minimum/maximum red/green/blue values within the
#' range 0-255 (e.g. c(20, 230) ).
#'
#' @param hex hexadecimal color value to check (e. g. "#DA80F2")
#' @param col color specification (possible choices are: "blue", "red",
#' "green", "white", "black", "hazy")
#' @param r a two-value vector
#' @param g a two-value vector
#' @param b a two-value vector
#'
#' @seealso \code{\link[grDevices]{col2rgb}}
#'
#' @export

is_color <- function(hex, col = NULL, r = NULL, g = NULL, b = NULL) {
  if (is.null(col) & (is.null(r) | is.null(g) | is.null(b))) {
    cat("You must specify either col, or all of r, g and b")
    return(NA)
  }
  if (is.na(hex) == TRUE) {
    return(NA)
  }
  if (!is.null(col)) {
    if (col == "blue") {
      comp_vals <- c(0, 80, 0, 80, 190, 255)
    }
    else if (col == "red") {
      comp_vals <- c(190, 255, 0, 80, 0, 80)
    }
    else if (col == "green") {
      comp_vals <- c(0, 80, 190, 255, 0, 80)
    }
    else if (col == "black") {
      comp_vals <- c(0, 33, 0, 33, 0, 33)
    }
    else if (col == "white") {
      comp_vals <- c(222, 255, 222, 255, 222, 255)
    }
    else if (col == "hazy") {
      comp_vals <- c(100, 150, 100, 150, 100, 150)
    }
  } else {
    comp_vals <- c(r, g, b)
  }
  pri_rgb <- col2rgb(hex)
  if (pri_rgb[1] >= comp_vals[1] && pri_rgb[1] <= comp_vals[2] &&
      pri_rgb[2] >= comp_vals[3] && pri_rgb[2] <= comp_vals[4] &&
      pri_rgb[3] >= comp_vals[5] && pri_rgb[3] <= comp_vals[6]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
