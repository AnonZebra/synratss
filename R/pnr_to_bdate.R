#' @title Convert a personal number to birth date
#'
#' @description Converts Swedish personal numbers (in the format
#' "YYYYMMDD-XXXX",
#' or "YYMMDD-XXXX") to birth dates. If the personal number is in the format
#' "YYMMDD-XXXX", the function assumes that all YY values above 25 indicate
#' someone born in the 1900's.
#'
#'
#' @param pnr a string/character value with a single personal number
#'
#' @seealso \code{\link[base]{sub}}
#'
#' @examples
#' x <- "19400830-1111"
#' pnr_bdate(x)
#' y <- "090830-1111"
#' pnr_bdate(y)
#'
#' @export

pnr_to_bdate <- function(pnr) {
  bdate <- pnr
  if (is.na(bdate)) {
    return(NA)
  }
  bdate <- sub("-.*$", "", pnr)
  if (nchar(bdate) != 6 & nchar(bdate) != 8) {
    return("Invalid pnr format")
  }
  if (nchar(bdate) == 6) {
    yy <- as.integer(substr(bdate, 1, 2))
    if (yy > 25) {
      bdate <- paste("19", bdate, sep = "")
    }
    else {
      bdate <- paste("20", bdate, sep = "")
    }
  }
  return(bdate)
}
