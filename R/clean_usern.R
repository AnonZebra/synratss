#' @title Clean up RATSS user names
#'
#' @description Takes a character vector of  entries that
#' have a user name in them in the format "<digit/digits>_<single digit>"
#' somewhere in them, extracts only the user names and returns a vector
#' with those.
#'
#' @param vec character vector
#'
#' @seealso \code{\link[stringr]{str_extract}}
#'
#' @examples
#' foo <- c("Twin202_2", "313_5", "1_2", "1_1", "randomchars12_2fillout")
#' clean_usern(foo)

clean_usern <- function(vec) {
  if (any(!grepl("[1-9]+[0-9]*_[0-9]", vec))) {
    warning("Not all vector elements have a <non-zero number>_<non-zero digit> pattern in them. Returning NULL.")
    return(NULL)
  }
  ocd_usern <- grepl("ocd", vec, ignore.case=TRUE)
  res <- sapply(vec, function(x) stringr::str_extract(x, "[1-9]+[0-9]*_[1-9]"))
  res[ocd_usern] <- paste0("OCD", res[ocd_usern])
  return(as.vector(res))
}
