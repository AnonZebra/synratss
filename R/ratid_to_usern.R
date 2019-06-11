#' @title Convert a RATSS study ID to username format
#'
#' @description Converts a RATSS code/study ID (in the format e.g.
#' "11 0104 01 01 14" or without any spaces "110104010114") to a
#' <family>_<twin_no> format (e.g. "104_1"). For parents, this leads to them
#' having usernames in the format "<family no>_5" or "<family no>_6". Note that
#' this parent coding might not be what you want or what has been used previously.
#'
#'
#' @param ratid a string/character vector with a single RATSS code
#'
#' @seealso \code{\link[base]{gsub}} \code{\link[base]{substr}}
#'
#' @examples
#' x <- "11 0104 01 01 14"
#' ratid_to_usern(x)
#' y <- "110104010114"
#' ratid_to_usern(y)
#'
#' @export
#'

ratid_to_usern <- function(ratid) {
  rawn <- ratid
  rawn <- gsub(" ", "", rawn)
  if (is.na(rawn)) {
    warning(paste("I was passed a NA value"))
    return(NA)
  }
  if (nchar(rawn) != 12) {
    warning(paste("Wrong format, I was passed this ID:", ratid))
    return(NA)}
  fam_no <- substr(rawn, 3, 6)
  fam_no <- sub("^0*", "", fam_no)
  twin_no <- substr(rawn, 8, 8)
  usern <- paste(fam_no, "_", twin_no, sep="")
  return(usern)
}
