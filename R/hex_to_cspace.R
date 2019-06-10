#' @title Convert hex code to color space
#'
#' @description Converts hexadecimal color specifications to different
#' color-space representations, where columns represent different responses/colors,
#' and each row represents one color axis (e.g. R-G-B)
#'
#' @param hexcobj a data frame (single row) or character vector with hexadecimal color values
#' @param fmt specifies color axes/output format ("sRGB", "Luv", "Lab").
#' note that output sRGB values are on the scale 0-1
#'
#' @seealso \code{\link[grDevices]{col2rgb}}
#' \code{\link[grDevices]{convertColor}}
#'

hex_to_cspace <- function(hexcobj, fmt = "sRGB") {
  if (fmt == "sRGB") {
    outobj <- col2rgb(hexcobj)/255
  }
  else if (fmt == "Luv") {
    outobj <- t(convertColor(t(col2rgb(hexcobj) / 255),
                             from = "sRGB", to = "Luv"))
  }
  else if (fmt == "Lab") {
    outobj <- t(convertColor(t(col2rgb(hexcobj) / 255),
                             from = "sRGB", to = "Lab"))
  }
  return(outobj)
}
