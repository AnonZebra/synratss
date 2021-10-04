#' @title Combine data frames column-wise by matching rows
#'
#' @description Finds all rows in two data frames that have matching
#' entries for an index column and returns a data frame with only
#' these rows combined, ordered by the index column entries (as
#' \code{order()} would order them).
#'
#' @param df1 data frame
#' @param df2 data frame
#' @param ind_col character value giving the index column name
#'
#' @seealso \code{\link[base]{order}}
#'
#' @examples
#' df1 <- data.frame(z = c(3, 2, 8, 6), y = c("foo", "bar", "spam", "mayo"))
#' df2 <- data.frame(x = c(310, 580, 43, 7), y = c("top", "foo", "hurr", "bar"))
#' cbind_matches(df1, df2, "y")
#'
#' @export


cbind_matches <- function(df1, df2, ind_col) {
  ind1 <- order(as.vector(df1[, ind_col]))
  ind2 <- order(as.vector(df2[, ind_col]))

  ord_df1 <- df1[ind1, ]
  ord_df2 <- df2[ind2, ]

  iso_df1 <- ord_df1[, ind_col] %in% ord_df2[, ind_col]
  iso_df2 <- ord_df2[, ind_col] %in% ord_df1[, ind_col]

  ind_pos1 <- which(colnames(df1) == ind_col)

  res <- cbind(ord_df1[iso_df1, ], ord_df2[iso_df2, ])
  res <- res[, -ind_pos1]

  return(res)
}
