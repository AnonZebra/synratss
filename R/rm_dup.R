#' @title Remove duplicates from a data frame
#'
#' @description Finds all rows in a data frame which share the same
#' entry for a column and returns a data frame where only the first
#' or last of each set of duplicates is retained. Note that if there
#' are multiple NA entries in the column, only the first/last of
#' these rows will be retained as well.
#'
#' @param df data frame
#' @param ind_col character value giving the name of the column
#' to be searched for duplicate entries
#' @param keep_last logical value indicating if the last, instead
#' of the first, of each set of duplicates should be retained. defaults
#' to FALSE, i.e. to retaining the first of each set of duplicates.
#'
#' @seealso \code{\link[base]{order}}
#'
#' @examples
#' df <- data.frame(x = c(1, 3, 1, 1, 5, 8, 5, 9, 10),
#' y = c(3, 1, 4, 8, 10, 8, 9, 10, 11))
#' rm_dup(df, "x")

rm_dup <- function(df, ind_col, keep_last=FALSE) {
  man_df <- cbind(df, keepord=1:nrow(df))
  ord_var <- order(man_df[, ind_col])
  ord_df <- man_df[ord_var, ]

  dup_ind <- c()
  if (keep_last == FALSE) {
    for (i in 2:nrow(ord_df)) { #for loop that gets the position of all duplicates (including NA values)
      if (ord_df[i, ind_col] == ord_df[i-1, ind_col]) {
        dup_ind <- c(dup_ind, i)
      }
    }
  }
  else if (keep_last == TRUE) {
    for (i in 1:(nrow(ord_df)-1)) {
      if (ord_df[i, ind_col] == ord_df[i+1, ind_col]) {
        dup_ind <- c(dup_ind, i)
      }
    }
  }
  clean_df <- ord_df[-dup_ind, ]

  end_ord_var <- order(clean_df$keepord)
  res_df <- clean_df[end_ord_var, -ncol(clean_df)]

  return(res_df)
}
