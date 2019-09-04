#' @title Remove duplicates from a data frame
#'
#' @description Finds all rows in a data frame which share the same
#' entry for a target column and returns a data frame where only the first
#' or last of each set of duplicates is retained. Rows with
#' NA entries in the column are left as they are (even if there are
#' multiple NA's). If no duplicates are found, the data frame is returned
#' as-is.
#'
#' @param df data frame
#' @param ind_col character value giving the name of the column
#' to be searched for duplicate entries
#' @param keep_last logical value indicating if the last, instead
#' of the first, of each set of duplicates should be retained. defaults
#' to FALSE, i.e. to retaining the first of each set of duplicates.
#' @param rm_na logical value. if set to TRUE, rows with NA in the
#' specified column are removed.
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
      if (any(is.na(ord_df[i, ind_col]), is.na(ord_df[i-1, ind_col]))) {}
      else if (ord_df[i, ind_col] == ord_df[i-1, ind_col]) {
        dup_ind <- c(dup_ind, i)
      }
    }
  } else {
    for (i in 1:(nrow(ord_df)-1)) {
      if (any(is.na(ord_df[i, ind_col]), is.na(ord_df[i-1, ind_col]))) {}
      if (ord_df[i, ind_col] == ord_df[i+1, ind_col]) {
        dup_ind <- c(dup_ind, i)
      }
    }
  }
  if (!is.null(dup_ind)) {
    clean_df <- ord_df[-dup_ind, ]

    end_ord_var <- order(clean_df$keepord)
    res_df <- clean_df[end_ord_var, -ncol(clean_df)]

    return(res_df)
  }
  return(df)
}
