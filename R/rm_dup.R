#' @title Remove duplicates from a data frame
#'
#' @description \code{rm_dup()} finds all rows in a
#' data frame which share the same
#' entry for a target column and returns a data frame where only the first
#' or last of each set of duplicates is retained.
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
#' @details \code{rm_dup} finds all rows in a data frame which share the same
#' entry for a target column and returns a data frame where duplicates
#' have been removed.
#' For each set of duplicates, in a first step, the
#' row with the most non-missing/non-NA values
#' is retained. In a second step, if there are
#' duplicate rows where more than one row
#' has non-NA values for all columns, either the first
#' (keep_last=FALSE) or the last (keep_last=TRUE) row in
#' the set of duplicates is kept. Rows with NA entries in the
#' target column are left as they are
#' (even if there are multiple NA's). If no duplicates
#' are found, the data frame is returned as-is.
#'
#' @seealso \code{\link[base]{order}}
#'
#' @examples
#' df <- data.frame(x = c(1, 3, 1, 1, 5, 8, 5, 9, 10, NA),
#' y = c(3, 1, 4, 8, 10, 8, 9, 10, 11, 5))
#' rm_dup(df, "x")
#'
#' @export

rm_dup <- function(df, ind_col, keep_last=FALSE) {
  # add a "keepord" column for preserving original order
  man_df <- cbind(df, keepord = 1:nrow(df))
  # create vector giving element positions
  # by ascending order for the target column
  ord_var <- order(man_df[, ind_col])
  # reorder the data frame based on values in the target column
  ord_df <- man_df[ord_var, ]

  # store the row numbers of rows with NA values in the index column separately
  na_rows <- which(is.na(ord_df[, ind_col]))

  # initiates a hash environment/dictionary for storing "key-value" pairs,
  # with "keys" being target column levels, and "values" being target
  # column row(s) corresponding to that level. sets of duplicates are
  # represented by multiple "values" bound to one "key"
  targ_hash <- hash::hash()

  # for loop for generating "key"-"value" pairings as described above
  for (i in 1:nrow(ord_df)) {
    # if the target column value in row i is NA,
    # skip this and proceed to the next row
    if (is.na(ord_df[i, ind_col])) {
      next
    }
    # get the target column value in row i and
    # converts it to type character
    cell_val <- as.character(ord_df[i, ind_col])
    # check for row i if there is a "key" corresponding
    # to the row's target column value
    if (!hash::has.key(cell_val, targ_hash)) {
      # if there is no key yet, initiate a "key" for the
      # target column value and pair it with the row number
      targ_hash[[cell_val]] <- list(i)
    } else {
      targ_hash[[cell_val]] <- list(
        c(unlist(hash::values(targ_hash[cell_val])), i)
      )
    }
  }

  for (key in hash::keys(targ_hash)) {
    # get the "value"(s) (row numbers) paired with the key
    key_vals <- unlist(hash::values(targ_hash[key]))
    # check if there is only one "value" paired with the
    # "key" and proceeds in that case
    if (length(key_vals) < 2) {
      next
    }
    # get the number of valid (non-NA) elements for each
    # row pointed to by the row number "value"s
    num_valid <- sapply(key_vals, function(x) sum(!is.na(ord_df[x, ])))
    # get the highest number of in-row valid (non-NA) elements,
    # looking at the rows paired with "key"
    top <- max(num_valid)
    # get the row number(numbers, in case of a tie) with the
    # highest number of in-row valid elements
    most_valid <- key_vals[num_valid == top]
    # in case of a tie, keep the first row number if keep_last set to FALSE
    if (length(most_valid) > 1) {
      if (keep_last == FALSE) {
        most_valid <- most_valid[1]
      } else {
        # in case of a tie, keeps the last row number if keep_last set to TRUE
        most_valid <- most_valid[length(most_valid)]
      }
    }
    # replaces the "value"(s) paired with the "key" with
    # only one row number, from most_valid
    targ_hash[[key]] <- list(most_valid)
  }

  keep_rows <- c(unlist(hash::values(targ_hash), use.names = FALSE), na_rows)

  out_df <- ord_df[keep_rows, ]
  out_df <- out_df[order(out_df$keepord), ]
  return(subset(out_df, select = -keepord))
}
