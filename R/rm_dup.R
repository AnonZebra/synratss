#' @title Remove duplicates from a data frame
#'
#' @description \code{rm_dup()} finds all rows in a data frame which share the same
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
#' entry for a target column and returns a data frame where. Then for each set of
#' duplicates, in a first step, the row with the most non-missing/non-NA values
#' is retained. In a second step, if there are duplicate rows where more than one row
#' has non-NA values for all columns, either the first (keep_last=FALSE) or the last
#' (keep_last=TRUE) row in the set of duplicates is kept. Rows with NA entries in the
#' target column are left as they are (even if there are multiple NA's). If no duplicates
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
  man_df <- cbind(df, keepord = 1:nrow(df)) # adds a "keepord" column for preserving original order
  ord_var <- order(man_df[, ind_col]) # creates vector giving element positions by ascending order for the target column
  ord_df <- man_df[ord_var, ] # reorders the data frame based on values in the target column

  na_rows <- which(is.na(ord_df[, ind_col])) # stores the row numbers of rows with NA values in the index column separately


  targ_hash <- hash::hash() # initiates a hash environment/dictionary for storing "key-value" pairs, with "keys" being target column levels, and "values" being target column row(s) corresponding to that level. sets of duplicates are represented by multiple "values" bound to one "key"

  for (i in 1:nrow(ord_df)) { # for loop for generating "key"-"value" pairings as described above
    if (is.na(ord_df[i, ind_col])) {next} # if the target column value in row i is NA, skip this and proceed to the next row
    cell_val <- as.character(ord_df[i, ind_col]) # gets the target column value in row i and converts it to type character
    if (!hash::has.key(cell_val, targ_hash)) { # checks for row i if there is a "key" corresponding to the row's target column value
      targ_hash[[cell_val]] <- list(i) # if there is no key yet, initiate a "key" for the target column value and pair it with the row number
    } else {
      targ_hash[[cell_val]] <- list(c(unlist(hash::values(targ_hash[cell_val])), i))
    }
  }

  for (key in hash::keys(targ_hash)) {
    key_vals <- unlist(hash::values(targ_hash[key])) # gets the "value"(s) (row numbers) paired with the key
    if (length(key_vals) < 2) {next} # checks if there is only one "value" paired with the "key" and proceeds in that case
    num_valid <- sapply(key_vals, function(x) sum(!is.na(ord_df[x, ]))) # gets the number of valid (non-NA) elements for each row pointed to by the row number "value"s
    top <- max(num_valid) # gets the highest number of in-row valid (non-NA) elements, looking at the rows paired with "key"
    most_valid <- key_vals[num_valid == top] # gets the row number(numbers, in case of a tie) with the highest number of in-row valid elements
    if (length(most_valid) > 1) {
      if (keep_last == FALSE) {
        most_valid <- most_valid[1] # in case of a tie, keeps the first row number if keep_last set to FALSE
      } else {most_valid <- most_valid[length(most_valid)]} # in case of a tie, keeps the last row number if keep_last set to TRUE
    }
    targ_hash[[key]] <- list(most_valid) # replaces the "value"(s) paired with the "key" with only one row number, from most_valid
  }

  keep_rows <- c(unlist(hash::values(targ_hash), use.names = FALSE), na_rows)

  out_df <- ord_df[keep_rows,]
  out_df <- out_df[order(out_df$keepord), ]
  return(subset(out_df, select = -keepord))
}
