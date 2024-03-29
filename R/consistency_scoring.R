#' @title Calculate consistency scores
#'
#' @description Calculates consistency scores using data from a consistency
#' test for synesthesia. Can also create plots for each data set (using the
#' synratss::plot_syn_cons function).
#'
#' @param part_df participant consistency test raw data frame
#' @param plotdir (optional) directory path for directory to put plots in
#' @param method method to use in color space distance calculations
#' ("euclidean" for Euclidean distances, "manhattan" for
#' Manhattan/city block distances - "euclidean" is standard)
#' @param fmt specifies color format/space to be used ("sRGB", "Luv")
#' @param nameby (optional) name, in quotes, of column to name
#' output plots by, e. g. "USERNAME"
#' @param swedish_chars specify if swedish characters are in
#' the data frame or not
#' @param swedish_weekdays specify if swedish weekdays are in
#' the data frame or not
#' @param participant_ids a vector for naming output plots. needs
#' to be in the same order as the part_df participants.
#' @param category_lines if TRUE and plotdir is set,
#' drawn plots will include lines
#' representing the mean score per category
#' @param multi_char_category if TRUE, plotdir is set, and
#' category_lines is TRUE,
#' category lines will include a line for representing
#' mean score of multi character
#' category
#' @seealso \code{\link[synratss]{prop_col}} \code{\link[synratss]{dist_sum}}
#' \code{\link[synratss]{plot_syn_cons}}
#'
#' @export
#'

consistency_scoring <- function(
  part_df, id_col_name, timestamp_col_name,
  plotdir = NULL, method = "euclidean",
  fmt = "Luv", nameby = NULL,
  swedish_chars = FALSE,
  swedish_weekdays = FALSE,
  participant_ids = NULL,
  category_lines = FALSE,
  multi_char_category = FALSE
) {
  # "------" represents "no color" choices, which is recoded here as NA
  part_df[part_df == "------" | part_df == "nocolor" | part_df == ""] <- NA
  # grabs all column names that have "symbol" in them (which is where
  # the presented grapheme data is, at KIND)
  symbols <- grep("^symbol", names(part_df))
  # grabs all column names that have "timing" in them (which is
  # where the response timing data is, at KIND)
  timings <- grep("^timing", names(part_df))
  # # grabs all column names that have "color" in
  # them (which is where the response color data is, at KIND)
  colors <- grep("^color", names(part_df))
  # initiate empty data frame that is to be filled
  # up with values and then returned by the function
  df_total <- data.frame()

  digits_chr <- as.character(0:9)

  graphemes <- c(LETTERS, digits_chr)
  if (swedish_chars) {
    swe_letters <- c("Å", "Ä", "Ö")
    swe_alphabet <- c(LETTERS, swe_letters)
    graphemes <- c(graphemes, swe_letters)
  }
  if (swedish_weekdays) {
    swe_wkdays <- c("Måndag", "Tisdag", "Onsdag", "Torsdag", "Fredag",
                    "Lördag", "Söndag")
    graphemes <- c(graphemes, swe_wkdays)
  }

  for (foo in 1:nrow(part_df)) {
    dat1 <- part_df[foo, ]
    hexcolors <- data.frame(
      grapheme = graphemes,
      rep1 = NA,
      rep2 = NA,
      rep3 = NA,
      stringsAsFactors = FALSE
    )
    #This forms a new data frame with the single participant's data,
    # with rows representing one response instance each, and columns
    # for 1 ID 2 symbol 3 chosen color 4 timing
    gather_data <- data.frame(
      ID = rep(dat1[, "ID"],
      each = length(symbols)),
      symbol = as.character(
        dat1[, symbols]),
        color = paste("#", as.character(dat1[, colors]), sep = ""),
        timing = as.character(dat1[, timings]),
        stringsAsFactors = FALSE
      )

    # This fills up the hexcolors data frame with
    # response color hexadecimal values for each response
    for (i in 1:nrow(hexcolors)) {
      symbol_mask <- gather_data$symbol == hexcolors[i, 1]
      hexcolors[i, 2:4] <- gather_data$color[symbol_mask]
    }
    #converts "no color selected" responses to NA
    hexcolors[hexcolors == "#------"] <- NA
    hexcolors[hexcolors == "#NA"] <- NA
    # "out" is a data frame for storing item
    # consistency and mean consistency per participant values
    out <- matrix(
      dimnames = list(c(),
      hexcolors$grapheme),
      ncol = nrow(hexcolors)
    )

    for (i in 1:nrow(hexcolors)) {
      # if any of the three responses for an item is NA, the total
      # item score is also coded as NA
      if (any(is.na(hexcolors[i, 2:4]))) {
        item_score <- NA
      }
      # if all the responses fall within the category of 'black'
      # (hex value <= 22)
      else if (prop_col(hexcolors[i, 2:4], col = "black", byrow = TRUE)) {
        item_score <- NA
      }
      else {
        x <- hex_to_cspace(hexcolors[i, 2:4], fmt)
        item_score <- dist_sum(x, method)
        out[1, i] <- item_score
      }
    }

    out <- cbind(
      out,
      "part_mean_tot" = mean(out[1, ], na.rm = TRUE)
    )
    if (swedish_chars) {
      out <- cbind(
        out,
        "part_mean_A_Ö" = mean(out[1, colnames(out) %in% swe_alphabet],
        na.rm = TRUE))
    } else {
      out <- cbind(
      out,
      "part_mean_A_Z" = mean(out[1, colnames(out) %in% LETTERS], na.rm = TRUE)
    )
    }
    if (swedish_weekdays) {
      out <- cbind(
        out,
        "part_mean_wkdays" = mean(
          out[1, colnames(out) %in% swe_wkdays],
          na.rm = TRUE
        )
      )
    }
    out <- cbind(
      out,
      "part_mean_0_9" = mean(
        out[1, colnames(out) %in% digits_chr],
        na.rm = TRUE
      )
    )

    out <- cbind(out, "three_resp_tot" = sum(!is.na(out)))
    if (swedish_chars) {
      out <- cbind(
        out,
        "three_resp_A_Ö" = sum(
          !is.na(out[1, colnames(out) %in% swe_alphabet])
        )
      )
      out <- cbind(
        out,
        "three_resp_vocals" = sum(
          !is.na(out[1, 
          tolower(colnames(out)) %in% c("a", "e", "i", "o", "u", "y")])
        )
      )
    } else {
      out <- cbind(out,
      "three_resp_A_Z" = sum(!is.na(out[1, colnames(out) %in% LETTERS])))
      out <- cbind(
        out,
        "three_resp_vocals" = sum(
          !is.na(out[1, tolower(colnames(out)) %in% c("a", "e", "i", "o", "u", "y", "ä", "å", "ö")])
        )
      )
    }
    if (swedish_weekdays) {
      out <- cbind(
        out, 
        "three_resp_wkdays" = sum(
          !is.na(out[1, colnames(out) %in% swe_wkdays]
          )
        )
      )
    }
    out <- cbind(
      out, "three_resp_0_9" = sum(
        !is.na(out[1, colnames(out) %in% digits_chr])
      )
    )
    out <- cbind(
      out,
      "prop_black_tot" = prop_col(hexcolors[, 2:4],
      col = "black", byrow = TRUE)
    )
    out <- cbind(
      out,
      "prop_blue_tot" = prop_col(hexcolors[, 2:4],
      col = "blue", byrow = TRUE)
    )
    out <- cbind(
      out,
      "prop_hazy_tot" = prop_col(hexcolors[, 2:4],
      col = "hazy", byrow = TRUE)
    )
    out <- cbind(
      out,
      "prop_white_tot" = prop_col(hexcolors[, 2:4],
      col = "white", byrow = TRUE)
    )
    if (swedish_chars) {
      out <- cbind(
        out,
        "prop_black_A_Ö" = prop_col(
          hexcolors[hexcolors$grapheme %in% swe_alphabet, 2:4],
          col = "black",
          byrow = TRUE
        )
      )
      out <- cbind(
        out,
        "prop_blue_A_Ö" = prop_col(
          hexcolors[hexcolors$grapheme %in% swe_alphabet, 2:4],
          col = "blue",
          byrow = TRUE
        )
      )
      out <- cbind(
        out,
        "prop_hazy_A_Ö" = prop_col(
          hexcolors[hexcolors$grapheme %in% swe_alphabet, 2:4],
          col = "hazy",
          byrow = TRUE
        )
      )
      out <- cbind(
        out,
        "prop_white_A_Ö" = prop_col(
          hexcolors[hexcolors$grapheme %in% swe_alphabet, 2:4],
          col = "white",
          byrow = TRUE
        )
      )
      out <- cbind(
        out,
        "prop_black_vocals" = prop_col(
          hexcolors[tolower(hexcolors$grapheme) %in% c("a", "e", "i", "o", "u", "y", "å", "ä", "ö"), 2:4],
          col = "black",
          byrow = TRUE
        )
      )
    } else {
      out <- cbind(
        out,
        "prop_black_A_Z" = prop_col(
          hexcolors[hexcolors$grapheme %in% LETTERS, 2:4],
          col = "black",
          byrow = TRUE
        )
      )
      out <- cbind(
        out,
        "prop_blue_A_Z" = prop_col(
          hexcolors[hexcolors$grapheme %in% LETTERS, 2:4],
          col = "blue",
          byrow = TRUE
        )
      )
      out <- cbind(
        out,
        "prop_hazy_A_Z" = prop_col(
          hexcolors[hexcolors$grapheme %in% LETTERS, 2:4],
          col = "hazy",
          byrow = TRUE
        )
      )
      out <- cbind(
        out,
        "prop_white_A_Z" = prop_col(
          hexcolors[hexcolors$grapheme %in% LETTERS, 2:4],
          col = "white",
          byrow = TRUE
        )
      )
      out <- cbind(out, "prop_black_vocals" = prop_col(
        hexcolors[tolower(hexcolors$grapheme) %in% c("a", "e", "i", "o", "u", "y"), 2:4],
        col = "black",
        byrow = TRUE
        )
      )
    }
    if (swedish_weekdays) {
      out <- cbind(
      out,
      "prop_black_wkdays" = prop_col(
        hexcolors[hexcolors$grapheme %in% swe_wkdays, 2:4],
        col = "black",
        byrow = TRUE
      )
    )
      out <- cbind(
      out,
      "prop_blue_wkdays" = prop_col(hexcolors[hexcolors$grapheme %in% swe_wkdays, 2:4],
      col = "blue",
      byrow = TRUE)
    )
      out <- cbind(
      out,
      "prop_hazy_wkdays" = prop_col(hexcolors[hexcolors$grapheme %in% swe_wkdays, 2:4],
      col = "hazy",
      byrow = TRUE)
    )
      out <- cbind(
      out,
      "prop_white_wkdays" = prop_col(hexcolors[hexcolors$grapheme %in% swe_wkdays, 2:4],
      col = "white",
      byrow = TRUE)
    )
    }
    out <- cbind(
      out,
      "prop_black_0_9" = prop_col(hexcolors[hexcolors$grapheme %in% digits_chr, 2:4],
      col = "black",
      byrow = TRUE)
    )
    out <- cbind(
      out,
      "prop_blue_0_9" = prop_col(hexcolors[hexcolors$grapheme %in% digits_chr, 2:4],
      col = "blue",
      byrow = TRUE)
    )
    out <- cbind(
      out,
      "prop_hazy_0_9" = prop_col(hexcolors[hexcolors$grapheme %in% digits_chr, 2:4],
      col = "hazy",
      byrow = TRUE)
    )
    out <- cbind(
      out,
      "prop_white_0_9" = prop_col(hexcolors[hexcolors$grapheme %in% digits_chr, 2:4],
      col = "white",
      byrow = TRUE)
    )

    df_total <- rbind(df_total, out)
    if (!is.null(plotdir)) {
      if (is.null(participant_ids)) {
        # calls the plot_syn_cons function to produce a
        # plot for the participant in the specified "plotdir" directory.
        plot_syn_cons(
          out,
          hexcolors,
          savepath = paste0(
            plotdir,
            "/Consistency plot ",
            foo,
            ".pdf"
          )
        )
      } else {
        # calls the plot_syn_cons function to produce
        # a plot for the participant in the specified "plotdir" directory.
        plot_syn_cons(
          out, hexcolors,
          savepath = paste0(
            plotdir,
            "/Consistency plot ",
            as.character(participant_ids[foo]),
            ".pdf"
          ),
          category_lines = category_lines,
          multi_char_category = multi_char_category
        )
      }
    }

  }
  df_total <- cbind(
    "participant_id" = part_df[, id_col_name],
    "cons_test_time" = part_df[, timestamp_col_name],
    df_total
  )
  return(df_total)
}
