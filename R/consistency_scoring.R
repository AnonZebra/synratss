#' @title Calculate consistency scores
#'
#' @description Calculates consistency scores using data from a consistency
#' test for synesthesia. Can also create plots for each data set using the
#' plot_syn_cons function.
#'
#' @param dfp participant data csv file path (has to be formatted with
#' comma "," separator, and have a header), e. g.
#' "/Users/myname/Data_folder/bokstaverochsiffror.csv"
#' @param pdir directory path for directory to put plots in (optional)
#' @param savepath (optional) file path for saving produced plot
#' @param method method for use in color space distance calculations
#' ("euclidean" for Euclidean distances, "manhattan" for
#' Manhattan/city block distances - "euclidean" is standard)
#' @param fmt fmt: specifies color format/space to be used ("sRGB", "Luv")
#'
#' @seealso \code{\link[synratss]{prop_col}} \code{\link[synratss]{dist_sum}}
#' \code{\link[synratss]{plot_syn_cons}}
#'
#' @export
#'


consistency_scoring <- function(pfp, pdir=NULL, method = "euclidean", fmt = "Luv") {
  dat <- read.csv(pfp, stringsAsFactors = FALSE, header = TRUE, na.strings = c(" ", "", "NA"))
  dat[dat=="------"] <- NA # "------" represents "no color" choices, which is recoded here as NA
  symbols <- grep("^symbol", names(dat))
  df_total <- data.frame()

  for (foo in 1:nrow(dat)){
    dat1 <-dat[foo,]
    hexcolors <- data.frame(letter = c(LETTERS, "Å", "Ä", "Ö", 0:9), rep1 = NA, rep2 = NA, rep3 = NA,
                            stringsAsFactors = FALSE)
    gather_data <- data.frame(ID = rep(dat1[, "ID"], each = length(symbols)), ##This forms a new data frame with the single participant's data, with rows representing one response instance each, and columns for 1 ID 2 symbol 3 chosen color 4 position 5 timing
                              symbol = as.character(dat1[, symbols]),
                              color = paste("#", as.character(dat1[, symbols - 2]), sep = ""),
                              position = as.character(dat1[, symbols - 1]),
                              timing = as.character(dat1[, symbols - 3]),
                              stringsAsFactors = FALSE)

    for (i in 1:39){ ##This fills up the hexcolors data frame with response color hexadecimal values for each response
      hexcolors[i, 2:4] <- gather_data$color[gather_data$symbol == hexcolors[i, 1]]
    }
    hexcolors[hexcolors=="#------"] <- NA #converts "no color selected" responses to NA
    hexcolors[hexcolors=="#NA"] <- NA
    out <- array(NA, dim=c(1,57)) # "out" is a data frame for storing item consistency and mean consistency per participant values

    for (i in 1:39) {
      if ( any(is.na(hexcolors[i,2:4])) ) { #if any of the three responses for an item is NA, the total item score is also coded as NA
        item.score <- NA
      }
      else {
        x <- hex_to_cspace(hexcolors[i,2:4], fmt)
        item.score <- dist_sum(x, method)
        out[1,i]<- item.score
      }
    }

    out[1, 40] <- mean(out[1,c(1:26, 30:39)], na.rm = TRUE) # calculates the "mean of per-item summed differences" for the participant (excluding umlaut letters)
    out[1, 41] <- mean(out[1,1:26], na.rm = TRUE) # for letters A-Z only, calculates the "mean of per-item summed differences" for the participant
    out[1, 42] <- mean(out[1,30:39], na.rm = TRUE) # for digits 0-9 only, calculates the "mean of per-item summed differences" for the participant
    out[1, 43] <- sum(!is.na(out[1,c(1:26, 30:39)])) # calculates for how many items there are 3 valid responses, excluding umlaut letters
    out[1, 44] <- sum(!is.na(out[1,1:26])) # calculates for how many letters there are 3 valid responses, excluding umlaut letters
    out[1, 45] <- sum(!is.na(out[1,30:39])) # calculates for how many digits there are 3 valid responses
    out[1, 46] <- prop_col(hexcolors[c(1:26, 30:39), 2:4], col = "black", byrow = TRUE) #calculates proportion of items for which all responses were "black", excluding umlaut letters
    out[1, 47] <- prop_col(hexcolors[c(1:26, 30:39), 2:4], col = "blue", byrow = TRUE)
    out[1, 48] <- prop_col(hexcolors[c(1:26, 30:39), 2:4], col = "hazy", byrow = TRUE)
    out[1, 49] <- prop_col(hexcolors[c(1:26, 30:39), 2:4], col = "white", byrow = TRUE)
    out[1, 50] <- prop_col(hexcolors[c(1:26), 2:4], col = "black", byrow = TRUE) #calculates proportion of non-umlaut letters for which all responses were "black"
    out[1, 51] <- prop_col(hexcolors[c(1:26), 2:4], col = "blue", byrow = TRUE)
    out[1, 52] <- prop_col(hexcolors[c(1:26), 2:4], col = "hazy", byrow = TRUE)
    out[1, 53] <- prop_col(hexcolors[c(1:26), 2:4], col = "white", byrow = TRUE)
    out[1, 54] <- prop_col(hexcolors[c(30:39), 2:4], col = "black", byrow = TRUE) #calculates proportion of digits for which all responses were "black"
    out[1, 55] <- prop_col(hexcolors[c(30:39), 2:4], col = "blue", byrow = TRUE)
    out[1, 56] <- prop_col(hexcolors[c(30:39), 2:4], col = "hazy", byrow = TRUE)
    out[1, 57] <- prop_col(hexcolors[c(30:39), 2:4], col = "white", byrow = TRUE)


    df_total <- rbind(df_total, out)
    if (!is.null(pdir)) {
      plot_syn_cons(out, hexcolors, # calls the plot_syn_cons function to produce a plot for the participant in the specified "pdir" directory.
                    savepath = paste(pdir, "/Consistency plot ", foo, ".pdf", sep = ""))
    }

  }
  df_total <- cbind(dat[, 5], dat[, 3], df_total)
  colnames(df_total) <- c("PROFILEID", "cons_test_time", # sets column names
                          LETTERS, "Å", "Ä", "Ö", 0:9,
                          "part_mean_tot", "part_mean_A_Z", "part_mean_0_9",
                          "three_resp_tot", "three_resp_A_Z", "three_resp_0_9",
                          "prop_black_tot", "prop_blue_tot",
                          "prop_hazy_tot", "prop_white_tot",
                          "prop_black_A_Z", "prop_blue_A_Z",
                          "prop_hazy_A_Z", "prop_white_A_Z",
                          "prop_black_0_9", "prop_blue_0_9",
                          "prop_hazy_0_9", "prop_white_0_9")
  return(df_total)
}
