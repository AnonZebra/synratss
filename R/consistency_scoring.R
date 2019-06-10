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
    out <- array(NA, dim=c(1,47)) # "out" is a data frame for storing item consistency and mean consistency per participant values
    z=0

    for (i in 1:39) {
      if ( any(is.na(hexcolors[i,2:4])) ) { #make sure non-values replaced with NA
        item.score <- NA
      }
      else {
        x <- hex_to_cspace(hexcolors[i,2:4], fmt)
        item.score <- dist_sum(x, method)
        z<- z+1
        out[1,i]<- item.score
      }
    }

    out[1, 40] <- mean(out[1,1:39], na.rm = TRUE) # calculates the "mean of per-item summed differences" for the participant
    out[1, 41] <- mean(out[1,1:26], na.rm = TRUE) # for letters A-Z only, calculates the "mean of per-item summed differences" for the participant
    out[1, 42] <- mean(out[1,30:39], na.rm = TRUE) # for digits 0-9 only, calculates the "mean of per-item summed differences" for the participant
    out[1, 43] <- z # L: this column shows for how many items there are 3 valid responses
    out[1, 44] <- prop_col(hexcolors[, 2:4], col = "black", byrow = TRUE) #calculates proportion of items for which all responses were "black" (see prop_col's and related functions' descriptions)
    out[1, 45] <- prop_col(hexcolors[, 2:4], col = "blue", byrow = TRUE)
    out[1, 46] <- prop_col(hexcolors[, 2:4], col = "hazy", byrow = TRUE)
    out[1, 47] <- prop_col(hexcolors[, 2:4], col = "white", byrow = TRUE)
    df_total <- rbind(df_total, out)
    if (!is.null(pdir)) {
      plot_syn_cons(out, hexcolors, # calls the plot_syn_cons function to produce a plot for the participant in the specified "pdir" directory.
                    savepath = paste(pdir, "/Consistency plot ", foo, ".pdf", sep = ""))
    }

  }
  df_total <- cbind(dat[, 5], dat[, 3], df_total)
  colnames(df_total) <- c("PROFILEID", "Cons_test_time", LETTERS, "Å", "Ä", # sets column names
                          "Ö", 0:9, "Participant_mean", "Part_mean_A_Z",
                          "Part_mean_0_9", "three_responses", "Prop_black",
                          "Prop_blue", "Prop_hazy", "Prop_white")
  return(df_total)
}
