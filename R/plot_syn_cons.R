#' @title Plot consistency scores and responses by synesthetic inducer
#'
#' @description Creates (inducer ; response variation score) bar plots for data
#' from a consistency test for synesthesia, with each inducer in the color that
#' correspond to each participant response beneath each bar.
#'
#' @param ddf named single-row array/matrix of mean difference between response colors.
#' @param hexcolors data frame with hexadecimal color codes for responses
#' for all graphemes
#' @param savepath (optional) file path for saving produced plot
#' @param tovar tovar: if TRUE, resulting plot is returned so that you
#' can save it to a variable. defaults to FALSE
#' @param category_lines: if TRUE, plots include lines representing the mean
#' consistency score per category (digits, single letters, graphemes made up of multiple characters)
#' @param multi_char_category: only relevant if category_lines set to TRUE. then, if TRUE,
#' plots include a line representing mean for graphemes made up of multiple characters
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @export
#'

plot_syn_cons <- function(ddf, hexcolors, savepath=NULL, tovar = FALSE, category_lines=FALSE,
                          multi_char_category=FALSE) {
  # sort graphemes based on alphabetical order, then on number of characters
  # doing weird stuff here because R converts the matrix into a numeric
  # vector when rearranging the columns, causing headaches
  ddf <- ddf[, order(colnames(ddf))]
  ddf <- ddf[order(nchar(names(ddf)))]
  foo_matrix <- matrix(ddf, ncol=length(ddf))
  colnames(foo_matrix) <- names(ddf)
  ddf <- foo_matrix
  hexcolors <- hexcolors[order(hexcolors[, 1]), ]
  hexcolors <- hexcolors[order(nchar(hexcolors[, 1])), ]

  # finds all columns in ddf vector/data frame that correspond to a grapheme (assuming that other columns start with 'part_','prop_' or 'three_')
  grapheme_cols <- which(!grepl('^(part_|three_|prop)', colnames(ddf)))



  # get string representation of graphemes (cuts down to maximum of 2 characters)
  grapheme_repr <- ifelse(nchar(colnames(ddf)[grapheme_cols]) < 3,
                          colnames(ddf)[grapheme_cols],
                          substr(colnames(ddf)[grapheme_cols], 1, 2))


  plotdf <- data.frame(Sum_distance=as.vector(ddf[grapheme_cols]),
                       Grapheme=grapheme_repr,
                       stringsAsFactors=FALSE)

  y_up_lim <- ifelse(max(plotdf$Sum_distance, na.rm = TRUE) == -Inf,
                     5,
                     max(plotdf$Sum_distance, na.rm = TRUE))
  plotdf$Grapheme <- factor(plotdf$Grapheme, levels=plotdf$Grapheme, ordered=TRUE)
  myplot <- ggplot2::ggplot(data=plotdf, ggplot2::aes(x=Grapheme, y=Sum_distance)) +
    ggplot2::geom_col(fill="black", color="black", width=0.5) + #produces columns using the aes() arguments from the ggplot call
    ggplot2::geom_hline(yintercept=135.30, color="blue") + #adds an horizontal blue line showing the "mean of mean difference between responses" (remove this line if this is unwanted)
    ggplot2::labs(x="Grapheme", y="Sum distance between responses") + #x- and y-axis titles
    ggplot2::scale_x_discrete(labels=NULL) + #removes titles for the x axis values/ticks
    ggplot2::scale_y_continuous(breaks=round(seq(0,
                                        y_up_lim,
                                        length.out = 10),
                                    -floor(log10(y_up_lim)))
    ) + #specifies y axis ticks/breaks
    ggplot2::geom_text(y=-y_up_lim * 0.04, label=plotdf$Grapheme, size = 3.5, #adds first line of graphemes
              color = hexcolors[, 2]) +
    ggplot2::geom_text(y=-y_up_lim * 0.08, label=plotdf$Grapheme, size = 3.5, #adds second line of graphemes
              color = hexcolors[, 3]) +
    ggplot2::geom_text(y=-y_up_lim * 0.12, label=plotdf$Grapheme, size = 3.5, #adds third line of graphemes
              color = hexcolors[, 4]) +
    ggplot2::theme(axis.ticks.x=ggplot2::element_blank(), #removes x axis ticks
          panel.grid.major.x=ggplot2::element_blank(), #removes vertical grid lines
          panel.grid.minor.y=ggplot2::element_blank()) + #removes the smaller horizontal grid lines
    ggplot2::coord_cartesian(y = c(-y_up_lim / 10,
                          y_up_lim)) #specifies y axis limits
  if (category_lines) {
    digit_mean <- mean(ddf[1, grepl("^[0-9]$", colnames(ddf))], na.rm=TRUE)
    letter_mean <- mean(ddf[1, grepl("^[A-Za-z]$", colnames(ddf))], na.rm=TRUE)
    if (!is.na(digit_mean)) {
      myplot <- myplot +
        ggplot2::geom_hline(yintercept=digit_mean, color="red")
    }
    if (!is.na(letter_mean)) {
      myplot <- myplot +
        ggplot2::geom_hline(yintercept=letter_mean, color="yellow")
    }
    if (multi_char_category) {
      weekdays_bool <- grepl("..+", colnames(ddf)) & !grepl("(prop|part|resp)_", colnames(ddf))
      print(weekdays_bool)
      multi_mean <- mean(ddf[1, weekdays_bool], na.rm=TRUE)
      if (!is.na(multi_mean)) {
        myplot <- myplot +
          ggplot2::geom_hline(yintercept=multi_mean, color="green")
      }
    }
  }
  if (!is.null(savepath)){
    ggplot2::ggsave(savepath)
  }

  if (tovar==TRUE){
    return(myplot)
  }
}
