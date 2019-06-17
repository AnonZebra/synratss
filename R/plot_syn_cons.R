#' @title Plot consistency scores and responses by synesthetic inducer
#'
#' @description Creates (inducer ; response variation score) bar plots for data
#' from a consistency test for synesthesia, with each inducer in the color that
#' correspond to each participant response beneath each bar.
#'
#' @param ddf vector/single-row array of mean difference between response colors.
#' @param hexcolors data frame with hexadecimal color codes for responses
#' for all graphemes
#' @param savepath (optional) file path for saving produced plot
#' @param tovar tovar: if TRUE, resulting plot is returned so that you
#' can save it to a variable. defaults to FALSE
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @export
#'

plot_syn_cons <- function(ddf, hexcolors, savepath=NULL, tovar = FALSE) {
  plotdf <- data.frame(Sum_distance=as.vector(ddf)[1:39], #L: This assumes that there are 39 characters (including umlaut) with accompanying data
                       Character=hexcolors[, "letter"],
                       stringsAsFactors=FALSE
  )
  y_up_lim <- ifelse(max(plotdf$Sum_distance, na.rm = TRUE) == -Inf,
                     5,
                     max(plotdf$Sum_distance, na.rm = TRUE))
  myplot <- ggplot2::ggplot(data=plotdf, aes(x=Character, y=Sum_distance)) +
    ggplot2::geom_col(fill="black", color="black", width=0.5) + #produces columns using the aes() arguments from the ggplot call
    ggplot2::geom_hline(yintercept=135.30, color="blue") + #adds a horizontal blue line showing the "mean of mean difference between responses" (remove this line if this is unwanted)
    ggplot2::labs(x="Grapheme", y="Sum distance between responses") + #x- and y-axis titles
    ggplot2::scale_x_discrete(labels=NULL) + #removes titles for the x axis values/ticks
    ggplot2::scale_y_continuous(breaks=round(seq(0,
                                        y_up_lim,
                                        length.out = 10),
                                    -floor(log10(y_up_lim)))
    ) + #specifies y axis ticks/breaks
    ggplot2::geom_text(y=-y_up_lim * 0.04, label=plotdf$Character, size = 3.5, #adds first line of graphemes
              color = hexcolors[, 2]) +
    ggplot2::geom_text(y=-y_up_lim * 0.08, label=plotdf$Character, size = 3.5, #adds second line of graphemes
              color = hexcolors[, 3]) +
    ggplot2::geom_text(y=-y_up_lim * 0.12, label=plotdf$Character, size = 3.5, #adds third line of graphemes
              color = hexcolors[, 4]) +
    ggplot2::theme(axis.ticks.x=element_blank(), #removes x axis ticks
          panel.grid.major.x=element_blank(), #removes vertical grid lines
          panel.grid.minor.y=element_blank()) + #removes the smaller horizontal grid lines
    ggplot2::coord_cartesian(y = c(-y_up_lim / 10,
                          y_up_lim)) #specifies y axis limits
  if (!is.null(savepath)){
    ggplot2::ggsave(savepath)
  }

  if (tovar==TRUE){
    return(myplot)
  }

}
