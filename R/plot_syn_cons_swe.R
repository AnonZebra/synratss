#' @title Plot consistency scores and
#' responses by synesthetic inducer (Swe ver.)
#'
#' @description This function is identical to \code{plot_syn_cons},
#' except the plots it produces are in Swedish and adjusted for participants.
#'
#' @param ddf vector/single-row array of mean
#' difference between response colors.
#' @param hexcolors data frame with hexadecimal color codes for responses
#' for all graphemes
#' @param savepath (optional) file path for saving produced plot
#' @param tovar tovar: if TRUE, resulting plot is returned so that you
#' can save it to a variable. defaults to FALSE
#'
#' @seealso \code{\link[synratss]{plot_syn_cons}}
#'
#' @export
#'

plot_syn_cons_swe <- function(ddf, hexcolors, savepath = NULL, tovar = FALSE) {
  # This assumes that there are 39 characters (including umlaut)
  # with accompanying data
  plotdf <- data.frame(
    Sum_distance = as.vector(ddf)[1:39],
    Character = hexcolors[, "letter"],
    stringsAsFactors = FALSE
  )
  y_up_lim <- ifelse(max(plotdf$Sum_distance, na.rm = TRUE) == -Inf,
                     5,
                     max(plotdf$Sum_distance, na.rm = TRUE))
  myplot <- ggplot2::ggplot(
    data = plotdf, ggplot2::aes(x = Character, y = Sum_distance)
  ) +
    ggplot2::geom_col(fill = "black", color = "black", width = 0.5) +
    ggplot2::labs(x = "Symbol", y = "Poäng för hur konsekvent du svarat") +
    ggplot2::scale_x_discrete(labels = NULL) +
    ggplot2::scale_y_continuous(
      breaks = round(
        seq(
          0,
          y_up_lim,
          length.out = 10
        ),
        -floor(log10(y_up_lim))
      )
    ) +
    # add first line of graphemes
    ggplot2::geom_text(
      y = -y_up_lim * 0.04,
      label = plotdf$Character,
      size = 3.5,
      color = hexcolors[, 2]
    ) +
    # add second line of graphemes
    ggplot2::geom_text(
      y = -y_up_lim * 0.08,
      label = plotdf$Character,
      size = 3.5,
      color = hexcolors[, 3]
    ) +
    # add third line of graphemes
    ggplot2::geom_text(
      y = -y_up_lim * 0.12,
      label = plotdf$Character,
      size = 3.5,
      color = hexcolors[, 4]
    ) +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(y = c(-y_up_lim / 10,
                                   y_up_lim))
  if (!is.null(savepath)) {
    ggplot2::ggsave(savepath)
  }

  if (tovar == TRUE) {
    return(myplot)
  }
}
