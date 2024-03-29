% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_syn_cons.R
\name{plot_syn_cons}
\alias{plot_syn_cons}
\title{Plot consistency scores and responses by synesthetic inducer}
\usage{
plot_syn_cons(
  ddf,
  hexcolors,
  savepath = NULL,
  tovar = FALSE,
  category_lines = FALSE,
  multi_char_category = FALSE
)
}
\arguments{
\item{ddf}{named single-row array/matrix of mean
difference between response colors.}

\item{hexcolors}{data frame with hexadecimal color codes for responses
for all graphemes}

\item{savepath}{(optional) file path for saving produced plot}

\item{tovar}{tovar: if TRUE, resulting plot is returned so that you
can save it to a variable. defaults to FALSE}

\item{category_lines:}{if TRUE, plots include lines representing the mean
consistency score per category (digits, single letters,
graphemes made up of multiple characters)}

\item{multi_char_category:}{only relevant if category_lines
set to TRUE. then, if TRUE,
plots include a line representing mean for
graphemes made up of multiple characters}
}
\description{
Creates (inducer ; response variation score) bar plots for data
from a consistency test for synesthesia, with each inducer in the color that
correspond to each participant response beneath each bar.
}
\seealso{
\code{\link[ggplot2]{ggplot}}
}
