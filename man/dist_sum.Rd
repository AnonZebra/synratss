% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_sum.R
\name{dist_sum}
\alias{dist_sum}
\title{Calculate sum euclidean distances in color space}
\usage{
dist_sum(x, method = "euclidean")
}
\arguments{
\item{x}{a 3 row, 3 columns data frame with color space specifications}

\item{fmt}{specifies type of distance to compute ("euclidean"
for Euclidean distances, "manhattan" for Manhattan/city block distances
- "euclidean" is standard)}
}
\description{
Calculates the sum of euclidean or Manhattan/city block
distances, using a hex color code data frame with three rows (one for each
color axis - e. g. [R] [G] [B]) and three columns (one for each response).
}
\seealso{
\code{\link[stats]{dist}} \code{\link[base]{t}}
}
