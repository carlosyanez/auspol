% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/house_results_plots.R
\name{house_results_tally}
\alias{house_results_tally}
\title{Election Tally}
\usage{
house_results_tally(
  year,
  individualise_IND = FALSE,
  merge_parties = NULL,
  add_majority_line = TRUE,
  include_labels = FALSE,
  extra_colours = NULL,
  include_data = FALSE
)
}
\arguments{
\item{year}{Election year}

\item{individualise_IND}{If set to TRUE, party abbreviations for each independent candidate will be changed
from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.}

\item{merge_parties}{list of parties to merge in one line following, the format list(NEWCODE=c(code1,code2,etc.))}

\item{add_majority_line}{add line representing 50\% +1 of the seats}

\item{include_labels}{If set to TRUE, the plot will include each value.}

\item{extra_colours}{manual mapping of colours for each party, as a named vector.}

\item{include_data}{If set to TRUE, data will be included under <<output_var>>$source_data (defaults to FALSE)}
}
\value{
preference flow, ggplot2 object
}
\description{
Plot party totals for a given election. Can aggregate parties into groups, amongst other filters.
}
\examples{
\dontrun{
# Basic example
house_results_tally(2013)
# Coalition votes put together
house_results_tally(2013, merge_parties = list(COAL=c("CLP","LP","LNP","NP")))
}
}
\keyword{houseplots}
