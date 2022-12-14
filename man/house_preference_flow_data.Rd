% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/house_preferences_data.R
\name{house_preference_flow_data}
\alias{house_preference_flow_data}
\title{Flow of preferences in a division.}
\usage{
house_preference_flow_data(
  division,
  year,
  individualise_IND = TRUE,
  exclude_parties = NULL,
  exclude_rounds = 0
)
}
\arguments{
\item{division}{division}

\item{year}{election year}

\item{individualise_IND}{If set to TRUE, party abbreviations for each independent candidate will be changed
from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.}

\item{exclude_parties}{vector with party acronyms to exclude from plot}

\item{exclude_rounds}{If parties are excluded, include vector indicating from which rounds should them be excluded}
}
\value{
list with data frames with results for each round
}
\description{
retrieves data containing preferential voting rounds for a division in a particular election (as published by the AEC).
Can be filtered by polling place (including special modes of voting) or it can be presented as an aggregate per division.
}
\examples{
\dontrun{
#get preferences for Wills, 2019
get_house_preferences("Wills",2019)
show results for absent votes only
get_house_preferences("Wills",2019, polling_places=c("ABSENT"),aggregation = FALSE)

}
}
\keyword{houseconvenience}
