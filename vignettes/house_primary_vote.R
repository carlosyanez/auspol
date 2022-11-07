## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
library(auspol)
get_house_primary_vote()


## -----------------------------------------------------------------------------
get_house_primary_vote(division="Brisbane",year=2022)

## -----------------------------------------------------------------------------
get_house_primary_vote(division="Brisbane",year=c(2019,2022))

## -----------------------------------------------------------------------------
get_house_primary_vote(division=c("Brisbane","Perth"),year=c(2019,2022))

## -----------------------------------------------------------------------------
get_house_primary_vote(division=c("Brisbane","Perth"),year=c(2019,2022), polling_places = c("Yokine North"))

## -----------------------------------------------------------------------------
get_house_primary_vote(division=c("Brisbane","Perth"),year=c(2019,2022),aggregation = TRUE)

## -----------------------------------------------------------------------------
get_house_primary_vote(state=c("TAS"),year=c(2019,2022),aggregation = TRUE)

## -----------------------------------------------------------------------------
get_house_primary_vote(state=c("NT"),year=c(2019,2022),aggregation = TRUE, party_abb=c("ALP","CLP"))



## -----------------------------------------------------------------------------
house_primary_vote_summary(division = "Brisbane", year=2022)

## -----------------------------------------------------------------------------

house_primary_vote_summary(state = "QLD", year=2022,parties="ALP")


## -----------------------------------------------------------------------------
house_primary_vote_summary(division="Franklin",parties="LP")

