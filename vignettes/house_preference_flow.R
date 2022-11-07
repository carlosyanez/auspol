## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
library(auspol)


## -----------------------------------------------------------------------------
get_house_preferences("Wills",2019)


## -----------------------------------------------------------------------------
get_house_preferences("Wills",2019,aggregation = FALSE) |> head(10)
                      

## -----------------------------------------------------------------------------
get_house_preferences("Wills",2019, polling_places=c("ABSENT"),aggregation = FALSE) |> head(10)


## -----------------------------------------------------------------------------
house_preference_flow_data(division = "Wills", year=2019)

## -----------------------------------------------------------------------------
house_preference_flow_plot(division = "Wills",year=2019) +
  labs(title="Prefence Flow for Wills, 2019")

## -----------------------------------------------------------------------------
house_preference_flow_plot(division = "Wills",year=2019,var="Preference Count") +
  labs(title="Prefence Flow for Wills, 2019")


## -----------------------------------------------------------------------------
house_preference_flow_plot(division = "Warringah",year=2022) +
  labs(title="Prefence Flow for Warringah, 2022")


## -----------------------------------------------------------------------------
house_preference_flow_plot(division = "Warringah",year=2022, exclude_parties = c("LP","IND-STEGGALL")) +
  labs(title="Flow of preference votes to Liberal and Teals in Warringah, 2022")


## -----------------------------------------------------------------------------
#adding teal
house_preference_flow_plot(division = "Warringah",year=2022, 
                     exclude_parties = c("LP","IND-STEGGALL"),
                     extra_colours = c("IND-STEGGALL"="#008080")) +
  labs(title="Flow of preference votes to Liberal and Teals in Warringah, 2022")

## -----------------------------------------------------------------------------
p <- house_preference_flow_plot(division = "Warringah",year=2022, 
                     exclude_parties = c("LP","IND-STEGGALL"),
                     extra_colours = c("IND-STEGGALL"="#008080"),
                     include_data=TRUE
                     )

p$source_data[[1]]

