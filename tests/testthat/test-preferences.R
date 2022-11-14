test_that("preferences", {
  get_house_preferences("Wills",2019)
  get_house_preferences("Wills",2019,aggregation = FALSE) |> head(10)
  get_house_preferences("Wills",2019, polling_places=c("ABSENT"),aggregation = FALSE) |> head(10)

  house_preference_flow_data(division = "Wills", year=2019)
  house_preference_flow_plot(division = "Wills",year=2019) +
    labs(title="Preference Flow for Wills, 2019")

  house_preference_flow_plot(division = "Wills",year=2019,var="Preference Count") +
    labs(title="Preference Flow for Wills, 2019")
  house_preference_flow_plot(division = "Warringah",year=2022) +
    labs(title="Preference Flow for Warringah, 2022")
  house_preference_flow_plot(division = "Warringah",year=2022, exclude_parties = c("LP","IND-STEGGALL")) +
    labs(title="Flow of preference votes to Liberal and Teal in Warringah, 2022")

  house_preference_flow_plot(division = "Warringah",year=2022,
                             exclude_parties = c("LP","IND-STEGGALL"),
                             extra_colours = c("IND-STEGGALL"="#008080")) +
    labs(title="Flow of preference votes to Liberal and Teal in Warringah, 2022")


  house_preference_flow_plot(division = "Warringah",year=2022,
                             exclude_parties = c("LP","IND-STEGGALL"),
                             extra_colours = c("IND-STEGGALL"="#008080"),
                             include_data=TRUE
  )

  get_house_2PF(division = "Lingiari",year=2013,aggregation = TRUE)


  house_2PF_plot("Lingiari",2013,plot_format = "alluvial")
  house_2PF_plot(division="Burt",
                 year=2022,
                 plot_format = "bar")

  get_house_2PP(division = "Fraser",year=2004,aggregation = TRUE)

  house_2PP_comparison_plot(year=2022,state="VIC")

  house_2PP_historical_plot(division="Aston")


})
