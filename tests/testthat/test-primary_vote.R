test_that("multiplication works", {
  get_house_primary_vote(division="Brisbane",year=2022)
  get_house_primary_vote(division="Brisbane",year=c(2019,2022))
  get_house_primary_vote(division=c("Brisbane","Perth"),year=c(2019,2022))
  get_house_primary_vote(division=c("Brisbane","Perth"),year=c(2019,2022),aggregation = TRUE)
  get_house_primary_vote(division=c("Brisbane","Perth"),year=c(2019,2022), polling_places = c("Yokine North"))
  get_house_primary_vote(state=c("TAS"),year=c(2019,2022),aggregation = TRUE)
  get_house_primary_vote(state=c("NT"),year=c(2019,2022),aggregation = TRUE, party_abb=c("ALP","CLP"))

  house_primary_vote_summary(division = "Brisbane", year=2022)
  house_primary_vote_summary(state = "QLD", year=2022,parties="ALP")
  house_primary_vote_summary(division="Franklin",parties="LP")

  house_primary_historic_plot("Canberra")
  house_primary_historic_plot("Canberra", parties =3,
                              parties_year = 2022,
                              include_others = TRUE )
  house_primary_historic_plot(division="Brisbane",parties=5,
                              merge_parties = list(LNP=c("LNP","LNQ","LP"),
                                                   ON=c("HAN","ON")))
  house_primary_comparison_plot(division = "Kooyong", year=2022,individualise_IND = TRUE)
  house_primary_comparison_plot(state="TAS",year=2022,parties=c("LP"),plot_format = "bar")







})
