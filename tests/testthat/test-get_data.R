test_that("getting data", {
  get_house_primary_vote(division = c("Cowan","Curtin"),year=c(2019,2022))

  get_house_primary_vote(division = c("Cowan","Curtin"),
                         year=c(2019,2022),
                         polling_places = "Perth")

  get_house_primary_vote(division = c("Cowan","Curtin"),
                         year=c(2019,2022),
                         aggregation = TRUE)

  get_house_preferences("Wills",2019)

  get_house_preferences("Wills",2019,
                        aggregation = TRUE)

  get_house_preferences("Wills",2019, polling_places=c("ABSENT"))

  get_house_2PF(division="Jagajaga",year=2013,aggregation = TRUE)

  get_house_2PP(division = "Indi",
                year=2016,
                aggregation = TRUE)

  get_house_MPs(division = c("Melbourne","Cooper"),
                year = c(2019,2022))

  get_house_turnout(year=2019)

})
