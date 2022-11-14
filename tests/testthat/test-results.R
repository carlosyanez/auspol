test_that("multiplication works", {
  get_house_MPs(division="Bennelong")

  get_house_MPs(year=2013)


  house_results_tally(2013)
  house_results_tally(2013, merge_parties = list(COAL=c("CLP","LP","LNP","NP")))
  house_results_tally(2022,
                      individualise_IND = TRUE,
                      merge_parties = list(COAL=c("CLP","LP","LNP","NP"),
                                           TEAL=c("IND-DANIEL","IND-RYAN",
                                                  "IND-SCAMPS","IND-CHANEY",
                                                  "IND-SPENDER","IND-STEGGALL",
                                                  "IND-HAINES","IND-TINK")
                      ))



  house_results_historic(merge_parties = list(COAL=c("CLP","LP","LNP","NP")),
                         parties =c("COAL","ALP","GRN"),
                         include_other=TRUE)


})
