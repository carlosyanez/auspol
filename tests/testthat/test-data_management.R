test_that("data management", {
  data_census_update()
  data_census_info()
  data_census_delete()
  find_census_cache()
})
