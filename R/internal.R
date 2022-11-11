#############################################################
### Internal functions ####
#############################################################



#' Obtain get data from filename
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr filter if_any
#' @importFrom rlang .data
#' @importFrom methods is
#' @param  division vector with division names
#' @param  year vector with election years
#' @param filename where the file is (parquet or zip containing parquet file)
#' @noRd
get_auspol_house_data <- function(filename,division=NULL, year=NULL){

  df <- load_auspol(filename)


  if(!is.null(year)){
      df <- df |> filter(if_any("Year",~ .x %in% year))

  }
  if(!is.null(division)){
      df <- df |> filter(if_any("DivisionNm", ~ .x %in% division))

  }

  return(df)
}

#' Check if division exists for a given year
#' @return data frame Division Name and state
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
#' @param  division vector with division names
#' @param  year vector with election years
#' @param filename where the file is (parquet or zip containing parquet file)
#' @noRd
check_division <- function(division,year){

  list_divisions() |>
    pivot_longer(-c("StateAb","DivisionID","DivisionNm"),
                 values_to="flag",names_to="ElectionYear") |>
    filter(if_any("flag", ~ .x==TRUE)) |>
    filter(if_any("DivisionNm", ~ .x==division)) |>
    filter(if_any("ElectionYear", ~ .x==year)) |>
    select(-matches(c("flag")))

}


