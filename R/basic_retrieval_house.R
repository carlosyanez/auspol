#######################################################
### Functions to interact with House of Reps' data ####
#######################################################


#' Obtain primary vote results for house of representatives
#' @return sf object with selected polygons
#' @importFrom dplyr filter
#' @param  division vector with division names
#' @param  election_year vector with election years
get_auspol_house_data <- function(filename,division, election_year){

  df <- load_auspol(filename)

  # filter by year, ignore if year == "all"
  if(!("all" %in% election_year)&(class(election_year)=="numeric")){

    df  <- df |> filter(Year %in% election_year)
  }

  #filter by division

  if(!("all" %in% division)){

    df <- df |> filter(DivisionNm %in% division)
  }

  return(df)
}


#' Obtain primary vote results for house of representatives
#' @return sf object with selected polygons
#' @importFrom dplyr filter
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @export get_house_primary_vote
get_house_primary_vote <- function(division="all", election_year="all"){

  get_auspol_house_data("house_primary_vote.zip",division,election_year)
  }

#' Obtain primary vote results for house of representatives
#' @return sf object with selected polygons
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @export get_house_primary_vote
get_house_MPs <- function(division="all",election_year="all"){
  get_auspol_house_data("house_elected.zip",division,election_year)
}

get_MPs <- get_house_MPs

get_house_turnout <- function(division="all",election_year="all"){
  get_auspol_house_data("house_turnout.zip",division,election_year)
}





#' Obtain primary vote results for house of representatives
#' @return sf object with selected polygons
#' @importFrom dplyr filter
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @export get_house_primary_vote
list_divisions <- function(){
  load_auspol("house_electorates.zip")
}

list_electorates <- list_divisions





