#######################################################
### Functions to interact with House of Reps' data ####
#######################################################


#' Obtain get data from filename
#' @return data frame with data from file, filtered by division and election year
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


#' Obtain primary vote per candidate for a given set of divisions, for a set of years
#' @return sf object with selected polygons
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @param  state_abb  vector with state/territory acronym (e.g. NSW,VIC,QLD,etc.)
#' @param  party_abb  vector with party abbreviation (e.g. ALP,LIB,NP,GRN,etc.)
#' @param  aggregation Wether to present division totals (defaults to FALSE)
#' @importFrom dplyr filter group_by mutate
#' @export
#' @keywords house_get_data
#' #' @examples \dontrun{
#' # Get list of primary votes for Wills and Cooper in 2022
#' get_house_primary_vote(c("Wills","Cooper),2022)
#' # Get primary vote for all Green Candidates across Victoria, 2019 election
#' get_house_primary_vote(election_year=2019,state_abb="VIC",party_abb="GRN")
#' }
get_house_primary_vote <- function(division="all",
                                   election_year="all",
                                   state_abb = "all",
                                   party_abb = "all",
                                   aggregation=FALSE
                                   ){

  df <- get_auspol_house_data("house_primary_vote.zip",division,election_year)

  if(!("all" %in% state)){
    df <- df %>%
          filter(StateAb %in% state_abb)
  }

  if(!("all" %in% party_abb)){
    df <- df %>%
      filter(PartyAb %in% party_abb)
  }

  if(aggregation){
    df %>% group_by(Year,StateAb, DivisionID,DivisionNm,
                    CandidateID, Surname, GivenNm,
                    BallotPosition, Elected, HistoricElected,
                    PartyAb, PartyNm , SittingMemberFl) |>
      summarise(OrdinaryVotes=sum(OrdinaryVotes),.groups = "drop")

  }

  return(df)

  }

#' Reetrieve elected MPs, for groups of divisions, for given year
#' @return dataframe with list of elected MPs
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @export get_house_MPs
#' @keywords house_get_data
get_house_MPs <- function(division="all",election_year="all"){
  get_auspol_house_data("house_elected.zip",division,election_year)
}

get_MPs <- get_house_MPs

#' Get list of with turnout all elected MPs, for groups of divisions, for given year
#' @return dataframe with list of elected MPs
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @export get_house_turnout
#' @keywords house_get_data
get_house_turnout <- function(division="all",election_year="all"){
  get_auspol_house_data("house_turnout.zip",division,election_year)
}








