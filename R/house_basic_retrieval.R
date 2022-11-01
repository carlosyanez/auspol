#############################################################
### Basic functions to interact with House of Reps' data ####
#############################################################


#' Obtain get data from filename
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom methods is
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @param filename where the file is (parquet or zip containing parquet file)
#' @noRd
get_auspol_house_data <- function(filename,division, election_year){

  df <- load_auspol(filename)

  # filter by year, ignore if year == "all"
  if(!("all" %in% election_year)&(is(election_year,"numeric"))){

    df  <- df |> filter(.data$Year %in% election_year)
  }

  #filter by division

  if(!("all" %in% division)){

    df <- df |> filter(.data$DivisionNm %in% division)
  }

  return(df)
}

#' Check if division exists for a given year
#' @return data frame Division Name and state
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @param filename where the file is (parquet or zip containing parquet file)
#' @noRd
check_division <- function(division,year){

  list_divisions() |>
    pivot_longer(-c("StateAb","DivisionID","DivisionNm"),
                 values_to="flag",names_to="ElectionYear") |>
    filter(.data$flag) |>
    filter(.data$DivisionNm==division & .data$ElectionYear==year) |>
    select(-.data$flag)

}

#' Obtain primary vote per candidate for a given set of divisions, for a set of years
#' @return sf object with selected polygons
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @param  state_abb  vector with state/territory acronym (e.g. NSW,VIC,QLD,etc.)
#' @param  party_abb  vector with party abbreviation (e.g. ALP,LIB,NP,GRN,etc.)
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#' @importFrom dplyr filter group_by mutate summarise
#' @importFrom rlang .data
#' @export
#' @keywords housegetdata
get_house_primary_vote <- function(division="all",
                                   election_year="all",
                                   state_abb = "all",
                                   party_abb = "all",
                                   aggregation=FALSE
                                   ){

  df <- get_auspol_house_data("house_primary_vote.zip",division,election_year)

  if(!("all" %in% state_abb)){
    df <- df|>
          filter(.data$StateAb %in% state_abb)
  }

  if(!("all" %in% party_abb)){
    df <- df|>
      filter(.data$PartyAb %in% party_abb)
  }

  if(aggregation){
    df <- df|> group_by(.data$Year,.data$StateAb,
                  .data$DivisionID,.data$DivisionNm,
                  .data$CandidateID,.data$Surname,.data$GivenNm,
                  .data$BallotPosition,.data$Elected,.data$HistoricElected,
                  .data$PartyAb,.data$PartyNm ,.data$SittingMemberFl) |>
      summarise(OrdinaryVotes=sum(.data$OrdinaryVotes),.groups = "drop")

  }

  return(df)

  }

#' Reetrieve elected MPs, for groups of divisions, for given year
#' @return dataframe with list of elected MPs
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @export
#' @keywords housegetdata
get_MPs <- function(division="all",election_year="all"){
  get_auspol_house_data("house_elected.zip",division,election_year)
}

get_house_MPs <- get_MPs


#' Get list of with turnout all elected MPs, for groups of divisions, for given year
#' @return dataframe with list of elected MPs
#' @param  division character with division name
#' @param  election_year number with election year
#' @export
#' @keywords housegetdata
get_house_turnout <- function(division="all",election_year="all"){
  get_auspol_house_data("house_turnout.zip",division,election_year)
}

#' Get list of with turnout all elected MPs, for groups of divisions, for given year
#' @return dataframe with list of elected MPs
#' @param  division vector with division names
#' @param  election_year vector with election years
#' @importFrom   stringr str_c
#' @importFrom rlang .data
#' @export
#' @keywords housegetdata
get_house_preferences <- function(division,election_year){

  division_info <- check_division(division,election_year)

  if(nrow(division_info)==1){

  filename <-  str_c("house_flow_",division_info$StateAb,".zip")

  data <- get_auspol_house_data(filename,division,election_year) |>
          filter(.data$DivisionNm==division,.data$Year==election_year)

  return(data)

  }else{
    message(str_c("Division of ", division," didn't exist for  the ", election_year," election."))
  }
}

get_preferences <- get_house_preferences


#' Get 2-party preferred flow (as calculated by the ABS), for an division on a given election
#' @return dataframe with list of elected MPs
#' @param  division character with division name
#' @param  election_year number with election year
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#'@importFrom stringr str_c
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate summarise group_by if_else
#' @export
#' @keywords housegetdata
get_house_2PF <- function(division,election_year,aggregation=FALSE){

  division_info <- check_division(division,election_year)

  if(nrow(division_info)==1){

    filename <-  str_c("house_2CP_",division_info$StateAb,".zip")

    data <- get_auspol_house_data(filename,division,election_year) |>
      filter(.data$DivisionNm==division,.data$Year==election_year)

    if(aggregation){
      print(1)
      data <- data|>
        group_by(.data$Year,.data$StateAb,
                    .data$DivisionId,.data$DivisionNm,
                    .data$FromCandidateId,.data$FromCandidatePartyAb,.data$FromCandidatePartyNm,
                    .data$FromCandidateSurname,.data$FromCandidateGivenNm,.data$FromCandidateBallotPosition,
                    .data$ToCandidateId,.data$ToCandidatePartyAb,.data$ToCandidatePartyNm,
                    .data$ToCandidateSurname,.data$ToCandidateGivenNm,.data$ToCandidateBallotPosition) |>
        summarise(TransferCount=sum(.data$TransferCount),.groups = "drop") |>
        mutate(FromCandidateId=if_else(.data$FromCandidateId==0,.data$ToCandidateId,.data$FromCandidateId),
               FromCandidatePartyAb=if_else(.data$FromCandidatePartyAb=="",.data$ToCandidatePartyAb,.data$FromCandidatePartyAb),
               FromCandidatePartyNm=if_else(.data$FromCandidatePartyNm=="",.data$ToCandidatePartyNm,.data$FromCandidatePartyNm),
               FromCandidateSurname=if_else(.data$FromCandidateSurname=="First Preferences",
                                            .data$ToCandidateSurname,.data$FromCandidateSurname),
               FromCandidateGivenNm=if_else(.data$FromCandidateGivenNm=="",.data$ToCandidateGivenNm,.data$FromCandidateGivenNm),
               FromCandidateBallotPosition=if_else(.data$FromCandidateBallotPosition==0,
                                                   .data$ToCandidateBallotPosition,.data$FromCandidateBallotPosition)
               )

    }

    return(data)

  }else{
    message(str_c("Division of ", division," didn't exist for  the ", election_year," election."))
  }
}


#' Get 2-party preferred party summary (Coalition vs ALP)
#' @return dataframe with list of elected MPs
#' @param  division character with division name
#' @param  election_year number with election year
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#' @param  state_abb  vector with state/territory acronym (e.g. NSW,VIC,QLD,etc.)
#'@importFrom stringr str_c
#' @importFrom rlang .data
#' @importFrom  dplyr filter mutate summarise group_by if_else
#' @export
#' @keywords housegetdata
get_house_2PP <- function(division="all",
                          election_year="all",
                          state_abb = "all",
                          aggregation=FALSE){


    df <- get_auspol_house_data("house_2PP.zip",division,election_year)

    if(!("all" %in% state_abb)){
      df <- df|>
        filter(.data$StateAb %in% state_abb)
    }


    if(aggregation){
      df <- df|>
        group_by(.data$Year,.data$StateAb,
                 .data$DivisionID,.data$DivisionNm) |>
        summarise(`Liberal/National Coalition Votes`=sum(.data$`Liberal/National Coalition Votes`),
                  `Australian Labor Party Votes`=sum(.data$`Australian Labor Party Votes`),
                  TotalVotes = sum(.data$TotalVotes),
                  .groups = "drop") |>
        mutate(`Liberal/National Coalition Percentage`=.data$`Liberal/National Coalition Votes`/.data$TotalVotes,
               `Australian Labor Party Percentage`=.data$`Australian Labor Party Votes`/.data$TotalVotes)



    }

    return(df)

}

