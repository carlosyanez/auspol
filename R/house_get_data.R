#############################################################
### Basic functions to interact with House of Reps' data ####
#############################################################



#' Obtain primary vote per candidate for a given set of divisions, for a set of years
#' @return sf object with selected polygons
#' @param  division vector with division names
#' @param  year vector with election years
#' @param  state_abb  vector with state/territory acronym (e.g. NSW,VIC,QLD,etc.)
#' @param  party_abb  vector with party abbreviation (e.g. ALP,LIB,NP,GRN,etc.)
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#' @param  polling_places vector with polling places
#' @importFrom dplyr filter group_by mutate summarise if_any
#' @importFrom rlang .data
#' @export
#' @keywords housegetdata
get_house_primary_vote <- function(division="all",
                                   year="all",
                                   state_abb = "all",
                                   party_abb = "all",
                                   aggregation=FALSE,
                                   polling_places=NULL
                                   ){

  df <- get_auspol_house_data("house_primary_vote.zip",division,year)

  if(!("all" %in% state_abb)){
    df <- df|>
         filter(if_any(c("StateAb"), ~ .x %in% state_abb))
  }

  if(!("all" %in% party_abb)){
    df <- df|>
      filter(if_any(c("PartyAb"), ~ .x %in% party_abb))
  }

  if(!is.null(polling_places)){
    df <- df |>
      filter(if_any(c("PollingPlace"), ~ .x %in% polling_places))
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
#' @param  year vector with election years
#' @export
#' @keywords housegetdata
get_house_MPs <- function(division="all",year="all"){
  get_auspol_house_data("house_elected.zip",division,year)
}

#' @rdname get_house_MPs
#' @export
get_MPs <- get_house_MPs


#' Get list of with turnout all elected MPs, for groups of divisions, for given year
#' @return dataframe with list of elected MPs
#' @param  division character with division name
#' @param  year number with election year
#' @export
#' @keywords housegetdata
get_house_turnout <- function(division="all",year="all"){
  get_auspol_house_data("house_turnout.zip",division,year)
}

#' Get list of with turnout all elected MPs, for groups of divisions, for given year
#' @return dataframe with list of elected MPs
#' @param  division vector with division names
#' @param  year vector with election years
#' @param  polling_places list of polling places
#' @param  aggregation whether to aggregate by division
#' @importFrom dplyr filter if_any
#' @importFrom   stringr str_c str_detect
#' @importFrom rlang .data
#' @include internal.R
#' @export
#' @keywords housegetdata
#' @examples \dontrun{
#' # basic use
#' get_house_preferences("Wills",2019)
#' # disaggregated version
#' get_house_preferences("Wills",2019,aggregation = FALSE) |> head(10)
#' # filtered by polling place
#' get_house_preferences("Wills",2019, polling_places=c("ABSENT"),aggregation = FALSE) |> head(10)
#' }
get_house_preferences <- function(division,
                                  year,
                                  polling_places=NULL,
                                  aggregation=TRUE){

  division_info <- check_division(division,year)

  if(nrow(division_info)!=1) stop(str_c("Division of ", division," didn't exist for  the ", year," election."))

  filename <-  str_c("house_flow_",division_info$StateAb,".zip")

  data <- get_auspol_house_data(filename,division,year) |>
          filter(if_any("DivisionNm", ~ .x==division))       |>
          filter(if_any("Year", ~ .x==year))

  if(!is.null(polling_places)){
    data <- data |>
           filter(if_any("PPNm", ~ .x %in% polling_places))
  }

  if(aggregation){

    data <- data |>
      filter(if_any("CalculationType", ~ str_detect(.x,"Count"))) |>
      group_by(across(starts_with(c("Year","StateAb","Division","CountNum",
                                     "BallotPosition","CandidateId","Surname","GivenNm",
                                    "PartyAb","PartyNm","CalculationType"))))  |>
      summarise(CalculationValue=sum(.data$CalculationValue,na.rm=TRUE),.groups = "drop")

  }

  return(data)


}

#' @rdname get_house_preferences
#' @export
get_preferences <- get_house_preferences


#' Get 2-party preferred flow (as calculated by the ABS), for an division on a given election
#' @return dataframe with list of elected MPs
#' @param  division character with division name
#' @param  year number with election year
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#'@importFrom stringr str_c
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate summarise group_by if_else
#' @export
#' @keywords housegetdata
get_house_2PF <- function(division,year,aggregation=FALSE){

  division_info <- check_division(division,year)

  if(nrow(division_info)==1){

    filename <-  str_c("house_2CP_",division_info$StateAb,".zip")

    data <- get_auspol_house_data(filename,division,year) |>
      filter(.data$DivisionNm==division,.data$Year==year)

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
    message(str_c("Division of ", division," didn't exist for  the ", year," election."))
  }
}


#' Get 2-party preferred party summary (Coalition vs ALP)
#' @return dataframe with list of elected MPs
#' @param  division character with division name
#' @param  year number with election year
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#' @param  state_abb  vector with state/territory acronym (e.g. NSW,VIC,QLD,etc.)
#'@importFrom stringr str_c
#' @importFrom rlang .data
#' @importFrom  dplyr filter mutate summarise group_by if_else
#' @export
#' @keywords housegetdata
get_house_2PP <- function(division="all",
                          year="all",
                          state_abb = "all",
                          aggregation=FALSE){


    df <- get_auspol_house_data("house_2PP.zip",division,year)

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


