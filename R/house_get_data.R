#############################################################
### Basic functions to interact with House of Reps' data ####
#############################################################



#' Primary vote for House Elections
#' @description Get primary vote for one or more divisions, for one or more elections.
#' Data can be filtered by state, political party of polling locations. Results can be presented by
#' polling station or aggregated by division.
#' @return sf object with selected polygons
#' @param  division character vector with division names. When left blank, returns all division.
#' @param  year number vector with election years. When left blank, returns all years.
#' @param  state_abb  vector with state/territory acronym (e.g. NSW,VIC,QLD,etc.)
#' @param  party_abb  vector with party abbreviation (e.g. ALP,LIB,NP,GRN,etc.)
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#' @param  polling_places vector with regex for polling places
#' @importFrom dplyr filter group_by mutate summarise if_any
#' @importFrom rlang .data
#' @examples \dontrun{
#' # Primary vote in Brisbane, 2022 election
#' get_house_primary_vote(division="Brisbane",year=2022)
#' # Primary vote in Perth and Brisbane in 2019 and 2022 (aggregated)
#' get_house_primary_vote(division=c("Brisbane","Perth"),year=c(2019,2022),aggregation = TRUE)
#' # Primary vote for Greens candidates in Tasmania and the Northern Territory, 2019
#' get_house_primary_vote(state=c("TAS","NT"),year=2019,aggregation = TRUE, party_abb=c("GRN"))
#' }
#' @export
#' @keywords housegetdata
get_house_primary_vote <- function(division=NULL,
                                   year=NULL,
                                   state_abb = NULL,
                                   party_abb = NULL,
                                   aggregation=FALSE,
                                   polling_places=NULL
                                   ){

  df <- get_auspol_house_data("house_primary_vote.zip",division,year)


  if(!is.null(state_abb)){
    df <- df|>
         filter(if_any(c("StateAb"), ~ .x %in% state_abb))
  }

  if(!is.null(party_abb)){
    df <- df|>
      filter(if_any(c("PartyAb"), ~ .x %in% party_abb))
  }

  if(!is.null(polling_places)){

    df <- df |> mutate(place=TRUE)

    for(i in 1:length(polling_places)){

      df <- df |>
            mutate(place=.data$place & str_detect(.data$PollingPlace,polling_places[i]))

    }

    df <- df |>
      filter(if_any(c("place"), ~ .x==TRUE)) |>
      select(-any_of(c("place")))
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

#' Elected MPs
#' @description Retrieve list of elected MPs, filterable by division and year
#' @return data frame with list of elected MPs
#' @param  division character vector with division names. When left blank, returns all division.
#' @param  year number vector with election years. When left blank, returns all years.
#' @export
#' @keywords housegetdata
#' @examples \dontrun{
#' # Elected MPs in Melbourne and Cooper, 2019 and 2022
#' get_house_MPs(division = c("Melbourne","Cooper"),
#'               year = c(2019,2022))
#' }
get_house_MPs <- function(division=NULL,year=NULL){
  get_auspol_house_data("house_elected.zip",division,year)
}

#' @rdname get_house_MPs
#' @export
get_MPs <- get_house_MPs


#' Election turn out
#' @description Retrieve election turnout, filterable by division and year
#' @return data frame turnout numbers
#' @param  division character vector with division names. When left blank, returns all division.
#' @param  year number vector with election years. When left blank, returns all years.
#' @export
#' @keywords housegetdata
#' @examples \dontrun{
#' # Turnout in Riverina
#'  get_house_turnout(division="Riverina",yeat)
#' }
get_house_turnout <- function(division=NULL,year=NULL){
  get_auspol_house_data("house_turnout.zip",division,year)
}

#' Preferences
#' @description Retrieves preference flow, filterable by election and year. Results can be presented by
#' polling place - as retrived from the AEC - or aggregated by electoral division.
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
#' get_house_preferences("Wills",2019) |> head(10)
#' # aggregated version
#' get_house_preferences("Wills",2019,aggregation = TRUE)
#' # filtered by polling place
#' get_house_preferences("Wills",2019, polling_places=c("ABSENT")) |> head(10)
#'
#' }
get_house_preferences <- function(division,
                                  year,
                                  polling_places=NULL,
                                  aggregation=FALSE){

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


#' Two-party preferred flow
#' @description  Get flow from primary vote to finalists, for an division on a given election
#' @return dataframe with list of elected MPs
#' @param  division character vector with division names. When left blank, returns all division.
#' @param  year number vector with election years. When left blank, returns all years.
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#'@importFrom stringr str_c
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate summarise group_by if_else
#' @export
#' @keywords housegetdata
#' @examples \dontrun{
#' # get primary to finalist flow of preferences for Jagajaga in the 2013 election
#' get_house_2PF(division="Jagajaga",year=2013,aggregation = TRUE)
#' }
get_house_2PF <- function(division,year,aggregation=FALSE){

  division_info <- check_division(division,year)

  if(nrow(division_info)==1){

    filename <-  str_c("house_2CP_",division_info$StateAb,".zip")

    data <- get_auspol_house_data(filename,division,year) |>
      filter(.data$DivisionNm==division,.data$Year==year)

    if(aggregation){
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


#' Two-party-preferred summary
#' @description Get 2-party preferred party summary (Coalition vs ALP), as calculated by the AEC.
#' @return dataframe with list of elected MPs
#' @param  division character vector with division names. When left blank, returns all division.
#' @param  year number vector with election years. When left blank, returns all years.
#' @param  aggregation Whether to present division totals (defaults to FALSE)
#' @param  state_abb  vector with state/territory acronym (e.g. NSW,VIC,QLD,etc.)
#'@importFrom stringr str_c
#' @importFrom rlang .data
#' @importFrom  dplyr filter mutate summarise group_by if_else
#' @export
#' @keywords housegetdata
#' @examples \dontrun{
#' get_house_2PP(division = "Indi",
#' year=2016,
#' aggregation = TRUE)
#' }
get_house_2PP <- function(division=NULL,
                          year=NULL,
                          state_abb = NULL,
                          aggregation=FALSE){


    df <- get_auspol_house_data("house_2PP.zip",division,year)

    if(!is.null(state_abb)){
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


