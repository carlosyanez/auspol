####################################################################
### Convenience Functions to interact with House of Reps' data #####
####################################################################


#' Helper function to download  data
#' @importFrom dplyr select mutate if_else group_by summarise filter arrange pull desc distinct across starts_with left_join relocate
#' @importFrom tidyselect any_of where
#' @importFrom stringr  str_c
#' @importFrom utils head
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang .data
#' @param division vector with names of electoral divisions (e.g. "Banks", "Wills","Indi")
#' @param state if divisions are not provide, provide a vector with state initials e.g. c("NT","TAS")
#' @param year numeric vector with election years (from 2004), defaults to all.
#' @param parties which parties to include in the summary. All (default), a vector of strings
#'  with the party acronyms (see list_parties()), or a number indicating the top n parties from a certain year.
#' @param parties_year If *parties* has is NULL or a number, this indicates if the selection needs to be from
#' a certain year (.e.g only select the historical data for the three top parties in 2012)
#' @param include_others  Boolean used along *parties* to included the remaining votes in one "Other" category.
#' @param include_informal Boolean to add informal votes in addition to the party selection.
#'  Informal votes will be included if no parties are selected, or the top n parties are selected,
#'   and it happens to be in the top n - even if this flag is set to false.
#' @param include_names whether to include the candidates name and surname in the extract (TRUE by default).
#' @param individualise_IND If set to TRUE, party abbreviations for each independent candidate will be changed
#' from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.
#' @param wide_format Whether to present the result in long format, like the AEC's source, or a year-by-year summary.
#' Options include NULL (no summarisation, default), "OrdinaryVotes" (absolute numbers), "Percentage_with_Informal"
#'  and  "Percentage" (which is the percentage counted on elections).
#' @return dataframe
#' @export
#' @keywords houseconvenience
house_primary_vote_summary <- function(division=NULL,
                                 state=NULL,
                                 year=NULL,
                                 parties=NULL,
                                 parties_year=NULL,
                                 include_others=FALSE,
                                 include_informal=FALSE,
                                 include_names = TRUE,
                                 individualise_IND = FALSE,
                                 wide_format=NULL){

  #get data

  if(is.null(division)){
    if(is.null(state)) stop("If don't provide divisions,pease provided a list of states")

    division <-list_divisions() |>
               filter(.data$StateAb %in% c(state)) |>
               pivot_longer(-c("StateAb","DivisionID","DivisionNm"),values_to = "flag",names_to = "year")

    if(!is.null(year)){
      division <- division |> filter(.data$year %in% year)
    }

    division <- division |>
                filter(.data$flag) |>
                pull(.data$DivisionNm) |>
                unique()

  }

  agg <- TRUE
  votes   <- get_house_primary_vote(division=division,year=year,aggregation = agg) |>
    select(any_of(c("Year","StateAb" ,"DivisionNm",
                    "GivenNm"   ,"Surname",
                    "Elected","PartyAb","PartyNm",
                    "OrdinaryVotes")))

  if(!is.null(state)){
    votes <- votes |>
             filter(.data$StateAb %in% state)

  }

  # individualised Independent?
  if(individualise_IND){
    votes <- votes |>
      mutate(PartyAb=if_else(.data$PartyAb=="IND",
                             str_c("IND-",.data$Surname),
                             .data$PartyAb))
  }


  totals <- votes |>
    group_by(.data$Year,.data$DivisionNm) |>
    summarise(Total = sum(.data$OrdinaryVotes,na.rm = TRUE),.groups = "drop")

  totals_no_inf <-  votes |>
    filter(.data$PartyAb!="Informal") |>
    group_by(.data$Year,.data$DivisionNm) |>
    summarise(Total_no_inf = sum(.data$OrdinaryVotes,na.rm = TRUE),.groups = "drop")

  totals <- totals |> left_join(totals_no_inf,by=c("Year","DivisionNm"))

  # determine list of parties

  if(is.null(parties)){

    if(is.null(parties_year)){
      parties_year <- unique(votes$Year)
    }

    parties <- votes |>
      filter(.data$Year %in% parties_year) |>
      pull(.data$PartyAb)
  }


  if(is(parties,"numeric")){
    if(is.null(parties_year)){
      parties_year <- max(votes$Year)
    }

    parties <- votes |> filter(.data$Year %in% parties_year) |>
      arrange(desc(.data$OrdinaryVotes)) |>
      head(parties) |>
      pull(.data$PartyAb)
  }


  # add informal votes?

  if(!("Informal" %in% parties)){
    if(include_informal){
      parties <- c(parties, "Informal")
    }else{
      votes <- votes |>
      filter(.data$PartyAb!="Informal")
    }
  }

  parties <- unique(parties)

  # aggregate other parties?
  if(include_others){

    votes <- votes |>
      mutate(PartyAb=if_else(.data$PartyAb %in% parties,.data$PartyAb,"Other"))

    party_list <- votes |>
      distinct(.data$PartyAb,.data$PartyNm) |>
      group_by(.data$PartyAb) |>
      summarise(PartyNm=str_c(.data$PartyNm,collapse=", "))

    votes <- votes |>
      mutate(PartyAb=if_else(.data$PartyAb %in% parties,.data$PartyAb,"Other")) |>
      group_by(across(!starts_with(c("Elected","OrdinaryVotes","PartyNm"))))  |>
      summarise(OrdinaryVotes=sum(.data$OrdinaryVotes,na.rm = TRUE),
                Elected=any(.data$Elected),
                .groups="drop") |>
      left_join(party_list,by="PartyAb") |>
      relocate("PartyNm",.after="PartyAb")
  }else{
    votes <- votes |>
      filter(.data$PartyAb %in% parties)
  }


  df <- votes |> left_join(totals,by=c("Year","DivisionNm")) |>
    mutate(Percentage_with_Informal=100*.data$OrdinaryVotes/.data$Total,
           Percentage = 100*.data$OrdinaryVotes/.data$Total_no_inf) |>
    select(-any_of(c("Total","Total_no_inf"))) |>
    relocate("Elected",.after = "Percentage")

  if(!is.null(wide_format)){

    if(!(wide_format %in% colnames(df))) stop("Invalid wide option")

    data_cols <- c("Year","DivisionNm","StateAb",
                   "PartyNm","PartyAb")

    df <- df |>
      select(all_of(c(data_cols,wide_format))) |>
      pivot_wider(values_from = !!wide_format,names_from= .data$Year) |>
      group_by(across(data_cols[data_cols!="Year"])) |>
      summarise(PartyNm=str_c(.data$PartyNm,collapse=", "),
                across(where(is.numeric),~sum(.x,na.rm = TRUE)),
                .groups="drop")
  }

  if(!include_names){
    df <- df |> select(-any_of(c("Surname","GivenNm")))
  }

  return(df)
}

