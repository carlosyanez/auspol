####################################################################
### Convenience Functions to interact with House of Reps' data #####
####################################################################



primary_vote_summary <- function(division,
                                 year="all",
                                 parties=NULL,
                                 parties_year=NULL,
                                 include_others=FALSE,
                                 include_informal=FALSE,
                                 include_names = FALSE,
                                 individualise_IND = FALSE,
                                 wide_format="no"){

  #get data
  agg <- TRUE
  votes   <- get_house_primary_vote(division=division,election_year=year,aggregation = agg) |>
    select(any_of(c("Year","StateAb" ,"DivisionNm",
                    "GivenNm"   ,"Surname",
                    "Elected","PartyAb","PartyNm",
                    "OrdinaryVotes")))

  totals <- votes |>
    group_by(.data$Year) |>
    summarise(Total = sum(.data$OrdinaryVotes,na.rm = TRUE))


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

  # individualised Independent?
  if(("IND" %in% parties)&individualise_IND){
    votes <- votes |>
             mutate(PartyAb=if_else(.data$PartyAb=="IND",
                                    str_c("IND-",.data$Surname),
                                    .data$PartyAb))

    p_in_votes <- votes |>
                  filter(str_detect(.data$PartyAb,"IND")) |>
                  distinct(.data$PartyAb) |>
                  pull()


    parties <- c(parties[parties!="IND"],p_in_votes)

  }

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




  df <- votes %>% left_join(totals,by="Year") |>
    mutate(Percentage=100*.data$OrdinaryVotes/.data$Total) |>
    select(-any_of(c("Total"))) |>
    relocate(.data$Elected,.after = "Percentage")

  if(wide_format=="votes"){
    df <- df |>
      select(-any_of(c("Percentage","Elected"))) |>
      pivot_wider(values_from = .data$OrdinaryVotes,names_from= .data$Year) |>
      group_by(.data$StateAb ,.data$DivisionNm,.data$PartyAb) |>
      summarise(PartyNm=str_c(.data$PartyNm,collapse=", "),
                across(where(is.numeric),~sum(.x,na.rm = TRUE)),
                .groups="drop")
  }

  if(wide_format=="percentages"){
    df <- df |>
      select(-any_of(c("OrdinaryVotes","Elected"))) |>
      pivot_wider(values_from =  .data$Percentage,names_from= .data$Year) |>
      group_by(.data$StateAb ,.data$DivisionNm,.data$PartyAb) |>
      summarise(PartyNm=str_c(.data$PartyNm,collapse=", "),
                across(where(is.numeric),~sum(.x,na.rm = TRUE)),
                .groups="drop")
  }

  return(df)
}
primary_vote_summary("Goldstein",2022)

