#############################################################
### Internal funcions ####
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
#' @importFrom tidyselect matches
#' @param  division vector with division names
#' @param  election_year vector with election years
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


#' Auxiliary function to convert preferences to alluvial
#' @param preferences output of preference_flow_data()
#' @param name_correction number to correct round index
#' @param var either "Preference Count" or "Percent"
#' @importFrom dplyr mutate rename select group_by summarise distinct filter if_any any_of bind_rows across
#' @importFrom tibble tibble
#' @returns preferences in alluvial format
#' @noRd
preference_distributor <- function(preferences,name_correction, var="Preference Count"){

  no_rounds <- length(preferences)

  first_round <-  preferences[[1]] |>
    mutate(Candidate = .data$PartyAb) |>
    rename(count=!!var) |>
    select(any_of(c("Candidate","PartyAb","count")))     |>
    group_by(across(c("Candidate","PartyAb"))) |>
    summarise(Count=sum(.data$count,na.rm = TRUE),.groups="drop")

  data <- tibble()

  for(i in 0:(no_rounds-1)){


    last_round <- preferences[[no_rounds-i]] |>
      mutate(Candidate = .data$PartyAb) |>
      distinct(.data$Candidate)

    if(nrow(data)!=0){
      last_round <- last_round |>
        filter(if_any("Candidate", ~ !(.x %in% data$Candidate)))
    }


    round       <-      first_round |>
      filter(if_any("Candidate",~ .x %in% last_round$Candidate)) |>
      select(any_of(c("Candidate","Count"))) |>
      mutate(Round=(no_rounds-i))


    for(j in 1:(no_rounds-i)){
      round_name <- str_c("Round_",j+name_correction)

      round       <- round |>
        mutate(new=.data$Candidate) |>
        rename(!!round_name:=.data$new)
    }


    data <- bind_rows(data,round)

  }

  return(data)
}

#' Convert preferences in lode format
#' @importFrom  dplyr mutate select row_number  any_of bind_cols
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_c str_remove str_order
#' @importFrom forcats fct_relevel
#' @importFrom rlang .data :=
#' @param preferences output of preference_flow_data()
#' @param var either "Preference Count" or "Percent"
#' @return preferences in lodes format
#' @noRd
preferences_lode         <- function(preferences,var){

  no_rounds <-    length(preferences)
  total_votes <-  sum(preferences[[no_rounds]]$`Preference Count`)


  transfer_count <- "Transfer Count"


  if(var=="Percent"){
    for(i in 1:no_rounds){

      preferences[[i]] <- preferences[[i]] |>
        mutate(Percent = 100*.data$`Preference Count`/total_votes,
               PercentTransfer=100*.data$`Transfer Count`/total_votes)

      transfer_count <- "PercentTransfer"

    }
  }

  preferences[[no_rounds]] <-preferences[[no_rounds]] |>
    mutate(Last = FALSE)


  data <- preference_distributor(preferences,0,var)
  data_1 <- data

  data_rounds <- data |> select(any_of(c("Candidate","Round")))

  for(i in 2:no_rounds){

    p <- preferences[i:no_rounds]

    cand_round  <- str_c("Round_",i-1)
    cand_rounds <- str_c("Round_",1:(i-1))

    candidate_name <- data |>
      filter(.data$Round==i-1) |>
      distinct(.data$Candidate) |>
      pull()

    candidate    <- data |>
      filter(if_any(c(cand_round), ~ .x==candidate_name)) |>
      select(any_of(c(cand_rounds,"Count"))) |>
      mutate(Perc=.data$Count/sum(.data$Count)) |>
      select(-"Count") |>
      arrange(desc(.data$Perc))

    split <- nrow(candidate)

    data_i <- preference_distributor(p,i-1,transfer_count)  |>
      mutate(Round=0)


    if(split==1){

      data_i <- data_i |>
        filter(Candidate!=candidate_name)


      for(m in (i-1):1){
        candidate_m <-data_rounds |>
          filter(.data$Round==m) |>
          distinct(.data$Candidate) |>
          pull()


        data_i      <- data_i |>
          mutate(!!str_c("Round_",m):=candidate_m,.after="Round")

      }

    }else{

      data_ii <- list()
      for(j in 1:split){

        renmant_m <- candidate[j,]
        renmant   <- tibble()

        for(m in 1:nrow(data_i)){
          renmant <- bind_rows(renmant, renmant_m)

        }

        data_ii[[j]] <-  data_i|>
          bind_cols(renmant)|>
          mutate(Count=floor(.data$Count*.data$Perc)) |>
          select(-starts_with("Perc"))

        round_names  <- names(data_ii[[j]])[str_detect(names(data_ii[[j]]),"Round_")]
        round_names  <- round_names[str_order(round_names,numeric=TRUE)]

        data_ii[[j]] <- data_ii[[j]] |>
          select(any_of(c("Candidate","Count","Round",round_names)))

      }

      # aggregation
      data_not_1 <- tibble()

      for(x in 2:length(data_ii)){
        data_not_1 <- bind_rows(data_ii[[x]])

      }

      data_not_1 <- data_not_1 |>
        group_by(.data$Candidate) |>
        summarise(Count_T=sum(.data$Count),.groups="drop") |>
        left_join(data_i |> select(any_of(c("Candidate","Count"))),by="Candidate") |>
        mutate(Diff_Count=.data$Count-.data$Count_T) |>
        select(any_of(c("Candidate","Diff_Count")))


      data_ii[[1]] <- data_ii[[1]] |>
        left_join(data_not_1,by="Candidate") |>
        mutate(Count=.data$Diff_Count) |>
        select(-any_of(c("Diff_Count")))

      data_j <- tibble()

      for(n in 1:length(data_ii)){

        data_j <- bind_rows(data_j,data_ii[[n]])

      }

      data_i <- data_j



    }

    data <- data |>
      filter(if_any(str_c("Round_",i), ~ .x!=candidate_name)) |>
      bind_rows(data_i)
  }


  data_lodes <- data |>
    select(-any_of(c("Candidate","Round"))) |>
    mutate(alluvium=row_number(),.before=1) |>
    pivot_longer(-c(.data$alluvium,.data$Count),names_to = "x",values_to="stratum") |>
    mutate(x=str_remove(x,"Round_")) |>
    mutate(x=fct_relevel(x,c(str_c(1:no_rounds))))

  return(data_lodes)

}

