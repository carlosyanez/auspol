####################################################################
### Plotting Functions to interact with House of Reps' data #####
####################################################################


#' Plot historical changes in primary vote in a electoral division (line chart)
#' @importFrom dplyr filter select all_of rename
#' @importFrom ggplot2 ggplot labs aes
#' @importFrom stringr str_c
#' @importFrom ggrepel geom_text_repel
#' @param division named vector additional colour (hex)
#' @param plotted_variable Variable to plot, out of "OrdinaryVotes", "Percentage" (default) and Percentage_with_Informal
#' @param parties which parties to include in the summary. All (default), a vector of strings
#'  with the party acronyms (see list_parties()), or a number indicating the top n parties from a certain year.
#' @param parties_year If *parties* has is NULL or a number, this indicates if the selection needs to be from
#' a certain year (.e.g only select the historical data for the three top parties in 2012)
#' @param include_others  Boolean used along *parties* to included the remaining votes in one "Other" category.
#' @param include_informal Boolean to add informal votes in addition to the party selection.
#'  Informal votes will be included if no parties are selected, or the top n parties are selected,
#'   and it happens to be in the top n - even if this flag is set to false.
#' @param individualise_IND If set to TRUE, party abbreviations for each independent candidate will be changed
#' from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param include_labels If set to TRUE, the plot will include each value.
#' @param year numeric vector with election years (from 2004), defaults to all.
#' @param include_data If set to TRUE, output of primary_vote_summary(), will be included under <<output_var>>$source_data (defaults to FALSE)
#' @param data Alternative, instead of providing a parameters, it is possible to provide the data frame with the data
#' to plot, folowing the format from the output of  primary_vote_summary().
#' @returns ggplot2 object
#' @export
#' @keywords houseplots
plot_primary_historic <- function(division=NULL,
                              plotted_variable="Percentage",
                              parties=NULL,
                              parties_year=NULL,
                              include_others=FALSE,
                              include_informal=FALSE,
                              individualise_IND = FALSE,
                              extra_colours=NULL,
                              include_labels =FALSE,
                              year=NULL,
                              include_data = FALSE,
                              data=NULL){


  if(is.null(data)){
    if(is.null(division)) stop("include either division Candidate or data")
    if(length(division)>1) stop("Must include just one division")


    data <- primary_vote_summary(division=division,
                                 year=year,
                                 parties=parties,
                                 parties_year=parties_year,
                                 include_others=include_others,
                                 include_informal=include_informal,
                                 include_names = FALSE,
                                 individualise_IND = individualise_IND,
                                 wide_format=NULL)


  }else{
    if(!is.null(division)) stop("include either division Candidate or data")
  }


  # Percentage excludes informal votes, hence overriding option

  if(plotted_variable=="Percentage"){
    data <- data |>
      filter(.data$PartyAb!="Informal")
  }

  if(include_data) data_orig <- data


  data_cols <- c("Year","PartyAb",plotted_variable)

  if(!(plotted_variable %in% colnames(data))) stop("Invalid plotting option")

  data <- data |>
    select(all_of(data_cols)) |>
    rename(value=!!plotted_variable)



  p <- data |>
    ggplot(aes(x=.data$Year,y=.data$value,colour=.data$PartyAb,
               label=round(.data$value,2))) +
    geom_auspol_line() + labs(y=plotted_variable)

  p <- auspol_theme(p,type="colour",extra_values=unique(data$PartyAb), legend_pos = "right")


  if(include_data){
    p$source_data <- data_orig
  }

 return(p)
}


#' Plot historical changes in primary vote in a electoral division (line chart)
#' @importFrom dplyr filter select all_of rename
#' @importFrom ggplot2 ggplot aes labs
#' @importFrom stringr str_c
#' @importFrom forcats fct_reorder
#' @param division Name of ONE electoral division
#' @param state  Code for one state
#' @param label  How to label the results, either by Candidate Name ("Name",default), Party Name ("PartyNm") or Party abbreviation ("PartyAb")
#' @param plotted_variable Variable to plot, out of "OrdinaryVotes", "Percentage" (default) and Percentage_with_Informal
#' @param sort_by_value  Whether to sort results by descending order (TRUE by default)
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param plot_format Whether to plot  lollipop chart ("lollipop", default) or a bar chart.
#' @param include_labels If set to TRUE, the plot will include each value.
#' @param nudge_x if labels are included, separation from chart/dot
#' @param year numeric vector with election years (from 2004), defaults to all.
#' @param parties which parties to include in the summary. All (default), a vector of strings
#'  with the party acronyms (see list_parties()), or a number indicating the top n parties from a certain year.
#' @param parties_year If *parties* has is NULL or a number, this indicates if the selection needs to be from
#' a certain year (.e.g only select the historical data for the three top parties in 2012)
#' @param include_others  Boolean used along *parties* to included the remaining votes in one "Other" category.
#' @param include_informal Boolean to add informal votes in addition to the party selection.
#'  Informal votes will be included if no parties are selected, or the top n parties are selected,
#'   and it happens to be in the top n - even if this flag is set to false.
#' @param individualise_IND If set to TRUE, party abbreviations for each independent candidate will be changed
#' from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.
#' @param include_data If set to TRUE, output of primary_vote_summary(), will be included under <<output_var>>$source_data (defaults to FALSE)
#' @param data Alternative, instead of providing a parameters, it is possible to provide the data frame with the data
#' to plot, following the format from the output of  primary_vote_summary().
#' @returns ggplot2 object
#' @export
#' @keywords houseplots
plot_primary_comparison <- function(division=NULL,
                                    state=NULL,
                                    label="Candidate",
                                    plotted_variable="Percentage",
                                    sort_by_value = TRUE,
                                    extra_colours=NULL,
                                    plot_format = "lollipop",
                                    include_labels =FALSE,
                                    nudge_x =5,
                                    year=NULL,
                                    parties=NULL,
                                    parties_year=NULL,
                                    include_others=FALSE,
                                    include_informal=FALSE,
                                    individualise_IND = FALSE,
                                    include_data = TRUE,
                                    data=NULL){


  #year must be declared
  if(is.null(year)) stop("Year must be declared")
  if(length(year)>1) stop("Select just one year!")

  #must contain one of the following

  if(is.null(data)){
    if(!(is.null(division)|is.null(state))) stop("Include either division Candidate,state code or data")
    if(!is.null(state) &(length(parties)!=1)) stop("If state is selected, ONE party must be declared")

    data <- primary_vote_summary(division=division,
                                 state=state,
                                 year=year,
                                 parties=parties,
                                 parties_year=parties_year,
                                 include_others=include_others,
                                 include_informal=include_informal,
                                 include_names = TRUE,
                                 individualise_IND = individualise_IND,
                                 wide_format=NULL) |>
                                 mutate(Candidate=str_c(.data$GivenNm," ",.data$Surname),
                                        Candidate=if_else(.data$GivenNm=="Informal","Informal",.data$Candidate))

  }else{
    if(!is.null(division)&!is.null(state)) stop("include either division Candidate,state code or data")

    data <- data |>
            mutate(Candidate=str_c(.data$GivenNm," ",.data$Surname))
  }


  # Percentage excludes informal votes, hence overriding option

  if(plotted_variable=="Percentage"){
    data <- data |>
      filter(.data$PartyAb!="Informal")
  }

  if(include_data) data_orig <- data

  data_cols <- c(label,"PartyAb",plotted_variable,"Party")
  data_cols <- unique(data_cols)

  data <- data |>
    mutate(Party=.data$PartyAb) |>
    select(all_of(data_cols)) |>
    rename(value=!!plotted_variable,
           label=!!label)

  if(label=="Candidate"){
    data <- data |>
            mutate(label=str_c(label, " (",.data$Party,")"))
  }

  if(sort_by_value) data <- data |> mutate(label=fct_reorder(.data$label,.data$value))

  aesthetics2 <- NULL
  if(plot_format=="lollipop"){
    aesthetics2 <- aes(yend=.data$label, xend=.data$value,
                       x=0,
                       colour=.data$Party)
  }


  p <- data |>
          ggplot(aes(y=.data$label,x=.data$value,
                     colour=.data$Party,fill=.data$Party,
                     label=round(.data$value,2))) +
          geom_auspol_lollipop(format="lollipop",
                               segment.mapping=aesthetics2,
                               include_labels = TRUE,
                               extra_values= unique(data$Party),labels.nudge_x = nudge_x) +
         labs(y=label)

   p <- auspol_theme(p,extra_colours = extra_colours, extra_values = unique(data$Party), coord_flip = FALSE)


  if(include_data){
    p$source_data <- data_orig
  }


  return(p)
}

#' Auxiliary function to convert preferences to alluvial
#' @param preferences output of preference_flow_data()
#' @param name_correction number to correct round index
#' @param var either "Preference Count" or "Percent"
#' @importFrom dplyr mutate rename select group_by summarise distinct filter if_any any_of bind_rows
#' @importFrom tibble tibble
#' @returns preferences in alluvial format
#' @noRd
preference_distributor <- function(preferences,name_correction, var="Preference Count"){

  no_rounds <- length(preferences)

  first_round <-  preferences[[1]] |>
    mutate(Candidate = .data$PartyAb) |>
    rename(count=!!var) |>
    select(Candidate,PartyAb,count)     |>
    group_by(Candidate,PartyAb) |>
    summarise(Count=sum(count,na.rm = TRUE),.groups="drop")

  data <- tibble()

  for(i in 0:(no_rounds-1)){


    last_round <- preferences[[no_rounds-i]] |>
      mutate(Candidate = .data$PartyAb) |>
      distinct(Candidate)

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
        rename(!!round_name:=new)
    }


    data <- bind_rows(data,round)

  }

  return(data)
}

#' Convert preferences in lode format
#' @importFrom  dplyr mutate select row_number
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_c str_remove
#' @importFrom forcats fct_relevel
#' @param preferences output of preference_flow_data()
#' @param var either "Preference Count" or "Percent"
#' @return preferences in lodes format
#' @noRd
preferences_lode         <- function(preferences,var){

  no_rounds <- length(preferences)
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


  data <- distributor(preferences,0,var)
  data_1 <- data

  data_rounds <- data |> select(Candidate,Round)

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
      arrange(desc(Perc))

    split <- nrow(candidate)

    #candidate <- candidate |>
    #             pivot_wider(values_from = Perc,names_from = Name)


    data_i <- distributor(p,i-1,transfer_count)  |>
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
        summarise(Count_T=sum(Count),.groups="drop") |>
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
    select(-Candidate,-Round) |>
    mutate(alluvium=row_number(),.before=1) |>
    pivot_longer(-c(alluvium,Count),names_to = "x",values_to="stratum") |>
    mutate(x=str_remove(x,"Round_")) |>
    mutate(x=fct_relevel(x,c(str_c(1:no_rounds))))

  return(data_lodes)

}

#' Plot preference flow, for a division for a given year
#' @importFrom ggplot2 ggplot aes labs
#' @importFrom ggalluvial geom_flow geom_stratum
#' @param division Electoral division
#' @param year Election year
#' @param parties_exclude vector with party acronyms to exclude from plot
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param include_data If set to TRUE, output of primary_vote_summary(), will be included under <<output_var>>$source_data (defaults to FALSE)
#' @return preference flow, ggplot2 object
#' @export
#' @keywords houseplots
plot_preference_flow <- function(division,year,
                                 var="Percent",
                                 parties_exclude= NULL,
                                 extra_colours=NULL,
                                 include_data=FALSE
){


  if(var=="Percent"){
    y_label <- "Percent"
  }else{
    y_label <- "Votes"
  }



  preferences <- preference_flow_data(division,
                                      year,
                                      parties_exclude = parties_exclude)


  data_lodes <- preferences_lode(preferences,var)


  p <- (ggplot(data_lodes,
               aes(x = x, stratum = stratum, alluvium = alluvium,
                   color=stratum,
                   y=Count,
                   fill = stratum)) +
          #  scale_x_discrete(expand = c(.001, .001)) +
          geom_flow() +
          geom_stratum(alpha = 1,color=NA)
  ) |>
    auspol_theme(extra_colours=extra_colours,
                 extra_values = unique(data_lodes$stratum),
                 legend_pos = "bottom") +
    labs(x="Round",y=y_label)


  if(include_data){
    p$orig_data <- preferences
  }

  return(p)
}
