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
house_primary_historic_plot <- function(division=NULL,
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
house_primary_comparison_plot <- function(division=NULL,
                                    year=NULL,
                                    state=NULL,
                                    label="Candidate",
                                    plotted_variable="Percentage",
                                    sort_by_value = TRUE,
                                    extra_colours=NULL,
                                    plot_format = "lollipop",
                                    include_labels =FALSE,
                                    nudge_x =5,
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
    print(1)
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


#' Plot preference flow, for a division for a given year
#' @importFrom ggplot2 ggplot aes labs
#' @importFrom ggalluvial geom_flow geom_stratum
#' @param division Electoral division
#' @param year Election year
#' @param var Variable to be plotted "Percent" (default) or "Preference Count"
#' @param exclude_parties vector with party acronyms to exclude from plot
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param include_data If set to TRUE, output of primary_vote_summary(), will be included under <<output_var>>$source_data (defaults to FALSE)
#' @return preference flow, ggplot2 object
#' @include internal.R
#' @export
#' @keywords houseplots
house_preference_flow_plot <- function(division,year,
                                 var="Percent",
                                 exclude_parties= NULL,
                                 extra_colours=NULL,
                                 include_data=FALSE
){


  if(var=="Percent"){
    y_label <- "Percent"
  }else{
    y_label <- "Votes"
  }



  preferences <- house_preference_flow_data(division=division,
                                      year=year,
                                      individualise_IND = TRUE,
                                      exclude_parties = exclude_parties,
                                      exclude_rounds = 0)


  data_lodes <- preferences_lode(preferences,var)


  p <- (ggplot(data_lodes,
               aes(x = .data$x,
                   stratum = .data$stratum,
                   alluvium = .data$alluvium,
                   color=.data$stratum,
                   y=.data$Count,
                   fill = .data$stratum)) +
          #  scale_x_discrete(expand = c(.001, .001)) +
          geom_flow() +
          geom_stratum(alpha = 1,color=NA)
  ) |>
    auspol_theme(extra_colours=extra_colours,
                 extra_values = unique(data_lodes$stratum),
                 legend_pos = "bottom") +
    labs(x="Round",y=y_label)


  if(include_data){
    p$source_data <- preferences
  }

  return(p)
}
