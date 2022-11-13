####################################################################
### Plotting Functions to interact with House of Reps' data #####
####################################################################


#' Plot primary vote history
#' @description Plot historical primary vote results for a division or group of divisions, being able to select
#' and aggregate political parties. Can plot either percentages or absolute number of ordinary votes.
#' @importFrom dplyr filter select all_of rename
#' @importFrom ggplot2 ggplot labs aes
#' @importFrom stringr str_c
#' @importFrom ggrepel geom_text_repel
#' @param division named vector with division names
#' @param plotted_variable Variable to plot, out of "OrdinaryVotes", "Percentage" (default) and Percentage_with_Informal
#' @param parties which parties to include in the summary. All (default), a vector of strings
#'  with the party acronyms (see list_parties()), or a number indicating the top n parties from a certain year.
#' @param parties_year If *parties* has is NULL or a number, this indicates if the selection needs to be from
#' a certain year (.e.g only select the historical data for the three top parties in 2012).
#' @param merge_parties list of parties to merge in one line following, the format list(NEWCODE=c(code1,code2,etc.))
#' @param include_others  Boolean used along *parties* to included the remaining votes in one "Other" category.
#' @param include_informal Boolean to add informal votes in addition to the party selection.
#'  Informal votes will be included if no parties are selected, or the top n parties are selected,
#'   and it happens to be in the top n - even if this flag is set to false.
#' @param individualise_IND If set to TRUE, party abbreviations for each independent candidate will be changed
#' from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param include_labels If set to TRUE, the plot will include each value.
#' @param year numeric vector with election years (from 2004), defaults to all.
#' @param include_data If set to TRUE, output of house_primary_vote_summary(), will be included under <<output_var>>$source_data (defaults to FALSE)
#' @param data Alternative, instead of providing a parameters, it is possible to provide the data frame with the data
#' to plot, folowing the format from the output of  house_primary_vote_summary().
#' @returns ggplot2 object
#' @export
#' @keywords houseplots
#' @examples \dontrun{
#' # Plot historic primary voting in Canberra, top 3 parties
#' house_primary_historic_plot("Canberra", parties =3,
#                               parties_year = 2022,
#                               include_others = TRUE )
#'
#' }
house_primary_historic_plot <- function(division=NULL,
                              plotted_variable="Percentage",
                              parties=NULL,
                              parties_year=NULL,
                              merge_parties=NULL,
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


    data <- house_primary_vote_summary(division=division,
                                 year=year,
                                 parties=parties,
                                 parties_year=parties_year,
                                 include_others=include_others,
                                 merge_parties=NULL,
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


#' Plot historical changes in primary vote
#' @description Line chart with historial changes for a division, group of candidates in a party,
#'  selected parties, etc.
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
#' @param hor_nudge if labels are included, separation from chart/dot
#' @param year numeric vector with election years (from 2004), defaults to all.
#' @param parties which parties to include in the summary. All (default), a vector of strings
#'  with the party acronyms (see list_parties()), or a number indicating the top n parties from a certain year.
#' @param parties_year If *parties* has is NULL or a number, this indicates if the selection needs to be from
#' a certain year (.e.g only select the historical data for the three top parties in 2012)
#' @param merge_parties list of parties to merge in one line following, the format list(NEWCODE=c(code1,code2,etc.))
#' @param include_others  Boolean used along *parties* to included the remaining votes in one "Other" category.
#' @param include_informal Boolean to add informal votes in addition to the party selection.
#'  Informal votes will be included if no parties are selected, or the top n parties are selected,
#'   and it happens to be in the top n - even if this flag is set to false.
#' @param individualise_IND If set to TRUE, party abbreviations for each independent candidate will be changed
#' from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.
#' @param include_data If set to TRUE, output of house_primary_vote_summary(), will be included under <<output_var>>$source_data (defaults to FALSE)
#' @param data Alternative, instead of providing a parameters, it is possible to provide the data frame with the data
#' to plot, following the format from the output of  house_primary_vote_summary().
#' @returns ggplot2 object
#' @export
#' @keywords houseplots
#' @examples \dontrun{
#' # Compare primary voting in Kooyong in 2022,  lollipop chart (default)
#' house_primary_comparison_plot(division = "Kooyong",
#'                               year=2022,
#'                               individualise_IND = TRUE)
#
#' # Liberal Primary vote in Tasmania in 2022, bar chart
#' house_primary_comparison_plot(state="TAS",
#'                               year=2022,
#'                               parties=c("LP"),
#'                               plot_format = "bar")
#'
#' }
house_primary_comparison_plot <- function(division=NULL,
                                    year=NULL,
                                    state=NULL,
                                    label="Candidate",
                                    plotted_variable="Percentage",
                                    sort_by_value = TRUE,
                                    extra_colours=NULL,
                                    plot_format = "lollipop",
                                    include_labels =FALSE,
                                    hor_nudge=5,
                                    parties=NULL,
                                    parties_year=NULL,
                                    merge_parties=NULL,
                                    include_others=FALSE,
                                    include_informal=FALSE,
                                    individualise_IND = TRUE,
                                    include_data = TRUE,
                                    data=NULL){


  #year must be declared
  if(is.null(year)) stop("Year must be declared")
  if(length(year)>1) stop("Select just one year!")
  if(!(plot_format %in% c("lollipop","bar"))) stop("Only bar and lollipop formats allowed")
  #must contain one of the following

  if(is.null(data)){
    if(!(is.null(division)|is.null(state))) stop("Include either division Candidate,state code or data")
    if(!is.null(state) &(length(parties)!=1)) stop("If state is selected, ONE party must be declared")

    data <- house_primary_vote_summary(division=division,
                                 state=state,
                                 year=year,
                                 parties=parties,
                                 parties_year=parties_year,
                                 merge_parties = merge_parties,
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

  #print(hor_nudge)
  if(!exists("hor_nudge")) hor_nudge <- 5

  p <- data |>
          ggplot(aes(y=.data$label,x=.data$value,
                     colour=.data$Party,fill=.data$Party,
                     label=round(.data$value,2))) +
          geom_auspol_lollipop(format=plot_format,
                               segment.mapping= aes(yend=.data$label, xend=.data$value,
                                                    x=0,
                                                    colour=.data$Party),
                               include_labels = TRUE,labels.nudge_x = 5) +
         labs(y=label)

   p <- auspol_theme(p,extra_colours = extra_colours, extra_values = unique(data$Party), coord_flip = FALSE)


  if(include_data){
    p$source_data <- data_orig
  }


  return(p)
}



