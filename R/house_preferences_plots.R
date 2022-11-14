
#' Plot House of reps preferences flow
#' @description Plot flow of preferences in a division as an alluvial plot.
#' @importFrom ggplot2 ggplot aes labs
#' @importFrom ggalluvial geom_flow geom_stratum
#' @param division Electoral division
#' @param year Election year
#' @param var Variable to be plotted "Percent" (default) or "Preference Count"
#' @param exclude_parties vector with party acronyms to exclude from plot
#' @param merge_parties list of parties to merge in one line following, the format list(NEWCODE=c(code1,code2,etc.))
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param include_data If set to TRUE, output of primary_vote_summary(), will be included under <<output_var>>$source_data (defaults to FALSE)
#' @return preference flow, ggplot2 object
#' @include internal.R
#' @export
#' @keywords houseplots
#' @examples \dontrun{
#' # Preference flow for Wills, 2019
#' house_preference_flow_plot(division = "Wills",year=2019)
#' # Preference flow for Warringah 2022,
#' # excluding two finalists from round 1,
#' # independent candidate in teal.
#' house_preference_flow_plot(division = "Warringah",year=2022,
#   exclude_parties = c("LP","IND-STEGGALL"),
#   extra_colours = c("IND-STEGGALL"="#008080"))
#' }
house_preference_flow_plot <- function(division,year,
                                       var="Percent",
                                       exclude_parties= NULL,
                                       merge_parties = NULL,
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

#' Preferences flow from primary to finalists
#' @description Plot representing flow of preferences from first preferences to candidates in last round.
#' Can be present as alluvial plot or bar chart, showing votes count or percentages.
#' @importFrom ggplot2 ggplot aes labs theme element_blank
#' @importFrom ggalluvial geom_flow geom_stratum to_lodes_form
#' @importFrom forcats fct_relevel
#' @importFrom dplyr mutate select any_of across arrange pull rename summarise group_by
#' @param division Electoral division
#' @param year Election year
#' @param var Variable to be plotted "Percent" (default) or "Transfer Count"
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param plot_format Whether to plot  alluvial chart ("alluvial") or a bar chart ("bar", default).
#' @param include_data If set to TRUE, data will be included under <<output_var>>$source_data (defaults to FALSE)
#' @param individualise_IND If set to TRUE, party abbreviations for each independent candidate will be changed
#' from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.
#' @return preference flow, ggplot2 object
#' @include internal.R
#' @export
#' @keywords houseplots
#' @examples \dontrun{
#' # Preference flow for Burt, 2022
#' house_2PF_plot("Burt",2022,plot_format = "alluvial")
#' # Preference flow for Warringah 2022,
#' house_2PF_plot("Spence",2013,plot_format = "bar")
#' }
house_2PF_plot <- function(division,year,
                           var="Percent",
                           extra_colours=NULL,
                           plot_format = "bar",
                           include_data=FALSE,
                           individualise_IND = TRUE){


  if(is.null(year)) stop("Year must be declared")
  if(length(year)>1) stop("Select just one year!")
  if(!(plot_format %in% c("alluvial","bar"))) stop("Only bar and alluvial formats allowed")

  #get data
  data <- get_house_2PF(division = division,
                        year=year,
                        aggregation = TRUE)

 if(is.null(data)) stop("Division didn't exist in the provided year.")

  # individualise independents
  if(individualise_IND){
    data <- data |>
      mutate(FromCandidatePartyAb=if_else(.data$FromCandidatePartyAb=="IND",
                                          str_c("IND-",.data$FromCandidateSurname),
                                          .data$FromCandidatePartyAb),
             ToCandidatePartyAb=if_else(.data$ToCandidatePartyAb=="IND",
                                        str_c("IND-",.data$ToCandidateSurname),
                                        .data$ToCandidatePartyAb),
      )
  }

  #choose between percentage or absolute values
  if(var=="Percent"){
    y_label <- "Percent"
    total_votes <- sum(data$TransferCount)

    data <- data |>
      mutate(TransferCount=.data$TransferCount*100/total_votes)

  }else{
    y_label <- "Votes"
  }


  #save original
  data_orig <- data

  #rename for plotting
  data <- data |>
    select(any_of(c("FromCandidatePartyAb",
                    "ToCandidatePartyAb",
                    "TransferCount"))) |>
    rename("From"="FromCandidatePartyAb",
           "To"="ToCandidatePartyAb")

  #levels for sorting

  levels <- data |>
    group_by(across(c("From"))) |>
    summarise(sum=sum(as.numeric(.data$TransferCount)),
              .groups="drop") |>
    arrange(sum) |>
    pull(.data$From)


  if(plot_format=="alluvial"){

    data_lodes <- to_lodes_form(data,
                                key = "x", value = "stratum",
                                id = "alluvium",axes=1:2)


    levels_end <- data |>
      group_by(across(c("To"))) |>
      summarise(sum=sum(as.numeric(.data$TransferCount)),
                .groups="drop") |>
      arrange(sum) |>
      pull(.data$To)


    levels <- levels[!(levels %in% levels_end)]
    levels <- c(levels_end[1],levels,levels_end[2])


    p <- (ggplot(data_lodes,
                 aes(x = .data$x,
                     stratum = fct_relevel(.data$stratum,levels),
                     alluvium = .data$alluvium,
                     color=.data$stratum,
                     y=.data$TransferCount,
                     fill = .data$stratum)) +
            geom_auspol_bar() +
            geom_stratum(alpha = 1,color=NA)
    ) |>
      auspol_theme(extra_colours=extra_colours,
                   extra_values = unique(as.character(data_lodes$stratum)),
                   legend_pos = "bottom") +
      theme(axis.title.x = element_blank())+
      labs(y=y_label)

  }else{

    p <- (data |>
            ggplot(aes(x=.data$To,y=.data$TransferCount,
                       group=fct_relevel(.data$From,levels),
                       fill=.data$From)) +
            geom_col()
    ) |>
      auspol_theme(type="fill",extra_colours=extra_colours,
                   extra_values = unique(as.character(data$From)),
                   legend_pos = "right") +
      theme(axis.title.x = element_blank())+
      labs(y=y_label)

  }

  if(include_data){
    p$source_data <- data_orig
  }


  return(p)
}




#' Two Party-Preferred Comparison
#' @description Plot with two-party preferred values for one of more divisions, for a given year
#' Can be present as alluvial plot or bar chart, showing votes count or percentages.
#' @importFrom ggplot2 ggplot aes labs geom_col theme element_blank
#' @importFrom ggalluvial geom_flow geom_stratum to_lodes_form
#' @importFrom forcats fct_relevel
#' @importFrom dplyr mutate select any_of contains case_when if_else if_any filter distinct arrange desc pull
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract str_squish
#' @param division Electoral division
#' @param year Election year
#' @param state If division is left null, use this to select all divisions in one of more states.
#' @param var Variable to be plotted "Percentage" (default) or "Votes"
#' @param include_data If set to TRUE, data will be included under <<output_var>>$source_data (defaults to FALSE)
#' @return ggplot2 object
#' @include internal.R
#' @export
#' @keywords houseplots
#' @examples \dontrun{
#' # Two party preferred plot for Victoria, 2022
#' house_2PP_comparison_plot(year=2022,state="VIC")
#' }
house_2PP_comparison_plot <- function(division=NULL,year,state=NULL,var="Percentage",include_data=TRUE){

  if(is.null(year)) stop("Year must be declared")
  if(length(year)>1) stop("Select just one year!")
  if(!(var %in% c("Percentage","Votes"))) stop("Only Percentage and Votes")


  data <- get_house_2PP(division = division,state_abb=state,year=year,aggregation = TRUE)
  data_orig <- data

  data <- data |>
    select(any_of(c("DivisionNm")),
           contains(" Votes"),
           contains(" Percentage")) |>
    pivot_longer(-c("DivisionNm"),names_to="Party",values_to="value") |>
    mutate(Type=str_extract(.data$Party,"Votes|Percentage"),
           Party=str_squish(str_remove(.data$Party,"Votes|Percentage")),
           PartyAb=case_when(
             str_detect(.data$Party,"Coalition") ~ "COAL",
             str_detect(.data$Party,"Labor")     ~ "ALP",
             str_detect(.data$Party,"Green")     ~ "GRN",
             TRUE                                ~ "Other"),
           value = if_else(.data$Type=="Percentage",.data$value*100,.data$value)
    ) |>
    filter(if_any(c("Type"), ~ .x==var))

  party_levels <- c("COAL","Other","ALP","GRN")
  party_levels <- party_levels[party_levels %in% unique(data$PartyAb)]

  division_levels <- data |>
    distinct(.data$DivisionNm) |>
    arrange(desc(.data$DivisionNm)) |>
    pull(.data$DivisionNm)


  p <- (data |>
          ggplot(aes(y=fct_relevel(.data$DivisionNm,division_levels),
                     x = .data$value,
                     fill = fct_relevel(.data$PartyAb,party_levels)
          )) +
          geom_col()) |>
    auspol_theme(extra_values = unique(data$PartyAb),
                 legend_pos = "bottom") +
    theme(axis.title.y = element_blank())+
    labs(x=var)

  if(include_data){
    p$source_data <- data_orig
  }

  return(p)

}


#' Two Party-Preferred Comparison
#' @description Plot with two-party preferred values for one of more divisions, for a given year
#' Can be present as alluvial plot or bar chart, showing votes count or percentages.
#' @importFrom ggplot2 ggplot aes labs  theme element_blank
#' @importFrom ggalluvial geom_flow geom_stratum to_lodes_form
#' @importFrom forcats fct_relevel
#' @importFrom dplyr mutate select any_of contains case_when if_else if_any filter distinct arrange desc pull
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract str_squish
#' @param division Electoral division
#' @param year Election year
#' @param var Variable to be plotted "Percentage" (default) or "Votes"
#' @param include_labels If set to TRUE, the plot will include each value.
#' @param include_data If set to TRUE, data will be included under <<output_var>>$source_data (defaults to FALSE)
#' @return ggplot2 object
#' @include internal.R
#' @export
#' @keywords houseplots
#' @examples \dontrun{
#' # Plot historical 2PP for Aston
#' house_2PP_historical_plot(division="Aston")
#' }
house_2PP_historical_plot <- function(division,year=NULL,var="Percentage",
                                      include_labels=TRUE,
                                      include_data=TRUE){

  if(is.null(division)) stop("Year must be declared")
  if(length(division)>1) stop("Select just one year!")
  if(!(var %in% c("Percentage","Votes"))) stop("Only Percentage and Votes")


  data <- get_house_2PP(division = division,year=year,aggregation = TRUE)
  data_orig <- data

  data <- data |>
    select(any_of(c("Year")),
           contains(" Votes"),
           contains(" Percentage")) |>
    pivot_longer(-c("Year"),names_to="Party",values_to="value") |>
    mutate(Type=str_extract(.data$Party,"Votes|Percentage"),
           Party=str_squish(str_remove(.data$Party,"Votes|Percentage")),
           PartyAb=case_when(
             str_detect(.data$Party,"Coalition") ~ "COAL",
             str_detect(.data$Party,"Labor")     ~ "ALP",
             str_detect(.data$Party,"Green")     ~ "GRN",
             TRUE                                ~ "Other"),
           value = if_else(.data$Type=="Percentage",.data$value*100,.data$value)
    ) |>
    filter(if_any(c("Type"), ~ .x==var))


  p <- (data |>
          ggplot(aes(x=.data$Year,
                     y=.data$value,
                     colour=.data$PartyAb))+
          geom_auspol_line(include_labels = include_labels)) |>
          auspol_theme(extra_values = unique(data$PartyAb),
                 legend_pos = "bottom") +
    theme(axis.title.y = element_blank())+
    labs(x=var)

  if(include_data){
    p$source_data <- data_orig
  }


  return(p)

}
