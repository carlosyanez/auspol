

#' Election Tally
#' @description Plot party totals for a given election. Can aggregate parties into groups, amongst other filters.
#' @importFrom ggplot2 ggplot aes labs theme element_blank
#' @importFrom forcats fct_relevel
#' @importFrom dplyr mutate select any_of across arrange pull rename summarise group_by
#' @importFrom stringr str_c
#' @param year Election year
#' @param individualise_IND If set to TRUE, party abbreviations for each independent candidate will be changed
#' from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.
#' @param merge_parties list of parties to merge in one line following, the format list(NEWCODE=c(code1,code2,etc.))
#' @param add_majority_line add line representing 50% +1 of the seats
#' @param include_labels If set to TRUE, the plot will include each value.
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param include_data If set to TRUE, data will be included under <<output_var>>$source_data (defaults to FALSE)
#' @return preference flow, ggplot2 object
#' @include internal.R
#' @export
#' @keywords houseplots
#' @examples \dontrun{
#' # Basic example
#' house_results_tally(2013)
#' # Coalition votes put together
#' house_results_tally(2013, merge_parties = list(COAL=c("CLP","LP","LNP","NP")))
#' }
house_results_tally <- function(year,
                                individualise_IND=FALSE,
                                merge_parties=NULL,
                                add_majority_line=TRUE,
                                include_labels=FALSE,
                                extra_colours=NULL,
                                include_data=FALSE){

  data <- get_house_MPs(year=year)
  data_orig <- data

  # individualised Independent?
  if(individualise_IND){
    data <- data |>
      mutate(PartyAb=if_else(.data$PartyAb=="IND",
                             str_c("IND-",.data$Surname),
                             .data$PartyAb))
  }

  data <- data |>
    count(.data$PartyAb)

  if(!is.null(merge_parties)){
    for(i in 1:length(merge_parties)){
      data <- data |>
        mutate(PartyAb=if_else(.data$PartyAb %in% merge_parties[[i]],
                               names(merge_parties)[i],
                               .data$PartyAb))
    }

    data <- data |>
      group_by(across(starts_with(c("PartyAb"))))  |>
      summarise(n=sum(.data$n,na.rm = TRUE),
                .groups="drop")

  }

  party_levels <- data |>
    arrange(.data$n) |>
    pull(.data$PartyAb)

  if(add_majority_line){
    majority <- floor(sum(data$n)/2)+1
  }
  p <- (data |>
          ggplot(aes(x=.data$n,
                     y=fct_relevel(.data$PartyAb,party_levels),
                     fill=.data$PartyAb,
                     label=.data$n)) +
          geom_auspol_bar(include_labels=TRUE, reference_line=majority,
                          ref_line.colour="green",ref_line.linetype=6,
                          ref_line.size=1.1)) |>
    auspol_theme(extra_colours = extra_colours,
                 extra_values = unique(data$PartyAb),
                 legend_pos = "right")

  p <- p +
    theme(axis.title.y=element_blank()) +
    labs(x="Seats")



  if(include_data){
    p$source_data <- data_orig
  }

  return(p)

}


#' Historic Resuls
#' @description Plot seats by party across time. Parties can be filtered and grouped by coalitions.
#' @importFrom ggplot2 ggplot aes labs
#' @importFrom dplyr mutate if_else count
#' @importFrom stringr str_c
#' @param individualise_IND If set to TRUE, party abbreviations for each independent candidate will be changed
#' from "IND" to "IND-<<candidate's surname>>", effectively separating them in party aggregations.
#' @param merge_parties list of parties to merge in one line following, the format list(NEWCODE=c(code1,code2,etc.))
#' @param parties List of political party abbreviations to filter on. If merge_parties is used, those names can be included too.
#' @param include_others  Boolean used along *parties* to included the remaining votes in one "Other" category.
#' @param include_labels If set to TRUE, the plot will include each value.
#' @param extra_colours manual mapping of colours for each party, as a named vector.
#' @param include_data If set to TRUE, data will be included under <<output_var>>$source_data (defaults to FALSE)
#' @return preference flow, ggplot2 object
#' @include internal.R
#' @export
#' @keywords houseplots
#' @examples \dontrun{
#' # Historic results, focusing showing tally for Coalition, ALP, Greens - others merged together
#' house_results_historic(merge_parties = list(COAL=c("CLP","LP","LNP","NP")),
#'                        parties =c("COAL","ALP","GRN"),
#'                        include_other=TRUE))
#' }
house_results_historic <- function(individualise_IND=FALSE,
                                   merge_parties=NULL,
                                   parties=NULL,
                                   include_others=FALSE,
                                   include_labels=TRUE,
                                   extra_colours=NULL,
                                   include_data = FALSE
                                   ){

  data <- get_house_MPs()
  data_orig <- data


  if(individualise_IND){
    data <- data |>
      mutate(PartyAb=if_else(.data$PartyAb=="IND",
                             str_c("IND-",.data$Surname),
                             .data$PartyAb))
  }

  data <- data |>
    count(.data$Year,.data$PartyAb)

  if(!is.null(merge_parties)){
    for(i in 1:length(merge_parties)){
      data <- data |>
        mutate(PartyAb=if_else(.data$PartyAb %in% merge_parties[[i]],
                               names(merge_parties)[i],
                               .data$PartyAb))
    }

    data <- data |>
      group_by(across(c("PartyAb","Year")))  |>
      summarise(n=sum(.data$n,na.rm = TRUE),
                .groups="drop")

  }

  if(is.null(parties)){
    parties <- unique(data$PartyAb)
  }

  if(include_others){
    data <- data |>
      mutate(PartyAb=if_else(.data$PartyAb %in% parties,
                             .data$PartyAb,
                             "Other")) |>
      group_by(across(c("PartyAb","Year")))  |>
      summarise(n=sum(.data$n,na.rm = TRUE),
                .groups="drop")


  }else{

    data <- data |>
      filter(if_any(c("PartyAb"), ~ .x %in% parties))


  }


  p <- data |>
         ggplot(aes(x=.data$Year,y=.data$n,colour=.data$PartyAb,fill=.data$PartyAb,label=.data$n)) +
         geom_auspol_line(include_labels = include_labels) +
         labs(y="Seats")


  p <- auspol_theme(p, legend_pos = "right",
                    extra_values = unique(data$PartyAb),
                    extra_colours = extra_colours)

  if(include_data){
    p$source_data <- data_orig
  }

  return(p)


}

