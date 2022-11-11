
#' Plot House of reps preferences flow
#' @description Plot flow of preferences in a division as an alluvial plot.
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
