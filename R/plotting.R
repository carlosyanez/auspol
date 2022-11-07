####################################################################
### Plotting Functions  #####
####################################################################

#' Named vector with common parry colours, with option to add custom/additonal values
#' @param extra named vector additional colour (hex) values
#' @returns named vector
#' @noRd
party_colours <- function(extra=NULL){

  colours <- c("ALP"="#E13940",
               "CLP"="#FF7701",
               "GRN" ="#009C3D",
               "GVIC"="#009C3D",
               "HAN" ="#0176BC",
               "JLN" ="#000000",
               "KAP" ="#DF1014",
               "LNP" ="#ADD8E6",
               "LNQ" ="#ADD8E6",
               "LP"  ="#1C4F9C",
               "NP"  ="#006946",
               "ON"  = "#0176BC",
               "UAPP"= "#FFFF00",
               "XEN" = "#FF8000",
               "COAL" ="#1C4F9C",
               "TEAL" ="#008080",
               "Other" ="#414141",
               "IND" = "#9933FF",
               "Informal"="#C0C0C0")


  if(!is.null(extra)){
    colours <- colours[!(names(colours) %in% names(extra))]
    colours <- c(colours,extra)
  }

  return(colours)

}


#' Helper function generate colour palette
#' @returns named vector with colours
#' @param extra_colours named vector additional colour (hex) values
#' @param extra_values  vector with all unique combinations (to assign each a colour)
#' @param palette palette to replace brewer.pal
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @keywords plotting
manage_colours <- function(extra_colours=NULL,extra_values=NULL,palette=NULL){

  #Manage Colours
  colours <- party_colours(extra_colours)
  if(is.null(extra_values)) stop("Must provide list of values for mapping")

    colours <- colours[names(colours) %in% c(extra_values)]
    colours_nl_names <- unique(extra_values)[!(extra_values %in% names(colours))]

    if(length(colours_nl_names)>0){

      if(is.null(palette)){
        palette <- brewer.pal(name = "Set3",n=12)
      }
      repeat_times <- ceiling(length(colours_nl_names)/12)
      palette <- rep(palette,repeat_times)[1:length(colours_nl_names)]

      palette <- as.vector(palette)

      colour_names <- c(names(colours),colours_nl_names)

      colours <- unique(c(colours,palette))
      names(colours) <- colour_names

  }

  return(colours)
}


#' Helper function generate colour palette
#' @returns ggplot object
#' @param p ggplot object
#' @param type type of scales: c("colour","fill)
#' @param extra_colours named vector additional colour (hex) values
#' @param extra_values  vector with all unique combinations (to assign each a colour)
#' @param coord_flip  whether to flip coordinate axes
#' @param palette additional colour palette for unnamed parties
#' @param legend_pos legend position
#' @importFrom ggplot2 scale_color_manual theme_minimal  scale_fill_manual coord_flip theme
#' @export
#' @keywords plotting
auspol_theme <- function(p,type=c("colour","fill"),
                           extra_colours=NULL,
                           extra_values=NULL,
                           coord_flip = FALSE,
                           palette=NULL,
                           legend_pos="none"){

  colours <- manage_colours(extra_colours,extra_values,palette)
  scale_name <- "Party"

  ggpck <- p +
           theme_minimal()

  if("colour" %in% type){
    ggpck <- ggpck +
             scale_color_manual(values=colours,name=scale_name)
  }


  if("fill" %in% type){
    ggpck <- ggpck +
             scale_fill_manual(values=colours,name=scale_name)
  }


  if(coord_flip){
    ggpck <- ggpck + coord_flip()
  }

  ggpck <- ggpck + theme(legend.position=legend_pos)

  return(ggpck)

}


#' Lollipop or bar chart, custommised for this package.
#' @importFrom ggpackets ggpacket %+%
#' @importFrom ggplot2 geom_segment geom_point geom_col geom_text
#' @param format Output format : "lollipop" (default) or "bar".
#' @param include_labels Whether to include numeric labels (TRUE by default)
#' @param ... parameters for {ggplot2} geom_segment() (segmnet.prefix), geom_point(), geom_col() and geom_text() (labels. prefix).
#' @export
#' @keywords plotting
geom_auspol_lollipop <- function(format="lollipop",include_labels=TRUE,...){

  if(format=="lollipop"){

    ggpck <- ggpacket(...) %+%
             geom_segment(.id="segment",...) %+%
             geom_point(size=4,...)

  }else{

    ggpck <- ggpacket(...) %+%
             geom_col(...)
  }

  if(include_labels){
    ggpck <- ggpck %+% geom_text(.id="labels",show.legend = FALSE,...)
  }

  return(ggpck)

}


#' Line chart, custommised for this package.
#' @importFrom ggpackets ggpacket %+%
#' @importFrom ggplot2  geom_point geom_line
#' @importFrom ggrepel geom_text_repel
#' @param include_labels Whether to include numeric labels (TRUE by default)
#' @param ... parameters for {ggplot2} geom_segment() (segmnet.prefix), geom_point(), geom_col() and geom_text() (labels. prefix).
#' @export
#' @keywords plotting
geom_auspol_line <- function(include_labels=TRUE,...){


  ggpck <-  ggpacket(...)   %+%
            geom_point(...) %+%
            geom_line(...)


  if(include_labels){
    ggpck <- ggpck %+% geom_text_repel(.id="labels",show.legend = FALSE,...)
  }

  return(ggpck)

}
