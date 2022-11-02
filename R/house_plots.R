####################################################################
### Plotting Functions to interact with House of Reps' data #####
####################################################################


party_colours <- function(extra=NULL){

  colours <- c("ALP"="#E13940",
               "CLP"="#FF7701",
               "GRN" ="#009C3D",
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


plot_primary_vote <- function(division=NULL,
                              parties=NULL,
                              parties_year=NULL,
                              include_others=FALSE,
                              include_informal=FALSE,
                              extra_colours=NULL,
                              include_labels =FALSE,
                              individualise_IND = FALSE,
                              year="all",
                              data=NULL){


  if(is.null(data)){
    if(is.null(division)) stop("include either division name or data")

    data <- primary_vote_summary(division,
                                 year=year,
                                 parties,
                                 parties_year,
                                 include_others,
                                 include_informal,
                                 individualise_IND=TRUE)
  }

  #Manage Colours
  colours <- party_colours(extra_colours)
  colours <- colours[names(colours) %in% c(unique(data$PartyAb),"Other")]

  colours_nl_names <- unique(data$PartyAb)[!(unique(data$PartyAb) %in% names(colours))]
  colours_nl <-colRoz::colRoz_pal("c.decresii", type = "continuous", n = length(colours_nl_names))
  names(colours_nl) <- colours_nl_names
  colours <- c(colours,colours_nl)



  p <- data |>
    ggplot(aes(x=.data$Year,y=.data$Percentage,colour=.data$PartyAb)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values=colours) +
    labs(title=str_c(division," - Primary Vote")) +
    theme_minimal()

  if(include_labels){
    p <- p +
         geom_text_repel(aes(label=round(.data$Percentage,2)))

  }

  return(p)

}


plot_primary_vote("Goldstein",5,include_others = TRUE,include_labels = TRUE)
