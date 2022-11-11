##############################################################
### Functions to retrieve lists of ..electorates, parties ####
##############################################################



#' Get election years.
#' @description Very simple function listing the election years included in this package.
#' @returns vector with years
#' @export
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all divisions
#' list_years()
#'  }
list_years <- function(){

  data <- load_auspol("house_primary_vote.zip")

  years  <- unique(data$Year)
  return(years)
}


#' Get list of divisions
#' @description get list of all the Australian Federal electoral divisions, being able to
#' filter by any attribute. Covers all divisions from the 2004 Election.
#' @importFrom methods is
#' @importFrom dplyr filter across if_any if_any
#' @param filters *(optional)* list() with filters in the form list(Column="Value")
#' @returns data frame with lists of divisions
#'
#' @export
#' @keywords lists housegetdata
#' @examples \dontrun{
#' # Get list of all divisions
#' list_divisions()
#'
#' #Get list containing only Wills and Melbourne
#' list_divisions(filters=list(DivisionNm=c("Wills","Melbourne")))
#'  }
list_divisions <- function(filters=NULL){

   data <- load_auspol("house_electorates.zip")

   if(is(filters,"list")){
     for(i in length(filters)){

       data <- data |> filter(if_any(names(filters)[i], ~ .x %in% filters[[i]]))

     }
   }
   return(data)
}


#' List all political parties.
#' @description Lists all political parties that have participated from the 2004 Election onwards.
#' Parties are presented as recorded by the AEC. List can be filtered by party names matching a regular expression.
#' @importFrom methods is
#' @importFrom dplyr filter across if_any arrange mutate
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' @param filters (optional) list() with filters in the form list(Column="Value").
#' @param party_regex additional filter for party names, taking a regular expression.
#' @returns data frame with lists of divisions
#' @export
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all registered political parties
#' list_parties()
#' #
#' # Get list of all parties whose name start with "Australia"
#' list_parties(party_regex="^Australia")
#'  }
list_parties <- function(filters=NULL,party_regex=NULL){
  data <- load_auspol("house_parties.zip")

  if(is(filters,"list")){
    for(i in length(filters)){

      data <- data |> filter(if_any(names(filters)[i], ~  .x %in% filters[[i]]))

    }
  }

  if(!is.null(party_regex)){
    data <- data |> filter(if_any("PartyNm", ~ str_detect(.x,party_regex)))
  }

  data <- data |>
          mutate(dummy=TRUE)|>
          pivot_wider(values_from = .data$dummy,names_from=.data$Year) |>
          arrange("PartyAb","StateAb")

  return(data)
}

#' List all polling stations
#' @description Retrieve list of all polling station that been used from 2044 onwards.
#' Names as recorded by the AEC. List can be filtered by state, division names and
#' regular expressions matching their names.
#' @importFrom methods is
#' @importFrom dplyr filter across if_any
#' @param filters (optional) list() with filters in the form list(Column="Value")
#' @returns data frame with lists of polling stations
#' @export
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all registered parties
#' list_parties()
#' # Get list of polling places in the division of Hasluck
#' list_parties(list)
#'
#'  }
list_polling_places <- function(filters=NULL){
  data <- load_auspol("polling_places.zip")

  if(is(filters,"list")){
    for(i in length(filters)){

      data <- data |> filter(if_any(names(filters)[i], ~ .x %in% filters[[i]]))

    }
  }

  data <- data |>
    mutate(dummy=TRUE) |>
    pivot_wider(names_from = .data$Year, values_from = .data$dummy) |>
    distinct()

  return(data)
}

