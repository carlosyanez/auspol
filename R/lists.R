#######################################################
### Functions to retrieve lists of ..electorates, parties ####
#######################################################


#' List all divisions
#' @returns data frame with lists of divisions
#' @export
#' @keywords lists housegetdata

#' @examples \dontrun{
#' # Get list of all divisions
#' list_divisions()
#'  }
list_divisions <- function(){

   load_auspol("house_electorates.zip")

}


#' List all political parties participating in a election year, for a state, or matching a pattern in their full name
#' @returns data frame with lists of divisions
#' @export
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all registered political parties
#' list_parties()
#'  }
list_parties <- function(){
  load_auspol("house_parties.zip")
}

#' List all polling stations
#' @returns data frame with lists of polling stations
#' @export
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all registered parties
#' list_parties()
#'  }
list_polling_stations <- function(){
  load_auspol("polling_places.zip")
}

