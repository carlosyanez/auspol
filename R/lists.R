#######################################################
### Functions to retrieve lists of ..electorates, parties ####
#######################################################


#' List all divisions
#' @returns data frame with lists of divisions
#' @export get_house_primary_vote
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all divisions
#' list_divisions()
#'  }
list_divisions <- function(){

   load_auspol("house_electorates.zip")

}

list_electorates <- list_divisions

#' List all political parties participating in a election year, for a state, or matching a pattern in their full name
#' @returns data frame with lists of divisions
#' @export get_house_primary_vote
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all divisions in 2013
#' list_parties()
#'  }
list_parties <- function(){
  load_auspol("house_parties.zip")
}

