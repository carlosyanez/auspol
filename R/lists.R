##############################################################
### Functions to retrieve lists of ..electorates, parties ####
##############################################################


#' List all divisions
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


#' List all political parties participating in a election year, for a state, or matching a pattern in their full name
#' @importFrom methods is
#' @importFrom dplyr filter across if_any
#' @param filters (optional) list() with filters in the form list(Column="Value")
#' @returns data frame with lists of divisions
#' @export
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all registered political parties
#' list_parties()
#'  }
list_parties <- function(filters=NULL){
  data <- load_auspol("house_parties.zip")

  if(is(filters,"list")){
    for(i in length(filters)){

      data <- data |> filter(if_any(names(filters)[i], ~ .x %in% filters[[i]]))

    }
  }
  return(data)
}

#' List all polling stations
#' @importFrom methods is
#' @importFrom dplyr filter across if_any
#' @param filters (optional) list() with filters in the form list(Column="Value")
#' @returns data frame with lists of polling stations
#' @export
#' @keywords lists
#' @examples \dontrun{
#' # Get list of all registered parties
#' list_parties()
#'  }
list_polling_places <- function(filters=NULL){
  data <- load_auspol("polling_places.zip")

  if(is(filters,"list")){
    for(i in length(filters)){

      data <- data |> filter(if_any(names(filters)[i], ~ .x %in% filters[[i]]))

    }
  }
  return(data)
}

