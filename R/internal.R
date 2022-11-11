#############################################################
### Internal functions ####
#############################################################



#' Obtain get data from filename
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr filter if_any
#' @importFrom rlang .data
#' @importFrom methods is
#' @param  division vector with division names
#' @param  year vector with election years
#' @param filename where the file is (parquet or zip containing parquet file)
#' @noRd
get_auspol_house_data <- function(filename,division=NULL, year=NULL){

  df <- load_auspol(filename)


  if(!is.null(year)){
      df <- df |> filter(if_any("Year",~ .x %in% year))

  }
  if(!is.null(division)){
      df <- df |> filter(if_any("DivisionNm", ~ .x %in% division))

  }

  return(df)
}


## Based on https://github.com/walkerke/auspol/blob/master/R/helpers.R , released under MIT licence.


#' Helper function to download  data
#'
#' @importFrom  piggyback pb_download_url
#' @importFrom  arrow read_parquet
#' @importFrom  stringr str_remove str_c str_detect
#' @importFrom utils download.file
#' @importFrom  zip unzip
#' @importFrom fs path
#' @param auspol_file name of the file to download.
#' @param refresh Whether to re-download shapefiles if cached. Defaults to value of the global
#' option "auspol_refresh" if that option is, and FALSE if not. This will override the behavior
#' set in "auspol_refresh" option if a value (TRUE or FALSE) is provided.
#' @param auspol_type Added as an attribute to return object (used internally).
#' @return dataframe
#' @noRd
load_auspol <- function(auspol_file,
                        refresh=getOption("auspol_refresh", FALSE),
                        auspol_type=NULL,force=FALSE) {




  obj <- NULL


  if (Sys.getenv("cache_dir") != "") {
    cache_dir <- Sys.getenv("cache_dir")
    cache_dir <- path.expand(cache_dir)
  } else {
    cache_dir <- manage_cache_dir(path("auspol"))
  }

  file_loc <- file.path(cache_dir, auspol_file)

  if(str_detect(file_loc,"zip")){
    file_detect <- str_c(str_remove(file_loc,"zip"),"parquet")
  }else{
    file_detect <- file_loc
  }

  if (!file.exists(file_detect)|force) {


    if(!str_detect(auspol_file,"https")){
      url  <- pb_download_url(auspol_file,
                              repo = "carlosyanez/auspol",
                              tag = "data")
    }else{
      url <- auspol_file
    }

    download.file(url,file_loc)

    if(str_detect(file_loc,"zip")){
      unzip(file_loc,exdir = cache_dir)
      file.remove(file_loc)
    }

  }


  obj <- read_parquet(file_detect)
  return(obj)

}




#' Check if division exists for a given year
#' @return data frame Division Name and state
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
#' @param  division vector with division names
#' @param  year vector with election years
#' @param filename where the file is (parquet or zip containing parquet file)
#' @noRd
check_division <- function(division,year){

  list_divisions() |>
    pivot_longer(-c("StateAb","DivisionID","DivisionNm"),
                 values_to="flag",names_to="ElectionYear") |>
    filter(if_any("flag", ~ .x==TRUE)) |>
    filter(if_any("DivisionNm", ~ .x==division)) |>
    filter(if_any("ElectionYear", ~ .x==year)) |>
    select(-matches(c("flag")))

}


