# Based on https://github.com/walkerke/auspol/blob/master/R/helpers.R , released under MIT licence.

#' Set the cache directory to store parquet files with auspol
#'
#' @description By default, auspol uses the rappdirs package to determine a suitable location to store shapefiles
#' on the user's computer.  However, it is possible that the user would want to store shapefiles in a custom
#' location.  This function allows users to set the cache directory, and stores the result in the user's
#' .Renviron so that auspol will remember the location.
#'
#' Windows users: please note that you'll need to use double-backslashes or forward slashes
#' when specifying your cache directory's path in R.
#'
#' @param path The full path to the desired cache directory
#' @importFrom utils read.table write.table
#' @noRd
#' @examples \dontrun{
#' # Set the cache directory
#' auspol_cache_dir('PATH TO MY NEW CACHE DIRECTORY')
#'
#' # Check to see if it has been set correctly
#' Sys.getenv('AUSPOL_CACHE_DIR')
#' }
auspol_cache_dir <- function(path) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if(!dir.exists(path)){
    dir.create(path,recursive = TRUE)
  }


  if (!file.exists(renv)) {
    file.create(renv)
  }

  check <- readLines(renv)

  if (isTRUE(any(grepl("AUSPOL_CACHE_DIR", check)))) {
    oldenv <- read.table(renv,sep="=",stringsAsFactors = FALSE)
    newenv <- oldenv[!grepl("AUSPOL_CACHE_DIR", oldenv$V1), ]
    write.table(newenv, renv, quote = FALSE, sep = "=",
                col.names = FALSE, row.names = FALSE)
  }

  var <- paste0("AUSPOL_CACHE_DIR=", "'", path, "'")

  write(var, renv, sep = "\n", append = TRUE)
  message(sprintf("Your new auspol cache directory is %s. \nTo use now, restart R or run `readRenviron('~/.Renviron')`", path))

}


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
#'
#' @return dataframe
#' @noRd
load_auspol <- function(auspol_file,
                       refresh=getOption("auspol_refresh", FALSE),
                       auspol_type=NULL,force=FALSE) {




  obj <- NULL


  if (Sys.getenv("AUSPOL_CACHE_DIR") != "") {
      cache_dir <- Sys.getenv("AUSPOL_CACHE_DIR")
      cache_dir <- path.expand(cache_dir)
  } else {
      cache_dir <- auspol_cache_dir(path("auspol"))
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

#' Helper function to update/download  data
#' @param file file name from repository. By default, downloads all files
#' @returns nothing
#' @export
#' @keywords helpers
#'
update_data <- function(file="all"){

  if(file=="all"){
    file <- pb_download_url(repo = "carlosyanez/auspol",
                    tag = "data")
  }

  for(f in file){
    load_auspol(f,force=TRUE)
  }

}


#' Helper function generate colour palette
#' @returns nothing
#' @importFrom RColorBrewer brewer.pal
#' @noRd
#'
manage_colours <- function(extra_colours,extra_values){

  #Manage Colours
  colours <- party_colours(extra_colours)
  colours <- colours[names(colours) %in% c(unique(extra_values))]

  colours_nl_names <- unique(extra_values)[!(unique(extra_values) %in% names(colours))]

  if(length(colours_nl_names)>0){

    colours_nl <- brewer.pal(name = "Set3",n=12)
    repeat_times <- ceiling(length(colours_nl_names)/12)
    colours_nl <- rep(colours_nl,repeat_times)[1:length(colours_nl_names)]

    colours_nl <- as.vector(colours_nl)

    colour_names <- c(names(colours),colours_nl_names)

    colours <- unique(c(colours,colours_nl))
    names(colours) <- colour_names
  }

  return(colours)
}

