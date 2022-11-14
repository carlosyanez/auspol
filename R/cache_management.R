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
#' cache_dir('PATH TO MY NEW CACHE DIRECTORY')
#'
#' # Check to see if it has been set correctly
#' Sys.getenv('auspol_cache_dir')
#' }
manage_cache_dir <- function(path) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if(!dir.exists(path)){
    dir.create(path,recursive = TRUE)
  }


  if (!file.exists(renv)) {
    file.create(renv)
  }

  check <- readLines(renv)

  if (isTRUE(any(grepl('auspol_cache_dir', check)))) {
    oldenv <- read.table(renv,sep="=",stringsAsFactors = FALSE)
    newenv <- oldenv[!grepl('auspol_cache_dir', oldenv$V1), ]
    write.table(newenv, renv, quote = FALSE, sep = "=",
                col.names = FALSE, row.names = FALSE)
  }

  var <- paste0("auspol_cache_dir=", "'", path, "'")

  write(var, renv, sep = "\n", append = TRUE)
  message(sprintf("Your new cache directory is %s. \nTo use now, restart R or run `readRenviron('~/.Renviron')`", path))

}

#' Helper function to update/download  data
#' @param file vectors with file name from repository. By default, downloads all files
#' @importFrom fs file_exists
#' @returns nothing
#' @export
#' @keywords helpers
#'
data_update <- function(file=NULL){

  if(is.null(file)){
    file <- pb_download_url(repo = "carlosyanez/auspol",
                    tag = "data")
    file<- str_remove(file,"https://github.com/carlosyanez/auspol/releases/download/data/")
  }

  for(f in file){
    if(file_exists(f)) data_delete(f)
    load_auspol(f,force=TRUE)
  }

}


#' Helper function to update/download  data
#' @importFrom fs dir_info
#' @returns nothing
#' @export
#' @keywords helpers
data_info <- function(){
  cache_dir <- Sys.getenv('auspol_cache_dir')
  dir_info(cache_dir)
}

#' Helper function to update/download  data
#' @importFrom fs dir_ls file_delete
#' @returns nothing
#' @param file to delete - defaults to all of them
#' @export
#' @keywords helpers
data_delete <- function(file=NULL){
  if(is.null(file)){
    file <- data_info()$path
  }

  file_delete(file)
}

#' Helper function to update/download  data
#' @importFrom  fs file_copy
#' @returns nothing
#' @param file file to import to the cache
#' @export
#' @keywords helpers
data_import <- function(file){
  cache_dir <- Sys.getenv('auspol_cache_dir')
  file_copy(file,path(cache_dir,file),TRUE)

}

#' Helper function to find cache folder
#' @returns nothing
#' @export
#' @keywords helpers
find_cache<- function(){
  cache_dir <- Sys.getenv('auspol_cache_dir')
  return(cache_dir)

}
