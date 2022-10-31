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
#' @export
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
#' @importFrom  zip unzip
#' @param auspol_file name of the file to download.
#' @param refresh Whether to re-download shapefiles if cached. Defaults to value of the global
#' option "auspol_refresh" if that option is, and FALSE if not. This will override the behavior
#' set in "auspol_refresh" option if a value (TRUE or FALSE) is provided.
#' @param auspol_type Added as an attribute to return object (used internally).
#' @param class Class of return object. Must be one of "sf" (the default) or "sp".
#' @param progress_bar If set to FALSE, do not display download progress bar
#' (helpful for R Markdown documents). Defaults to TRUE.
#' @param keep_zipped_shapefile If set to TRUE, do not delete zipped shapefile
#' (stored in temporary directory or AUSPOL_CACHE_DIR depending on the configuration of
#' global option "auspol_use_cache"). Defaults to FALSE.
#' @param query # Defunct. Has no effect.
#'
#' @return dataframe
#'
load_auspol <- function(auspol_file,
                       refresh=getOption("auspol_refresh", FALSE),
                       auspol_type=NULL,
                       class = getOption("auspol_class", "sf"),
                       progress_bar = TRUE,
                       keep_zipped_shapefile = FALSE,
                       query = NULL) {




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

  if (!file.exists(file_detect)) {

        url  <- pb_download_url(auspol_file,
                                repo = "carlosyanez/auspol",
                                tag = "data")

        download.file(url,file_loc)

        if(str_detect(file_loc,"zip")){
          unzip(file_loc,exdir = cache_dir)
          file.remove(file_loc)
        }

  }


  obj <- read_parquet(file_detect)
  return(obj)

}

