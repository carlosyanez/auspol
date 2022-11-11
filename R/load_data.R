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
#'
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
    cache_dir <- cache_dir(path("auspol"))
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
