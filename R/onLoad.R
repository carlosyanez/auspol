.onLoad <- function(libname, pkgname){
 # packageStartupMessage("Loading {auspol}")

  #default cache
  home       <- Sys.getenv("HOME")
  cache_path <- file.path(home, ".auspol_cache")

  if(!dir.exists(cache_path)){
    manage_cache_dir(cache_path)
    Sys.setenv(aussie_cache_dir=cache_path)

  }

}
