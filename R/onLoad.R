.onLoad <- function(libname, pkgname){
  packageStartupMessage("Loading {auspol}")

  #default cache
  home       <- Sys.getenv("HOME")
  cache_path <- file.path(home, ".auspol_cache")

  auspol_cache_dir(cache_path)


}
