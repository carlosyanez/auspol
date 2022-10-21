library(here)
library(fs)
library(tidyverse)

raw_files_dir <- here("data-raw","files")

# Get List ----

sources <- read_csv(here("data-raw","sources.csv"))

sources  <- sources%>%
  mutate(filename = str_c(ElectionYear,"-",
                          Chamber,"-",
                          Type,"-",
                          State,".csv"))


# Download all files ----

for(i in 1:nrow(sources)){
  source       <- sources[i,]$Source
  destination  <- path(raw_files_dir,sources[i,]$filename)

  if(!file_exists(destination))
    download.file(source,destination)

}
