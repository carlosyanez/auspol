library(here)
library(fs)
library(tidyverse)

raw_files_dir <- here("data-raw","files")

# Get List ----

sources <- read_csv(here("data-raw","sources.csv"))

sources  <- sources%>%
  mutate(extension=if_else(str_detect(Source,"zip$"),".zip",".csv")) %>%
  mutate(filename = str_c(ElectionYear,"-",
                          Chamber,"-",
                          Type,"-",
                          State,extension))



# Download all files ----

for(i in 1:nrow(sources)){
  source       <- sources[i,]$Source
  destination  <- path(raw_files_dir,sources[i,]$filename)

  if(!file_exists(destination))
    download.file(source,destination)

}
