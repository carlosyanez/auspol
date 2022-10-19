library(here)
library(fs)
library(tidyverse)

raw_files_dir <- here("data-raw","files")

# Get List ----

sources <- read_csv(here("data-raw","sources.csv"))

sources  <- sources%>%
  mutate(filename = str_c(ElectionYear,"-",
                          Type,"-",
                          State,".csv"))


# Download all files ----

for(i in 1:nrow(sources)){
  source       <- sources[i,]$Source
  destination  <- path(raw_files_dir,sources[i,]$filename)

  download.file(source,destination)

}

# Load data ----

representatives <- tibble()

rep_sources <- sources %>% filter(Type=="Representatives")

for(i in 1:nrow(rep_sources)){

  year  <- rep_sources[i,]$ElectionYear
  state <- rep_sources[i,]$ElectionYear
  file  <- rep_sources[i,]$filename

  df    <-  read_csv(path(raw_files_dir,file))

  representatives <- representatives %>%
                     bind_rows(
                        read_csv(path(raw_files_dir,file),skip=1) %>%
                          mutate()
                     )

}


