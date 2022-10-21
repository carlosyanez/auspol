# Setup ----
library(here)
library(fs)
library(tidyverse)
library(rio)

raw_files_dir <- here("data-raw","files")
dest_dir      <- here("data-raw","processed")

# Aux function ----
clean_parties <- function(df,var1,var2){

  df %>%
    mutate(  {{var1}} := if_else({{var2}}=="Informal","Informal",{{var1}},{{var1}}),
             {{var2}} := case_when(
             {{var1}}=="NAFD" ~ "Non Affiliated",
             {{var1}}=="UNAM" ~ " Unendorsed and Ungrouped",
             TRUE ~  {{var2}}
           ))

}


# Get List ----

sources <- read_csv(here("data-raw","sources.csv"))

sources  <- sources%>%
  mutate(filename = str_c(ElectionYear,"-",
                          Chamber,"-",
                          Type,"-",
                          State,".csv"))


# Load data  - Reps primary vote ----

## Load and transform
representatives <- tibble()

rep_sources <- sources %>% filter(Chamber=="House"& Type=="PrimaryVote")

for(i in 1:nrow(rep_sources)){
  print(i)
  year  <- rep_sources[i,]$ElectionYear
  state <- rep_sources[i,]$State
  file  <- path(raw_files_dir,rep_sources[i,]$filename)


  df    <-  import(file) %>%
            add_column(Year=year,.before=1)
  representatives <- representatives %>%
                     bind_rows(df)
  rm(df,year,state,file)

}


representatives<- representatives %>%
                  clean_parties(PartyAb,PartyNm) %>%
                  mutate(across(c("Elected","HistoricElected"), ~ if_else(.x=="Y",TRUE,FALSE,FALSE)),
                         SittingMemberFl=if_else(SittingMemberFl=="#",TRUE,FALSE)) %>%
                  replace_na(list(SittingMemberFl=FALSE))


saveRDS(representatives, path(dest_dir,"house_primary_vote.rds"))
