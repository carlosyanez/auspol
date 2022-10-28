# Setup ----
library(here)
library(fs)
library(tidyverse)
library(rio)
library(arrow)

raw_files_dir <- here("data-raw","files")
dest_dir      <- here("data-raw","processed")

# Aux functions ----
clean_parties <- function(df,var1,var2){

  df %>%
    mutate(  {{var1}} := if_else({{var2}}=="Informal","Informal",{{var1}},{{var1}}),
             {{var2}} := case_when(
             {{var1}}=="NAFD" ~ "Non Affiliated",
             {{var1}}=="UNAM" ~ " Unendorsed and Ungrouped",
             TRUE ~  {{var2}}
           ))

}

load_data <- function(Chamber,Type,file_sources,base_path=raw_files_dir){
  df  <- tibble()

  rep_sources <- file_sources %>% filter(Chamber==Chamber& Type==Type)

  for(i in 1:nrow(rep_sources)){
    year  <- rep_sources[i,]$ElectionYear
    state <- rep_sources[i,]$State
    file  <- path(base_path,rep_sources[i,]$filename)

    print(str_c(i, " - ",file))

    df_i   <-  import(file) %>%
               add_column(Year=year,.before=1)

    df <- df %>%
          bind_rows(df_i)

    rm(df_i,year,state,file)

  }
  return(df)

}


# Get List ----

sources <- read_csv(here("data-raw","sources.csv"))

sources  <- sources %>%
  mutate(extension=if_else(str_detect(Source,"zip"),".zip",".csv")) %>%
  mutate(filename = str_c(ElectionYear,"-",
                          Chamber,"-",
                          Type,"-",
                          State,extension))


# Load data  - Reps primary vote ----

## Load and transform

representatives <- load_data("House","PrimaryVote",sources)

representatives<- representatives %>%
                  clean_parties(PartyAb,PartyNm) %>%
                  mutate(across(c("Elected","HistoricElected"), ~ if_else(.x=="Y",TRUE,FALSE,FALSE)),
                         SittingMemberFl=if_else(SittingMemberFl=="#",TRUE,FALSE)) %>%
                  replace_na(list(SittingMemberFl=FALSE))


write_parquet(representatives,path(dest_dir,"house_primary_vote.parquet"),compression = "brotli")
zip(path(dest_dir,"house_primary_vote.zip"),path(dest_dir,"house_primary_vote.parquet"))

# Load data  - Reps turnout  ----

reps_turnout <- load_data("House","Turnout",sources)
reps_turnout  <- reps_turnout %>% select(-StateNm)

write_parquet(resps_turnout,path(dest_dir,"house_turnout.parquet"),compression="brotli")



# Load data - Reps flow of preferences ----

unzipped_flow <- dir_create(path(raw_files_dir,"flow"))

#unzip files

flow <- sources %>% filter(Chamber =="House" & Type == "Flow")
unzipped_sources <- tibble()


for(i in 1:nrow(flow)){

  #record year for each file
  unzipped_sources_i  <-zip::zip_list(path(raw_files_dir,flow[i,]$filename)) %>%
                        select(filename) %>%
                        add_column(ElectionYear=flow[i,]$ElectionYear,
                                    State       = flow[i,]$State)

  unzipped_sources <- unzipped_sources %>%
                      bind_rows(unzipped_sources_i)

  if(!file_exists(path(raw_files_dir,flow[i,]$filename)))

   zip::unzip(path(raw_files_dir,flow[i,]$filename),
               exdir=unzipped_flow)

}

unzipped_sources$Chamber <- "House"
unzipped_sources$Type    <- "Flow"



for(state in unique(unzipped_sources$State)){

  house_flow <- load_data("House","Flow",unzipped_sources %>% filter(State==state),unzipped_flow)
  write_parquet(house_flow,path(dest_dir,str_c("house_flow_",state,".parquet")),compression="brotli")

}

