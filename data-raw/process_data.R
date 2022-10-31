# Setup ----
library(here)
library(fs)
library(tidyverse)
library(rio)
library(arrow)
library(zip)

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

load_data <- function(chamber,type,file_sources,base_path=raw_files_dir){
  df  <- tibble()

  print(chamber)
  print(type)
  rep_sources <- file_sources %>% filter(Chamber==chamber& Type==type)
  print(nrow(rep_sources))

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

save_zip_parquet <- function(df,name,dest_dir){

  zipname     <- str_c(name,".zip")
  parquetname <- str_c(name,".parquet")

  write_parquet(df,path(dest_dir,parquetname),compression="brotli")
  zip::zip(zipfile=path(dest_dir,zipname),
           files=path(dest_dir,parquetname),
           mode = "cherry-pick")
  fs::file_delete(path(dest_dir,parquetname))

}


# Get List ----

sources <- read_csv(here("data-raw","sources.csv"))

sources  <- sources %>%
  mutate(extension=if_else(str_detect(Source,"zip"),".zip",".csv")) %>%
  mutate(filename = str_c(ElectionYear,"-",
                          Chamber,"-",
                          Type,"-",
                          State,extension))


# Load House  - Reps primary vote ----

## Load and transform - primary vote

representatives <- load_data("House","PrimaryVote",sources)

representatives<- representatives %>%
                  clean_parties(PartyAb,PartyNm) %>%
                  mutate(across(c("Elected","HistoricElected"), ~ if_else(.x=="Y",TRUE,FALSE,FALSE)),
                         SittingMemberFl=if_else(SittingMemberFl=="#",TRUE,FALSE)) %>%
                  replace_na(list(SittingMemberFl=FALSE))

save_zip_parquet(representatives,"house_primary_vote",dest_dir)


# Load House - get list of electorates ----

electorates <- representatives %>%
               distinct(Year,StateAb,DivisionID,DivisionNm) %>%
               filter(!is.na(DivisionID))   %>%
               filter(!is.na(StateAb)) %>%
               mutate(dummy=TRUE)      %>%
               pivot_wider(names_from = Year,values_from = dummy) %>%
               arrange(StateAb,DivisionID)

save_zip_parquet(electorates,"house_electorates",dest_dir)


# Load House - List of Parties ----

parties <- representatives %>%
           distinct(Year,StateAb,PartyAb,PartyNm) %>%
           filter(!(PartyAb %in% c("Informal","NAFD","UNAM"))) %>%
           arrange(PartyAb,StateAb,Year)


save_zip_parquet(parties,"house_parties",dest_dir)


# Load House  - Reps turnout  ----

reps_turnout <- load_data("House","Turnout",sources)
reps_turnout  <- reps_turnout %>% select(-StateNm)

save_zip_parquet(resps_turnout,"house_turnout",dest_dir)


# Load House - Reps flow of preferences ----

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

  save_zip_parquet(house_flow,str_c("house_flow_",state),dest_dir)


}


# Load House  - Elected -----

mps <- load_data("House","Elected",sources %>% filter(Type=="Elected"))
save_zip_parquet(mps,"house_elected",dest_dir)

# Load House - Two candidate preferred -----



unzipped_flow <- dir_create(path(raw_files_dir,"2cpflow"))

#unzip files

flow <- sources %>% filter(Chamber =="House" & Type == "TwoCandidatePref")
unzipped_sources <- tibble()


for(i in 1:nrow(flow)){

  #record year for each file
  unzipped_sources_i  <-zip::zip_list(path(raw_files_dir,flow[i,]$filename)) %>%
    select(filename) %>%
    add_column(ElectionYear=flow[i,]$ElectionYear,
               State       = flow[i,]$State)

  unzipped_sources <- unzipped_sources %>%
    bind_rows(unzipped_sources_i)

  #if(!file_exists(path(raw_files_dir,flow[i,]$filename)))

    zip::unzip(path(raw_files_dir,flow[i,]$filename),
               exdir=unzipped_flow)

}

unzipped_sources$Chamber <- "House"
unzipped_sources$Type    <- "2CP"


for(state in unique(unzipped_sources$State)){

  twocp_flow <- load_data("House","2CP",unzipped_sources %>% filter(State==state),unzipped_flow)

  save_zip_parquet(twocp_flow,str_c("house_2CP_",state),dest_dir)


}
