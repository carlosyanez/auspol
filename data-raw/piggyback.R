library(piggyback)
library(here)
library(fs)

files_dir      <- here("data-raw","processed")
repo           <- "carlosyanez/auspol"
version        <- "data"

#pb_new_release(repo,version)

files_list <- dir_ls(files_dir)



pb_upload(file=here("data-raw/processed/house_primary_vote.zip"),repo,version)

