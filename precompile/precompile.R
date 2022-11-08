
# Precompiled vignettes

library(knitr)
library(here)
library(fs)
library(stringr)


orig_folder <- here("precompile")
vignettes_folder <- here("vignettes")


vignettes <- dir_ls(orig_folder,regexp=".Rmd") |> str_remove_all(str_c(orig_folder,"/"))

for(vig in vignettes){
 knit(here(orig_folder,vig), here(vignettes_folder,vig))
}

i<-1
knit(here(orig_folder,vignettes[i]), here(vignettes_folder,vignettes[i]))
