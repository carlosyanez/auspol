
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
#knit(here(orig_folder,vignettes[i]), here(vignettes_folder,vignettes[i]))

pkgdown::build_articles()

##compile site

pngs <- vignettes <- dir_ls(vignettes_folder,regexp=".png")
article_img<-here("docs","articles","articles")
dir_create(article_img)
file_copy(pngs, article_img)
