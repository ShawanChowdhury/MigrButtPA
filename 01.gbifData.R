# Loading required libraries
library(rgbif)
library(readr)
library(tidyverse)

# Reading species list
sp_list <- read.csv("data/migr_species.csv", header = T)
head(sp_list)
sp <- unique(sp_list$species)

# match the names 
gbif_taxon_keys <- 
  sp_list %>% 
  pull("species") %>% 
  name_backbone_checklist()  %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) # get the gbif taxonkeys
# gbif_taxon_keys should be a long vector like this c(2977832,2977901,2977966,2977835,2977863)
# !!very important here to use pred_in!!

occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",
  user= "shawan_zl",pwd= "nabolakothaFB89",email= "shawan1094061@gmail.com"
)

# Using the regulardownload function, we can only download a maximum of 200,000/species. This way, we obtained
# all the records from GBIF. Because of sharing the user details, we obtained the entire download information
# in my personal GBIF ID. Here is the DOI: https://www.gbif.org/occurrence/download/0297462-220831081235567.
