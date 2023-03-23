# Loading required libraries
library(rgbif)
library(readr)
library(tidyverse)
library(data.table)

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
  user= "USER",pwd= "PASSWORD",email= "EMAIL"
)

# Using the regulardownload function, we can only download a maximum of 200,000/species. This way, we obtained
# all the records from GBIF. Because of sharing the user details, we obtained the entire download information
# in my personal GBIF ID. Here is the DOI: https://www.gbif.org/occurrence/download/0297462-220831081235567.

################################################
gbif <- fread("data/gbif/gbif/0297462-220831081235567.csv", header = T, sep="\t")
gbif <- gbif %>% 
  dplyr::select("species", "decimalLongitude", "decimalLatitude", "countryCode", 
                "gbifID", "family", "taxonRank", "coordinateUncertaintyInMeters", "year", "month",
                "basisOfRecord", "institutionCode")
# During my previous encounter with these data, I noticed that synonyms were included into different rows.
# To control this, we are renaming the following species.

# Renaming species
gbif <- gbif %>% mutate(species = str_replace(species, "Glutophrissa epaphia", "Appia epaphia"))
gbif <- gbif %>% mutate(species = str_replace(species, "Glutophrissa drusilla", "Appias drusilla"))
gbif <- gbif %>% mutate(species = str_replace(species, "Colias alfacariensis", "Colias sareptensis"))
gbif <- gbif %>% mutate(species = str_replace(species, "Heliconius aliphera", "Eueides aliphera"))
gbif <- gbif %>% mutate(species = str_replace(species, "Occidryas editha", "Euphydryas editha"))
gbif <- gbif %>% mutate(species = str_replace(species, "Euploea nemertes", "Euploea eunice"))
gbif <- gbif %>% mutate(species = str_replace(species, "Euploea diocletianus", "Euploea radamanthus"))
gbif <- gbif %>% mutate(species = str_replace(species, "Craspedophorus gratiosus", "Eurema gratiosa"))
gbif <- gbif %>% mutate(species = str_replace(species, "Pyrisitia lisa", "Eurema lisa"))
gbif <- gbif %>% mutate(species = str_replace(species, "Abaeis nicippe", "Eurema nicippe"))
gbif <- gbif %>% mutate(species = str_replace(species, "Euthalia aeropa", "Lexias aeropa"))
gbif <- gbif %>% mutate(species = str_replace(species, "Anaea philumena", "Memphis philumena"))
gbif <- gbif %>% mutate(species = str_replace(species, "Oeneis polixenes", "Papilio polyxenes"))
gbif <- gbif %>% mutate(species = str_replace(species, "Eresia aveyrana", "Phyciodes mylitta"))
gbif <- gbif %>% mutate(species = str_replace(species, "Lycaena acmon", "Plebejus acmon"))
gbif <- gbif %>% mutate(species = str_replace(species, "Cyaniris semiargus", "Polyommatus semiargus"))
gbif <- gbif %>% mutate(species = str_replace(species, "Danaus hamata", "Tirumala hamata"))
gbif <- gbif %>% mutate(species = str_replace(species, "Tellervo septentrionis", "Tirumala septentrionis"))
gbif <- gbif %>% mutate(species = str_replace(species, "Bassaris itea", "Vanessa itea"))
gbif <- gbif %>% mutate(species = str_replace(species, "Abaeis nicippe", "Eurema nicippe"))
gbif <- gbif %>% mutate(species = str_replace(species, "Colias behrii", "Colias eurymus"))
gbif <- gbif %>% mutate(species = str_replace(species, "Euterpe hylonome", "Actinote hylonome"))
gbif <- gbif %>% mutate(species = str_replace(species, "Hypolimna anomala", "Hypolimnas anomala"))
gbif <- gbif %>% mutate(species = str_replace(species, "Nymphalis canace", "Kaniska canace"))
gbif <- gbif %>% mutate(species = str_replace(species, "Nymphalis vaualbum", "Nymphalis I-album"))
gbif <- gbif %>% mutate(species = str_replace(species, "Palaeochrysophanus hippothoe", "Lycaena hippothoe"))
gbif <- gbif %>% mutate(species = str_replace(species, "Papilio sarpedon", "Graphium sarpedon"))


################################################
# When I was inspecting the species-wise data, I noticed that there are many more species than I requested for.
# It's because GBIF did not have data for some species and it downloaded the entire genus instead. Besides, some 
# species were entirely missing from the dataset. For this, I manually downloaded data that I needed in this analysis.
# https://doi.org/10.15468/dl.aje9rq
################################################
gbif1 <- fread("data/gbif/gbif1/occurrence.txt", header = T, sep="\t")
gbif1 <- gbif1 %>% 
  dplyr::select("species", "decimalLongitude", "decimalLatitude", "countryCode", 
                "gbifID", "family", "taxonRank", "coordinateUncertaintyInMeters", "year", "month",
                "basisOfRecord", "institutionCode")

# I am following the same renaming approach that I followed earlier.
# Renaming species
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Glutophrissa epaphia", "Appia epaphia"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Glutophrissa drusilla", "Appias drusilla"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Colias alfacariensis", "Colias sareptensis"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Heliconius aliphera", "Eueides aliphera"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Occidryas editha", "Euphydryas editha"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Euploea nemertes", "Euploea eunice"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Euploea diocletianus", "Euploea radamanthus"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Craspedophorus gratiosus", "Eurema gratiosa"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Pyrisitia lisa", "Eurema lisa"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Abaeis nicippe", "Eurema nicippe"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Euthalia aeropa", "Lexias aeropa"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Anaea philumena", "Memphis philumena"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Oeneis polixenes", "Papilio polyxenes"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Eresia aveyrana", "Phyciodes mylitta"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Lycaena acmon", "Plebejus acmon"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Cyaniris semiargus", "Polyommatus semiargus"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Danaus hamata", "Tirumala hamata"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Tellervo septentrionis", "Tirumala septentrionis"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Bassaris itea", "Vanessa itea"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Abaeis nicippe", "Eurema nicippe"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Colias behrii", "Colias eurymus"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Euterpe hylonome", "Actinote hylonome"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Hypolimna anomala", "Hypolimnas anomala"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Nymphalis canace", "Kaniska canace"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Nymphalis vaualbum", "Nymphalis I-album"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Palaeochrysophanus hippothoe", "Lycaena hippothoe"))
gbif1 <- gbif1 %>% mutate(species = str_replace(species, "Papilio sarpedon", "Graphium sarpedon"))

# I will merge both GBIF files to create a single dataframe before cleaning
gbif_combined <- rbind(gbif, gbif1)

# Now I only want to keep species that I need [migratory butterflies]
migr_sp <- read.csv("data/migr_species.csv", header = T)

gbif_combined <- dplyr::left_join(migr_sp, gbif_combined, by = "species", multiple = "all")
# Removing 'NA' values
gbif_combined <- gbif_combined[complete.cases(gbif_combined), ]

# Total 18814922 records for 483 species

# Exporting output
fwrite(gbif_combined, "data/gbif_combined.csv")
