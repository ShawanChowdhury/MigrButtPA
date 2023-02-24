# Loading required libraries
library(dplyr)
library(countrycode)
library(CoordinateCleaner)
library(tidyverse)
library(rworldmap)

##############################
# Cleaning GBIF data
# Reading GBIF data
gbif_data <- fread("data/gbif/gbif.csv", header = T, sep="\t")

gbif_data <- gbif_data %>% 
  dplyr::select("species", "decimalLongitude", "decimalLatitude", "countryCode", 
                "gbifID", "family", "taxonRank", "coordinateUncertaintyInMeters", "year", "month",
                "basisOfRecord", "institutionCode")

# Removing blank cells
gbif_data <- gbif_data[!(is.na(gbif_data$species) | gbif_data$species == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$decimalLongitude) | gbif_data$decimalLongitude == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$decimalLatitude) | gbif_data$decimalLatitude == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$month) | gbif_data$month == ""),]

# Removing records before 1950
gbif_data <- gbif_data %>% 
  filter(year > 1949)

# # Removing duplicated records
# gbif_data1 <- gbif_data[!duplicated(gbif_data),]

# # Cleaning memory
# rm(gbif_data)

# Convert country code from ISO2c to ISO3c
gbif_data$countryCode <-  countrycode(gbif_data$countryCode, origin =  'iso2c', destination = 'iso3c')

# Flag problems
gbif_filter <- data.frame(gbif_data)
flags <- clean_coordinates(x = gbif_filter, lon = "decimalLongitude", lat = "decimalLatitude",
                           countries = "countryCode", 
                           species = "species",
                           tests = c("centroids", "gbif",
                                     "zeros", "countries")) # most test are on by default

# summary(flags)
# plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

# Exclude problematic records
gbif_filter <- gbif_filter[flags$.summary,]

# Filtering required columns
gbif_filter <- gbif_filter %>% 
  dplyr::select("species", "decimalLongitude", "decimalLatitude", "month")

# Removing duplicated records
gbif_filter_dedup <- gbif_filter[!duplicated(gbif_filter),]

fwrite(gbif_filter_dedup, "data/cleanedRecords_gbif.csv")

# Cleaning memory
rm(gbif_data1, flags)