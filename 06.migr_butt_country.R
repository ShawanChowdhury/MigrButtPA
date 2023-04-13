# Loading required libraries
library(sp)
library(sf)
library(raster)
library(dismo)
library(stringr)
library(rgdal)
library(terra)
library(exactextractr)
library(dplyr)
library(tidyverse)

# List of rasters
sdm_done <- list.files(path = "output/sdm/",pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
sdm_done <- sdm_done[stringr::str_detect(sdm_done, "Binary")]

# sdm_done <- sdm_done[1:2]

# Converting it to a dataframe
sdm_done_df <- as.data.frame(sdm_done)

# Reading dissolved world map
world <- st_read("data/layers/world_dissolved.shp")
world_df <- as.data.frame(world)

# Creating a blank dataframe to combine all the output with this
df <- data.frame()

for (i in 1:(NROW(sdm_done_df))) try({
  
  print(i)
  
  r_loc <- sdm_done_df[i,]
  
  r <- raster(r_loc)
  
  # Extracting species name
  r_name <- gsub("output/sdm//", "", r_loc)
  r_name <- gsub("Binary_", "", r_name)
  r_name <- gsub(".tif", "", r_name)
  
  r_name_df <- as.data.frame(r_name)
  r_name_df <- r_name_df %>% 
    separate(r_name, sep = "/", into = c("A", "B"))
  species <- r_name_df[,1]
  
  # Extracting season name
  season <- readr::parse_number(r_name)
  
  # Intersecting species suitability map with the world map to identify suitable countries
  sp_country <- exact_extract(r, world)
  
  for (j in 1:(NROW(sp_country))) {
    
    int_df <- as.data.frame(sp_country[j])
    
    if(NROW(int_df) > 0){
      
      country <- world_df[j,]
      country <- country[,2]
      
      int_sum <- int_df %>% 
        dplyr::mutate(country = country, species = species, season = season) %>% 
        dplyr::select(species, season, country, coverage_fraction)
      
      int_sum_country <- int_sum %>% 
        group_by(species, season, country) %>% 
        dplyr::summarise(total_area = sum(coverage_fraction))
      
      df <- rbind(df, int_sum_country)
      
    }
  }
  
}, silent = FALSE)

# Renaming column
colnames(df)[4] <- "area"

# Exporting output
write_csv(df, "output/species_suitability_country_original.csv")

# Calculating proportion
df_sum <- df %>% 
  group_by(species, season) %>% 
  mutate(tot_area = sum(area))

df_sum <- df_sum %>% 
  group_by(species, season, country, tot_area) %>% 
  dplyr::mutate(prop_area = ((area/tot_area)*100))

# Exporting output
write_csv(df_sum, "output/species_suitability_country_summarised.csv")
