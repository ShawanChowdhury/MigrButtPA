# Loading required libraries
library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(maptools)
library(rgeos)
library(data.table)
library(stringr)
library(rgdal)
library(tidyverse)

# # Reprojecting PA raster
# pa <- raster("data/layers/PA_1km_up.tif")
# sdm <- raster("test/ov_Acraea_serena.tif")
# 
# pa_reproject <- projectRaster(pa, crs = crs(sdm), res = res(sdm))
# 
# writeRaster(pa_reproject, "data/layers/pa_re.tif")

# Reading PA raster
pa <- raster("data/layers/pa_re.tif")

# Reading test species
test <- raster("test/ov_Acraea_serena.tif")

tifs <- list.files(path = "output/sdm/",pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
tifs <- tifs[stringr::str_detect(tifs, "Binary")]

# Converting it to a dataframe
tifs_df <- as.data.frame(tifs)

# Creating a blank dataframe to combine all the output with this
df <- data.frame()

for (i in 1:(NROW(tifs_df)))try({
  
  print(i)
  
  r_loc <- tifs_df[i,]
  
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
  
  # Cropping PA layer to match the extent of the suitability map
  pa_crop <- crop(pa, r)
  pa_crop <- resample(pa_crop, r, method = "ngb")
  # extent(pa_crop) <- extent(r)
    
  # Protected area overlap (CH)
  sdm.area <- cellStats(r, "sum") * 21.625
  ov.sdm <- cellStats(raster::Which(pa_crop > 0 & r > 0), "sum") * 21.625
  
  # Saving Model Output
  x <- cbind(sdm.area, ov.sdm)
  output <- 
    as.data.frame(x) %>% 
    mutate(species = species, season = season) %>% 
    select(species, season, sdm.area, ov.sdm)
  
  df <- rbind(df, output)
  
}, silent = FALSE)

# Calculating PA coverage proportion
df_sum <- df %>% 
  mutate(prop_coverage = (ov.sdm/sdm.area)*100)

# Exporting output
write_csv(df_sum, "output/pa_coverage.csv")
