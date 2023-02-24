# Loading required libraries
library(dplyr)
library(tidyverse)
library(rworldmap)
library(sp)
library(sf)
library(raster)

# Importing data
data <- read_csv("data/cleanedRecords_gbif.csv")

################################################
# Converting spatial data to shapefile
class(data)
coordinates(data) <- ~decimalLongitude + decimalLatitude
projection(data) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Importing zoogeographic region shapefiles
afr <- st_read("data/layers/regions/Afrotropical.shp")
aus <- st_read("data/layers/regions/Australian.shp")
mad <- st_read("data/layers/regions/Madagascan.shp")
nea <- st_read("data/layers/regions/Nearctic.shp")
neo <- st_read("data/layers/regions/Neotropical.shp")
oce <- st_read("data/layers/regions/Oceania.shp")
ori <- st_read("data/layers/regions/Oriental.shp")
pan <- st_read("data/layers/regions/Panamanian.shp")
sah <- st_read("data/layers/regions/SaharoArabian.shp")
pal <- st_read("data/layers/regions/Palaearctic.shp")
sin <- st_read("data/layers/regions/SinoJapanese.shp")

################################################
# Grouping spatial data layer by zoogeographic regions
# data_afr <- as.data.frame(intersect(data, afr))
# data_aus <- as.data.frame(intersect(data, aus))
# data_mad <- as.data.frame(intersect(data, mad))
# data_nea <- as.data.frame(intersect(data, nea))
# data_neo <- as.data.frame(intersect(data, neo))
# data_oce <- as.data.frame(intersect(data, oce))
# data_ori <- as.data.frame(intersect(data, ori))
# data_pan <- as.data.frame(intersect(data, pan))
# data_sah <- as.data.frame(intersect(data, sah))
# data_pal <- as.data.frame(intersect(data, pal))
# data_sin <- as.data.frame(intersect(data, sin))

# I started clipping the records using the above lines of codes but it didn't work properly -
# I obtained much more records than the present form and hence I did the procedure in ArcGIS

# Importing spatial data grouped by zoogeographic regions
data_afr <- st_read("data/layers/data_afr.shp")
data_aus <- st_read("data/layers/data_aus.shp")
data_mad <- st_read("data/layers/data_mad.shp")
data_nea <- st_read("data/layers/data_nea.shp")
data_neo <- st_read("data/layers/data_neo.shp")
data_oce <- st_read("data/layers/data_oce.shp")
data_ori <- st_read("data/layers/data_ori.shp")
data_pan <- st_read("data/layers/data_pan.shp")
data_sah <- st_read("data/layers/data_sah.shp")
data_pal <- st_read("data/layers/data_pal.shp")
data_sin <- st_read("data/layers/data_sin.shp")

################################################
# Afrotropical
################################################
# Converting the shapefile to a dataframe
data_afr <- as.data.frame(data_afr)
head(data_afr)

# Removing geometric information
data_afr$geometry <- NULL

# Selecting required columns
data_afr <- data_afr %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_afr %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_afr %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_afr %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_afr %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/afr_s1.csv")
write_csv(s2, "data/dist/afr_s2.csv")
write_csv(s3, "data/dist/afr_s3.csv")
write_csv(s4, "data/dist/afr_s4.csv")

################################################
# Australian
################################################
# Converting the shapefile to a dataframe
data_aus <- as.data.frame(data_aus)

# Removing geometric information
data_aus$geometry <- NULL

# Selecting required columns
data_aus <- data_aus %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_aus %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_aus %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_aus %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_aus %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/aus_s1.csv")
write_csv(s2, "data/dist/aus_s2.csv")
write_csv(s3, "data/dist/aus_s3.csv")
write_csv(s4, "data/dist/aus_s4.csv")


################################################
# Madagascan
################################################
# Converting the shapefile to a dataframe
data_mad <- as.data.frame(data_mad)

# Removing geometric information
data_mad$geometry <- NULL

# Selecting required columns
data_mad <- data_mad %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_mad %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_mad %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_mad %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_mad %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/mad_s1.csv")
write_csv(s2, "data/dist/mad_s2.csv")
write_csv(s3, "data/dist/mad_s3.csv")
write_csv(s4, "data/dist/mad_s4.csv")


################################################
# Nearctic
################################################
# Converting the shapefile to a dataframe
data_nea <- as.data.frame(data_nea)
head(data_nea)

# Removing geometric information
data_nea$geometry <- NULL

# Selecting required columns
data_nea <- data_nea %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_nea %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_nea %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_nea %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_nea %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/nea_s1.csv")
write_csv(s2, "data/dist/nea_s2.csv")
write_csv(s3, "data/dist/nea_s3.csv")
write_csv(s4, "data/dist/nea_s4.csv")


################################################
# Neotropical
################################################
# Converting the shapefile to a dataframe
data_neo <- as.data.frame(data_neo)

# Removing geometric information
data_neo$geometry <- NULL

# Selecting required columns
data_neo <- data_neo %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_neo %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_neo %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_neo %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_neo %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/neo_s1.csv")
write_csv(s2, "data/dist/neo_s2.csv")
write_csv(s3, "data/dist/neo_s3.csv")
write_csv(s4, "data/dist/neo_s4.csv")


################################################
# Oceania
################################################
# Converting the shapefile to a dataframe
data_oce <- as.data.frame(data_oce)

# Removing geometric information
data_oce$geometry <- NULL

# Selecting required columns
data_oce <- data_oce %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_oce %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_oce %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_oce %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_oce %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/oce_s1.csv")
write_csv(s2, "data/dist/oce_s2.csv")
write_csv(s3, "data/dist/oce_s3.csv")
write_csv(s4, "data/dist/oce_s4.csv")


################################################
# Oriental
################################################
# Converting the shapefile to a dataframe
data_ori <- as.data.frame(data_ori)

# Removing geometric information
data_ori$geometry <- NULL

# Selecting required columns
data_ori <- data_ori %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_ori %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_ori %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_ori %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_ori %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/ori_s1.csv")
write_csv(s2, "data/dist/ori_s2.csv")
write_csv(s3, "data/dist/ori_s3.csv")
write_csv(s4, "data/dist/ori_s4.csv")


################################################
# Palaearctic
################################################
# Converting the shapefile to a dataframe
data_pal <- as.data.frame(data_pal)

# Removing geometric information
data_pal$geometry <- NULL

# Selecting required columns
data_pal <- data_pal %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_pal %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_pal %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_pal %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_pal %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/pal_s1.csv")
write_csv(s2, "data/dist/pal_s2.csv")
write_csv(s3, "data/dist/pal_s3.csv")
write_csv(s4, "data/dist/pal_s4.csv")


################################################
# Panamanian
################################################
# Converting the shapefile to a dataframe
data_pan <- as.data.frame(data_pan)

# Removing geometric information
data_pan$geometry <- NULL

# Selecting required columns
data_pan <- data_pan %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_pan %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_pan %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_pan %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_pan %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/pan_s1.csv")
write_csv(s2, "data/dist/pan_s2.csv")
write_csv(s3, "data/dist/pan_s3.csv")
write_csv(s4, "data/dist/pan_s4.csv")


################################################
# Saharo-Arabian
################################################
# Converting the shapefile to a dataframe
data_sah <- as.data.frame(data_sah)

# Removing geometric information
data_sah$geometry <- NULL

# Selecting required columns
data_sah <- data_sah %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_sah %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_sah %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_sah %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_sah %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/sah_s1.csv")
write_csv(s2, "data/dist/sah_s2.csv")
write_csv(s3, "data/dist/sah_s3.csv")
write_csv(s4, "data/dist/sah_s4.csv")


################################################
# Sino-Japanese
################################################
# Converting the shapefile to a dataframe
data_sin <- as.data.frame(data_sin)

# Removing geometric information
data_sin$geometry <- NULL

# Selecting required columns
data_sin <- data_sin %>% 
  dplyr::select(species, decimalLon, decimalLat, month, Realm)

# Grouping data by months
s1 <- data_sin %>% 
  dplyr::filter(month %in% c(2, 3, 4)) %>% 
  mutate(season = "S1")
s2 <- data_sin %>% 
  dplyr::filter(month %in% c(5, 6, 7)) %>% 
  mutate(season = "S2")
s3 <- data_sin %>% 
  dplyr::filter(month %in% c(8, 9, 10)) %>% 
  mutate(season = "S3")
s4 <- data_sin %>% 
  dplyr::filter(month %in% c(11, 12, 1)) %>% 
  mutate(season = "S4")

# Exporting output
write_csv(s1, "data/dist/sin_s1.csv")
write_csv(s2, "data/dist/sin_s2.csv")
write_csv(s3, "data/dist/sin_s3.csv")
write_csv(s4, "data/dist/sin_s4.csv")

################################################
# Merged data
################################################
input_folder <- "data/dist/"
list <- dir(input_folder, "^.*\\.csv$", full.names = TRUE)
ov <- plyr::ldply(list, readr::read_csv)

# Checking column names
colnames(ov)
colnames(ov)[5] <- "region"

# Exporting output
write_csv(ov, "data/grouped_data_merged.csv")
