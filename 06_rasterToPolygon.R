rm(list = ls())
options(java.parameters = "-Xmx6g")

library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(rgeos)
library(spocc)
library(data.table)
library(stringr)
library(rgdal)
library(parallel)
library(foreach)
library(lwgeom)

# Raster to polygon

ecoregions <- c("Afrotropical", "Australian", "Madagascan", "Nearctic", "Neotropical",
                "Oceania", "Oriental", "Panamanian", "SaharoArabian", "Palaearctic",
                "SinoJapanese")

ecoregions <- c("Madagascan")


wdpa_crs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"


pa <- raster("PA_resampled.tif") 
crs(pa) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"


for (h in ecoregions) {
  print(h)
  
  shps <- list.files(path = "ZoogeographicRegions/",pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  shps <- shps[stringr::str_detect(shps, h)]
  
  shp <- st_read(shps)
  
  shp <- as(shp, "sf")
  shp <- st_transform(shp, crs = wdpa_crs)
  
  seasons <- c("S1", "S2", "S3", "S4")
  
  for(i in seasons) {
    print(i)
    
    
    tifs1 <- list.files(path = "SpeciesWiseRaster", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    tifs2 <- tifs1[stringr::str_detect(tifs1, h)]
    tifs2 <- tifs2[stringr::str_detect(tifs2, i)]
    
    rm(tifs1)
    
    sp <- read.csv("ModelledSpecies.csv")
    species <- unique(sp$species)
    
    for(j in species) try({
      
      print(j)
      speciesname <- gsub(" ", "_", j)
      
      raster_files <- tifs2[stringr::str_detect(tifs2, j)]
      
      
      raster_files <- raster_files[stringr::str_detect(raster_files, "binary")]
      
      if(length(raster_files) > 0) {
        sdm.hull <- raster(raster_files)
        
        crs(sdm.hull) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
        extent(sdm.hull) <- extent(shp)
        
        sdm.shp1 <- rasterToPolygons(sdm.hull, dissolve = TRUE)
        
        sp.name <- gsub("SpeciesWiseRaster/", "", raster_files)
        sp.name <- gsub(j, "", sp.name)
        sp.name <- gsub("/", j, sp.name)
        sp.name <- gsub("Mapbinary.tif", "", sp.name)
        
        writeOGR(sdm.shp1, j, sp.name, driver = "ESRI Shapefile")
        
      } 
      
    }, silent = FALSE)
    
    
  }
}




# In parallel
wdpa_crs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"


pa <- raster("PA_resampled.tif") 
crs(pa) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
ecoregions <- c("Neotropical")

n_threads <- 9
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(sp)
  library(raster)
  library(dismo)
  library(dplyr)
  library(sf)
  library(rgeos)
  library(data.table)
  library(stringr)
  library(rgdal)
  library(tidyverse)
  library(parallel)
  library(foreach)
})

clusterExport(cl, c("pa", "wdpa_crs"))


# Main processing
result <- parLapply(cl, ecoregions, function(h) {
  
  print(h)
  
  shps <- list.files(path = "ZoogeographicRegions/",pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  shps <- shps[stringr::str_detect(shps, h)]
  
  shp <- st_read(shps)
  
  shp <- as(shp, "sf")
  shp <- st_transform(shp, crs = wdpa_crs)
  
  seasons <- c("S4")
  
  for(i in seasons) {
    print(i)
    
    
    tifs1 <- list.files(path = "SpeciesWiseRaster", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    tifs2 <- tifs1[stringr::str_detect(tifs1, h)]
    tifs2 <- tifs2[stringr::str_detect(tifs2, i)]
    
    rm(tifs1)
    
    sp <- read.csv("ModelledSpecies.csv")
    species <- unique(sp$species)
    
    for(j in species) try({
      
      print(j)
      speciesname <- gsub(" ", "_", j)
      
      raster_files <- tifs2[stringr::str_detect(tifs2, j)]
      
      
      raster_files <- raster_files[stringr::str_detect(raster_files, "binary")]
      
      if(length(raster_files) > 0) {
        sdm.hull <- raster(raster_files)
        
        crs(sdm.hull) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
        extent(sdm.hull) <- extent(shp)
        
        sdm.shp1 <- rasterToPolygons(sdm.hull, dissolve = TRUE)
        
        sp.name <- gsub("SpeciesWiseRaster/", "", raster_files)
        sp.name <- gsub(j, "", sp.name)
        sp.name <- gsub("/", j, sp.name)
        sp.name <- gsub("Mapbinary.tif", "", sp.name)
        
        writeOGR(sdm.shp1, j, sp.name, driver = "ESRI Shapefile")
        
      } 
      
    }, silent = FALSE)
    
    
  } 
  
})

# Stop cluster
cl <- stopCluster(cl)