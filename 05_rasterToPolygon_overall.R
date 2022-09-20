# Loading required libraries
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

ecoregions <- c("Afrotropical", "Australian", "Madagascan", "Nearctic", "Neotropical",
                "Oceania", "Oriental", "Panamanian", "SaharoArabian", "Palaearctic",
                "SinoJapanese")

wdpa_crs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"


pa <- raster("PA_resampled.tif") 
crs(pa) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# In parallel
n_threads <- 23
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
  
  shps <- list.files(path = "ZoogeographicRegions/",pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  shps <- shps[stringr::str_detect(shps, h)]
  
  shp <- st_read(shps)
  
  shp <- as(shp, "sf")
  shp <- st_transform(shp, crs = wdpa_crs)
  sp <- read.csv("SpeciesList_AllSeasons.csv")
  species <- unique(sp$species)
  
  # species <- c("Papilio_polyxenes")
  
  tifs1 <- list.files(path = "overall", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  tifs1 <- tifs1[stringr::str_detect(tifs1, "binary")]
  tifs2 <- tifs1[stringr::str_detect(tifs1, h)]
    
    for(j in species) try({
      
      speciesname <- gsub(" ", "_", j)
      
      raster_files <- tifs2[stringr::str_detect(tifs2, j)]
      
      
      raster_files <- raster_files[stringr::str_detect(raster_files, "binary")]
      
      if(length(raster_files) > 0) {
        sdm.hull <- raster(raster_files)
        
        crs(sdm.hull) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
        extent(sdm.hull) <- extent(shp)
        
        # Converting the raster to a polygon
        sdm.shp1 <- rasterToPolygons(sdm.hull, dissolve = TRUE)
        
        # Setting-up the output file name
        sp.name <- gsub("overall/", "", raster_files)
        sp.name <- gsub(j, "", sp.name)
        sp.name <- gsub("/", j, sp.name)
        sp.name <- gsub("Mapbinary.tif", "", sp.name)
        
        # Exporting the output
        writeOGR(sdm.shp1, j, sp.name, driver = "ESRI Shapefile")
        
      } 
      
    }, silent = FALSE)
    
    
  } 
  
)

# Stop cluster
cl <- stopCluster(cl)
