options(java.parameters = "-Xmx6g")

library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(rgeos)
library(data.table)
library(stringr)
library(rgdal)
library(parallel)
library(foreach)
library(lwgeom)

# Name of the ecoregions [DOI: 10.1126/science.1228282]
ecoregions <- c("Afrotropical", "Australian", "Madagascan", "Nearctic", "Neotropical",
                "Oceania", "Oriental", "Panamanian", "SaharoArabian", "Palaearctic",
                "SinoJapanese")

# Setting projection
wdpa_crs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Reading the protected area distribution layer
pa <- raster("PA_resampled.tif") 
crs(pa) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"


for (h in ecoregions) {
  print(h)
  
  shps <- list.files(path = "cache/zoogeographicRegions/",pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  shps <- shps[stringr::str_detect(shps, h)]
  
  shp <- st_read(shps)
  
  shp <- as(shp, "sf")
  shp <- st_transform(shp, crs = wdpa_crs)
  
  seasons <- c("S1", "S2", "S3", "S4")
  
  for(i in seasons) {
    print(i)
    
    
    tifs1 <- list.files(path = "cache/speciesWiseRaster", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    tifs2 <- tifs1[stringr::str_detect(tifs1, h)]
    tifs2 <- tifs2[stringr::str_detect(tifs2, i)]
    
    rm(tifs1)
    
    sp <- read.csv("cache/modelledSpecies.csv")
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
        
        sp.name <- gsub("cache/SpeciesWiseRaster/", "", raster_files)
        sp.name <- gsub(j, "", sp.name)
        sp.name <- gsub("/", j, sp.name)
        sp.name <- gsub("Mapbinary.tif", "", sp.name)
        
        writeOGR(sdm.shp1, file = paste0("cache/SDM_layers/", j, sp.name, driver = "ESRI Shapefile"))
        
      } 
      
    }, silent = FALSE)
    
    
  }
}




# In parallel
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
})

clusterExport(cl, c("pa", "wdpa_crs"))


# Main processing
result <- parLapply(cl, ecoregions, function(h) {
  
  print(h)
  
  shps <- list.files(path = "cache/zoogeographicRegions/",pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  shps <- shps[stringr::str_detect(shps, h)]
  
  shp <- st_read(shps)
  
  shp <- as(shp, "sf")
  shp <- st_transform(shp, crs = wdpa_crs)
  
  seasons <- c("S1", "S2", "S3", "S4")
  
  for(i in seasons) {
    print(i)
    
    
    tifs1 <- list.files(path = "cache/speciesWiseRaster", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    tifs2 <- tifs1[stringr::str_detect(tifs1, h)]
    tifs2 <- tifs2[stringr::str_detect(tifs2, i)]
    
    rm(tifs1)
    
    sp <- read.csv("cache/modelledSpecies.csv")
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
        
        sp.name <- gsub("cache/speciesWiseRaster/", "", raster_files)
        sp.name <- gsub(j, "", sp.name)
        sp.name <- gsub("/", j, sp.name)
        sp.name <- gsub("Mapbinary.tif", "", sp.name)
        
        writeOGR(sdm.shp1, file = paste0("cache/SDM_layers/", j, sp.name, driver = "ESRI Shapefile"))
        
      } 
      
    }, silent = FALSE)
    
    
  } 
  
})

# Stop cluster
cl <- stopCluster(cl)