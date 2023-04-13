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
library(lwgeom)

wdpa_crs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

pa <- st_read("WDPA_cl/WDPA.shp") 
pa <- st_set_crs(pa, NA)
pa <- st_set_crs(pa, wdpa_crs)
pa <- st_set_precision(pa, 1500)
pa <- st_make_valid(pa)


ecoregions <- c("Afrotropical", "Australian", "Madagascan", "Nearctic", "Neotropical",
                "Oceania", "Oriental", "Panamanian", "SaharoArabian", "Palaearctic",
                "SinoJapanese")

# Running in parallel
n_threads <- 7
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
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
  library(lwgeom)
  library(parallel)
  library(foreach)
})

clusterExport(cl, c("pa", "wdpa_crs"))


# Main processing
result <- try(parLapply(cl, ecoregions, function(h) {
  print(h)
  
  shps <- list.files(path = "SDM_layers/",pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  shps <- shps[stringr::str_detect(shps, h)]
  
  # I did run it separately for each season
  seasons <- c("S1")
  
  for(i in seasons) {
    print(i)
    
    shps2 <- shps[stringr::str_detect(shps, i)]
    
    sp <- read.csv("ModelledSpecies1.csv")
    species <- unique(sp$species)
    
    
    for(j in species) {
      
      print(j)
      speciesname <- gsub(" ", "_", j)
      
      shp_files <- shps2[stringr::str_detect(shps2, j)]
      
      
      if(length(shp_files) > 0) {
        sdm.hull <- st_read(shp_files)
        sdm.hull <- sdm.hull[2,]
        sdm.hull = st_set_precision(sdm.hull, 1500)
        sdm.hull = st_make_valid(sdm.hull)
        new_sdm.hull = st_transform(sdm.hull, st_crs(pa))
        new_sdm.hull = st_set_precision(new_sdm.hull, 1500)
        new_sdm.hull = st_make_valid(new_sdm.hull)
        
        new_sdm.hull$sdm.area  <- st_area(new_sdm.hull)
        
        ov <- st_intersection(pa, new_sdm.hull)
        ov$sdm.ov  <- st_area(ov)
        
        ov1 <- as.data.frame(ov)
        
        
        
        if(NROW(ov1) == 0) {
          species = j
          ecoregion = h
          season = i
          method = "SDM"
          IUCN_CAT = ""
          Shape_Area = 0
          sdm.area = st_area(new_sdm.hull)
          sdm.ov = 0
          SDM = 0
          
          df <- as.data.frame(cbind(species, ecoregion, season, method, IUCN_CAT, Shape_Area, sdm.area, sdm.ov, SDM))
          
          write.csv(df, file = paste0("SDM/", h, i, speciesname, ".SDM", ".csv"))
          
        } else {
          ov1 <- ov1 %>% 
            dplyr::mutate(species = j)
          ov1 <- ov1 %>% 
            dplyr::mutate(ecoregion = h)
          ov1 <- ov1 %>% 
            dplyr::mutate(season = i)
          ov1 <- ov1 %>% 
            dplyr::mutate(method = "SDM")
          
          ov1$SDM <- (ov1$sdm.ov/ov1$sdm.area)*100
          
          
          ov1 <- ov1 %>%
            dplyr:: select("species", "ecoregion", "season", "method",
                           "IUCN_CAT","Shape_Area", "sdm.area", "sdm.ov", "SDM")
          
          
          
          write.csv(ov1, file = paste0("SDM/", h, i, speciesname, ".SDM", ".csv"))
          
        }
        
        
      } 
      
    }
    
    
  }
}), silent = FALSE)

# Stop cluster
cl <- stopCluster(cl)