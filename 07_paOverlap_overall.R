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
library(parallel)
library(foreach)

wdpa_crs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

pa <- st_read("WDPA_cl/WDPA.shp") 
pa <- st_set_crs(pa, NA)
pa <- st_set_crs(pa, wdpa_crs)
pa <- st_set_precision(pa, 1500)
pa <- st_make_valid(pa)


ecoregions <- c("Afrotropical", "Australian", "Madagascan", "Nearctic", "Neotropical",
                "Oceania", "Oriental", "Panamanian", "SaharoArabian", "Palaearctic",
                "SinoJapanese")

# In parallel
n_threads <- 23
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

result <- parLapply(cl, ecoregions, function(h) {
  
  shps <- list.files(path = "ov_SDM_layers/",pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  shps <- shps[stringr::str_detect(shps, h)]
  
  sp <- read.csv("SpeciesList_AllSeasons.csv")
  # sp <- prob %>% 
  #   dplyr::filter(ecoregion == "Australian")
  species <- unique(sp$species)
  # species <- c("Vagrans_egista")
    
    
    for(i in species) {
      
      speciesname <- gsub(" ", "_", i)
      
      shp_files <- shps[stringr::str_detect(shps, i)]
      
      
      if(length(shp_files) > 0) {
        sdm.hull <- st_read(shp_files)
        new_sdm.hull <- sdm.hull[2,]
        # sdm.hull = st_set_precision(sdm.hull, 1500)
        # new_sdm.hull = st_make_valid(sdm.hull)
        # new_sdm.hull = st_transform(sdm.hull, st_crs(pa))
        # new_sdm.hull = st_set_precision(new_sdm.hull, 1500)
        # new_sdm.hull = st_make_valid(new_sdm.hull)
        
        st_crs(new_sdm.hull) <- wdpa_crs
        
        new_sdm.hull$sdm.area  <- st_area(new_sdm.hull)
        
        ov <- st_intersection(pa, new_sdm.hull)
        
        ov$sdm.ov  <- st_area(ov)
        
        ov1 <- as.data.frame(ov)
        
        if(nrow(ov1) >0) {
          
          
          ov1$SDM <- (ov1$sdm.ov/ov1$sdm.area)*100
          
          ov1$species <- i
          ov1$ecoregion <- h
          ov1$method <- "SDM"
          
          ov1 <- ov1 %>%
            dplyr:: select("species", "ecoregion", "method",
                           "IUCN_CAT","Shape_Area", "sdm.area", "sdm.ov", "SDM")
          
          
          
          fwrite(ov1, file = paste0("ov_sdm/", h, speciesname, ".SDM", ".csv"))
          
        } else {
          ov1 <- ov1 %>% 
            dplyr::mutate(species = i)
          ov1 <- ov1 %>% 
            dplyr::mutate(ecoregion = h)
          ov1 <- ov1 %>% 
            dplyr::mutate(method = "SDM")
          
          ov1$SDM <- (ov1$sdm.ov/ov1$sdm.area)*100
          
          
          ov1 <- ov1 %>%
            dplyr:: select("species", "ecoregion", "method",
                           "IUCN_CAT","Shape_Area", "sdm.area", "sdm.ov", "SDM")
          
          
          
          fwrite(ov1, file = paste0("ov_sdm/", h, speciesname, ".SDM", ".csv"))
          
        }
      } 
      
    }
    
    
  }
)


# Stop cluster
cl <- stopCluster(cl)