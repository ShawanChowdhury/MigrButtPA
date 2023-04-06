# Loading required libraries
library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(rgeos)
library(ENMeval)
library(data.table)
library(stringr)
library(rgdal)
library(data.table)
library(tidyverse)

# Reading csv file
data <- fread("data/cleanedRecords_gbif.csv", header = T)

data <- data %>%
  dplyr::select("species", "decimalLongitude", "decimalLatitude", "season")

# Remove records without coordinates
data <- data %>%
  filter(!is.na(species))%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))%>%
  filter(!is.na(season))

# Remove species with low occurrence records
data_cl <- data %>%
  group_by(species) %>%
  filter(n() > 29) %>%
  ungroup()

# Remove duplicated records
data_cl <- data_cl[!duplicated(data_cl),]

#attaching climatic layers
vars<-c("bio1.tif","bio2.tif","bio3.tif","bio7.tif","bio15.tif","bio17.tif","bio18.tif","bio19.tif")
envs <- stack(paste(getwd(),"/data/clim/", vars, sep=""))

season <- unique(data_cl$season)

for (h in season) try({
  print(h)
  
  ses_data <- data_cl %>% 
    dplyr::filter(season == h)
  
  species <- unique(ses_data$species)
  
  for (i in species) {
    print(i)
    speciesname <- gsub(" ", "_", i)
    occs <- ses_data %>% filter(species==i)
    occs<- occs[,2:3]
    occs <- as.data.frame(occs)
    
    # Let's now remove occurrences that are cell duplicates -- these are
    # occurrences that share a grid cell in the predictor variable rasters.
    # Although Maxent does this by default, keep in mind that for other algorithms you may
    # or may not want to do this based on the aims of your study.
    # Another way to space occurrence records a defined distance from each other to avoid
    # spatial autocorrelation is with spatial thinning (Aiello-Lammens et al. 2015).
    occs.cells <- raster::extract(envs[[1]], occs, cellnumbers = TRUE)
    occs.cellDups <- duplicated(occs.cells[,1])
    occs <- occs[!occs.cellDups,]
    
    # We'll now experiment with a different spatial R package called sf (simple features).
    # Let's make our occs into a sf object -- as the coordinate reference system (crs) for these 
    # points is WGS84, a geographic crs (lat/lon) and the same as our envs rasters, we specify it 
    # as the RasterStack's crs.
    occs.sf <- sf::st_as_sf(occs, coords = c("decimalLongitude","decimalLatitude"), crs = raster::crs(envs))
    
    # Now, we project our point data to an equal-area projection, which converts our 
    # degrees to meters, which is ideal for buffering (the next step). 
    # We use the typical Eckert IV projection.
    eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
    occs.sf <- sf::st_transform(occs.sf, crs = eckertIV)
    
    # Buffer all occurrences by 500 km, union the polygons together 
    # (for visualization), and convert back to a form that the raster package 
    # can use. Finally, we reproject the buffers back to WGS84 (lat/lon).
    # We choose 500 km here to avoid sampling the Caribbean islands.
    occs.buf <- sf::st_buffer(occs.sf, dist = 100000) %>% 
      sf::st_union() %>% 
      sf::st_sf() %>%
      sf::st_transform(crs = raster::crs(envs))
    
    # Crop environmental rasters to match the study extent
    envs.bg <- raster::crop(envs, occs.buf)
    # Next, mask the rasters to the shape of the buffers
    envs.bg <- raster::mask(envs.bg, occs.buf)
    
    # Randomly sample 10,000 background points from one background extent raster 
    # (only one per cell without replacement). Note: Since the raster has <10,000 pixels, 
    # you'll get a warning and all pixels will be used for background. We will be sampling 
    # from the biome variable because it is missing some grid cells, and we are trying to 
    # avoid getting background points with NA. If one raster in the stack has NAs where the
    # other rasters have data, ENMeval internally converts these cells to NA.
    bg <- dismo::randomPoints(envs.bg[[8]], n = 10000) %>% as.data.frame()
    colnames(bg) <- colnames(occs)
    
    model <- ENMevaluate(occs, envs.bg, bg, method='checkerboard2', 
                         RMvalues=seq(0.5, 4, 0.5), fc=c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), 
                         parallel=TRUE, kfolds = 10, algorithm='maxent.jar')
    
    aic.opt <- model@models[[which.max((model@results$avg.test.AUC))]]
    dir.create(path = speciesname)
    
    output_dir <- paste0(speciesname, "/")
    
    ModelOutput <- 
      as.data.frame(aic.opt@results) %>% 
      rownames_to_column(var = "param") %>%
      spread(key = param, value = V1) %>%
      mutate(species = speciesname) %>%
      dplyr::select(species, everything())
    
    
    write.csv(ModelOutput, file = paste0(output_dir, "ModelOutput_", h, speciesname, ".csv"))
    
    VariableContribution <- var.importance((aic.opt))
    write.csv(VariableContribution, file = paste0(output_dir, "VariableContribution_", h, speciesname, ".csv"))
    
    r <- dismo::predict(aic.opt, envs.bg)
    writeRaster(r, paste0(output_dir, "Map_", h, speciesname, ".tif"), NAflag=-9999, overwrite = TRUE)
    
    r_bin <- r >= ModelOutput$Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
    writeRaster(r_bin, paste0(output_dir, "Binary_", h, speciesname, ".tif"), NAflag=-9999, overwrite = TRUE)
    
  }
}, silent = FALSE)
