# Loading required libraries
library(raster)
library(dismo)
library(spThin)
library(tidyverse)
library(dplyr)

# Give command line argument over to the R script
args <- commandArgs(trailing = T)
species_csv <- args[1]

# Importing data
data <- read_csv(species_csv)
head(data)

# Separate species-wise files


season <- unique(data$season)

# Species name
i <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

for (h in season) {
  
  ses_data <- data %>% 
    filter(season == h)
    
    # Spatial thinning
    thinned_dataset_full <-
      thin( loc.data = sp_data, 
            lat.col = "decimalLatitude", long.col = "decimalLongitude", 
            spec.col = "species", 
            thin.par = 4.65, reps = 10000, 
            locs.thinned.list.return = TRUE, 
            write.files = FALSE, 
            write.log.file = FALSE)
    
    max_idx <- which.max(sapply(thinned_dataset_full, nrow))
    thinned_dataset_max_rows <- thinned_dataset_full [[max_idx]]
    colnames(thinned_dataset_max_rows) <- c("decimalLongitude", "decimalLatitude")
    
    thin_data <- thinned_dataset_max_rows %>%
      dplyr::select("decimalLongitude", "decimalLatitude") %>% 
      mutate(species = i)
    
    write.csv(thin_data, paste0("data/thinned/", h, i, "_thin.csv"), row.names = FALSE)
  
}


