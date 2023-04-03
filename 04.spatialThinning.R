# The spatial thinning process using the 'spThin' process takes a long time, so we thinned the spatial
# data when developing the model using the ENMeval package.

# # Loading required libraries
# library(raster)
# library(dismo)
# library(spThin)
# library(tidyverse)
# library(dplyr)
# 
# data <- read_csv("data/cleanedRecords_gbif.csv")
# head(data)
# 
# season <- unique(data$season)
# 
# # Test-run
# # g <- "Sino-Japanese"
# # h <- "S4"
# # i <- "Aporia crataegi"
# 
# for (h in season) {
#     
#     print(h)
#     
#     ses_data <- data %>% 
#       filter(season == h)
#     
#     species <- unique(ses_data$species)
#     
#     for (i in species){
#       print(i)
#       
#       sp_data <- ses_data %>% 
#         filter(species == i)
#       
#       # Spatial thinning
#       thinned_dataset_full <-
#         thin( loc.data = sp_data, 
#               lat.col = "decimalLatitude", long.col = "decimalLongitude", 
#               spec.col = "species", 
#               thin.par = 4.65, reps = 10000, 
#               locs.thinned.list.return = TRUE, 
#               write.files = FALSE, 
#               write.log.file = FALSE)
#       
#       max_idx <- which.max(sapply(thinned_dataset_full, nrow))
#       thinned_dataset_max_rows <- thinned_dataset_full [[max_idx]]
#       colnames(thinned_dataset_max_rows) <- c("decimalLongitude", "decimalLatitude")
#       
#       thin_data <- thinned_dataset_max_rows %>%
#         dplyr::select("decimalLongitude", "decimalLatitude") %>% 
#         dplyr::mutate(species = i)
#       
#       write.csv(thin_data, paste0("data/thinned/", h, i, "_thin.csv"), row.names = FALSE)
#     }
#     
# }
# 
# 
