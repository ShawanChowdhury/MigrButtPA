library(dplyr)
library(data.table)
library(readr)

# Previous data
prev_data <- fread("UpdatedData.csv")
prev_data <- prev_data[, 2:10]

# Missing data
input_folder = "SDM/remaining"
pa.ov = dir(input_folder, "^.*\\.csv$", full.names = TRUE)
missing_data <- plyr::ldply(pa.ov, data.table::fread)

missing_data <- missing_data[, 2:10]

fwrite(missing_data, "missing_data.csv")

# Full data
full_data <- rbind(prev_data, missing_data)
fwrite(full_data, "full_data.csv")

# Average proportion across ecoregion-season
full_data <- fread("full_data.csv")

summarised_data <- full_data %>%
  group_by(species, ecoregion, season) %>%
  dplyr::summarise(sdm.area = sum(sdm.area), sdm.ov = sum(SDM))

fwrite(summarised_data, "species_season_region_ov.csv")