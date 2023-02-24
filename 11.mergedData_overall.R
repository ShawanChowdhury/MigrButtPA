library(dplyr)
library(data.table)
library(tidyverse)

# merging data
input_folder = "ov_sdm"
pa.ov = dir(input_folder, "^.*\\.csv$", full.names = TRUE)
ov <- plyr::ldply(pa.ov, data.table::fread)

# # Replace NA values with zero
# ov[is.na(ov)] <- 0

fwrite(ov, "ov_sdm.csv")

# Summarised data
summarised_data <- ov %>%
  group_by(species, ecoregion) %>%
  dplyr::summarise(sdm.area = sum(sdm.area), sdm.ov = sum(SDM))

fwrite(summarised_data, "species_region_ov.csv")