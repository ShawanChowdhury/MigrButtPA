library(tidyverse)
library(glmmTMB)

# Merging interpolation gap and taxonomic details
taxa <- read_csv("data/SpeciesList_AllSeasons.csv")
int <- read_csv("output/seasonal_interpolation_gap.csv")

sp_taxa <- dplyr::left_join(int, taxa, by = "species")

write_csv(sp_taxa, "output/seasonal_interpolation_taxa.csv")

########################################################
# Merging interpolation gap with the global IUCN Red List assessments
iucn <- read.csv("data/iucn.csv")

sp_taxa_iucn <- dplyr::left_join(sp_taxa, iucn, by = c("species", "family"))

write_csv(sp_taxa_iucn, "output/seasonal_interpolation_taxa_iucn.csv")

########################################################
# Seasonal fluctuation in PA coverage
ov <- fread("output/seasonal_interpolation_taxa.csv")
head(ov)
# ov <- ov[,2:8]

ov_gap_seasonal <- ov %>% 
  filter(season %in% c("1", "2", "3", "4")) %>% 
  group_by(species) %>% 
  summarise(fluctuation = (max(prop_coverage)-min(prop_coverage)), mean = mean(prop_coverage))

write.csv(ov_gap_seasonal, "output/ov_gap_seasonal.csv")

########################################################
# Merging with the country details
country <- read_csv("output/species_suitability_country_summarised.csv")
country <- country[,1:3]
int <- read_csv("output/seasonal_interpolation_taxa_iucn.csv")

# # Adding another column to show if the species met the target representation
# int <- int %>% 
#   dplyr::mutate(gap_status = ifelse(gap > 0, "NMT", "MT"))

# Combining dataframes
int_country <- dplyr::left_join(int, country, by = c("species", "season"))

# Selecting columns of interest
int_country <- int_country %>% 
  dplyr::select(species, season, gap, country)

# Summarising proportion of species meeting representation target by country
int_country_sum <- int_country %>% 
  group_by(season, country) %>% 
  dplyr::summarise(pos = sum(gap > 0),
                   neg = sum(gap <= 0),
                   prop_met = (pos/(pos + neg)*100))

write_csv(int_country_sum, "output/country_prop_sp_met.csv")

########################################################
# Cross-checking if all the country names matched perfectly
# Reading dissolved world map
world <- st_read("data/layers/world_dissolved.shp")
world_df <- as.data.frame(world)
world_df$geometry <- NULL
world_df <- world_df[,2]

world_df <- as.data.frame(world_df)

colnames(world_df) <- "country"

# Reading the interpolation dataset
int_country <- read_csv("output/country_prop_sp_met.csv")
int_country <- int_country[,2]
int_country <- unique(int_country)

# Combining both datasets by country name
com <- dplyr::full_join(world_df, int_country, by = "country")

# Everything matched perfectly