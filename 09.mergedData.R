library(tidyverse)
library(glmmTMB)

########################################################
# We are including species for which we obtained at least two seasonal suitability maps
int <- read_csv("output/seasonal_interpolation_gap.csv")
sp <- int[,1:2]

# Calculating number of seasonal suitability maps by species 
sp <- sp %>% 
  group_by(species) %>% 
  dplyr::summarise(n = NROW(species))

# Exporting output
write_csv(sp, "output/sp_season_n.csv")

# Filtering species with at least two suitability maps
sp <- sp %>% 
  dplyr::filter(n < 2) %>% 
  dplyr::select(species)

#### Total 425 species: 8 species with 1 seasonal map, 24 species with 2 seasonal maps
# 38 species with 3 seasonal maps, and 356 species with 4 seasonal maps

# 236 of 426 species

species_met_target_all_season <- species_met_target %>% 
  filter(n > 3)

# 62 of 426 species

write_csv(species_met_target_all_season, "output/species_met_target_all_season.csv")

########################################################
# Merging interpolation gap and taxonomic details
taxa <- read_csv("data/sp_family.csv")
int <- read_csv("output/seasonal_interpolation_gap.csv")

sp_taxa <- dplyr::left_join(int, taxa, by = "species")

# Filtering species for which we obtained at least 2 seasonal maps
sp_taxa <- dplyr::left_join(sp, sp_taxa, by = "species")

write_csv(sp_taxa, "output/seasonal_interpolation_taxa.csv")

########################################################
# Merging interpolation gap with the global IUCN Red List assessments
iucn <- read.csv("data/iucn.csv")

sp_taxa_iucn <- dplyr::left_join(sp_taxa, iucn, by = c("species", "family"))

write_csv(sp_taxa_iucn, "output/seasonal_interpolation_taxa_iucn.csv")

########################################################
# Seasonal fluctuation in PA coverage
ov <- read_csv("output/seasonal_interpolation_taxa.csv")
head(ov)
# ov <- ov[,2:8]

ov_gap_seasonal <- ov %>% 
  filter(season %in% c("1", "2", "3", "4")) %>% 
  group_by(species) %>% 
  summarise(fluctuation = (max(prop_coverage)-min(prop_coverage)), mean = mean(prop_coverage))

write.csv(ov_gap_seasonal, "output/ov_gap_seasonal.csv")

########################################################
# Calculating number of gap species
interpolation_gap <- read_csv("output/seasonal_interpolation_taxa_iucn.csv")

met_target <- interpolation_gap %>% 
  filter(season == "1" | gap <= 0,
         season == "2" | gap <= 0,
         season == "3" | gap <= 0,
         season == "4" | gap <= 0)

species_met_target <- met_target %>% 
  group_by(family, species, redlistCategory) %>% 
  summarise(n = NROW(season))

write_csv(species_met_target, "output/species_met_target.csv")

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
# Average by country
country_met <- read_csv("output/country_prop_sp_met.csv")

country_met_avg <- country_met %>% 
  group_by(country) %>% 
  dplyr::summarise(avg_prop_met = mean(prop_met))

write_csv(country_met_avg, "output/country_met_avg.csv")

########################################################
# Seasonal fluctuation in prop_met by countries
ov <- read_csv("output/country_prop_sp_met.csv")
head(ov)
# ov <- ov[,2:8]

ov_gap_seasonal_country <- ov %>% 
  group_by(country) %>% 
  summarise(fluctuation = (max(prop_met)-min(prop_met)), mean = mean(prop_met))

write.csv(ov_gap_seasonal_country, "output/ov_gap_seasonal_country.csv")
########################################################
# Mean PA coverage by species
int <- read_csv("output/seasonal_interpolation_taxa.csv")

mean_cov_sp <- int %>% 
  group_by(species) %>% 
  dplyr::summarise(avg_prop_met = mean(prop_coverage))

write_csv(mean_cov_sp, "output/mean_cov_sp.csv")

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


########################################################
# PA coverage and range size
df <- read_csv("output/seasonal_interpolation_taxa.csv")
head(df)

model1 <- glm(prop_coverage ~ sdm.area, data = df)
summary(model1)

Anova(model1, type = 3)
