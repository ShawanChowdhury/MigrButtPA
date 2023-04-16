# Loading required libraries
library(proto)
library(prioritizr)
library(tidyverse)

# Global interpolation gap
ov <- read_csv("output/pa_coverage.csv")
head(ov)

# ov <- ov %>% 
#   dplyr::select(ecoregion, species, season, Shape_Area, sdm.ov)
# 
# ov_sum <- ov %>% 
#   group_by(ecoregion, species, season) %>% 
#   summarise(total_area = sum(Shape_Area), total_ov = sum(sdm.ov), sdm = total_ov/total_area*100)
# 
# # m2 to km2
# ov_sum <- ov_sum %>% 
#   mutate(total_area_km2 = total_area/1000000, total_ov_km2 = total_ov/1000000)


# # Species list
# sp <- fread("SpeciesList_AllSeasons.csv")
# sp <- sp[,3]
# sp <- sp %>% mutate(species = str_replace(species, "Papilio_polixenes", "Papilio_polyxenes"))

# # Merge with the cumulative PA overlap file
# sdm <- dplyr::inner_join(sp, ov_sum, by = "species")

# Calculate target protection
ov <- ov[!(ov$sdm.area == ""), ]

############
# 16%
# create series of x-values
x <- ov$sdm.area

# interpolate y-values for the x-values given the two reference points:
y <- loglinear_interpolation(x, 1000, 100, 250000, 16)
y <- as.data.frame(y)
interpolation <- cbind(ov, y)
colnames(interpolation)[6] <- "target"
interpolation_gap <- interpolation %>%
  mutate(gap = (target - prop_coverage))
# interpolation_gap$target_16 <- NULL
head(interpolation_gap)

write_csv(interpolation_gap, "output/seasonal_interpolation_gap.csv")

################
# Calculating number of gap species
interpolation_gap <- read_csv("output/seasonal_interpolation_gap.csv")

met_target <- interpolation_gap %>% 
  filter(season == "1" | gap <= 0,
         season == "2" | gap <= 0,
         season == "3" | gap <= 0,
         season == "4" | gap <= 0)

species_met_target <- met_target %>% 
  group_by(species) %>% 
  summarise(n = NROW(season))

# 236 of 426 species

species_met_target_all_season <- species_met_target %>% 
  filter(n > 3)

# 62 of 426 species

fwrite(species_met_target_all_season, "output/species_met_target_all_season.csv")
