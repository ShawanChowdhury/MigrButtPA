# Loading required libraries
library(proto)
library(prioritizr)
library(tidyverse)
library(glmmTMB)

# Global interpolation gap
ov <- fread("full_data.csv")
head(ov)

ov <- ov %>% 
  dplyr::select(ecoregion, species, season, Shape_Area, sdm.ov)

ov_sum <- ov %>% 
  group_by(ecoregion, species, season) %>% 
  summarise(total_area = sum(Shape_Area), total_ov = sum(sdm.ov), sdm = total_ov/total_area*100)

# m2 to km2
ov_sum <- ov_sum %>% 
  mutate(total_area_km2 = total_area/1000000, total_ov_km2 = total_ov/1000000)

head(ov_sum)

# Species list
sp <- fread("SpeciesList_AllSeasons.csv")
sp <- sp[,3]
sp <- sp %>% mutate(species = str_replace(species, "Papilio_polixenes", "Papilio_polyxenes"))

# Merge with the cumulative PA overlap file
sdm <- dplyr::inner_join(sp, ov_sum, by = "species")


# Calculate target protection
sdm <- sdm[!(sdm$total_area_km2 == ""), ]

############
# 30%
# create series of x-values
x <- sdm$total_area_km2

# interpolate y-values for the x-values given the two reference points:
y <- loglinear_interpolation(x, 1000, 100, 250000, 30)
y <- as.data.frame(y)
interpolation_30 <- cbind(sdm, y)
colnames(interpolation_30)[9] <- "target_30"
interpolation_gap_30 <- interpolation_30 %>%
  mutate(gap_30 = (target_30 - sdm))
interpolation_gap_30$target_30 <- NULL
head(interpolation_gap_30)

# 16%
# create series of x-values
x <- interpolation_gap_30$total_area_km2

# interpolate y-values for the x-values given the two reference points:
y <- loglinear_interpolation(x, 1000, 100, 250000, 16)
y <- as.data.frame(y)
interpolation <- cbind(interpolation_gap_30, y)
colnames(interpolation)[10] <- "target_16"
interpolation_gap <- interpolation %>%
  mutate(gap_16 = (target_16 - sdm))
interpolation_gap$target_16 <- NULL
head(interpolation_gap)

fwrite(interpolation_gap, "seasonal_ecoregional_interpolation_gap_summarised.csv")

##########################
# 16%
interpolation_gap <- fread("seasonal_ecoregional_interpolation_gap_summarised.csv")
interpolation_gap <- interpolation_gap[!interpolation_gap$season=="Overall",]


met_target_16 <- interpolation_gap %>% 
  filter(season == "S1" | gap_16 <= 0,
         season == "S2" | gap_16 <= 0,
         season == "S3" | gap_16 <= 0,
         season == "S4" | gap_16 <= 0)

species <- met_target_16 %>% 
  group_by(ecoregion, species) %>% 
  summarise(n = NROW(season))

species_met_target_16 <- species %>% 
  filter(n > 3)
fwrite(species_met_target_16, "species_met_target_16_seasonal_ecoregional.csv")

# 30%
met_target_30 <- interpolation_gap %>% 
  filter(season == "S1" | gap_30 <= 0,
         season == "S2" | gap_30 <= 0,
         season == "S3" | gap_30 <= 0,
         season == "S4" | gap_30 <= 0)

species <- met_target_30 %>% 
  group_by(ecoregion, species) %>% 
  summarise(n = NROW(season))

species_met_target_30 <- species %>% 
  filter(n > 3)
fwrite(species_met_target_30, "species_met_target_30_seasonal_ecoregional.csv")


################################################
# Global coverage
# Average proportion across ecoregion-season
full_data <- fread("full_data.csv")

ov <- fread("ov_sdm.csv")
head(ov)
ov <- ov %>% dplyr::mutate(season = "Year-round")
ov <- ov %>% 
  dplyr::select(species, ecoregion, sdm.area, sdm.ov, season, SDM)


summarised_data_ov <- ov %>%
  group_by(species, season, ecoregion) %>%
  dplyr::summarise(sdm.area = mean(sdm.area), sdm.ov = sum(sdm.ov))

summarised_data_season <- full_data %>%
  group_by(species, season, ecoregion) %>%
  dplyr::summarise(sdm.area = mean(sdm.area), sdm.ov = sum(sdm.ov))

sum_data <- rbind(summarised_data_ov, summarised_data_season)
head(sum_data)

sum_data <- sum_data %>%
  group_by(ecoregion, species, season) %>%
  dplyr::summarise(sdm.area = sum(sdm.area), sdm.ov = sum(sdm.ov), sdm = (sdm.ov/sdm.area)*100)

# Subsetting 405 species
list <- read.csv("SpeciesList_AllSeasons.csv")
head(list)
list <- list[,1:3]

sum_data <- dplyr::inner_join(list, sum_data, by = "species")
write.csv(sum_data, "summarised_data_season_ecoregion_ov_global.csv")

# Calculate target protection [30%]
sdm <- sum_data[!(sum_data$sdm.area == ""), ]

# create series of x-values
x <- sdm$sdm.area

# hist(x)
# boxplot(x)
# interpolate y-values for the x-values given the two reference points:
# (200, 100) and (900, 15)
y <- loglinear_interpolation(x, 1000, 100, 250000, 30)
y <- as.data.frame(y)

interpolation_sdm <- cbind(sdm, y)

rm(sdm, y)

# Inadequate protection
head(interpolation_sdm)
colnames(interpolation_sdm)[9] <- c("TargetCoverage")

interpolation_gap_sdm <- interpolation_sdm %>%
  mutate(gap = (TargetCoverage - sdm))

fwrite(interpolation_gap_sdm, "interpolation_gap_sdm_global_30_ecoregion_season.csv")

########################################################
# Calculate target protection [16%]

sum_data <- fread("summarised_data_season_ecoregion_ov_global.csv")
sdm <- sum_data[!(sum_data$sdm.area == ""), ]

# create series of x-values
x <- sdm$sdm.area

# hist(x)
# boxplot(x)
# interpolate y-values for the x-values given the two reference points:
# (200, 100) and (900, 15)
y <- loglinear_interpolation(x, 1000, 100, 250000, 16)
y <- as.data.frame(y)

interpolation_sdm <- cbind(sdm, y)

rm(sdm, y)

# Inadequate protection
head(interpolation_sdm)
interpolation_sdm <- interpolation_sdm[,2:10]
colnames(interpolation_sdm)[9] <- c("TargetCoverage")

interpolation_gap_sdm <- interpolation_sdm %>%
  mutate(gap = (TargetCoverage - sdm))

fwrite(interpolation_gap_sdm, "interpolation_gap_sdm_global_16_ecoregion_season.csv")


################################################################
################################################
# Global coverage
# Average proportion across ecoregion-season
full_data <- fread("full_data.csv")
head(full_data)

ov <- fread("ov_sdm.csv")
head(ov)
ov <- ov %>% dplyr::mutate(season = "Year-round")
ov <- ov %>% 
  dplyr::select(species, ecoregion, sdm.area, sdm.ov, season, SDM)


summarised_data_ov <- ov %>%
  group_by(species, season, ecoregion) %>%
  dplyr::summarise(sdm.area = mean(sdm.area), sdm.ov = sum(sdm.ov))

summarised_data_season <- full_data %>%
  group_by(species, season, ecoregion) %>%
  dplyr::summarise(sdm.area = mean(sdm.area), sdm.ov = sum(sdm.ov))

sum_data <- rbind(summarised_data_ov, summarised_data_season)
head(sum_data)

sum_data <- sum_data %>%
  group_by(species, season) %>%
  dplyr::summarise(sdm.area = sum(sdm.area), sdm.ov = sum(sdm.ov), sdm = (sdm.ov/sdm.area)*100)

# Subsetting 405 species
list <- read.csv("SpeciesList_AllSeasons.csv")
head(list)
list <- list[,1:3]

sum_data <- dplyr::inner_join(list, sum_data, by = "species")
write.csv(sum_data, "summarised_data_season_ov_global.csv")

############################
sum_data <- fread("summarised_data_season_ov_global.csv")

# Calculate target protection [16%]
sdm <- sum_data[!(sum_data$sdm.area == ""), ]

# create series of x-values
x <- sdm$sdm.area

# hist(x)
# boxplot(x)
# interpolate y-values for the x-values given the two reference points:
# (200, 100) and (900, 15)
y <- loglinear_interpolation(x, 1000, 100, 250000, 16)
y <- as.data.frame(y)

interpolation_sdm <- cbind(sdm, y)

rm(sdm, y)

# Inadequate protection
head(interpolation_sdm)
colnames(interpolation_sdm)[9] <- c("TargetCoverage")

interpolation_gap_sdm <- interpolation_sdm %>%
  mutate(gap = (TargetCoverage - sdm))

fwrite(interpolation_gap_sdm, "interpolation_gap_sdm_global_16.csv")

# Number of species meeting target in all seasons
interpolation_gap_sdm <- fread("interpolation_gap_sdm_global_16.csv")

gap <- interpolation_gap_sdm %>% 
  dplyr::mutate(fill = ifelse(gap > 0, "NMT", "MT"))

season <- gap %>%
  dplyr::filter(season %in% c("S1", "S2", "S3", "S4") & fill == "MT")
head(season)

sp_n_season <- season%>%
  group_by(species) %>%
  summarise(n_season = NROW(species))

met_all_season <- sp_n_season %>%
  filter(n_season > 3)

# 68 species (16.79%) met the representation targets in all seasons

# Merging this with the taxonomic details
sp <- season %>% 
  dplyr::select(family, species)

sp <- unique(sp)
sp_taxa <- dplyr::left_join(sp_n_season, sp, by = "species")

write.csv(sp_taxa, "seasonal_target_met_16.csv")

##############
# IUCN [Global]
iucn <- fread("iucn.csv")
head(iucn)

interpolation_gap_sdm <- read.csv("interpolation_gap_sdm_global_16.csv")
head(interpolation_gap_sdm)

merge <- dplyr::left_join(iucn, interpolation_gap_sdm, by = c("species", "family"))

write.csv(merge, "iucn_gap_target_16.csv")

##################
# All season and IUCN

merge <- dplyr::inner_join(iucn, met_all_season, by = c("species"))

# PA coverage and range size
df <- read_csv("interpolation_gap_sdm_global_16.csv")
head(df)

model1 <- glm(sdm ~ sdm.area, data = df)
summary(model1)

Anova(model, type = 3)
Anova(model1, type = 3)