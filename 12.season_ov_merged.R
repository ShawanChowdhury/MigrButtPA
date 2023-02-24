library(data.table)
library(tidyverse)

# Importing data files
ov <- fread("interpolation_gap_sdm_ov.csv")
ov$season <- "Overall"
season <- fread("interpolation_gap_sdm_season.csv")

merged <- rbind(ov, season)

# Renaming species
merged <- merged %>% mutate(species = str_replace(species, "Glutophrissa_epaphia", "Appia_epaphia"))
merged <- merged %>% mutate(species = str_replace(species, "Glutophrissa_drusilla", "Appias_drusilla"))
merged <- merged %>% mutate(species = str_replace(species, "Colias_alfacariensis", "Colias_sareptensis"))
merged <- merged %>% mutate(species = str_replace(species, "Heliconius_aliphera", "Eueides_aliphera"))
merged <- merged %>% mutate(species = str_replace(species, "Occidryas_editha", "Euphydryas_editha"))
merged <- merged %>% mutate(species = str_replace(species, "Euploea_nemertes", "Euploea_eunice"))
merged <- merged %>% mutate(species = str_replace(species, "Euploea_diocletianus", "Euploea_radamanthus"))
merged <- merged %>% mutate(species = str_replace(species, "Craspedophorus_gratiosus", "Eurema_gratiosa"))
merged <- merged %>% mutate(species = str_replace(species, "Pyrisitia_lisa", "Eurema_lisa"))
merged <- merged %>% mutate(species = str_replace(species, "Abaeis_nicippe", "Eurema_nicippe"))
merged <- merged %>% mutate(species = str_replace(species, "Euthalia_aeropa", "Lexias_aeropa"))
merged <- merged %>% mutate(species = str_replace(species, "Anaea_philumena", "Memphis_philumena"))
merged <- merged %>% mutate(species = str_replace(species, "Oeneis_polixenes", "Papilio_polyxenes"))
merged <- merged %>% mutate(species = str_replace(species, "Eresia_aveyrana", "Phyciodes_mylitta"))
merged <- merged %>% mutate(species = str_replace(species, "Lycaena_acmon", "Plebejus_acmon"))
merged <- merged %>% mutate(species = str_replace(species, "Cyaniris_semiargus", "Polyommatus_semiargus"))
merged <- merged %>% mutate(species = str_replace(species, "Danaus_hamata", "Tirumala_hamata"))
merged <- merged %>% mutate(species = str_replace(species, "Tellervo_septentrionis", "Tirumala_septentrionis"))
merged <- merged %>% mutate(species = str_replace(species, "Bassaris_itea", "Vanessa_itea"))

# Exporting output
fwrite(merged, "merged_sdm_season_ov.csv")

#############################################################################
ov <- fread("full_data.csv")
head(ov)

# Renaming species
ov <- ov %>% mutate(species = str_replace(species, "Glutophrissa_epaphia", "Appia_epaphia"))
ov <- ov %>% mutate(species = str_replace(species, "Glutophrissa_drusilla", "Appias_drusilla"))
ov <- ov %>% mutate(species = str_replace(species, "Colias_alfacariensis", "Colias_sareptensis"))
ov <- ov %>% mutate(species = str_replace(species, "Heliconius_aliphera", "Eueides_aliphera"))
ov <- ov %>% mutate(species = str_replace(species, "Occidryas_editha", "Euphydryas_editha"))
ov <- ov %>% mutate(species = str_replace(species, "Euploea_nemertes", "Euploea_eunice"))
ov <- ov %>% mutate(species = str_replace(species, "Euploea_diocletianus", "Euploea_radamanthus"))
ov <- ov %>% mutate(species = str_replace(species, "Craspedophorus_gratiosus", "Eurema_gratiosa"))
ov <- ov %>% mutate(species = str_replace(species, "Pyrisitia_lisa", "Eurema_lisa"))
ov <- ov %>% mutate(species = str_replace(species, "Abaeis_nicippe", "Eurema_nicippe"))
ov <- ov %>% mutate(species = str_replace(species, "Euthalia_aeropa", "Lexias_aeropa"))
ov <- ov %>% mutate(species = str_replace(species, "Anaea_philumena", "Memphis_philumena"))
ov <- ov %>% mutate(species = str_replace(species, "Oeneis_polixenes", "Papilio_polyxenes"))
ov <- ov %>% mutate(species = str_replace(species, "Eresia_aveyrana", "Phyciodes_mylitta"))
ov <- ov %>% mutate(species = str_replace(species, "Lycaena_acmon", "Plebejus_acmon"))
ov <- ov %>% mutate(species = str_replace(species, "Cyaniris_semiargus", "Polyommatus_semiargus"))
ov <- ov %>% mutate(species = str_replace(species, "Danaus_hamata", "Tirumala_hamata"))
ov <- ov %>% mutate(species = str_replace(species, "Tellervo_septentrionis", "Tirumala_septentrionis"))
ov <- ov %>% mutate(species = str_replace(species, "Bassaris_itea", "Vanessa_itea"))

# Exporting output
fwrite(ov, "full_data.csv")

#############################################################################
ov <- fread("ov_sdm.csv")
head(ov)

# Renaming species
ov <- ov %>% mutate(species = str_replace(species, "Glutophrissa_epaphia", "Appia_epaphia"))
ov <- ov %>% mutate(species = str_replace(species, "Glutophrissa_drusilla", "Appias_drusilla"))
ov <- ov %>% mutate(species = str_replace(species, "Colias_alfacariensis", "Colias_sareptensis"))
ov <- ov %>% mutate(species = str_replace(species, "Heliconius_aliphera", "Eueides_aliphera"))
ov <- ov %>% mutate(species = str_replace(species, "Occidryas_editha", "Euphydryas_editha"))
ov <- ov %>% mutate(species = str_replace(species, "Euploea_nemertes", "Euploea_eunice"))
ov <- ov %>% mutate(species = str_replace(species, "Euploea_diocletianus", "Euploea_radamanthus"))
ov <- ov %>% mutate(species = str_replace(species, "Craspedophorus_gratiosus", "Eurema_gratiosa"))
ov <- ov %>% mutate(species = str_replace(species, "Pyrisitia_lisa", "Eurema_lisa"))
ov <- ov %>% mutate(species = str_replace(species, "Abaeis_nicippe", "Eurema_nicippe"))
ov <- ov %>% mutate(species = str_replace(species, "Euthalia_aeropa", "Lexias_aeropa"))
ov <- ov %>% mutate(species = str_replace(species, "Anaea_philumena", "Memphis_philumena"))
ov <- ov %>% mutate(species = str_replace(species, "Oeneis_polixenes", "Papilio_polyxenes"))
ov <- ov %>% mutate(species = str_replace(species, "Eresia_aveyrana", "Phyciodes_mylitta"))
ov <- ov %>% mutate(species = str_replace(species, "Lycaena_acmon", "Plebejus_acmon"))
ov <- ov %>% mutate(species = str_replace(species, "Cyaniris_semiargus", "Polyommatus_semiargus"))
ov <- ov %>% mutate(species = str_replace(species, "Danaus_hamata", "Tirumala_hamata"))
ov <- ov %>% mutate(species = str_replace(species, "Tellervo_septentrionis", "Tirumala_septentrionis"))
ov <- ov %>% mutate(species = str_replace(species, "Bassaris_itea", "Vanessa_itea"))

# Exporting output
fwrite(ov, "ov_sdm.csv")
