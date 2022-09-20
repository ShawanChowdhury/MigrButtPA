# Loading required libraries
library(data.table)
library(tidyverse)
library(DescTools)

# Interpolation gap
inter_gap_long <- fread("interpolation_overall_seasonal_gap_up.csv")
head(inter_gap_long)
# inter_gap_long <- inter_gap_long %>% 
#   dplyr::mutate(total_30 = (met_30 + not_met_30))

# Plot
# 16%
inter_gap_long %>% 
  dplyr::filter(group == 16) %>% 
  ggplot( aes(ecoregion, per, fill = category)) +
  geom_bar(stat = "identity", position = position_stack()) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  xlab("") + ylab("Percentages") + facet_wrap(~season, nrow = 1) + scale_fill_manual(values = c("blue", "orange")) +
  geom_vline(xintercept=seq(1.5, length(unique(inter_gap_long$ecoregion))-0.5, 1), 
             lwd=1, colour="grey94") + 
  ggsave("target_16_representation_gap.png")


# 30%
inter_gap_long %>% 
  dplyr::filter(group == 30) %>% 
  ggplot( aes(ecoregion, per, fill = category)) +
  geom_bar(stat = "identity", position = position_stack()) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  xlab("") + ylab("Percentages") + facet_wrap(~season, nrow = 2) + scale_fill_manual(values = c("blue", "orange")) +
  geom_vline(xintercept=seq(1.5, length(unique(inter_gap_long$ecoregion))-0.5, 1), 
             lwd=1, colour="grey94")
ggsave("target_30_representation_gap.png")

############################
# Global proportion histogram
ov <- fread("merged_sdm_season_ov.csv")
head(ov)
# sp <- fread("overall_interpolation_gap.csv")
# head(sp)
# sp <- sp[,1]

# merged <- dplyr::inner_join(ov, sp, by = "species")
# head(merged)
# merged_ov <- merged %>% 
#   dplyr::filter(season == "Overall")
# 
# head(merged_ov)
# 
# ecoregion_pa_coverage <- merged_ov %>% 
#   group_by(ecoregion) %>% 
#   dplyr::summarise(mean_overlap = mean(sdm.ov))

ov_ov <- ov %>% 
  dplyr::filter(season == "Overall")

ov_ov %>%
  dplyr::filter(ecoregion == "Afrotropical") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Afrotropical.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "Australian") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Australian.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "Madagascan") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Madagascan.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "Nearctic") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Nearctic.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "Neotropical") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Neotropical.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "Oceania") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Oceania.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "Oriental") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Oriental.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "Palaearctic") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Palaearctic.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "Panamanian") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/Panamanian.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "SaharoArabian") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/SaharoArabian.png", dpi = 96, height = 251, width = 391, units = "px")

ov_ov %>%
  dplyr::filter(ecoregion == "SinoJapanese") %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( fill="darkgoldenrod1", position = 'identity', bins = 150) +
  theme_bw() +
  labs(x = "", y = "")+ 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + xlim(0, 80)
ggsave("Figures/SinoJapanese.png", dpi = 96, height = 251, width = 391, units = "px")

# All
ov_ov %>% 
  ggplot( aes(x=sdm.ov)) +
  geom_histogram( color="#e9ecef", position = 'identity') +
  theme_bw() +
  labs(x = "", y = "") + facet_wrap(~ecoregion) +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank())


#########################
# Interpolation gap [global, 30%] [season]
gap <- read.csv("interpolation_gap_sdm_global_30.csv")
head(gap)
gap <- gap %>% 
  dplyr::mutate(fill = ifelse(gap > 0, "NMT", "MT"))

gap %>% 
  filter(season %in% c("S1", "S2", "S3", "S4")) %>% 
  ggplot( aes(x=gap, fill = fill)) +
  geom_histogram(position = 'identity', bins = 50) +
  theme_bw() +
  labs(x = "", y = "") + facet_wrap(~season) +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + 
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1"))

ggsave("Figures/seasonal_global_gap_30.png")

#####################
# Interpolation gap [global, 17.5%]
gap <- read.csv("interpolation_gap_sdm_global_17.5.csv")
head(gap)

gap <- gap %>% 
  dplyr::mutate(fill = ifelse(gap > 0, "NMT", "MT"))

gap %>% 
  ggplot( aes(x=gap, fill = fill)) +
  geom_histogram(position = 'identity', bins = 50) +
  theme_bw() +
  labs(x = "", y = "") + facet_wrap(~season) +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + 
  scale_fill_manual(values = c("deepskyblue4", "darkgoldenrod1"))

#######################
# Seasonal PA coverage
ov <- fread("summarised_data_season_ov_global.csv")
head(ov)
ov <- ov[,2:7]

ov <- ov %>% 
  filter(season %in% c("S1", "S2", "S3", "S4")) %>% 
  mutate(sdm = (sdm.ov/sdm.area)*100)

# Area_coverage
ggplot(ov, aes(sdm.area, sdm, col = season)) +
  geom_point() + 
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + 
  scale_color_viridis_d()
ggsave("Figures/area_coverage_up.png")

# Seasonal fluctuation
ggplot(ov, aes(sdm, fill = season)) +
  geom_histogram(position = 'identity', bins = 50) +
  theme_bw() +
  labs(x = "", y = "") + facet_wrap(~season) +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + 
  scale_fill_viridis_d()
ggsave("Figures/season_coverage.png")


#######################
# Seasonal fluctuation in PA coverage
ov <- fread("summarised_data_season_ov_global.csv")
head(ov)
ov <- ov[,2:8]

ov_gap_seasonal <- ov %>% 
  filter(season %in% c("S1", "S2", "S3", "S4")) %>% 
  group_by(species, family) %>% 
  summarise(fluctuation = (max(sdm)-min(sdm)), mean = mean(sdm))

write.csv(ov_gap_seasonal, "ov_gap_seasonal.csv")

head(ov_gap_seasonal)

ggplot(ov_gap_seasonal, aes(fluctuation)) +
  geom_histogram(fill = "darkgoldenrod1", position = 'identity', bins = 50)  +
  theme_bw() +
  labs(x = "", y = "")  +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank())
ggsave("Figures/seasonal_fluctuation_coverage.png")

###########################################################
# Gini Index
sp <- unique(ov$species)

for(i in sp){
  print(i)
  
  sp_data <- ov %>% 
    dplyr::filter(species == i)
  
  x <- as.data.frame(Gini(sp_data$sdm, unbiased = TRUE))
  colnames(x)[1] <- "coef"
  
  x <- x %>% 
    dplyr::mutate(species = i)
  
  write.csv(x, file = paste0("SeasonalDifference_Gini/", i, "_gini", ".csv"))
  
}

# Merge output
input_folder = "SeasonalDifference_Gini"
output = dir(input_folder, "^.*\\.csv$", full.names = TRUE)
gini <- plyr::ldply(output, readr::read_csv)

head(gini)

gini <- gini %>% dplyr::select(species, coef)

write_csv(gini, "seasonal_coverage_Gini.csv")

ggplot(gini, aes(coef)) +
  geom_histogram(fill = "darkgoldenrod1", position = 'identity', bins = 50)  +
  theme_bw() +
  labs(x = "", y = "")  +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank())

ggsave("Figures/seasonal_fluctuation_coverage_gini.png")

#############################################################
# Interpolation gap [global, 30%] [season]
#############################################################
gap <- read.csv("interpolation_gap_sdm_global_16_ecoregion_season_up.csv")
gap <- gap[1:3126,]
# gap <- gap[!gap$season=="Year-round",]
# gap <- gap %>% 
#   dplyr::mutate(fill = ifelse(gap <= 0, "MT", "NMT"))

head(gap)

gap_sum <- gap %>% 
  group_by(ecoregion, season) %>% 
  dplyr::mutate(mt = ifelse(gap <= 0, 1, 0)) %>% 
  dplyr::summarise(mt_per = sum(mt)/NROW(species)*100)

gap_sum_global <- gap_sum %>% 
  group_by(ecoregion) %>% 
  dplyr::summarise(mt_per = mean(mt_per))

head(gap_sum)

# Afrotropical
gap_sum %>% 
  filter(ecoregion %in% "Afrotropical") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

# gap %>% 
#   filter(ecoregion %in% "Afrotropical") %>% 
#   ggplot( aes(season, gap, col = season, fill = season)) +
#   geom_boxplot() +
#   theme_classic() +
#   labs(x = "", y = "") +
#   theme(legend.position = "none", legend.title = element_blank()) + 
#   scale_color_viridis_d()+ 
#   scale_fill_viridis_d()

ggsave("Figures/seasonal_global_gap_16_Afrotropical.png")

# Australian
gap_sum %>% 
  filter(ecoregion %in% "Australian") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_Australian.png")

# Madagascan
gap_sum %>% 
  filter(ecoregion %in% "Madagascan") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_Madagascan.png")

# Oriental
gap_sum %>% 
  filter(ecoregion %in% "Oriental") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_Oriental.png")

# Neotropical
gap_sum %>% 
  filter(ecoregion %in% "Neotropical") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_Neotropical.png")

# Panamanian
gap_sum %>% 
  filter(ecoregion %in% "Panamanian") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_Panamanian.png")

# SinoJapanese
gap_sum %>% 
  filter(ecoregion %in% "SinoJapanese") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_SinoJapanese.png")

# Nearctic
gap_sum %>% 
  filter(ecoregion %in% "Nearctic") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_Nearctic.png")

# Palaearctic
gap_sum %>% 
  filter(ecoregion %in% "Palaearctic") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_Palaearctic.png")

# SaharoArabian
gap_sum %>% 
  filter(ecoregion %in% "SaharoArabian") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_SaharoArabian.png")

# Oceanian
gap_sum %>% 
  filter(ecoregion %in% "Oceanian") %>% 
  ggplot( aes(ecoregion, mt_per, fill = season, col = season)) +
  geom_bar(position = 'dodge', stat = "identity") +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + scale_color_viridis_d() +
  scale_fill_viridis_d() + ylim(0, 100)

ggsave("Figures/seasonal_global_gap_16_Oceanian.png")

