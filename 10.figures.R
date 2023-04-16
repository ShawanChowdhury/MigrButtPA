# Loading required libraries
library(data.table)
library(tidyverse)
library(DescTools)
library(sp)
library(sf)
library(raster)
library(tmap)

##############################################
# Seasonal PA coverage
ov <- fread("output/seasonal_interpolation_taxa.csv")
head(ov)

# Area_coverage
ggplot(ov, aes(sdm.area, prop_coverage, col = season)) +
  geom_point() + 
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + 
  scale_color_viridis_c()

ggsave("figures/area_coverage_up.png")

# Seasonal fluctuation
ggplot(ov, aes(prop_coverage, fill = season)) +
  geom_histogram(position = 'identity', bins = 50) +
  theme_bw() +
  labs(x = "", y = "") + facet_wrap(~season) +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank()) + 
  scale_fill_viridis_c()
ggsave("figures/season_coverage.png")

##############################################
# Seasonal fluctuation in PA coverage
ov_gap_seasonal <- fread("output/ov_gap_seasonal.csv")
head(ov_gap_seasonal)

ggplot(ov_gap_seasonal, aes(fluctuation)) +
  geom_histogram(fill = "darkgoldenrod1", position = 'identity', bins = 50)  +
  theme_bw() +
  labs(x = "", y = "")  +
  theme(panel.border = element_blank(),legend.position = "none", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"),
        strip.background = element_blank())
ggsave("figures/seasonal_fluctuation_coverage.png")