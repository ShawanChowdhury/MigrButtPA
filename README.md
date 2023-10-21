# MigratoryButterfliesPA
In this project, we examined the performance of protected areas in conserving the annual cycle of migratory butterflies. Here are brief descriptions of the R scripts.

01.gbifData.R: collecting GBIF data

02.dataCleaning.R: cleaning GBIF data

03.dataPreparation.R: earlier script [We prepared the data for the analysis.]

04.spatialThinning.R: earlier script [We planned to do the thinning using the spThin R package, but considering the time it was taking for each species, we decided to run it when running the niche models]

04.spatialThinning_hpc.R: earlier script [We thought about running the thinning using HPC and then changed the plan.]

04.spatialThinning_hpc.sh: earlier script [We thought about running the thinning using HPC and then changed the plan.]

05.suitabilityMaps.R: running the niche model

06.migr_butt_country.R: calculating the suitability of migratory butterflies by country

07.paOverlap.R: calculated the amount of suitable habitat within the current protected area system

08.target_representation.R: estimated the amount of suitable habitat that needs to be protected

09.mergedData.R: data wrangling

10.figures.R: created some figures [The final versions were created using Adobe Photoshop.]
