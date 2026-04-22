# opening data

library(readxl)

data2015_raw <- read_excel("Z:/Documents/GitHub/finland_ew_time_analysis/data/earthworm data_2015.xlsx")
data2025_raw <- read_excel("Z:/Documents/GitHub/finland_ew_time_analysis/data/Finland_resampling2025_ID.xlsx")

# cleaning data 

library(tidyverse)

data2025 <- data2025_raw %>% 
  select(site_name, replicate, taxon, nb_obs) 

sites_resampled <- data2025 %>% # database with all sites resampled so we can select only those sites in the analysis
  select(site_name) %>% 
  distinct()

data2015 <- sites_resampled %>% # we select sites resampled
  rename(siteID = site_name) %>% 
  left_join(data2015_raw) %>% # we join the data from 2015
  pivot_longer(cols = 'juvenile D. octaedra':'adult L. terrestris') %>% # we change the format so one row is one observation per species
  select(siteID, UID, name, value) %>% 
  rename(site_name = siteID,
         replicate = UID, 
         taxon=name, 
         nb_obs=value)

# now we need to have the same species name
# I decide to merge A tuberculata and A caliginosa into A caliginosa
# I also put accepted names
# and I decide not to keep juveniles at the species level, because they have not been identified in 2025, and also because we can't be 100% sure of the identification

# However I consider subadults in 2025 as adults (ask Erin about how she IDed)

# (maybe in the future i'll upgrade identifications from 2025)

unique(data2025$taxon)
unique(data2015$taxon)

data2015$taxon <- gsub("juvenile D. octaedra", "Undetermined", data2015$taxon)
data2015$taxon <- gsub("juvenile D. rubidus", "Undetermined", data2015$taxon)
data2015$taxon <- gsub("juvenile Lumbricus", "Undetermined", data2015$taxon)
data2015$taxon <- gsub("juvenile endogeic", "Undetermined", data2015$taxon)
data2015$taxon <- gsub("adult D. octaedra", "Dendrobaena octaedra", data2015$taxon)
data2015$taxon <- gsub("adult D. rubidus", "Bimastos rubidus", data2015$taxon)
data2015$taxon <- gsub("adult L. rubellus", "Lumbricus rubellus", data2015$taxon)
data2015$taxon <- gsub("adult A. tuberculata", "Aporrectodea caliginosa", data2015$taxon)
data2015$taxon <- gsub("adult A. rosea", "Aporrectodea rosea", data2015$taxon)
data2015$taxon <- gsub("adult A. caliginosa", "Aporrectodea caliginosa", data2015$taxon)
data2015$taxon <- gsub("adult O. tyrtaeum", "Octolasion lacteum", data2015$taxon)
data2015$taxon <- gsub("adult L. terrestris", "Lumbricus terrestris/herculeus", data2015$taxon)

# I also change names in 2025 data, to keep only adults and subadults identified at species level

data2025$taxon <- gsub("Dendrobaena", "Undetermined", data2025$taxon)
data2025$taxon <- gsub("Lumbricus", "Undetermined", data2025$taxon)

unique(data2025$taxon)
unique(data2015$taxon)

# /!\/!\/!\ I will maybe have to keep juveniles identified for the count of species richness !!! /!\/!\/!\


