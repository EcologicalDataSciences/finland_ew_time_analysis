#### 1. Opening data ####

library(readxl)

data2015_raw <- read_excel("Z:/Documents/GitHub/finland_ew_time_analysis/data/earthworm data_2015.xlsx")
data2025_raw <- read_excel("Z:/Documents/GitHub/finland_ew_time_analysis/data/Finland_resampling2025_ID.xlsx")
sites_resampled2025 <- read_excel("Z:/Documents/GitHub/finland_ew_time_analysis/data/Finland_resampling2025_Sites.xlsx")
  
# cleaning data 

library(tidyverse)

sites <- sites_resampled2025 %>% # here we want the list of sites resampled
  select(site_name, replicate) %>% 
  distinct() %>% 
  filter(!grepl("X",replicate)) # removing replicate "X" that was just an unnecessary additional replicate we did on the field

data2025 <- sites %>% 
  left_join(data2025_raw) %>% 
  filter(development_stage!="undetermined") %>% # to remove pieces of ew that could not be ided and that are not "individual"
  select(site_name, replicate, taxon, nb_obs) 

data2015 <- sites %>% 
  rename(siteID = site_name,
         UID=replicate) %>% 
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

data2025$taxon <- gsub("Dendrobaena$", "Undetermined", data2025$taxon) # I put $ so all string that ends with "Dendrobaena" are changed in "undetermined". Then, "Dendrobaena" becomes "Undetermined" but "Dendrobaena octaedra" stays "Dendrobaedna octaedra" instead of "Undetermined octaedra"
data2025$taxon <- gsub("Lumbricus$", "Undetermined", data2025$taxon) # same as before

unique(data2025$taxon)
unique(data2015$taxon)

# /!\/!\/!\ I will maybe have to keep juveniles identified for the count of species richness !!! /!\/!\/!\

#### 2. Analyses ####

# abundance and richness change

data_comp <- rbind(data2015 %>% mutate(year="t0"), data2025 %>% mutate(year="t1")) %>% # i merge 2015 and 2025 data adding a column for year (t0 for 2015, t1 for 2025)
  group_by(site_name, taxon, year) %>% 
  summarise(tot_obs=sum(nb_obs)) %>% # i measure the total nb of obs per taxa
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0) %>% # one column per taxa, so I have some 0 when they are not observed, and it will be needed for mean abundances. Also sites with no obs (in 2025, they have a count of zero for taxa "NA")
  select(-'NA') %>%  # I remove the column "NA" that was needed for sites with no individuals in 2025
  pivot_longer('Aporrectodea caliginosa':Undetermined, values_to="tot_obs", names_to="taxon")

# we will use difference (abt0-abt1)

data_dif <- data_comp %>% 
  pivot_wider(names_from=year, values_from=tot_obs) %>% 
  ungroup() %>% 
  group_by(site_name) %>% 
  mutate(t0_site=sum(t0)) %>% 
  mutate(t1_site=sum(t1)) %>% 
  ungroup()

mean(data_dif$t0_site)
mean(data_dif$t1_site)

tapply(data_dif$t0, data_dif$taxon, mean)
tapply(data_dif$t1, data_dif$taxon, mean)

# we can maybe also use winner loser framework

library(adespatial)

# creation of site-by-species matrixes

mat2015 <- data_dif %>% 
  filter(taxon!="Undetermined") %>% 
  select(site_name, taxon, t0) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=t0) %>% 
  select(-site_name)

mat2015 <- as.matrix(mat2015)

mat2025 <- data_dif %>% 
  filter(taxon!="Undetermined") %>% 
  select(site_name, taxon, t1) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=t1) %>% 
  select(-site_name)

mat2025 <- as.matrix(mat2025)

a <-tpaired.krandtest(mat2015, mat2025, nperm=99)
a
p.adjust(a$t.test$p.param, method="holm") # p values corrected for multiple testing

# change in richness

richness_change <- data_comp %>% 
  mutate(occurence=ifelse(tot_obs>0, 1, 0))  %>% 
  group_by(site_name, year) %>%
  summarise(richness=sum(occurence))
 
tapply(richness_change$richness, richness_change$year, mean)

