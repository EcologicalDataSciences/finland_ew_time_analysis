########################################################
##### Code for New-Zealand journal of soil zoology #####
########################################################

#### 0. Libraries ####

library(readxl) # to open .xlsx sheets
library(tidyverse) # for data manipulation
library(adespatial) # to test change in species abundance
library(ggrepel) # for labels in ggplots
library(ggpubr) # for combining figures

library(sf) # for manipulating vector objects
library(terra) # for manipulating raster objects
library(rnaturalearth) # for dowloading world and country maps
#devtools::install_github("ropensci/rnaturalearthhires") # for precise versions of map
library(Rchelsa) # for extraction of CHELSA data

library(tidyterra) # for nice map plots
library(scales) # to create breaks

#remotes::install_github("rOpenGov/fmi2")
library(fmi2) # for extraction of fmi data
library(DT) #usefull ? (extraction of fmi data?)

#### 1. Community data ####

data2015_raw <- read_excel("data/earthworm data_2015.xlsx")
data2025_raw <- read.csv2("data/Finland_resampling2025_ID.csv")

sites_resampled2025 <- read_excel("data/Finland_resampling2025_Sites.xlsx")

sites_to_map <- sites_resampled2025 %>% 
  select(replicate) %>% 
  distinct() %>% 
  left_join(data2015_raw %>% rename(replicate=UID)) %>% 
  separate_wider_delim(replicate, "_", names = c("region", "site", "quadrat")) %>% 
  filter(quadrat=="6") %>% 
  select(region, site, Latitude, Longitude) %>% 
  distinct()

# now, we will "clean" them and have the same format for initial sampling and resampling

sites <- sites_resampled2025 %>% # here we want the list of sites resampled
  select(site_name, replicate) %>% 
  distinct() %>% 
  filter(!grepl("X",replicate)) # removing replicate "X" that was just an unnecessary additional replicate we did on the field in 2025

data2025cleaning <- sites %>% 
  left_join(data2025_raw %>% filter(development_stage!="undetermined")) %>%    # to remove pieces of ew that could not be ided and that do not count as "individual"
  select(site_name, replicate, taxon, nb_obs, development_stage)

data2015cleaning <- sites %>% 
  rename(siteID = site_name,
         UID=replicate) %>% 
  left_join(data2015_raw) %>% # we join the data from 2015
  pivot_longer(cols = 'juvenile D. octaedra':'adult L. terrestris') %>% # we change the format so one row is one observation per species
  select(siteID, UID, name, value) %>% 
  rename(site_name = siteID,
         replicate = UID, 
         taxon=name, 
         nb_obs=value) %>% 
  mutate(development_stage = ifelse(grepl("juvenile",taxon), "juvenile", "adult"))  %>% 
  group_by(taxon) %>% mutate(tot=sum(nb_obs)) 
  
# now we need to have the same species names
# I decide to merge A tuberculata and A caliginosa into A caliginosa
# I also put accepted names

unique(data2025cleaning$taxon)
unique(data2015cleaning$taxon)

data2015cleaning$taxon <- gsub("juvenile D. octaedra", "Dendrobaena octaedra", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("juvenile Lumbricus", "Lumbricus", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("juvenile D. rubidus", "Bimastos rubidus", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("juvenile endogeic", "Undetermined", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult D. octaedra", "Dendrobaena octaedra", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult D. rubidus", "Bimastos rubidus", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult L. rubellus", "Lumbricus rubellus", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult A. tuberculata", "Aporrectodea caliginosa", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult A. rosea", "Aporrectodea rosea", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult A. caliginosa", "Aporrectodea caliginosa", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult O. tyrtaeum", "Octolasion lacteum", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult L. terrestris", "Lumbricus terrestris", data2015cleaning$taxon)

# I also need to change the name of a NA observation in a site that is empty because only a part of individual has been found (see row 20)

data2025cleaning$taxon <- ifelse(is.na(data2025cleaning$taxon), "NA", data2025cleaning$taxon)
data2025cleaning$nb_obs <- ifelse(is.na(data2025cleaning$nb_obs), 0, data2025cleaning$nb_obs)
data2025cleaning$development_stage <- ifelse(is.na(data2025cleaning$development_stage), "NA", data2025cleaning$development_stage)

unique(data2025cleaning$taxon)
unique(data2015cleaning$taxon)

# i will also consider 2025 subadults as juveniles

data2025cleaning$development_stage <- gsub("subadult", "juvenile", data2025cleaning$development_stage)

# now I split the database into adult only and all (adult + juvenile), and give it the same format (so a row per taxa observed in the whole dataset for each site, like how data2015 is organised)
# results for adults only are found in Supplementary material of the paper

data2025 <- data2025cleaning %>% 
  group_by(site_name,replicate, taxon) %>% 
  summarise(tot_obs=sum(nb_obs)) %>%  
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0)%>% 
  select(-'NA') %>% 
  pivot_longer('Dendrobaena octaedra':'Octolasion lacteum') %>% 
  mutate(year="t1") %>% 
  rename(taxon=name, 
         tot_obs=value)

data2025ad <- sites %>% 
  left_join(data2025cleaning %>% filter(development_stage=="adult")) %>% 
  select(-development_stage) %>% 
  group_by(site_name,replicate, taxon) %>% 
  summarise(tot_obs=sum(nb_obs)) %>% 
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0) %>% 
  select(-'NA') %>% 
  pivot_longer('Dendrobaena octaedra':'Octolasion lacteum') %>% 
  mutate(year="t1")%>% 
  rename(taxon=name, 
         tot_obs=value)

data2015 <- data2015cleaning %>% 
  group_by(site_name,replicate, taxon) %>% 
  summarise(tot_obs=sum(nb_obs)) %>% 
  mutate(year="t0")

data2015ad <- sites %>% 
  left_join(data2015cleaning %>% filter(development_stage=="adult")) %>% 
  select(-development_stage) %>% 
  group_by(site_name,replicate, taxon) %>% 
  summarise(tot_obs=sum(nb_obs)) %>% 
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0) %>% 
  pivot_longer('Aporrectodea caliginosa':'Octolasion lacteum') %>% 
  mutate(year="t0")%>% 
  rename(taxon=name, 
         tot_obs=value)

data <- rbind(data2015, data2025) %>% ungroup() %>% 
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0) %>% 
  pivot_longer('Aporrectodea caliginosa':'Octolasion') %>% 
  rename(taxon=name, 
         tot_obs=value)

data_adult_only <- rbind(data2015ad, data2025ad) %>% ungroup() %>% 
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0) %>% 
  pivot_longer('Aporrectodea caliginosa':'Lumbricus castaneus') %>% 
  rename(taxon=name, 
         tot_obs=value)

# distance between sites 

mLevi <- terra::distance(sites_to_map[sites_to_map$region=="Levi", c("Longitude", "Latitude")], lonlat=TRUE)/1000
mLammi <- terra::distance(sites_to_map[sites_to_map$region=="Lammi", c("Longitude", "Latitude")], lonlat=TRUE)/1000
mTva <- terra::distance(sites_to_map[sites_to_map$region=="Tvarminne", c("Longitude", "Latitude")], lonlat=TRUE)/1000
mKilp <- terra::distance(sites_to_map[sites_to_map$region=="kilpisjarvi", c("Longitude", "Latitude")], lonlat=TRUE)/1000

mean(c(mLevi, mLammi, mTva, mKilp))
min(c(mLevi, mLammi, mTva, mKilp))
max(c(mLevi, mLammi, mTva, mKilp))
sd(c(mLevi, mLammi, mTva, mKilp))

#### 2. Climate data ####

#### 2.1. CHELSA ####

#### 2.1.1. Loading country vectors ####

# Load Finland
finland <- ne_countries(country="finland", scale="large", type="sovereignty")

# Load world
world50 <- ne_countries( scale="large")

#### 2.1.2. Extraction of CHELSA data ####

# note: it is not possible to extract the data we want for a long time period (01 JAN 2005-31 DEC 2024) so the extraction is done year by year

# 2005

temp_2005 <- getChelsa(
  var = "tas",
  startdate = as.Date("2005-01-01"),
  enddate   = as.Date("2005-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2005i <- temp_2005

temp_2005i[temp_2005i<273.15] = 1
temp_2005i[temp_2005i>=273.15] = 0

subzerodays2005 <- sum(temp_2005i)
meanT2005 <- mean(temp_2005)

# 2006

temp_2006 <- getChelsa(
  var = "tas",
  startdate = as.Date("2006-01-01"),
  enddate   = as.Date("2006-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2006i <- temp_2006

temp_2006i[temp_2006i<273.15] = 1
temp_2006i[temp_2006i>=273.15] = 0

subzerodays2006 <- sum(temp_2006i)
meanT2006 <- mean(temp_2006)

# 2007

temp_2007 <- getChelsa(
  var = "tas",
  startdate = as.Date("2007-01-01"),
  enddate   = as.Date("2007-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2007i <- temp_2007

temp_2007i[temp_2007i<273.15] = 1
temp_2007i[temp_2007i>=273.15] = 0

subzerodays2007 <- sum(temp_2007i)
meanT2007 <- mean(temp_2007)

# 2008

temp_2008 <- getChelsa(
  var = "tas",
  startdate = as.Date("2008-01-01"),
  enddate   = as.Date("2008-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2008i <- temp_2008

temp_2008i[temp_2008i<273.15] = 1
temp_2008i[temp_2008i>=273.15] = 0

subzerodays2008 <- sum(temp_2008i)
meanT2008 <- mean(temp_2008)

# 2009

temp_2009 <- getChelsa(
  var = "tas",
  startdate = as.Date("2009-01-01"),
  enddate   = as.Date("2009-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2009i <- temp_2009

temp_2009i[temp_2009i<273.15] = 1
temp_2009i[temp_2009i>=273.15] = 0

subzerodays2009 <- sum(temp_2009i)
meanT2009 <- mean(temp_2009)

# 2010

temp_2010 <- getChelsa(
  var = "tas",
  startdate = as.Date("2010-01-01"),
  enddate   = as.Date("2010-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2010i <- temp_2010

temp_2010i[temp_2010i<273.15] = 1
temp_2010i[temp_2010i>=273.15] = 0

subzerodays2010 <- sum(temp_2010i)
meanT2010 <- mean(temp_2010)

# 2011

temp_2011 <- getChelsa(
  var = "tas",
  startdate = as.Date("2011-01-01"),
  enddate   = as.Date("2011-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2011i <- temp_2011

temp_2011i[temp_2011i<273.15] = 1
temp_2011i[temp_2011i>=273.15] = 0

subzerodays2011 <- sum(temp_2011i)
meanT2011 <- mean(temp_2011)

# 2012

temp_2012 <- getChelsa(
  var = "tas",
  startdate = as.Date("2012-01-01"),
  enddate   = as.Date("2012-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2012i <- temp_2012

temp_2012i[temp_2012i<273.15] = 1
temp_2012i[temp_2012i>=273.15] = 0

subzerodays2012 <- sum(temp_2012i)
meanT2012 <- mean(temp_2012)

# 2013

temp_2013 <- getChelsa(
  var = "tas",
  startdate = as.Date("2013-01-01"),
  enddate   = as.Date("2013-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2013i <- temp_2013

temp_2013i[temp_2013i<273.15] = 1
temp_2013i[temp_2013i>=273.15] = 0

subzerodays2013 <- sum(temp_2013i)
meanT2013 <- mean(temp_2013)

# 2014

temp_2014 <- getChelsa(
  var = "tas",
  startdate = as.Date("2014-01-01"),
  enddate   = as.Date("2014-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2014i <- temp_2014

temp_2014i[temp_2014i<273.15] = 1
temp_2014i[temp_2014i>=273.15] = 0

subzerodays2014 <- sum(temp_2014i)
meanT2014 <- mean(temp_2014)

# 2015

temp_2015 <- getChelsa(
  var = "tas",
  startdate = as.Date("2015-01-01"),
  enddate   = as.Date("2015-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2015i <- temp_2015

temp_2015i[temp_2015i<273.15] = 1
temp_2015i[temp_2015i>=273.15] = 0

subzerodays2015 <- sum(temp_2015i)
meanT2015 <- mean(temp_2015)

# 2016

temp_2016 <- getChelsa(
  var = "tas",
  startdate = as.Date("2016-01-01"),
  enddate   = as.Date("2016-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2016i <- temp_2016

temp_2016i[temp_2016i<273.15] = 1
temp_2016i[temp_2016i>=273.15] = 0

subzerodays2016 <- sum(temp_2016i)
meanT2016 <- mean(temp_2016)

# 2017

temp_2017 <- getChelsa(
  var = "tas",
  startdate = as.Date("2017-01-01"),
  enddate   = as.Date("2017-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2017i <- temp_2017

temp_2017i[temp_2017i<273.15] = 1
temp_2017i[temp_2017i>=273.15] = 0

subzerodays2017 <- sum(temp_2017i)
meanT2017 <- mean(temp_2017)

# 2018

temp_2018 <- getChelsa(
  var = "tas",
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2018-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2018i <- temp_2018

temp_2018i[temp_2018i<273.15] = 1
temp_2018i[temp_2018i>=273.15] = 0

subzerodays2018 <- sum(temp_2018i)
meanT2018 <- mean(temp_2018)

# 2019

temp_2019 <- getChelsa(
  var = "tas",
  startdate = as.Date("2019-01-01"),
  enddate   = as.Date("2019-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2019i <- temp_2019

temp_2019i[temp_2019i<273.15] = 1
temp_2019i[temp_2019i>=273.15] = 0

subzerodays2019 <- sum(temp_2019i)
meanT2019 <- mean(temp_2019)

# 2020

temp_2020 <- getChelsa(
  var = "tas",
  startdate = as.Date("2020-01-01"),
  enddate   = as.Date("2020-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2020i <- temp_2020

temp_2020i[temp_2020i<273.15] = 1
temp_2020i[temp_2020i>=273.15] = 0

subzerodays2020 <- sum(temp_2020i)
meanT2020 <- mean(temp_2020)

# 2021

temp_2021 <- getChelsa(
  var = "tas",
  startdate = as.Date("2021-01-01"),
  enddate   = as.Date("2021-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2021i <- temp_2021

temp_2021i[temp_2021i<273.15] = 1
temp_2021i[temp_2021i>=273.15] = 0

subzerodays2021 <- sum(temp_2021i)
meanT2021 <- mean(temp_2021)

# 2022

temp_2022 <- getChelsa(
  var = "tas",
  startdate = as.Date("2022-01-01"),
  enddate   = as.Date("2022-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2022i <- temp_2022

temp_2022i[temp_2022i<273.15] = 1
temp_2022i[temp_2022i>=273.15] = 0

subzerodays2022 <- sum(temp_2022i)
meanT2022 <- mean(temp_2022)

# 2023

temp_2023 <- getChelsa(
  var = "tas",
  startdate = as.Date("2023-01-01"),
  enddate   = as.Date("2023-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2023i <- temp_2023

temp_2023i[temp_2023i<273.15] = 1
temp_2023i[temp_2023i>=273.15] = 0

subzerodays2023 <- sum(temp_2023i)
meanT2023 <- mean(temp_2023)

# 2024

temp_2024 <- getChelsa(
  var = "tas",
  startdate = as.Date("2024-01-01"),
  enddate   = as.Date("2024-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2024i <- temp_2024

temp_2024i[temp_2024i<273.15] = 1
temp_2024i[temp_2024i>=273.15] = 0

subzerodays2024 <- sum(temp_2024i)
meanT2024 <- mean(temp_2024)

# change between 2005-2014 and 2015-2024

## freezing days

mean_subzerodays2005_2014 <- mean(subzerodays2005,
                                  subzerodays2006,
                                  subzerodays2007,
                                  subzerodays2008,
                                  subzerodays2009,
                                  subzerodays2010,
                                  subzerodays2011,
                                  subzerodays2012,
                                  subzerodays2013,
                                  subzerodays2014)

mean_subzerodays2015_2024 <- mean(subzerodays2015,
                                  subzerodays2016,
                                  subzerodays2017,
                                  subzerodays2018,
                                  subzerodays2019,
                                  #    subzerodays2020, # 2020 has lots of blank spaces, weird
                                  subzerodays2021,
                                  subzerodays2022,
                                  subzerodays2023,
                                  subzerodays2024)

subzerodays_change_perc <- (mean_subzerodays2015_2024 - mean_subzerodays2005_2014)/mean_subzerodays2005_2014*100

## mean temperature

mean_T_2005_2014 <- mean(meanT2005,
                         meanT2006,
                         meanT2007,
                         meanT2008,
                         meanT2009,
                         meanT2010,
                         meanT2011,
                         meanT2012,
                         meanT2013,
                         meanT2014)

mean_T_2015_2024 <- mean(meanT2015,
                         meanT2016,
                         meanT2017,
                         meanT2018,
                         meanT2019,
                      #   meanT2020,
                         meanT2021,
                         meanT2022,
                         meanT2023,
                         meanT2024)

T_change <- mean_T_2015_2024 - mean_T_2005_2014

#### 2.1.3. Saving (and reopening) raster data ####

# saving rasters

#writeRaster(mean_subzerodays2005_2014, "derived_data/mean_subzerodays2005_2014.tif")
#writeRaster(mean_subzerodays2015_2024, "derived_data/mean_subzerodays2015_2024.tif")
#writeRaster(subzerodays_change_perc, "derived_data/subzerodays_change_perc.tif")
#writeRaster(mean_T_2005_2014, "derived_data/mean_T_2005_2014.tif")
#writeRaster(mean_T_2015_2024, "derived_data/mean_T_2015_2024.tif")
#writeRaster(T_change, "derived_data/T_change.tif")

# reopening them

#mean_subzerodays2005_2014 <- rast("derived_data/mean_subzerodays2005_2014.tif")
#mean_subzerodays2015_2024 <- rast("derived_data/mean_subzerodays2015_2024.tif")
#subzerodays_change_perc <- rast("derived_data/subzerodays_change_perc.tif")

#mean_T_2005_2014 <- rast("derived_data/mean_T_2005_2014.tif")
#mean_T_2015_2024 <- rast("derived_data/mean_T_2015_2024.tif")
#T_change <- rast("derived_data/T_change.tif")

#### 2.1.4. Figures ####

# freezing days 

finland_reprojected = st_transform(finland, st_crs(mean_subzerodays2015_2024))

subzerodays_change_perc_cropped = crop(subzerodays_change_perc, finland_reprojected)
subzerodays_change_perc_final = mask(subzerodays_change_perc_cropped, finland_reprojected)

##### *Figure 1* ####

FIGURE1 <- ggplot()+
  geom_sf(data=world50, fill="lightgrey")+
  geom_spatraster(data=subzerodays_change_perc_final, aes(fill=sum))+
  scale_fill_gradientn(colours = c("#560048","#b8179d","#f278de", "#f0f0f0","#4DA739"), 
                       values = rescale(c(-37,-20,-10,0,6.51)),
                       guide = "colorbar", limits=c(-37,6.51), na.value=NA)+
  theme_bw()+
  labs(fill = "Change (%) of mean yearly\nnumber of day with less\nthan 0°C between\n2005-2014 and 2015-2024")+
  geom_point(data = sites_to_map %>% mutate(region = fct_relevel(region, 
                                                                 "kilpisjarvi", "Levi", "Lammi", 
                                                                 "Tvarminne")),
             mapping = aes(x = Longitude, y = Latitude, shape=region), size=3)+
  scale_shape_manual(name = "Region",
                     labels = c("Kilpisjärvi (n=4)","Levi (n=5)", "Lammi (n=6)", "Tvärminne (n=6)"),
                     values = c(16, 17, 15, 18))+
  geom_sf(data=finland, fill=NA)+
  xlim(18, 33)+
  ylim(59, 71)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="figures/Figure_1.svg", plot=FIGURE1, height=12, width=12, units="cm")

# temperatures

T_change_cropped = crop(T_change, finland_reprojected)
T_change_final = mask(T_change_cropped, finland_reprojected)

##### *Figure S1* ####

FIGURES1 <- ggplot()+
  geom_sf(data=world50, fill="lightgrey")+
  geom_spatraster(data=T_change_final, aes(fill=mean))+
  scale_fill_whitebox_c(na.value=NA)+ 
  theme_bw()+
  labs(fill = "Mean temperature change \nbetween 2005-2014 and \n2015-2024")+
  geom_point(data = sites_to_map %>% mutate(region = fct_relevel(region, 
                                                                 "kilpisjarvi", "Levi", "Lammi", 
                                                                 "Tvarminne")),
             mapping = aes(x = Longitude, y = Latitude, shape=region), size=3)+
  scale_shape_manual(name = "Region",
                     labels = c("Kilpisjärvi","Levi", "Lammi", "Tvärminne"),
                     values = c(16, 17, 15, 18))+
  geom_sf(data=finland, fill=NA)+
  xlim(18, 33)+
  ylim(59, 71)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="figures/Figure_S1.svg", plot=FIGURES1, height=12, width=12, units="cm")

#### 2.2. Finnish meteorological station ####

# we want data from Kilpisjärvi, Levi, Lammi and Tvärminne

# closest stations:

# kilpisjarvi: fmisid=102016
# tvarminne: fmisid= 100953
#lammi: fmisid = 101154
# levi: fmisid = 101932

#### 2.2.1. Extraction ####

# i have to extract every year separately because the function will not allow me to extract data from such a large time scale

start <- c("2005-01-02",
           "2006-01-01",
           "2007-01-01",
           "2008-01-01",
           "2009-01-01",
           "2010-01-01",
           "2011-01-01",
           "2012-01-01",
           "2013-01-01",
           "2014-01-01",
           "2015-01-01",
           "2016-01-01",
           "2017-01-01",
           "2018-01-01",
           "2019-01-01",
           "2020-01-01",
           "2021-01-01",
           "2022-01-01",
           "2023-01-01",
           "2024-01-01")

end <- c("2005-12-31",
         "2006-12-31",
         "2007-12-31",
         "2008-12-31",
         "2009-12-31",
         "2010-12-31",
         "2011-12-31",
         "2012-12-31",
         "2013-12-31",
         "2014-12-31",
         "2015-12-31",
         "2016-12-31",
         "2017-12-31",
         "2018-12-31",
         "2019-12-31",
         "2020-12-31",
         "2021-12-31",
         "2022-12-31",
         "2023-12-31",
         "2024-12-31")

#kilpisjarvi

# (i first extract data from the first day and then add to this objects all the other days)

kilpisjarvi_data_init <- obs_weather_daily(starttime = "2005-01-01",
                                           endtime = "2005-01-01",
                                           fmisid = 102016)

for (i in 1:20){
  print(start[i])
  kilpisjarvi_data_i <- obs_weather_daily(starttime = start[i],
                                          endtime = end[i],
                                          fmisid = 102016)
  kilpisjarvi_data_init <- rbind(kilpisjarvi_data_init, kilpisjarvi_data_i)
  
}

kilpisjarvi_data <- st_drop_geometry(kilpisjarvi_data_init) %>%  
  #  filter(variable=="snow"|variable=="tday") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>% 
  pivot_wider(names_from = variable, values_from = value) 

#levi

levi_data_init <- obs_weather_daily(starttime = "2005-01-01",
                                    endtime = "2005-01-01",
                                    fmisid = 101932)

for (i in 1:20){
  print(start[i])
  levi_data_i <- obs_weather_daily(starttime = start[i],
                                   endtime = end[i],
                                   fmisid = 101932)
  levi_data_init <- rbind(levi_data_init, levi_data_i)
  
}

levi_data <- st_drop_geometry(levi_data_init) %>% 
  #  filter(variable=="snow"|variable=="tday") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>%
  pivot_wider(names_from = variable, values_from = value)

#lammi

lammi_data_init <- obs_weather_daily(starttime = "2005-01-01",
                                     endtime = "2005-01-01",
                                     fmisid = 101154)

for (i in 1:20){
  print(start[i])
  lammi_data_i <- obs_weather_daily(starttime = start[i],
                                    endtime = end[i],
                                    fmisid = 101154)
  lammi_data_init <- rbind(lammi_data_init, lammi_data_i)
  
}

lammi_data <- st_drop_geometry(lammi_data_init) %>% 
  #  filter(variable=="snow"|variable=="tday") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>%
  pivot_wider(names_from = variable, values_from = value)

#tvarminne

tvarminne_data_init <- obs_weather_daily(starttime = "2005-01-01",
                                         endtime = "2005-01-01",
                                         fmisid = 100953)

for (i in 1:20){
  print(start[i])
  tvarminne_data_i <- obs_weather_daily(starttime = start[i],
                                        endtime = end[i],
                                        fmisid = 100953)
  tvarminne_data_init <- rbind(tvarminne_data_init, tvarminne_data_i)
  
}

tvarminne_data <- st_drop_geometry(tvarminne_data_init) %>% 
  #  filter(variable=="snow"|variable=="tday") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>%
  pivot_wider(names_from = variable, values_from = value)

#### 2.2.2. Merging everything ####

# (it is called "snow" but has all climate data)

snow_all <- rbind(kilpisjarvi_data %>% mutate(region="kilpisjarvi") %>% select(-TG_PT12H_min),
                  levi_data %>% mutate(region="Levi"),
                  lammi_data %>% mutate(region="Lammi"),
                  tvarminne_data %>% mutate(region="Tvarminne"))  %>% 
  rename(snow_depth=snow) %>% 
  rename(year_id=year) %>% 
  mutate(year_id=as.numeric(year_id)) %>% 
  mutate(year=ifelse(year_id>=2005 & year_id<=2014, "t0", "t1")) %>% 
  distinct() 

snow_all$snow_depth <- gsub("-1", "0", snow_all$snow_depth)
snow_all$rrday <- gsub("-1", "0", snow_all$rrday)

#### * Table S1* ####

snow_all_means <- snow_all %>% 
  mutate(snow_depth=as.numeric(snow_depth)) %>% 
  mutate(rrday=as.numeric(rrday)) %>% 
  group_by(region, year_id) %>% 
  mutate(mean_snow_depth=mean(snow_depth, na.rm=TRUE),
         mean_tday=mean(tday, na.rm=TRUE),
         mean_tmin=mean(tmin, na.rm=TRUE),
         mean_tmax=mean(tmax, na.rm=TRUE), 
         mean_rrday=mean(rrday, na.rm=TRUE)) %>% 
  select(year_id, region, year, mean_snow_depth,mean_tday, mean_tmin, mean_tmax, mean_rrday) %>% distinct() %>% 
  ungroup() %>% group_by(region, year) %>% 
  summarise(mean_mean_snow_depth=mean(mean_snow_depth), 
            mean_mean_tday=mean(mean_tday), 
            mean_mean_tmin=mean(mean_tmin),
            mean_mean_tmax=mean(mean_tmax), 
            mean_mean_rrday=mean(mean_rrday)) %>% 
  pivot_wider(names_from = year, values_from = c(mean_mean_snow_depth, mean_mean_tday, mean_mean_tmin, mean_mean_tmax, mean_mean_rrday)) 

# now, measurement of nb of freezing days

freezing_days_FMI <- rbind(kilpisjarvi_data %>% mutate(region="kilpisjarvi")%>% select(-TG_PT12H_min),
                           levi_data %>% mutate(region="Levi"),
                           lammi_data %>% mutate(region="Lammi"),
                           tvarminne_data %>% mutate(region="Tvarminne")) %>% 
  select(-snow) %>% 
  rename(year_id=year) %>% 
  mutate(year_id=as.numeric(year_id)) %>% 
  mutate(year=ifelse(year_id>=2005 & year_id<=2014, "t0", "t1")) %>% 
  distinct() %>% 
  mutate(freeze=ifelse(tday<0, 1,0)) %>% 
  ungroup() %>% group_by(year_id, region) %>% 
  mutate(nb_freezing_days=sum(freeze)) %>% 
  select(year, region, nb_freezing_days) %>% distinct() %>% 
  group_by(year, region) %>%
  mutate(mean_freeze=mean(nb_freezing_days)) %>% 
  select(year, region, mean_freeze) %>%  distinct() %>% 
  pivot_wider(names_from = year, values_from =mean_freeze) %>% 
  mutate(freeze_change=(t1-t0)/t0*100)

#### 2.3. CHELSA vs FMI ####

# graph mean temp chelsa vs fmi
# graph freezing days chelsa vs fmi

temp_coord_2005_2014 <- getChelsa(
  var = "tas",
  startdate = as.Date("2005-01-01"),
  enddate   = as.Date("2014-12-31"),
  coords = sites_to_map %>% 
    select(Longitude, Latitude) %>% 
    rename(lon=Longitude,
           lat=Latitude),
  dataset = "chelsa-daily"
)

temp_coord_2015_2019 <- getChelsa(
  var = "tas",
  startdate = as.Date("2015-01-01"),
  enddate   = as.Date("2019-12-31"),
  coords = sites_to_map %>% 
    select(Longitude, Latitude) %>% 
    rename(lon=Longitude,
           lat=Latitude),
  dataset = "chelsa-daily"
)

temp_coord_2021_2024 <- getChelsa(
  var = "tas",
  startdate = as.Date("2021-01-01"),
  enddate   = as.Date("2024-12-31"),
  coords = sites_to_map %>% 
    select(Longitude, Latitude) %>% 
    rename(lon=Longitude,
           lat=Latitude),
  dataset = "chelsa-daily"
)

temp_coord_2015_2024 <- rbind(temp_coord_2015_2019, temp_coord_2021_2024)

temp_coord_2005_2014i <- temp_coord_2005_2014
temp_coord_2015_2024i <- temp_coord_2015_2024

temp_coord_2005_2014i[temp_coord_2005_2014i<273.15] = 1
temp_coord_2005_2014i[temp_coord_2005_2014i>=273.15] = 0

temp_coord_2015_2024i[temp_coord_2015_2024i<273.15] = 1
temp_coord_2015_2024i[temp_coord_2015_2024i>=273.15] = 0

comparison_temp <- sites_to_map %>% 
  mutate(mean_T_t0_chelsa=NA,
         mean_T_t1_chelsa=NA,
         freeze_t0_chelsa=NA,
         freeze_t1_chelsa=NA) %>% 
  left_join(snow_all_means %>% select(region, mean_mean_tday_t0, mean_mean_tday_t1)) %>% 
  left_join(freezing_days_FMI %>% select(region, freeze_change))

for (i in 1:21){
  comparison_temp[i, 5]<-mean(temp_coord_2005_2014[,i+1])
  comparison_temp[i, 6]<-mean(temp_coord_2015_2024[,i+1])
  comparison_temp[i, 7]<-sum(temp_coord_2005_2014i[,i+1])/10
  comparison_temp[i, 8]<-sum(temp_coord_2015_2024i[,i+1])/9
}

comparison_temp_final <- comparison_temp %>% 
  mutate(T_change_chelsa=mean_T_t1_chelsa-mean_T_t0_chelsa,
         freeze_change_chelsa=(freeze_t1_chelsa-freeze_t0_chelsa)/freeze_t0_chelsa*100,
         T_change=mean_mean_tday_t1-mean_mean_tday_t0) %>% 
  select(region, site, T_change, T_change_chelsa, freeze_change, freeze_change_chelsa)

##### *Figure S2*####

FIGURES2a <- ggplot(comparison_temp_final, aes(x=freeze_change, y=freeze_change_chelsa, col=region, shape=region))+
  geom_point(size=3)+
  theme_bw()+
  scale_colour_manual(name = "Region",
                      labels = c("Kilpisjärvi","Lammi", "Levi",  "Tvärminne"),
                      values = c("blue", "pink", "lightblue", "darkred")) +   
  scale_shape_manual(name = "Region",
                     labels = c("Kilpisjärvi","Lammi", "Levi", "Tvärminne"),
                     values = c(16, 15, 17, 18))+
  ylab("Change in % of number of \nfreezing days (CHELSA)")+
  xlab("Change in % of number of \nfreezing days (FMI)")+
  theme(legend.position = "none")

FIGURES2b <- ggplot(comparison_temp_final, aes(x=T_change, y=T_change_chelsa, col=region, shape=region))+
  geom_point(size=3)+
  theme_bw()+
  scale_colour_manual(name = "Region",
                      labels = c("Kilpisjärvi","Lammi", "Levi",  "Tvärminne"),
                      values = c("blue", "pink", "lightblue", "darkred")) +   
  scale_shape_manual(name = "Region",
                     labels = c("Kilpisjärvi","Lammi", "Levi", "Tvärminne"),
                     values = c(16, 15, 17, 18))+
  ylab("Change in temperature \n(CHELSA)")+
  xlab("Change in temperature \n(FMI)")

FIGURES2 <- ggarrange(FIGURES2a, FIGURES2b,
                      labels=c("A", "B"), 
                      ncol=2,
                      widths = c(1.08, 1.5))

#ggsave(file="figures/Figure_S2.svg", plot=FIGURES2, height=8, width=20, units="cm")


#### 3. Species abundance change ####

#### 3.1. All individuals ####

# creation of site-by-species matrices

mat2015 <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  filter(year=="t0") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=tot_obs_sp) %>% 
  ungroup() %>% select(-site_name)

mat2015 <- as.matrix(mat2015)

mat2025 <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  filter(year=="t1") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=tot_obs_sp)  %>% 
  ungroup() %>% select(-site_name) 


mat2025 <- as.matrix(mat2025)

# one-tailed paired t-test:

a <-tpaired.krandtest(mat2015, mat2025, nperm=999)

# figure

taxon_dif <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  group_by(site_name, taxon,year) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  ungroup() %>% group_by(year,taxon) %>% 
  summarise(mean_obs_sp= mean(tot_obs_sp)) %>% 
  pivot_wider(names_from = year, values_from = mean_obs_sp) %>% 
  mutate(diff=t1-t0) %>% select(-t0, -t1)

taxon_dif_reg <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion")  %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site")) %>% 
  group_by(site, taxon,year, region) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  ungroup() %>% group_by(year,taxon,region) %>% 
  summarise(mean_obs_sp= mean(tot_obs_sp)) %>% 
  pivot_wider(names_from = year, values_from = mean_obs_sp) %>% 
  mutate(diff=t1-t0) %>% select(-t0, -t1)

appearance_disappearance <- data %>%
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  group_by(taxon,year, site_name) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs_sp > 0, 1, 0))  %>% 
  select(-tot_obs_sp)  %>% 
  pivot_wider(names_from=year, values_from = occurence, values_fill = 0)  %>% 
  mutate(occ_change=t1-t0) %>% 
  ungroup() %>% 
  mutate(appearance=ifelse(occ_change==1, 1, 0)) %>% 
  mutate(disappearance=ifelse(occ_change==-1, 1, 0)) %>% 
  group_by(taxon) %>% 
  summarise(appearance=sum(appearance),
            disappearance=sum(disappearance)) %>% 
  left_join(taxon_dif)

appearance_disappearance$taxon <- gsub("Aporrectodea caliginosa", "Acali", appearance_disappearance$taxon)
appearance_disappearance$taxon <- gsub("Aporrectodea rosea", "Arose", appearance_disappearance$taxon)
appearance_disappearance$taxon <- gsub("Octolasion lacteum", "Olact", appearance_disappearance$taxon)
appearance_disappearance$taxon <- gsub("Lumbricus rubellus", "Lrube", appearance_disappearance$taxon)
appearance_disappearance$taxon <- gsub("Lumbricus castaneus", "Lcast", appearance_disappearance$taxon)
appearance_disappearance$taxon <- gsub("Lumbricus terrestris", "Lterre", appearance_disappearance$taxon)
appearance_disappearance$taxon <- gsub("Dendrobaena octaedra", "Docta", appearance_disappearance$taxon)
appearance_disappearance$taxon <- gsub("Bimastos rubidus", "Brubi", appearance_disappearance$taxon)


appearance_disappearance_reg <- data %>%
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site")) %>% 
  group_by(taxon,year, region, site) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>%  
  mutate(occurence=ifelse(tot_obs_sp > 0, 1, 0))  %>% 
  select(-tot_obs_sp)  %>% 
  pivot_wider(names_from=year, values_from = occurence, values_fill = 0) %>% 
  mutate(occ_change=t1-t0) %>% 
  ungroup() %>% 
  mutate(appearance=ifelse(occ_change==1, 1, 0)) %>% 
  mutate(disappearance=ifelse(occ_change==-1, 1, 0))  %>% 
  group_by(taxon, region) %>% 
  mutate(sum_occ_both_year=t0+t1) %>% 
  filter(sum_occ_both_year>0) %>% 
  select(-sum_occ_both_year) %>% 
  mutate(appearance=sum(appearance),
         disappearance=sum(disappearance)) %>% 
  left_join(taxon_dif_reg) %>% 
  mutate(change_occu=appearance-disappearance) %>% 
  select(taxon, region, diff, change_occu) %>% 
  distinct()


fig3a <- ggplot(appearance_disappearance %>% mutate(change_occu=appearance-disappearance), aes(x=change_occu, y=diff))+
  geom_hline(yintercept=0, alpha=0.1)+
  geom_vline(xintercept=0, alpha=0.1)+
  geom_point(aes(col=taxon))+
  geom_text_repel(label=appearance_disappearance$taxon, aes(col=taxon))+
  theme_bw()+
  xlab("Change in occupancy")+
  ylab("Change in mean abundance per site")+
  theme(legend.position = "none")

appearance_disappearance_reg$taxon <- gsub("Aporrectodea caliginosa", "Acali", appearance_disappearance_reg$taxon)
appearance_disappearance_reg$taxon <- gsub("Aporrectodea rosea", "Arose", appearance_disappearance_reg$taxon)
appearance_disappearance_reg$taxon <- gsub("Octolasion lacteum", "Olact", appearance_disappearance_reg$taxon)
appearance_disappearance_reg$taxon <- gsub("Lumbricus rubellus", "Lrube", appearance_disappearance_reg$taxon)
appearance_disappearance_reg$taxon <- gsub("Lumbricus castaneus", "Lcast", appearance_disappearance_reg$taxon)
appearance_disappearance_reg$taxon <- gsub("Lumbricus terrestris", "Lterre", appearance_disappearance_reg$taxon)
appearance_disappearance_reg$taxon <- gsub("Dendrobaena octaedra", "Docta", appearance_disappearance_reg$taxon)
appearance_disappearance_reg$taxon <- gsub("Bimastos rubidus", "Brubi", appearance_disappearance_reg$taxon)

fig3b <- ggplot(appearance_disappearance_reg, aes(x=change_occu, y=diff))+
  geom_hline(yintercept=0, alpha=0.1)+
  geom_vline(xintercept=0, alpha=0.1)+
  geom_point(aes(col=taxon))+
  geom_text_repel(label=appearance_disappearance_reg$taxon, aes(col=taxon))+
  facet_wrap(~factor(region, c("kilpisjarvi","Levi",  "Lammi", "Tvarminne"), labels = c("Kilpisjärvi","Levi", "Lammi","Tvärminne")))+
  theme_bw()+
  xlab("Change in occupancy")+
  ylab("Change in mean abundance per site")+
  theme(legend.position = "none")

blank <- ggplot() + theme_void()

##### *Figure 3* ####

FIGURE3 <- ggarrange(ggarrange(blank,fig3a, blank,nrow=3,heights = c(0.5,3,0.5)),
                     fig3b,
                     ncol=2, 
                     widths = c(1, 1.2),
                     labels=c("A", "B"))

#ggsave(file="figures/Figure_3.svg", plot=FIGURE3, height=12, width=20, units="cm")



#### 3.2. Adults only ####

mat2015a <- data_adult_only %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  filter(year=="t0") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=tot_obs_sp) %>% 
  ungroup() %>% select(-site_name)

mat2015a <- as.matrix(mat2015a)

mat2025a <- data_adult_only %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  filter(year=="t1") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=tot_obs_sp)  %>% 
  ungroup() %>% select(-site_name)

mat2025a <- as.matrix(mat2025a)

b <-tpaired.krandtest(mat2015a, mat2025a, nperm=999)


taxon_dif_ad <- data_adult_only %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  group_by(site_name, taxon,year) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  ungroup() %>% group_by(year,taxon) %>% 
  summarise(mean_obs_sp= mean(tot_obs_sp)) %>% 
  pivot_wider(names_from = year, values_from = mean_obs_sp) %>% 
  mutate(diff=t1-t0) %>% select(-t0, -t1)

appearance_disappearance_ad <- data_adult_only %>%
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  group_by(taxon,year, site_name) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs_sp > 0, 1, 0))  %>% 
  select(-tot_obs_sp)  %>% 
  pivot_wider(names_from=year, values_from = occurence, values_fill = 0)  %>% 
  mutate(occ_change=t1-t0) %>% 
  ungroup() %>% 
  mutate(appearance=ifelse(occ_change==1, 1, 0)) %>% 
  mutate(disappearance=ifelse(occ_change==-1, 1, 0)) %>% 
  group_by(taxon) %>% 
  summarise(appearance=sum(appearance),
            disappearance=sum(disappearance)) %>% 
  left_join(taxon_dif_ad)

#### 3.3. Species occurence and abundance per site per period ####

#### *Table S2* ####

sp2015 <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  filter(year=="t0") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  group_by(taxon) %>% 
  summarise(mean=mean(tot_obs_sp),
            sd=sd(tot_obs_sp)) 

sp2025 <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  filter(year=="t1") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct()%>% 
  group_by(taxon) %>% 
  summarise(mean=mean(tot_obs_sp),
            sd=sd(tot_obs_sp)) 

sp2015a <- data_adult_only %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  filter(year=="t0") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  group_by(taxon) %>% 
  summarise(mean=mean(tot_obs_sp),
            sd=sd(tot_obs_sp)) 

sp2025a <- data_adult_only %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  filter(year=="t1") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct()%>% 
  group_by(taxon) %>% 
  summarise(mean=mean(tot_obs_sp),
            sd=sd(tot_obs_sp)) 

occ <- data %>%
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  group_by(taxon,year, site_name) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs_sp > 0, 1, 0))  %>% 
  group_by(year, taxon) %>% 
  summarise(occupancy = sum(occurence))

occa <- data_adult_only %>%
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  group_by(taxon,year, site_name) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs_sp > 0, 1, 0))  %>% 
  group_by(year, taxon) %>% 
  summarise(occupancy = sum(occurence))

#### 4. Total abundance change ####

#### 4.1. All individuals ####

##### *Table 2 - abundance* ####

ab_per_site <- data %>% 
  group_by(site_name, year) %>% 
  summarise(tot_ab = sum(tot_obs)) %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F) 

mean(ab_per_site$tot_ab[ab_per_site$year=="t0"])
mean(ab_per_site$tot_ab[ab_per_site$year=="t1"])

sd(ab_per_site$tot_ab[ab_per_site$year=="t0"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t0"]))
sd(ab_per_site$tot_ab[ab_per_site$year=="t1"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t1"]))

wilcox.test(ab_per_site$tot_ab[ab_per_site$year=="t0"],
            ab_per_site$tot_ab[ab_per_site$year=="t1"],
            paired=TRUE)

## per region

# Lammi

mean(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Lammi"])
mean(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Lammi"])

sd(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Lammi"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Lammi"]))
sd(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Lammi"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Lammi"]))

wilcox.test(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Lammi"],
            ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Lammi"],
            paired=TRUE)

# kilpisjarvi

mean(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="kilpisjarvi"])
mean(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="kilpisjarvi"])

sd(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="kilpisjarvi"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="kilpisjarvi"]))
sd(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="kilpisjarvi"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="kilpisjarvi"]))

wilcox.test(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="kilpisjarvi"],
            ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="kilpisjarvi"],
            paired=TRUE)

# Tvarminne

mean(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Tvarminne"])
mean(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Tvarminne"])

sd(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Tvarminne"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Tvarminne"]))
sd(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Tvarminne"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Tvarminne"]))

wilcox.test(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Tvarminne"],
            ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Tvarminne"],
            paired=TRUE)

# Levi

mean(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Levi"])
mean(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Levi"])

sd(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Levi"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Levi"]))
sd(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Levi"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Levi"]))

wilcox.test(ab_per_site$tot_ab[ab_per_site$year=="t0" & ab_per_site$region=="Levi"],
            ab_per_site$tot_ab[ab_per_site$year=="t1" & ab_per_site$region=="Levi"],
            paired=TRUE)

#### 4.2. Adults only ####

##### *Table S3 - abundance* ####

ab_per_site_ad <- data_adult_only %>% 
  group_by(site_name, year) %>% 
  summarise(tot_ab = sum(tot_obs)) %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F) 

mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"])
mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"])

sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"]))
sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"]))

wilcox.test(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"],
            ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"],
            paired=TRUE)


## per region

# Lammi

mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Lammi"])
mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Lammi"])

sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Lammi"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Lammi"]))
sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Lammi"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Lammi"]))

wilcox.test(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Lammi"],
            ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Lammi"],
            paired=TRUE)

# kilpisjarvi

mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="kilpisjarvi"])
mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="kilpisjarvi"])

sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="kilpisjarvi"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="kilpisjarvi"]))
sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="kilpisjarvi"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="kilpisjarvi"]))

wilcox.test(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="kilpisjarvi"],
            ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="kilpisjarvi"],
            paired=TRUE)

# Tvarminne

mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Tvarminne"])
mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Tvarminne"])

sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Tvarminne"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Tvarminne"]))
sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Tvarminne"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Tvarminne"]))

wilcox.test(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Tvarminne"],
            ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Tvarminne"],
            paired=TRUE)

# Levi

mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Levi"])
mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Levi"])

sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Levi"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Levi"]))
sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Levi"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Levi"]))

wilcox.test(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0" & ab_per_site_ad$region=="Levi"],
            ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1" & ab_per_site_ad$region=="Levi"],
            paired=TRUE)

#### 5. Richness change ####

#### 5.1. All individuals ####

##### *Table 2 - richness* ####

richness <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  group_by(site_name, taxon, year) %>% 
  summarise(tot_obs=sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs > 0, 1, 0))  %>% 
  ungroup() %>% group_by(site_name, year) %>% 
  summarise(richness=sum(occurence)) %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F) 

mean(richness$richness[richness$year=="t0"])
mean(richness$richness[richness$year=="t1"])

sd(richness$richness[richness$year=="t0"])/sqrt(length(richness$richness[richness$year=="t0"]))
sd(richness$richness[richness$year=="t1"])/sqrt(length(richness$richness[richness$year=="t1"]))

wilcox.test(richness$richness[richness$year=="t0"],
            richness$richness[richness$year=="t1"],
            paired=TRUE)

# per region

## Lammi

mean(richness$richness[richness$year=="t0" & richness$region=="Lammi"])
mean(richness$richness[richness$year=="t1" & richness$region=="Lammi"])

sd(richness$richness[richness$year=="t0" & richness$region=="Lammi"])/sqrt(length(richness$richness[richness$year=="t0" & richness$region=="Lammi"]))
sd(richness$richness[richness$year=="t1" & richness$region=="Lammi"])/sqrt(length(richness$richness[richness$year=="t1" & richness$region=="Lammi"]))

wilcox.test(richness$richness[richness$year=="t0" & richness$region=="Lammi"],
            richness$richness[richness$year=="t1" & richness$region=="Lammi"],
            paired=TRUE)

## Tvarminne

mean(richness$richness[richness$year=="t0" & richness$region=="Tvarminne"])
mean(richness$richness[richness$year=="t1" & richness$region=="Tvarminne"])

sd(richness$richness[richness$year=="t0" & richness$region=="Tvarminne"])/sqrt(length(richness$richness[richness$year=="t0" & richness$region=="Tvarminne"]))
sd(richness$richness[richness$year=="t1" & richness$region=="Tvarminne"])/sqrt(length(richness$richness[richness$year=="t1" & richness$region=="Tvarminne"]))

wilcox.test(richness$richness[richness$year=="t0" & richness$region=="Tvarminne"],
            richness$richness[richness$year=="t1" & richness$region=="Tvarminne"],
            paired=TRUE)

## Levi

mean(richness$richness[richness$year=="t0" & richness$region=="Levi"])
mean(richness$richness[richness$year=="t1" & richness$region=="Levi"])

sd(richness$richness[richness$year=="t0" & richness$region=="Levi"])/sqrt(length(richness$richness[richness$year=="t0" & richness$region=="Levi"]))
sd(richness$richness[richness$year=="t1" & richness$region=="Levi"])/sqrt(length(richness$richness[richness$year=="t1" & richness$region=="Levi"]))

wilcox.test(richness$richness[richness$year=="t0" & richness$region=="Levi"],
            richness$richness[richness$year=="t1" & richness$region=="Levi"],
            paired=TRUE)

## Kilpisjarvi

mean(richness$richness[richness$year=="t0" & richness$region=="kilpisjarvi"])
mean(richness$richness[richness$year=="t1" & richness$region=="kilpisjarvi"])

sd(richness$richness[richness$year=="t0" & richness$region=="kilpisjarvi"])/sqrt(length(richness$richness[richness$year=="t0" & richness$region=="kilpisjarvi"]))
sd(richness$richness[richness$year=="t1" & richness$region=="kilpisjarvi"])/sqrt(length(richness$richness[richness$year=="t1" & richness$region=="kilpisjarvi"]))

wilcox.test(richness$richness[richness$year=="t0" & richness$region=="kilpisjarvi"],
            richness$richness[richness$year=="t1" & richness$region=="kilpisjarvi"],
            paired=TRUE)

#### 5.2. Adults only ####

##### *Table S3 - richness* ####

richness_ad <- data_adult_only %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  group_by(site_name, taxon, year) %>% 
  summarise(tot_obs=sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs > 0, 1, 0))  %>% 
  ungroup() %>% group_by(site_name, year) %>% 
  summarise(richness_ad=sum(occurence)) %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F) 

mean(richness_ad$richness_ad[richness_ad$year=="t0"])
mean(richness_ad$richness_ad[richness_ad$year=="t1"])

sd(richness_ad$richness_ad[richness_ad$year=="t0"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t0"]))
sd(richness_ad$richness_ad[richness_ad$year=="t1"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t1"]))

wilcox.test(richness_ad$richness_ad[richness_ad$year=="t0"],
            richness_ad$richness_ad[richness_ad$year=="t1"],
            paired=TRUE)

# per region

## Lammi

mean(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Lammi"])

mean(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Lammi"])

sd(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Lammi"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Lammi"]))
sd(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Lammi"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Lammi"]))

wilcox.test(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Lammi"],
            richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Lammi"],
            paired=TRUE)

## Tvarminne

mean(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Tvarminne"])
mean(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Tvarminne"])

sd(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Tvarminne"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Tvarminne"]))
sd(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Tvarminne"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Tvarminne"]))

wilcox.test(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Tvarminne"],
            richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Tvarminne"],
            paired=TRUE)

## Levi

mean(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Levi"])
mean(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Levi"])

sd(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Levi"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Levi"]))
sd(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Levi"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Levi"]))

wilcox.test(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="Levi"],
            richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="Levi"],
            paired=TRUE)

## Kilpisjarvi

mean(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="kilpisjarvi"])
mean(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="kilpisjarvi"])

sd(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="kilpisjarvi"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="kilpisjarvi"]))
sd(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="kilpisjarvi"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="kilpisjarvi"]))

wilcox.test(richness_ad$richness_ad[richness_ad$year=="t0" & richness_ad$region=="kilpisjarvi"],
            richness_ad$richness_ad[richness_ad$year=="t1" & richness_ad$region=="kilpisjarvi"],
            paired=TRUE)

#### 6. Temporal beta diversity ####

# for TBI, I have to remove rows with only zero in both 2015 and 2025 for the function to work

#TBI(mat2015,mat2025) # rows 7 and 11 have only zeros in both matrices

TBI <- TBI(mat2015[-c(7,11),], mat2025[-c(7,11),], nperm=999)$BCD.mat %>% 
  select(-Change) %>% 
  rename(TBI_losses='B/(2A+B+C)', 
         TBI_gains='C/(2A+B+C)', 
         TBI='D=(B+C)/(2A+B+C)')

temp_beta <- cbind(unique(data %>% select(site_name))[-c(7,11),],TBI) %>% # i removed rows 7 and 11 in list of sites because these sites could not be used to calculate TBI
  pivot_longer(c(2:4)) %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F) %>% 
  group_by(region, name) %>% mutate(mean=mean(value, na.rm=TRUE)) 

##### *Figure 4* ####

FIGURE4 <- ggplot(temp_beta ,
                       aes(x=name, y=value))+
  geom_boxplot()+
  stat_summary(fun="mean", shape=8, col="black")+
  geom_point(data =temp_beta %>% 
               mutate(region = fct_relevel(region, "kilpisjarvi", "Levi", "Lammi", "Tvarminne")),
             mapping=aes(x=name, y=mean, col=region, shape=region), size=3)+
  scale_colour_manual(name = "Region",
                      labels = c("Lammi","Levi", "Tvärminne",  "Kilpisjärvi"),
                      values = c("pink", "lightblue", "darkred", "blue")) +   
  scale_shape_manual(name = "Region",
                     labels = c("Lammi","Levi", "Tvärminne", "Kilpisjärvi"),
                     values = c(15, 17, 18, 16))+
  
  ylab("")+
  xlab("")+
  theme_bw()

ggsave(file="figures/Figure_4.svg", plot=FIGURE4, height=8, width=12, units="cm")
