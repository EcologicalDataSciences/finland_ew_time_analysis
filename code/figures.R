
# Source - https://stackoverflow.com/a/79554408
# Posted by L Tyrone, modified by community. See post 'Timeline' for change history
# Retrieved 2026-05-18, License - CC BY-SA 4.0

#### 1. Libraries ####

library(sf)
library(dplyr)
library(rnaturalearth)
#devtools::install_github("ropensci/rnaturalearthhires") # for precisversion of map
library(ggplot2)
library(tidyverse)

#### 2. Mapping Finland and situation map ####

# Define custom orthographic PROJ4 string
# based on your example coordinates
ortho_prj_0_90 <- "+proj=ortho +lat_0=90 +lon_0=0"

#load finland
finland <- ne_countries(country="finland", scale="large", type="sovereignty")

# Load world sf
world_4326 <- ne_countries() |>
  select() |>
  st_make_valid()

# Project to custom projection
world_ortho_0_90 <- world_4326 |>
  st_transform(ortho_prj_0_90) |>
  st_make_valid()

finland_ortho <- finland |>
  st_transform(ortho_prj_0_90) |>
  st_make_valid()


# Generate custom graticules
graticules <- st_graticule(lon = seq(-180, 180, 30),
                           lat = seq(-90, 90, 15)) |>
  st_as_sf()

# Create 'water' polygon sf
poly_sf_0_90 <- st_sfc(st_point(c(0, 90)), crs = 4326) |>
  st_transform(ortho_prj_0_90) |>
  st_buffer(6377341)

# Full plot
ggplot() +
  geom_sf(data = poly_sf_0_90,
          fill = "#EBEBEB",
          colour ="#EBEBEB",
          linewidth = 0.25) +
  geom_sf(data = graticules,
          colour = "white",
          linewidth = 0.25) +
  geom_sf(data = world_ortho_0_90,
          colour = "grey40",
          fill = "white",
          linewidth = 0.1) +
  geom_sf(data = finland_ortho, 
          colour="red", 
          fill="red")+
  theme_void()

#### Finnish map

sites_to_map <- sites_resampled2025 %>% 
  select(replicate) %>% 
  distinct() %>% 
  left_join(data2015_raw %>% rename(replicate=UID)) %>% 
  separate_wider_delim(replicate, "_", names = c("region", "site", "quadrat")) %>% 
  filter(quadrat=="6") %>% 
  select(region, site, Latitude, Longitude) %>% 
  distinct()

world50 <- ne_countries( scale=50)


fig00 <- ggplot(world50) + 
  # first plotting Finland
  geom_sf(color = "grey40", fill="white", size = 0.1, show.legend = FALSE) +
  xlim(c(19, 32)) +
  ylim(c(60, 70))+ 
  # then plotting sites
  geom_point(data = sites_to_map %>% mutate(region = fct_relevel(region, 
                                                                 "kilpisjarvi", "Levi", "Lammi", 
                                                                 "Tvarminne")),
             mapping = aes(x = Longitude, y = Latitude, shape=region), size=2)+
  scale_shape_discrete(labels=c("Kilpisjärvi","Levi", "Lammi", "Tvärminne"),name="Region")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("")+
  ylab("")

ggsave(file="figures/fig00.svg", plot=fig00, height=8, width=8, units="cm")


#### 3. ab change compard with richness change ####

ab_vs_change <- ab_per_site %>% 
  left_join(richness) 

ggplot(ab_vs_change, aes(y=log(1+tot_ab), x=richness))+
  geom_point(aes(col=year))+
  theme_bw()

ab_change_vs_richness_change <- ab_vs_change %>% 
  pivot_wider(names_from = year, values_from = c(tot_ab, richness)) %>% 
  mutate(richness_change = richness_t1-richness_t0,
         ab_change = tot_ab_t1-tot_ab_t0)

ggplot(ab_change_vs_richness_change, aes(y=ab_change, x=richness_change))+
  geom_point(aes(col=region))+
  theme_bw()+
  geom_hline(yintercept=0, alpha=0.3)+
  geom_vline(xintercept=0, alpha=0.3)+
  xlab("Richness change")+
  ylab("Abundance change")
  

#### 4. Extracting climate data from CHELSA ####

# first test for snow cover, but with chelsa we can only have the mean for 1971-2010 and 2011-2040

library(Rchelsa)

getChelsa(var="scd", # snow cover days
          date="1981-2010",
          coords= data.frame(lon = 24.7, lat = 62.4),
          dataset = "chelsa-bioclim")

getChelsa(var="scd", # snow cover days
          date="2011-2040",
          coords= data.frame(lon = 24.7, lat = 62.4),
          dataset = "chelsa-bioclim",
          ssp = "ssp126",
          forcing="MPI-ESM1-2-HR")

# here I want to extract daily temperature data to know the nb of days with sub zero degree per year

# - 1 extraction of daily temperature from chelsa for each year between 2005 and 2024
# - 2 if less than zero celsius degree (273.15 K), cell=1, 0 otherwise
# - 3 sum of cell for each day per year : we end up with the nb of day with a mean of less than zero degre
# - 4 then we'll calculate the mean for 2005-2014 and 2015-2024
# - 5 we save this summary raster
# - and then we plot cropping finland, two maps or a map with "change in nb of subzero days"

library(Rchelsa)
library(sf)
library(terra)

# test for january 2005

temp_2005_test <- getChelsa(
  var = "tas",
  startdate = as.Date("2005-01-01"),
  enddate   = as.Date("2005-01-10"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2005_test[temp_2005_test<273.15] = 1
temp_2005_test[temp_2005_test>=273.15] = 0

subzerodays2005_test <- sum(temp_2005_test)

plot(subzerodays2005_test)

subzerodays2005_test

finland_reprojected = st_transform(finland, st_crs(subzerodays2005_test))
subzerodays2005_test_cropped = crop(subzerodays2005_test, finland_reprojected)
subzerodays2005_test_final = mask(subzerodays2005_test_cropped, finland_reprojected)
plot(subzerodays2005_test_final)

# we'll repeat that for all years !! and each day of the year

# 2005

temp_2005 <- getChelsa(
  var = "tas",
  startdate = as.Date("2005-01-01"),
  enddate   = as.Date("2005-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2005[temp_2005<273.15] = 1
temp_2005[temp_2005>=273.15] = 0

subzerodays2005 <- sum(temp_2005)

# 2006

temp_2006 <- getChelsa(
  var = "tas",
  startdate = as.Date("2006-01-01"),
  enddate   = as.Date("2006-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2006[temp_2006<273.15] = 1
temp_2006[temp_2006>=273.15] = 0

subzerodays2006 <- sum(temp_2006)

# 2007

temp_2007 <- getChelsa(
  var = "tas",
  startdate = as.Date("2007-01-01"),
  enddate   = as.Date("2007-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2007[temp_2007<273.15] = 1
temp_2007[temp_2007>=273.15] = 0

subzerodays2007 <- sum(temp_2007)

# 2008

temp_2008 <- getChelsa(
  var = "tas",
  startdate = as.Date("2008-01-01"),
  enddate   = as.Date("2008-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2008[temp_2008<273.15] = 1
temp_2008[temp_2008>=273.15] = 0

subzerodays2008 <- sum(temp_2008)

# 2009

temp_2009 <- getChelsa(
  var = "tas",
  startdate = as.Date("2009-01-01"),
  enddate   = as.Date("2009-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2009[temp_2009<273.15] = 1
temp_2009[temp_2009>=273.15] = 0

subzerodays2009 <- sum(temp_2009)

# 2010

temp_2010 <- getChelsa(
  var = "tas",
  startdate = as.Date("2010-01-01"),
  enddate   = as.Date("2010-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2010[temp_2010<273.15] = 1
temp_2010[temp_2010>=273.15] = 0

subzerodays2010 <- sum(temp_2010)

# 2011

temp_2011 <- getChelsa(
  var = "tas",
  startdate = as.Date("2011-01-01"),
  enddate   = as.Date("2011-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2011[temp_2011<273.15] = 1
temp_2011[temp_2011>=273.15] = 0

subzerodays2011 <- sum(temp_2011)

# 2012

temp_2012 <- getChelsa(
  var = "tas",
  startdate = as.Date("2012-01-01"),
  enddate   = as.Date("2012-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2012[temp_2012<273.15] = 1
temp_2012[temp_2012>=273.15] = 0

subzerodays2012 <- sum(temp_2012)

# 2013

temp_2013 <- getChelsa(
  var = "tas",
  startdate = as.Date("2013-01-01"),
  enddate   = as.Date("2013-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2013[temp_2013<273.15] = 1
temp_2013[temp_2013>=273.15] = 0

subzerodays2013 <- sum(temp_2013)

# 2014

temp_2014 <- getChelsa(
  var = "tas",
  startdate = as.Date("2014-01-01"),
  enddate   = as.Date("2014-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2014[temp_2014<273.15] = 1
temp_2014[temp_2014>=273.15] = 0

subzerodays2014 <- sum(temp_2014)

# 2015

temp_2015 <- getChelsa(
  var = "tas",
  startdate = as.Date("2015-01-01"),
  enddate   = as.Date("2015-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2015[temp_2015<273.15] = 1
temp_2015[temp_2015>=273.15] = 0

subzerodays2015 <- sum(temp_2015)

# 2016

temp_2016 <- getChelsa(
  var = "tas",
  startdate = as.Date("2016-01-01"),
  enddate   = as.Date("2016-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2016[temp_2016<273.15] = 1
temp_2016[temp_2016>=273.15] = 0

subzerodays2016 <- sum(temp_2016)

# 2017

temp_2017 <- getChelsa(
  var = "tas",
  startdate = as.Date("2017-01-01"),
  enddate   = as.Date("2017-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2017[temp_2017<273.15] = 1
temp_2017[temp_2017>=273.15] = 0

subzerodays2017 <- sum(temp_2017)

# 2018

temp_2018 <- getChelsa(
  var = "tas",
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2018-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2018[temp_2018<273.15] = 1
temp_2018[temp_2018>=273.15] = 0

subzerodays2018 <- sum(temp_2018)

# 2019

temp_2019 <- getChelsa(
  var = "tas",
  startdate = as.Date("2019-01-01"),
  enddate   = as.Date("2019-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2019[temp_2019<273.15] = 1
temp_2019[temp_2019>=273.15] = 0

subzerodays2019 <- sum(temp_2019)

# 2020

temp_2020 <- getChelsa(
  var = "tas",
  startdate = as.Date("2020-01-01"),
  enddate   = as.Date("2020-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2020[temp_2020<273.15] = 1
temp_2020[temp_2020>=273.15] = 0

subzerodays2020 <- sum(temp_2020)

# 2021

temp_2021 <- getChelsa(
  var = "tas",
  startdate = as.Date("2021-01-01"),
  enddate   = as.Date("2021-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2021[temp_2021<273.15] = 1
temp_2021[temp_2021>=273.15] = 0

subzerodays2021 <- sum(temp_2021)

# 2022

temp_2022 <- getChelsa(
  var = "tas",
  startdate = as.Date("2022-01-01"),
  enddate   = as.Date("2022-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2022[temp_2022<273.15] = 1
temp_2022[temp_2022>=273.15] = 0

subzerodays2022 <- sum(temp_2022)

# 2023

temp_2023 <- getChelsa(
  var = "tas",
  startdate = as.Date("2023-01-01"),
  enddate   = as.Date("2023-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2023[temp_2023<273.15] = 1
temp_2023[temp_2023>=273.15] = 0

subzerodays2023 <- sum(temp_2023)

# 2024

temp_2024 <- getChelsa(
  var = "tas",
  startdate = as.Date("2024-01-01"),
  enddate   = as.Date("2024-12-31"),
  extent = c(18, 33, 59, 71),
  dataset = "chelsa-daily"
)

temp_2024[temp_2024<273.15] = 1
temp_2024[temp_2024>=273.15] = 0

subzerodays2024 <- sum(temp_2024)

#

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

subzerodays_change <- mean_subzerodays2015_2024 - mean_subzerodays2005_2014


plot(subzerodays_change)
plot(mean_subzerodays2005_2014)
plot(subzerodays2015)

writeRaster(mean_subzerodays2005_2014, "derived_data/mean_subzerodays2005_2014.tif")
writeRaster(mean_subzerodays2015_2024, "derived_data/mean_subzerodays2015_2024.tif")
writeRaster(subzerodays_change, "derived_data/subzerodays_change.tif")

writeRaster(subzerodays2005, "derived_data/subzerodays2005.tif")
writeRaster(subzerodays2006, "derived_data/subzerodays2006.tif")
writeRaster(subzerodays2007, "derived_data/subzerodays2007.tif")
writeRaster(subzerodays2008, "derived_data/subzerodays2008.tif")
writeRaster(subzerodays2009, "derived_data/subzerodays2009.tif")
writeRaster(subzerodays2010, "derived_data/subzerodays2010.tif")
writeRaster(subzerodays2011, "derived_data/subzerodays2011.tif")
writeRaster(subzerodays2012, "derived_data/subzerodays2012.tif")
writeRaster(subzerodays2013, "derived_data/subzerodays2013.tif")
writeRaster(subzerodays2014, "derived_data/subzerodays2014.tif")
writeRaster(subzerodays2015, "derived_data/subzerodays2015.tif")
writeRaster(subzerodays2016, "derived_data/subzerodays2016.tif")
writeRaster(subzerodays2017, "derived_data/subzerodays2017.tif")
writeRaster(subzerodays2018, "derived_data/subzerodays2018.tif")
writeRaster(subzerodays2019, "derived_data/subzerodays2019.tif")
writeRaster(subzerodays2020, "derived_data/subzerodays2020.tif")
writeRaster(subzerodays2021, "derived_data/subzerodays2021.tif")
writeRaster(subzerodays2022, "derived_data/subzerodays2022.tif")
writeRaster(subzerodays2023, "derived_data/subzerodays2023.tif")
writeRaster(subzerodays2024, "derived_data/subzerodays2024.tif")

# crop raster to match Finland boundaries

finland_reprojected = st_transform(finland, st_crs(subzerodays_change))

mean_subzerodays2005_2014_cropped = crop(mean_subzerodays2005_2014, finland_reprojected)
mean_subzerodays2005_2014_final = mask(mean_subzerodays2005_2014_cropped, finland_reprojected)
plot(mean_subzerodays2005_2014_final)


mean_subzerodays2015_2024_cropped = crop(mean_subzerodays2015_2024, finland_reprojected)
mean_subzerodays2015_2024_final = mask(mean_subzerodays2015_2024_cropped, finland_reprojected)
plot(mean_subzerodays2015_2024_final)


subzerodays_change_cropped = crop(subzerodays_change, finland_reprojected)
subzerodays_change_final = mask(subzerodays_change_cropped, finland_reprojected)
plot(subzerodays_change_final)

#### 4.2. nice plots ####

library(tidyterra)
library(scales)


# i have to merge my raster and name them so I can plot them with facet and have the same legend with same breaks

for_plot <- c(mean_subzerodays2005_2014_final,mean_subzerodays2015_2024_final)
names(for_plot[[1]]) <- "2005-2014"
names(for_plot[[2]]) <- "2015-2024"


fig0 <- ggplot()+
  geom_spatraster(data=for_plot)+
  facet_wrap(~lyr)+
  scale_fill_grass_b(palette = "sepia", breaks = c(34,60,90,120,150,180,210,250), na.value=NA)+
  theme_bw()+
  labs(fill = "Mean yearly number of day\nwith less than 0°C")

ggsave(file="figures/fig0.svg", plot=fig0, height=24, width=24, units="cm")



fig1 <- ggplot()+
  geom_spatraster(data=subzerodays_change_final, aes(fill=sum))+
  scale_fill_gradient2( na.value=NA)+
  theme_bw()+
  labs(fill = "Change in mean yearly\nnumber of day with less\nthan 0°C between\n2005-2014 and 2015-2024")+
  geom_point(data = sites_to_map %>% mutate(region = fct_relevel(region, 
                                                          "kilpisjarvi", "Levi", "Lammi", 
                                                          "Tvarminne")),
             mapping = aes(x = Longitude, y = Latitude, shape=region), size=3)+
  scale_shape_discrete(labels=c("Kilpisjärvi","Levi", "Lammi", "Tvärminne"),name="Region")

ggsave(file="figures/fig1.svg", plot=fig1, height=12, width=12, units="cm")

##

mean_subzerodays2005_2014 <- rast("derived_data/mean_subzerodays2005_2014.tif")
mean_subzerodays2015_2024 <- rast("derived_data/mean_subzerodays2015_2024.tif")
subzerodays_change_perc <- (mean_subzerodays2015_2024 - mean_subzerodays2005_2014)/mean_subzerodays2005_2014*100

finland_reprojected = st_transform(finland, st_crs(mean_subzerodays2015_2024))

subzerodays_change_perc_cropped = crop(subzerodays_change_perc, finland_reprojected)
subzerodays_change_perc_final = mask(subzerodays_change_perc_cropped, finland_reprojected)
plot(subzerodays_change_perc_final)

fig2 <- ggplot()+
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
  scale_shape_discrete(labels=c("Kilpisjärvi","Levi", "Lammi", "Tvärminne"),name="Region")

ggsave(file="figures/fig2.svg", plot=fig2, height=12, width=12, units="cm")


fig2bis <- ggplot()+
  geom_spatraster(data=subzerodays_change_perc_final, aes(fill=sum))+
  scale_fill_grass_b(palette = "sepia")+
  theme_bw()+
  labs(fill = "Change (%) of mean yearly\nnumber of day with less\nthan 0°C between\n2005-2014 and 2015-2024")+
  geom_point(data = sites_to_map %>% mutate(region = fct_relevel(region, 
                                                                 "kilpisjarvi", "Levi", "Lammi", 
                                                                 "Tvarminne")),
             mapping = aes(x = Longitude, y = Latitude, shape=region), size=3)+
  scale_shape_discrete(labels=c("Kilpisjärvi","Levi", "Lammi", "Tvärminne"),name="Region")

ggsave(file="figures/fig2bis.svg", plot=fig2bis, height=12, width=12, units="cm")

sites_to_map$freeze_change <- extract(subzerodays_change_perc, sites_to_map[,c(4,3)])[[2]]

#### 5. Models ####

library(DHARMa)

#### 5.1 Freezing days ####

#### abundance

# normal?
ggplot(ab_change_vs_richness_change, aes(x = ab_change)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  theme_bw()+
  xlab("Change in abundance")

mod1 <- lm(ab_change ~ freeze_change, data = sites_to_map %>% left_join(ab_change_vs_richness_change))

simulationOutput1 <- simulateResiduals(fittedModel = mod1, plot = FALSE)
plot(simulationOutput1)

summary(mod1)

#### richness

#check wich ione as normal but only integer: I think normal is fine
ggplot(ab_change_vs_richness_change, aes(x = richness_change)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  theme_bw()+
  xlab("Change in richness")

mod2 <- lm(richness_change ~ freeze_change,  data = sites_to_map %>% left_join(ab_change_vs_richness_change))

simulationOutput2 <- simulateResiduals(fittedModel = mod2, plot = FALSE)
plot(simulationOutput2)

summary(mod2)

#### turnover

#binomial

ggplot(temp_beta, aes(x = temp_beta)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  theme_bw()+
  xlab("Turnover (Bray-Curtis)")

mod3 <- glm(temp_beta  ~ freeze_change, family="binomial", data = sites_to_map %>% left_join(temp_beta %>%   separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F)))

simulationOutput3 <- simulateResiduals(fittedModel = mod3, plot = FALSE)
plot(simulationOutput3)

summary(mod3)

#### plots

library(modelsummary)

modelsummary(list("Abundance change"=mod1,"Richness change"=mod2, "Turnover"=mod3))


abun <- ggplot(sites_to_map %>% left_join(ab_change_vs_richness_change), aes(x=freeze_change, y=ab_change))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in number of freezing days")+
  ylab("Change in abundance")+
  theme_bw()+
  ggtitle("R2 = 0.149; p-value = 0.0835")+
  theme(plot.title = element_text(size=10,hjust = 1))


rich <- ggplot(sites_to_map %>% left_join(ab_change_vs_richness_change), aes(x=freeze_change, y=richness_change))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in number of freezing days")+
  ylab("Change in richness")+
  theme_bw()+
  ggtitle("R2 = 0.000; p-value = 0.936")+
  theme(plot.title = element_text(size=10,hjust = 1))

beta <- ggplot( sites_to_map %>% left_join(temp_beta %>%   separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F)),
                aes(x=freeze_change, y=temp_beta))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in number of freezing days")+
  ylab("Turnover (Bray-Curtis)")+
  theme_bw() +
  ggtitle("R2 = 0.114; p-value = 0.158")+
  theme(plot.title = element_text(size=10,hjust = 1))

richabun <- ggarrange(abun, rich, nrow=1, ncol=2, labels=c("A", "B"))
richabunturn <- ggarrange(abun, rich, beta, nrow=1, ncol=3, labels=c("A", "B", "C"))

ggsave(file="figures/fig4.svg", plot=richabun, height=8, width=16, units="cm")
ggsave(file="figures/fig4bis.svg", plot=richabunturn, height=8, width=24, units="cm")

#### 5.2 Mean snow depth ####

#### abundance 

mod4 <- lm(ab_change ~ snow_depth_change, data = sites_to_map %>% left_join(ab_change_vs_richness_change) %>% left_join(snow_all_means))

simulationOutput4 <- simulateResiduals(fittedModel = mod4, plot = FALSE)
plot(simulationOutput4)

summary(mod4)

#### richness

mod5 <- lm(richness_change ~ snow_depth_change, data = sites_to_map %>% left_join(ab_change_vs_richness_change) %>% left_join(snow_all_means))

simulationOutput5 <- simulateResiduals(fittedModel = mod5, plot = FALSE)
plot(simulationOutput5)

summary(mod5)

#### turnover

mod6 <- glm(temp_beta ~ snow_depth_change, family="binomial", data = sites_to_map %>%  left_join(temp_beta %>%   separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F)) %>% left_join(snow_all_means))

simulationOutput6 <- simulateResiduals(fittedModel = mod6, plot = FALSE)
plot(simulationOutput6)

summary(mod6)

#### plots

abun2 <- ggplot(sites_to_map %>% left_join(ab_change_vs_richness_change%>% left_join(snow_all_means)), aes(x=snow_depth_change, y=ab_change))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in mean snow cover")+
  ylab("Change in abundance")+
  theme_bw()+
 # ggtitle("R2 = 0.149; p-value = 0.0835")+
  theme(plot.title = element_text(size=10,hjust = 1))


rich2 <- ggplot(sites_to_map %>% left_join(ab_change_vs_richness_change%>% left_join(snow_all_means)), aes(x=snow_depth_change, y=richness_change))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in mean snow cover")+
  ylab("Change in richness")+
  theme_bw()+
 # ggtitle("R2 = 0.000; p-value = 0.936")+
  theme(plot.title = element_text(size=10,hjust = 1))

beta2 <- ggplot( sites_to_map %>% left_join(temp_beta %>%   separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F))%>% left_join(snow_all_means),
                aes(x=snow_depth_change, y=temp_beta))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in mean snow cover")+
  ylab("Turnover (Bray-Curtis)")+
  theme_bw() +
 # ggtitle("R2 = 0.114; p-value = 0.158")+
  theme(plot.title = element_text(size=10,hjust = 1))

richabunturn2 <- ggarrange(abun2, rich2, beta2, nrow=1, ncol=3, labels=c("A", "B", "C"))

ggsave(file="figures/fig5bis.svg", plot=richabunturn2, height=8, width=24, units="cm")

#### 5.3 Snow depth variability ####

#### abundance 

mod7 <- lm(ab_change ~ variability_snow_depth_change, data = sites_to_map %>% left_join(ab_change_vs_richness_change) %>% left_join(snow_all_means))

simulationOutput7 <- simulateResiduals(fittedModel = mod7, plot = FALSE)
plot(simulationOutput7)

summary(mod7)

#### richness

mod8 <- lm(richness_change ~ variability_snow_depth_change, data = sites_to_map %>% left_join(ab_change_vs_richness_change) %>% left_join(snow_all_means))

simulationOutput8 <- simulateResiduals(fittedModel = mod8, plot = FALSE)
plot(simulationOutput8)

summary(mod8)

#### turnover

mod9 <- glm(temp_beta ~ variability_snow_depth_change, family="binomial", data = sites_to_map %>%  left_join(temp_beta %>%   separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F)) %>% left_join(snow_all_means))

simulationOutput9 <- simulateResiduals(fittedModel = mod9, plot = FALSE)
plot(simulationOutput9)

summary(mod9)


#### plots

abun3 <- ggplot(sites_to_map %>% left_join(ab_change_vs_richness_change%>% left_join(snow_all_means)), aes(x=variability_snow_depth_change, y=ab_change))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in snow cover variability")+
  ylab("Change in abundance")+
  theme_bw()+
  # ggtitle("R2 = 0.149; p-value = 0.0835")+
  theme(plot.title = element_text(size=10,hjust = 1))


rich3 <- ggplot(sites_to_map %>% left_join(ab_change_vs_richness_change%>% left_join(snow_all_means)), aes(x=variability_snow_depth_change, y=richness_change))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in snow cover variability")+
  ylab("Change in richness")+
  theme_bw()+
  # ggtitle("R2 = 0.000; p-value = 0.936")+
  theme(plot.title = element_text(size=10,hjust = 1))

beta3 <- ggplot( sites_to_map %>% left_join(temp_beta %>%   separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F))%>% left_join(snow_all_means),
                 aes(x=variability_snow_depth_change, y=temp_beta))+
  geom_point()+
  geom_smooth(method="lm", color="#CC9933", fill="#CC9933")+
  xlab("Change in snow cover variability")+
  ylab("Turnover (Bray-Curtis)")+
  theme_bw() +
  # ggtitle("R2 = 0.114; p-value = 0.158")+
  theme(plot.title = element_text(size=10,hjust = 1))

richabunturn3 <- ggarrange(abun3, rich3, beta3, nrow=1, ncol=3, labels=c("A", "B", "C"))

ggsave(file="figures/fig6bis.svg", plot=richabunturn3, height=8, width=24, units="cm")

####

ggplot(snow_all_means, aes(x=variability_snow_depth_change, y=snow_depth_change))+
  geom_point()

ggplot(sites_to_map %>% left_join(snow_all_means), aes(x=freeze_change, y=snow_depth_change))+
  geom_point()

ggplot(sites_to_map %>% left_join(snow_all_means), aes(x=freeze_change, y=variability_snow_depth_change))+
  geom_point()

#### 6. Distance between sites ####

mLevi <- terra::distance(sites_to_map[sites_to_map$region=="Levi", c("Longitude", "Latitude")], lonlat=TRUE)/1000
mLammi <- terra::distance(sites_to_map[sites_to_map$region=="Lammi", c("Longitude", "Latitude")], lonlat=TRUE)/1000
mTva <- terra::distance(sites_to_map[sites_to_map$region=="Tvarminne", c("Longitude", "Latitude")], lonlat=TRUE)/1000
mKilp <- terra::distance(sites_to_map[sites_to_map$region=="kilpisjarvi", c("Longitude", "Latitude")], lonlat=TRUE)/1000

mean(c(mLevi, mLammi, mTva, mKilp))
min(c(mLevi, mLammi, mTva, mKilp))
max(c(mLevi, mLammi, mTva, mKilp))
sd(c(mLevi, mLammi, mTva, mKilp))

