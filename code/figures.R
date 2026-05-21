
# Source - https://stackoverflow.com/a/79554408
# Posted by L Tyrone, modified by community. See post 'Timeline' for change history
# Retrieved 2026-05-18, License - CC BY-SA 4.0

library(sf)
library(dplyr)
library(rnaturalearth)
#devtools::install_github("ropensci/rnaturalearthhires") # for precisversion of map
library(ggplot2)
library(tidyverse)

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


ggplot(finland) + 
  # first plotting Finland
  geom_sf(color = "grey40", fill="white", size = 0.1, show.legend = FALSE) +
  xlim(c(19, 32)) +
  ylim(c(60, 70))+ 
  # then plotting sites
  geom_point(data = sites_to_map, mapping = aes(x = Longitude, y = Latitude, col=region))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


#### ab change compard with richness change

ab_vs_change <- ab_per_site %>% 
  left_join(richness) %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F) 

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
  

##### Extracting climate data

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

# here I want to extract daily temperature data to know the nb of days with sub zero degreeper year

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
                      subzerodays2020,
                      subzerodays2021,
                      subzerodays2022,
                      subzerodays2023,
                      subzerodays2024)

subzerodays_change <- mean_subzerodays2015_2024 - mean_subzerodays2005_2014
