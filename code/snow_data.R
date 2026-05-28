# extracting climate data from finnish meteorological institute 

# i am using this tutorial:
#https://tutorials.inbo.be/tutorials/spatial_wfs_services/

library(sf) # simple features packages for handling vector GIS data
library(httr) # generic webservice package
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(ows4R) # interface for OGC webservices

#URL of the service
wfs_bwk <- "http://opendata.fmi.fi/wfs"

url <- parse_url(wfs_bwk)
url$query <- list(service = "WFS",
                  #version = "2.0.0", # facultative, newest version by default
                  request = "describeStoredQueries"
)
request <- build_url(url)
request

https://opendata.fmi.fi/wfs?service=WFS&version=2.0.0&request=getFeature&storedquery_id=fmi::observations::weather::daily::multipointcoverage&bbox=18,33,59,71

url$query <- list(service = "WFS",
              #version = "2.0.0", # facultative, newest version by default
              request = "getFeature",
              storedquery_id="fmi::observations::weather::daily::multipointcoverage",
              bbox="18,33,59,71")

request <- build_url(url)
request


request <- "https://opendata.fmi.fi/wfs?service=WFS&version=2.0.0&request=getFeature&storedquery_id=fmi::observations::weather::daily::multipointcoverage&bbox=18,33,59,71"
GET(request)

library(helsinki)

a <- wfs_api(base.url="http://opendata.fmi.fi/wfs",
        queries=c("service"="WFS",
                  "version" = "2.0.0",
                  "request" = "getFeature",
                  "storedquery_id"="fmi::observations::weather::daily::multipointcoverage",
                  "bbox"="18,33,59,71"))


#remotes::install_github("rOpenGov/fmi2")

library(fmi2)
library(DT)
station_dataX <- fmi2::fmi_stations()

# first we want data from Kilpisjärvi, Levi, Lammi and Tvärminne

# kilpisjarvi= 102016
# tvarminne= 100953
#lammi = 101154
# levi = 101932

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

#kilpis

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
  filter(variable=="snow") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>% 
  rename(snow_depth=value) %>% 
  select(-variable)
  

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
  filter(variable=="snow") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>% 
  rename(snow_depth=value) %>% 
  select(-variable)

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
  filter(variable=="snow") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>% 
  rename(snow_depth=value) %>% 
  select(-variable)

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
  filter(variable=="snow") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>% 
  rename(snow_depth=value) %>% 
  select(-variable)

snow_all <- rbind(kilpisjarvi_data %>% mutate(region="kilpisjarvi"),
                  levi_data %>% mutate(region="Levi"),
                  lammi_data %>% mutate(region="Lammi"),
                  tvarminne_data %>% mutate(region="Tvarminne")) %>% 
  rename(year_id=year) %>% 
  mutate(year_id=as.numeric(year_id)) %>% 
  mutate(year=ifelse(year_id>=2005 & year_id<=2014, "t0", "t1"))

snow_all$snow_depth <- gsub("-1", "0", snow_all$snow_depth )

snow_all_means <- snow_all %>% 
  mutate(snow_depth=as.numeric(snow_depth)) %>% 
  group_by(region, year) %>% 
  summarise(mean_snow_depth=mean(snow_depth),
         sd_snow_depth=sd(snow_depth))
  

### extracting ALL

station_data <- as.numeric(fmi2::fmi_stations()$fmisid)

# i m going to have to select station with oap start 2005 and oap end 2024

start <- c("2005-01-01",
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


snow_data_init <- obs_weather_daily(starttime = start[1],
                                    endtime = end[1],
                                    fmisid = station_data[1])

for (i in 2:20){
  print(start[i])
  for (j in 2:length(station_data)){
    print(j)
    snow_data_i <- obs_weather_daily(starttime = start[i],
                                endtime = end[i],
                                fmisid = station_data[j])
    snow_data_init <- rbind(snow_data_init, snow_data_i)
  }
}
