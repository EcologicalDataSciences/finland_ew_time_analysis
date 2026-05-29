# extracting climate data from finnish meteorological institute 

#### 1. Libraries ####

#remotes::install_github("rOpenGov/fmi2")

library(fmi2)
library(DT) #usefull ?
station_dataX <- fmi2::fmi_stations()

#### 2. Extraction snow data from FMI ####

# first we want data from Kilpisjärvi, Levi, Lammi and Tvärminne

# kilpisjarvi: fmisid=102016
# tvarminne: fmisid= 100953
#lammi: fmisid = 101154
# levi: fmisid = 101932

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

#### 3. Merging everything ####

snow_all <- rbind(kilpisjarvi_data %>% mutate(region="kilpisjarvi"),
                  levi_data %>% mutate(region="Levi"),
                  lammi_data %>% mutate(region="Lammi"),
                  tvarminne_data %>% mutate(region="Tvarminne")) %>% 
  rename(year_id=year) %>% 
  mutate(year_id=as.numeric(year_id)) %>% 
  mutate(year=ifelse(year_id>=2005 & year_id<=2014, "t0", "t1")) %>% 
  distinct() 
  
snow_all$snow_depth <- gsub("-1", "0", snow_all$snow_depth )

snow_all_means <- snow_all %>% 
  mutate(snow_depth=as.numeric(snow_depth)) %>% 
  group_by(region, year_id) %>% 
  mutate(mean_snow_depth=mean(snow_depth),
         sd_snow_depth=sd(snow_depth)) %>% 
  ungroup() %>% group_by(region, year) %>% 
  summarise(mean_mean_snow_depth=mean(mean_snow_depth), 
            mean_sd_snow_depth=mean(sd_snow_depth)) %>% 
  pivot_wider(names_from = year, values_from = c(mean_mean_snow_depth,mean_sd_snow_depth)) %>% 
  mutate(snow_depth_change=(mean_mean_snow_depth_t1-mean_mean_snow_depth_t0)/mean_mean_snow_depth_t0*100)%>% 
  mutate(variability_snow_depth_change=(mean_sd_snow_depth_t1-mean_sd_snow_depth_t0)/mean_sd_snow_depth_t0*100)

# snow_all_means can be used now in models
  