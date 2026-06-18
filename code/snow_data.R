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
#  filter(variable=="snow"|variable=="tday") %>% 
  filter(!is.na(value)) %>% 
  separate_wider_delim(time, "-", names = c("year", "month", "day")) %>% 
  pivot_wider(names_from = variable, values_from = value) 
  
completeness_kilpis <- kilpisjarvi_data %>% 
  group_by(year) %>% 
  mutate(n=n()) %>% 
  select(year, n) %>% 
  distinct()

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

completeness_levi <- levi_data %>% 
  group_by(year) %>% 
  mutate(n=n()) %>% 
  select(year, n) %>% 
  distinct()

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

completeness_lammi <- lammi_data %>% 
  group_by(year) %>% 
  mutate(n=n()) %>% 
  select(year, n) %>% 
  distinct()

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

completeness_tva <- tvarminne_data %>% 
  group_by(year) %>% 
  mutate(n=n()) %>% 
  select(year, n) %>% 
  distinct()

#### 3. Merging everything ####

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
  
snow_days_FMI <- rbind(kilpisjarvi_data %>% mutate(region="kilpisjarvi")%>% select(-TG_PT12H_min),
                           levi_data %>% mutate(region="Levi"),
                           lammi_data %>% mutate(region="Lammi"),
                           tvarminne_data %>% mutate(region="Tvarminne")) %>% 
  select(-tday, -tmin, -tmax, -rrday) %>% 
  rename(year_id=year) %>% 
  mutate(year_id=as.numeric(year_id)) %>% 
  mutate(year=ifelse(year_id>=2005 & year_id<=2014, "t0", "t1")) %>% 
  distinct() %>% 
  mutate(snowday=ifelse(snow<=0, 0,1)) %>% 
  ungroup() %>% group_by(year_id, region) %>% 
  mutate(nb_snow_days=sum(snowday))  %>% 
  select(year, region, nb_snow_days) %>% distinct() %>% 
  group_by(year, region) %>%
  mutate(mean_snowdays=mean(nb_snow_days, na.rm=TRUE)) %>% 
  select(year, region, mean_snowdays) %>%  distinct() %>% 
  pivot_wider(names_from = year, values_from =mean_snowdays) %>% 
  mutate(snowdays_change=(t1-t0)/t0*100)

# snow_all_means can be used now in models
  