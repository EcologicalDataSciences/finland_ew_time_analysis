### weather check ###

library(tidyverse)
library(fmi2) # for extraction of fmi data
library(DT) #usefull ? (extraction of fmi data?)
library(sf)
library(ggpubr) # for combining figures

# 0: Extraction

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
           "2024-01-01",
           "2025-01-01",
           "2026-01-01")

end <- c("2005-06-30",
         "2006-06-30",
         "2007-06-30",
         "2008-06-30",
         "2009-06-30",
         "2010-06-30",
         "2011-06-30",
         "2012-06-30",
         "2013-06-30",
         "2014-06-30",
         "2015-06-30",
         "2016-06-30",
         "2017-06-30",
         "2018-06-30",
         "2019-06-30",
         "2020-06-30",
         "2021-06-30",
         "2022-06-30",
         "2023-06-30",
         "2024-06-30",
         "2025-06-30",
         "2026-06-30")

#kilpisjarvi

# (i first extract data from the first day and then add to this objects all the other days)

kilpisjarvi_data_init <- obs_weather_daily(starttime = "2005-01-01",
                                           endtime = "2005-01-01",
                                           fmisid = 102016)

for (i in 1:22){
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

for (i in 1:22){
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

for (i in 1:22){
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

for (i in 1:22){
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
  mutate(year=ifelse(year_id>=2005 & year_id<=2014, "t0", 
                     ifelse(year_id=="2025", "2025", 
                            ifelse(year_id=="2026", "2026", "t1")))) %>% 
  distinct() 

snow_all$snow_depth <- gsub("-1", "0", snow_all$snow_depth)
snow_all$rrday <- gsub("-1", "0", snow_all$rrday)


# 1: how winter weather changed in 10 years

winter_means <- snow_all %>% 
  filter(month=="01"|month=="02"|month=="03") %>% 
  filter(year_id!="2025") %>% 
  filter(year_id!="2026") %>% 
  mutate(snow_depth=as.numeric(snow_depth)) %>% 
  mutate(rrday=as.numeric(rrday)) %>% 
  group_by(region, year_id) %>% 
  mutate(mean_snow_depth=mean(snow_depth, na.rm=TUE),
         mean_tday=mean(tday, na.rm=TRUE), 
         sum_rrday=sum(rrday, na.rm=TRUE)) %>% 
  select(year_id, region, year, mean_tday, sum_rrday) %>% distinct() %>% 
  ungroup() %>% group_by(region, year) %>% 
  summarise(mean_mean_tday=mean(mean_tday),
            mean_sum_rrday=mean(sum_rrday)) %>% 
  pivot_wider(names_from = year, values_from = c(mean_mean_tday, mean_sum_rrday)) 


# 2: how spring weather changed in 10 years

spring_means <- snow_all %>% 
  filter(month=="04"|month=="05"|month=="06") %>% 
  filter(year_id!="2025") %>% 
  filter(year_id!="2026") %>% 
  mutate(snow_depth=as.numeric(snow_depth)) %>% 
  mutate(rrday=as.numeric(rrday)) %>% 
  group_by(region, year_id) %>% 
  mutate(mean_tday=mean(tday, na.rm=TRUE), 
         sum_rrday=sum(rrday, na.rm=TRUE)) %>% 
  select(year_id, region, year, mean_tday, sum_rrday) %>% distinct() %>% 
  ungroup() %>% group_by(region, year) %>% 
  summarise(mean_mean_tday=mean(mean_tday),
            mean_sum_rrday=mean(sum_rrday)) %>% 
  pivot_wider(names_from = year, values_from = c(mean_mean_tday, mean_sum_rrday)) 


# 3: What was the year winter weather and how different was it from the norm

winter_sample <- snow_all %>% 
  filter(month=="01"|month=="02"|month=="03") %>% 
  filter(year_id=="2015"|year_id=="2025"|year_id=="2026") %>% 
  mutate(snow_depth=as.numeric(snow_depth)) %>% 
  mutate(rrday=as.numeric(rrday)) %>% 
  group_by(region, year_id) %>% 
  mutate(mean_tday=mean(tday, na.rm=TRUE), 
         sum_rrday=sum(rrday, na.rm=TRUE)) %>% 
  select(year_id, region, mean_tday, sum_rrday) %>% distinct() %>% 
  ungroup() %>% group_by(region, year_id) %>% 
  summarise(mean_mean_tday=mean(mean_tday),
            mean_sum_rrday=mean(sum_rrday)) %>% 
  pivot_wider(names_from = year_id, values_from = c(mean_mean_tday, mean_sum_rrday)) 

# conclu:
# in winter...
# 2015 and 2025 winter were hoter than typical, 2026 was colder than typical
# 2015 was quite rainy compared to typical, 2025 was somehow normal, 2026 was way drier

# 4: What was the year spring weather and how different was it from the norm

spring_sample <- snow_all %>% 
  filter(month=="04"|month=="05"|month=="06") %>% 
  filter(year_id=="2015"|year_id=="2025"|year_id=="2026") %>% 
  mutate(snow_depth=as.numeric(snow_depth)) %>% 
  mutate(rrday=as.numeric(rrday)) %>% 
  group_by(region, year_id) %>% 
  mutate(mean_tday=mean(tday, na.rm=TRUE), 
         sum_rrday=sum(rrday, na.rm=TRUE)) %>% 
  select(year_id, region, mean_tday, sum_rrday) %>% distinct() %>% 
  ungroup() %>% group_by(region, year_id) %>% 
  summarise(mean_mean_tday=mean(mean_tday),
            mean_sum_rrday=mean(sum_rrday)) %>% 
  pivot_wider(names_from = year_id, values_from = c(mean_mean_tday, mean_sum_rrday)) |> 
  mutate(across(region, ~factor(., levels=c("Kilpisjärvi","Levi","Lammi", "Tvärminne"))))

# in spring...
# 2015, 2025 and 2026 wer cold ! Maybe 2026 a little less cold
# 2015 was ok in precipitation, 2025 and 2026 were way drier

#### graph ####

snow_all_monthly_mean <- snow_all %>% 
  group_by(year_id, month, year, region) %>% 
  mutate(snow=mean(as.numeric(snow_depth),na.rm=TRUE),
         tday=mean(tday, na.rm=TRUE),
         rrday=sum(as.numeric(rrday), na.rm=TRUE)) %>% 
  select(year, month, tday, rrday, snow) %>% 
  distinct() %>% 
  mutate(month=as.numeric(month)) %>% 
  filter(month<=6) %>%
  mutate(region = str_replace_all(region, pattern = "kilpisjarvi", replacement = "Kilpisjärvi")) |> 
  mutate(region = str_replace_all(region, pattern = "Tvarminne", replacement = "Tvärminne")) |> 
  mutate(region=(region, ~factor(., levels=c("Kilpisjärvi","Levi","Lammi", "Tvärminne"))))

# temp

temp <- ggplot() +
  geom_line(
    data = snow_all_monthly_mean,
    aes(x = month, y = tday, group = year_id, lty="2005-2026"), size=1
  ) +
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2015),
    aes(x = month, y = tday, color = "2015"), size=1)+
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2025),
    aes(x = month, y = tday, color = "2025"), size=1)+  
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2026),
    aes(x = month, y = tday,     color = "2026"), size=1)+
  facet_grid(~factor(region, c("Kilpisjärvi", "Levi", "Lammi", "Tvärminne")))+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  theme_minimal()+
  ylab("Average monthly temperature")+
  theme(legend.title=element_blank())+
  xlab("Month")



# prec


prec <- ggplot() +
  geom_line(
    data = snow_all_monthly_mean,
    aes(x = month, y = rrday, group = year_id, lty="2005-2026"), size=1
  ) +
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2015),
    aes(x = month, y = rrday, color = "2015"), size=1)+
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2025),
    aes(x = month, y = rrday, color = "2025"), size=1)+  
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2026),
    aes(x = month, y = rrday,     color = "2026"), size=1)+
  facet_grid(~factor(region, c("Kilpisjärvi", "Levi", "Lammi", "Tvärminne")))+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  theme_minimal()+
  ylab("Monthly precipitations")+
  theme(legend.title=element_blank())+
  xlab("Month")


# snow


snow <- ggplot() +
  geom_line(
    data = snow_all_monthly_mean,
    aes(x = month, y = snow, group = year_id, lty="2005-2026"), size=1
  ) +
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2015),
    aes(x = month, y = snow, color = "2015"), size=1)+
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2025),
    aes(x = month, y = snow, color = "2025"), size=1)+  
  geom_line(
    data = snow_all_monthly_mean %>% filter(year_id == 2026),
    aes(x = month, y = snow,     color = "2026"), size=1)+
  facet_grid(~factor(region, c("Kilpisjärvi", "Levi", "Lammi", "Tvärminne")))+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  theme_minimal()+
  ylab("Average monthly snow depth")+
  theme(legend.title=element_blank())+
  xlab("Month")


# all
FIGURES4 <- ggarrange(temp, prec, snow, 
                      ncol=1)

ggsave(file="figures/Figure_S4.svg", plot=FIGURES4, height=24, width=24, units="cm")

## conclu winter et spring temp augmente, winter surtout
## precipitations d'hiver augmente pas mal, mais precipitation de printemps diminuent un peu 

## 2015 : hiver + chaud, printemps un peu + froid par rapport au normal (10 dernieres annees), annee pluvieuse hiver comme printemps... plutot propice aux vdt ?
## 2025 : hiver + chaud, printemps un peu + froid par rapport au normal (10 dernieres annees)
## 2026 : hiver plus froid !! au printemps + chaud; hiver moins pluvieu (car neige a la place?), et printemps plus pluvieux au nord mais plus sec au sud... notamment tvarminne


finland_small <- ne_countries(country="finland", scale="small", type="sovereignty")

small_finland <- ggplot()+
  geom_sf(data=finland_small, fill="white")+
  geom_point(data = sites_to_map,
             mapping = aes(x = Longitude, y = Latitude, shape=region))+
  scale_shape_manual(values = c(16, 17, 15, 18))+
  theme_void()+
  theme(legend.position="none")+
  xlim(20,31)

ggsave(file="figures/small_finland.svg", plot=small_finland, height=2, width=3, units="cm")
