sites_to_map <- sites_resampled2025 %>% 
  select(replicate) %>% 
  distinct() %>% 
  left_join(data2015_raw %>% rename(replicate=UID)) %>% 
  separate_wider_delim(replicate, "_", names = c("region", "site", "quadrat")) %>% 
  filter(quadrat=="6") %>% 
  select(region, site, Latitude, Longitude) %>% 
  distinct()

library(sf) # for manipulating vectors spatial objects
#install.packages("ggmagnify", repos = c("https://hughjonesd.r-universe.dev", "https://cloud.r-project.org"))
library(ggmagnify)

finland <- st_read("data/ne_50m_admin_0_map_units/ne_50m_admin_0_map_units.shp") %>% filter(SOVEREIGNT=="Finland"|SOVEREIGNT=="Sweden"|SOVEREIGNT=="Norway"|SOVEREIGNT=="Russia"|SOVEREIGNT=="Estonia") %>% select(NAME_LONG)

ggplot(finland) + 
  # first plotting Finland
  geom_sf(color = "grey40", fill="white", size = 0.1, show.legend = FALSE) +
  # then plotting sites
  geom_point(data = sites_to_map, mapping = aes(x = Longitude, y = Latitude))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  xlim(c(19, 45)) +
  ylim(c(60, 70))+ 
geom_magnify(
  from = c(24.5, 26, 67.5, 68),
  to = c(33, 44, 66, 70), 
  linewidth = 0.5, shadow = TRUE,
  colour = "red"
)


