
# Source - https://stackoverflow.com/a/79554408
# Posted by L Tyrone, modified by community. See post 'Timeline' for change history
# Retrieved 2026-05-18, License - CC BY-SA 4.0

library(sf)
library(dplyr)
library(rnaturalearth)
library(ggplot2)

# Define custom orthographic PROJ4 string
# based on your example coordinates
ortho_prj_0_90 <- "+proj=ortho +lat_0=90 +lon_0=0"

#load finland
finland <- ne_countries(country="finland", scale=50)

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


ggplot(world50) + 
  # first plotting Finland
  geom_sf(color = "grey40", fill="white", size = 0.1, show.legend = FALSE) +
  xlim(c(19, 35)) +
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
  
  