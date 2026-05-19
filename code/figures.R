
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