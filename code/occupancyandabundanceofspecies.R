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

