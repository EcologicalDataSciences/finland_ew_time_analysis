#  blowes


blowes <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Octolasion") %>% 
  separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F) %>% 
  group_by(region, taxon, year) %>% 
  summarise(tot_obs=sum(tot_obs)) %>% 
  select(region, taxon, year, tot_obs) %>% 
  mutate(occurence=ifelse(tot_obs > 0, 1, 0)) %>% 
  ungroup() %>% group_by(region, year) %>% 
  summarise(gamma=sum(occurence)) %>% 
  pivot_wider(names_from = year, values_from=gamma) %>% 
  mutate(gamma_change=t1-t0) %>% 
  select(region, gamma_change) %>% 
  left_join(richness_change %>%  separate_wider_delim(site_name, "_", names = c("region", "site"), cols_remove=F))

ggplot(blowes, aes(x=richness_change, y=gamma_change, col=region))+
  geom_point(size=3)+
  theme_minimal()+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth=1) +  # Ajouter la ligne pointillée pour x=0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth=1) +  # Ajouter la ligne pointillée pour y=0
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey", linewidth=1)   # Ajouter la ligne 

