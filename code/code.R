#### 1. Opening data ####

library(readxl)

data2015_raw <- read_excel("data/earthworm data_2015.xlsx")
data2025_raw <- read_excel("data/Finland_resampling2025_ID.xlsx")
sites_resampled2025 <- read_excel("data/Finland_resampling2025_Sites.xlsx")
  
# cleaning data 

library(tidyverse)

sites <- sites_resampled2025 %>% # here we want the list of sites resampled
  select(site_name, replicate) %>% 
  distinct() %>% 
  filter(!grepl("X",replicate)) %>% # removing replicate "X" that was just an unnecessary additional replicate we did on the field
  filter(!grepl("Tvarminne",site_name))

data2025cleaning <- sites %>% 
  left_join(data2025_raw %>% filter(development_stage!="undetermined")) %>%  # to remove pieces of ew that could not be ided and that are not "individual"
  select(site_name, replicate, taxon, nb_obs, development_stage)

data2015cleaning <- sites %>% 
  rename(siteID = site_name,
         UID=replicate) %>% 
  left_join(data2015_raw) %>% # we join the data from 2015
  pivot_longer(cols = 'juvenile D. octaedra':'adult L. terrestris') %>% # we change the format so one row is one observation per species
  select(siteID, UID, name, value) %>% 
  rename(site_name = siteID,
         replicate = UID, 
         taxon=name, 
         nb_obs=value) %>% 
  mutate(development_stage = ifelse(grepl("juvenile",taxon), "juvenile", "adult")) %>% 
  group_by(taxon) %>% mutate(tot=sum(nb_obs)) %>% filter(tot!=0) %>% select(-tot) %>% ungroup() # this row is to remove taxon with 0 observation as they could be there because they were found in 2015 data but they are not in any of the sites resampled

  

# now we need to have the same species name
# I decide to merge A tuberculata and A caliginosa into A caliginosa
# I also put accepted names

unique(data2025cleaning$taxon)
unique(data2015cleaning$taxon)

data2015cleaning$taxon <- gsub("juvenile D. octaedra", "Dendrobaena octaedra", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("juvenile Lumbricus", "Lumbricus", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("juvenile endogeic", "Undetermined", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult D. octaedra", "Dendrobaena octaedra", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult D. rubidus", "Bimastos rubidus", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult L. rubellus", "Lumbricus rubellus", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult A. tuberculata", "Aporrectodea caliginosa", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult A. rosea", "Aporrectodea rosea", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult A. caliginosa", "Aporrectodea caliginosa", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult O. tyrtaeum", "Octolasion lacteum", data2015cleaning$taxon)
data2015cleaning$taxon <- gsub("adult L. terrestris", "Lumbricus terrestris", data2015cleaning$taxon)

# I also change names in 2025 data, to keep only adults and subadults identified at species level

data2025cleaning$taxon <- gsub("Dendrobaena$", "Dendrobaena octaedra", data2025cleaning$taxon) # I put $ so all string that ends with "Dendrobaena" are changed in "undetermined". Then, "Dendrobaena" becomes "Undetermined" but "Dendrobaena octaedra" stays "Dendrobaedna octaedra" instead of "Undetermined octaedra"
data2025cleaning$taxon <- gsub("Lumbricus$", "Lumbricus", data2025cleaning$taxon) # same as before

# I also need to change the name of a NA observation in a site that is empty because only a part of individual has been found

data2025cleaning$taxon <- ifelse(is.na(data2025cleaning$taxon), "NA", data2025cleaning$taxon)
data2025cleaning$nb_obs <- ifelse(is.na(data2025cleaning$nb_obs), 0, data2025cleaning$nb_obs)
data2025cleaning$development_stage <- ifelse(is.na(data2025cleaning$development_stage), "NA", data2025cleaning$development_stage)

unique(data2025cleaning$taxon)
unique(data2015cleaning$taxon)

# i will also consider 2025 subadults as juveniles

data2025cleaning$development_stage <- gsub("subadult", "juvenile", data2025cleaning$development_stage)

# now I split the database into adult only and all (adult + juvenile), and give it the same format (so a row per taxa observed in the whole dataset for each site, like how data2015 is oranised)

data2025 <- data2025cleaning %>% 
  group_by(site_name,replicate, taxon) %>% 
  summarise(tot_obs=sum(nb_obs)) %>%  
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0)%>% 
  select(-'NA') %>% 
  pivot_longer('Dendrobaena octaedra':'Aporrectodea rosea') %>% 
  mutate(year="t1") %>% 
  rename(taxon=name, 
         tot_obs=value)
  
data2025ad <- sites %>% 
  left_join(data2025cleaning %>% filter(development_stage=="adult")) %>% 
  select(-development_stage) %>% 
  group_by(site_name,replicate, taxon) %>% 
  summarise(tot_obs=sum(nb_obs)) %>% 
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0) %>% 
  select(-'NA') %>% 
  pivot_longer('Dendrobaena octaedra':'Aporrectodea rosea') %>% 
  mutate(year="t1")%>% 
  rename(taxon=name, 
         tot_obs=value)

data2015 <- data2015cleaning %>% 
  group_by(site_name,replicate, taxon) %>% 
  summarise(tot_obs=sum(nb_obs)) %>% 
  mutate(year="t0")
  
data2015ad <- sites %>% 
  left_join(data2015cleaning %>% filter(development_stage=="adult")) %>% 
  select(-development_stage) %>% 
  group_by(site_name,replicate, taxon) %>% 
  summarise(tot_obs=sum(nb_obs)) %>% 
  pivot_wider(names_from = taxon, values_from = tot_obs, values_fill = 0) %>% 
  pivot_longer('Aporrectodea caliginosa':'Octolasion lacteum') %>% 
  mutate(year="t0")%>% 
  rename(taxon=name, 
         tot_obs=value)

data <- rbind(data2015, data2025) %>% ungroup()
data_adult_only <- rbind(data2015ad, data2025ad) %>% ungroup()

#rm(data2015_raw, data2015cleaning, data2015, data2015ad,
#   data2025_raw, data2025cleaning, data2025, data2025ad,)

#### 2. Analyses ####

#### 2.1 Total abundance change ####

ab_per_site <- data %>% 
  group_by(site_name, year) %>% 
  summarise(tot_ab = sum(tot_obs))

mean(ab_per_site$tot_ab[ab_per_site$year=="t0"])
mean(ab_per_site$tot_ab[ab_per_site$year=="t1"])

sd(ab_per_site$tot_ab[ab_per_site$year=="t0"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t0"]))
sd(ab_per_site$tot_ab[ab_per_site$year=="t1"])/sqrt(length(ab_per_site$tot_ab[ab_per_site$year=="t1"]))

wilcox.test(ab_per_site$tot_ab[ab_per_site$year=="t0"],
            ab_per_site$tot_ab[ab_per_site$year=="t1"],
            paired=TRUE)

t.test(ab_per_site$tot_ab[ab_per_site$year=="t0"],
            ab_per_site$tot_ab[ab_per_site$year=="t1"],
            paired=TRUE)

ggplot(ab_per_site, aes(x=year, y=tot_ab))+
  geom_boxplot(width=0.2, col="brown", fill=NA)+
  geom_jitter(width=0.1, alpha=0.5)+
  theme_bw()

#### 2.2 Total abundance change - adult only ####

ab_per_site_ad <- data_adult_only %>% 
  group_by(site_name, year) %>% 
  summarise(tot_ab = sum(tot_obs))

mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"])
mean(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"])

sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"]))
sd(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"])/sqrt(length(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"]))

wilcox.test(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"],
            ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"],
            paired=TRUE)

t.test(ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t0"],
       ab_per_site_ad$tot_ab[ab_per_site_ad$year=="t1"],
       paired=TRUE)

ggplot(ab_per_site_ad, aes(x=year, y=tot_ab))+
  geom_boxplot(width=0.2, col="brown", fill=NA)+
  geom_jitter(width=0.1, alpha=0.5)+
  theme_bw()

#### 2.3 Species abundance change ####

library(adespatial)

# creation of site-by-species matrixes

mat2015 <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(year=="t0") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=tot_obs_sp) %>% 
  ungroup() %>% select(-site_name)

mat2015 <- as.matrix(mat2015)

mat2025 <- data %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(year=="t1") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=tot_obs_sp)  %>% 
  ungroup() %>% select(-site_name) %>% 
  mutate('Octolasion lacteum'=0) # I add a species with no obs for O lacteum because we need the same species for comparison
  

mat2025 <- as.matrix(mat2025)

a <-tpaired.krandtest(mat2015, mat2025, nperm=99)
a
p.adjust(a$t.test$p.param, method="holm") # p values corrected for multiple testing

# figure

taxon_dif <- data %>% 
  group_by(taxon,year) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  pivot_wider(names_from = year, values_from = tot_obs_sp, values_fill = 0) %>% 
  summarise(diff=(t1-t0)/(t1+t0))

plot1 <- ggplot(taxon_dif,
  aes(x=diff, y=reorder(taxon, diff), fill=sign(diff)))+
    geom_col()+
  theme(legend.position = "none", 
        axis.title.y=element_blank()) 

appearance_disappearance <- data %>%
  group_by(taxon,year, site_name) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs_sp > 0, 1, 0))  %>% 
  select(-tot_obs_sp)  %>% 
  pivot_wider(names_from=year, values_from = occurence, values_fill = 0)  %>% 
  mutate(occ_change=t1-t0) %>% 
  ungroup() %>% 
  mutate(appearance=ifelse(occ_change==1, 1, 0)) %>% 
  mutate(disappearance=ifelse(occ_change==-1, 1, 0)) %>% 
  group_by(taxon) %>% 
  summarise(appearance=sum(appearance),
            disappearance=sum(disappearance)) %>% 
  left_join(taxon_dif)

plot2 <- ggplot(appearance_disappearance)+
  geom_col(aes(x=appearance, y=reorder(taxon, diff)), fill="green")+
  geom_col(aes(x=-disappearance, y=taxon), fill="red")+
  theme(axis.title.y=element_blank()) 
  
library(ggpubr)

ggarrange(plot1, plot2, 
          ncol=2)

#### 2.4 Species abundance change - adult only ####

mat2015a <- data_adult_only %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(year=="t0") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=tot_obs_sp) %>% 
  ungroup() %>% select(-site_name)

mat2015a <- as.matrix(mat2015a)

mat2025a <- data_adult_only %>% 
  filter(taxon!="Undetermined") %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(year=="t1") %>% 
  group_by(site_name, taxon) %>% 
  summarise(tot_obs_sp = sum(tot_obs)) %>% 
  select(site_name, taxon, tot_obs_sp) %>% 
  distinct() %>% 
  pivot_wider(names_from = taxon, values_from=tot_obs_sp)  %>% 
  ungroup() %>% select(-site_name) %>% 
  mutate('Octolasion lacteum'=0) # I add a species with no obs for O lacteum because we need the same species for comparison


mat2025a <- as.matrix(mat2025a)

b <-tpaired.krandtest(mat2015a, mat2025a, nperm=99)
b
p.adjust(b$t.test$p.param, method="holm") # p values corrected for multiple testing

# figure

taxon_dif_ad <- data_adult_only %>% 
  group_by(taxon,year) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  pivot_wider(names_from = year, values_from = tot_obs_sp, values_fill = 0) %>% 
  summarise(diff=(t1-t0)/(t1+t0))

plot1ad <- ggplot(taxon_dif_ad,
                aes(x=diff, y=reorder(taxon, diff), fill=sign(diff)))+
  geom_col()+
  theme(legend.position = "none", 
        axis.title.y=element_blank()) 

appearance_disappearance_ad <- data_adult_only %>%
  group_by(taxon,year, site_name) %>% 
  summarise(tot_obs_sp= sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs_sp > 0, 1, 0))  %>% 
  select(-tot_obs_sp)  %>% 
  pivot_wider(names_from=year, values_from = occurence, values_fill = 0)  %>% 
  mutate(occ_change=t1-t0) %>% 
  ungroup() %>% 
  mutate(appearance=ifelse(occ_change==1, 1, 0)) %>% 
  mutate(disappearance=ifelse(occ_change==-1, 1, 0)) %>% 
  group_by(taxon) %>% 
  summarise(appearance=sum(appearance),
            disappearance=sum(disappearance)) %>% 
  left_join(taxon_dif_ad)

plot2ad <- ggplot(appearance_disappearance_ad)+
  geom_col(aes(x=appearance, y=reorder(taxon, diff)), fill="green")+
  geom_col(aes(x=-disappearance, y=taxon), fill="red")+
  theme(axis.title.y=element_blank()) 

ggarrange(plot1ad, plot2ad, 
          ncol=2)

require("ggrepel")

ggplot(appearance_disappearance_ad %>% mutate(change_occu=appearance-disappearance), aes(x=change_occu, y=diff))+
  geom_point()+
  geom_text_repel(label=appearance_disappearance_ad$taxon)+
  xlim(-2.5,2.5)+ylim(-1.5,1.5)+
  theme_bw()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

#### 2.5 Richness change ####

test <-data %>% group_by(site_name, taxon, year) %>% summarise(tot_obs=sum(tot_obs)) %>% filter(grepl("Lumbricus", taxon)) %>% pivot_wider(names_from=taxon, values_from=tot_obs)

# a few sites with only Lumbricus sp as Lumbricus, I'll try to remove them

richness <- data %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Undetermined") %>%
  group_by(site_name, taxon, year) %>% 
  summarise(tot_obs=sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs > 0, 1, 0))  %>% 
  ungroup() %>% group_by(site_name, year) %>% 
  summarise(richness=sum(occurence))
  
mean(richness$richness[richness$year=="t0"])
mean(richness$richness[richness$year=="t1"])

sd(richness$richness[richness$year=="t0"])/sqrt(length(richness$richness[richness$year=="t0"]))
sd(richness$richness[richness$year=="t1"])/sqrt(length(richness$richness[richness$year=="t1"]))

wilcox.test(richness$richness[richness$year=="t0"],
            richness$richness[richness$year=="t1"],
            paired=TRUE)

t.test(richness$richness[richness$year=="t0"],
       richness$richness[richness$year=="t1"],
       paired=TRUE)

ggplot(richness, aes(x=year, y=richness))+
  geom_boxplot(width=0.2, col="brown", fill=NA)+
  geom_jitter(width=0.1, alpha=0.5)+
  theme_bw()

#### 2.5 Richness change - adult only ####

richness_ad <- data_adult_only %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Undetermined") %>%
  group_by(site_name, taxon, year) %>% 
  summarise(tot_obs=sum(tot_obs)) %>% 
  mutate(occurence=ifelse(tot_obs > 0, 1, 0))  %>% 
  ungroup() %>% group_by(site_name, year) %>% 
  summarise(richness_ad=sum(occurence))

mean(richness_ad$richness_ad[richness_ad$year=="t0"])
mean(richness_ad$richness_ad[richness_ad$year=="t1"])

sd(richness_ad$richness_ad[richness_ad$year=="t0"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t0"]))
sd(richness_ad$richness_ad[richness_ad$year=="t1"])/sqrt(length(richness_ad$richness_ad[richness_ad$year=="t1"]))

wilcox.test(richness_ad$richness_ad[richness_ad$year=="t0"],
            richness_ad$richness_ad[richness_ad$year=="t1"],
            paired=TRUE)

t.test(richness_ad$richness_ad[richness_ad$year=="t0"],
       richness_ad$richness_ad[richness_ad$year=="t1"],
       paired=TRUE)

ggplot(richness_ad, aes(x=year, y=richness_ad))+
  geom_boxplot(width=0.2, col="brown", fill=NA)+
  geom_jitter(width=0.1, alpha=0.5)+
  theme_bw()

#### 2.6. Beta (and Gamma) diversity ####

##### 2.6.1 Additive Whittaker partitioning ####

betadiv <- data %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Undetermined") %>%
  separate_wider_delim(site_name, "_", names = c("region", NA), cols_remove = FALSE) %>% 
  filter(tot_obs>0) %>% 
  group_by(year) %>% 
  mutate(gamma=length(unique(taxon))) %>% 
  group_by(year, region) %>% 
  mutate(alpha_3=length(unique(taxon))) %>% 
  group_by(year, region, site_name) %>% 
  mutate(alpha_2=length(unique(taxon))) %>% 
  group_by(year, region, site_name, replicate) %>% 
  mutate(alpha_1=length(unique(taxon))) %>% 
  mutate(beta_3=gamma-alpha_3, 
         beta_2=alpha_3-alpha_2,
         beta_1=alpha_2-alpha_1) %>% 
  right_join(data %>% separate_wider_delim(site_name, "_", names = c("region", NA), cols_remove = FALSE) %>% select(region, site_name, replicate, year)) %>% 
  select(-taxon) %>% distinct() %>% 
  replace(is.na(.), 0) %>% 
  mutate(gamma=ifelse(year=="t0", 7, 6)) #i have to think about  a better way 
  
ggplot(betadiv %>% 
         select(alpha_1, alpha_2, alpha_3, gamma) %>%  
         pivot_longer(col=c(alpha_1, alpha_2, alpha_3, gamma)),
       aes(y=value, x=name, fill=year))+
  geom_bar(stat = "summary", fun = "mean", position=position_dodge()) +
  theme_bw()

ggplot(betadiv %>% 
         select(beta_1, beta_2, beta_3) %>%  
         pivot_longer(col=c(beta_1, beta_2, beta_3)),
       aes(y=value, x=name, fill=year))+
  geom_bar(stat = "summary", fun = "mean", position=position_dodge()) 

##### 2.6.2 Multiplicative Whittaker partitioning ####

betadiv2 <- data %>% 
  filter(taxon!="Lumbricus") %>% 
  filter(taxon!="Undetermined") %>%
  separate_wider_delim(site_name, "_", names = c("region", NA), cols_remove = FALSE) %>% 
  filter(tot_obs>0) %>% 
  group_by(year) %>% 
  mutate(gamma=length(unique(taxon))) %>% 
  group_by(year, region) %>% 
  mutate(alpha_3=length(unique(taxon))) %>% 
  group_by(year, region, site_name) %>% 
  mutate(alpha_2=length(unique(taxon))) %>% 
  group_by(year, region, site_name, replicate) %>% 
  mutate(alpha_1=length(unique(taxon))) %>% 
  mutate(beta_3=gamma/alpha_3, 
         beta_2=alpha_3/alpha_2,
         beta_1=alpha_2/alpha_1) %>% 
  right_join(data %>% separate_wider_delim(site_name, "_", names = c("region", NA), cols_remove = FALSE) %>% select(region, site_name, replicate, year)) %>% 
  select(-taxon) %>% distinct() %>% 
  replace(is.na(.), 0) %>% 
  mutate(gamma=ifelse(year=="t0", 7, 6))

ggplot(betadiv2 %>% 
         select(beta_1, beta_2, beta_3) %>%  
         pivot_longer(col=c(beta_1, beta_2, beta_3)),
       aes(y=value, x=name, fill=year))+
  geom_bar(stat = "summary", fun = "mean", position=position_dodge()) 

##### 2.6.3 Sorensen #####

library(betapart)

occ2015<-ifelse(mat2015>0,1,0)
occ2025<-ifelse(mat2025>0,1,0)

beta.multi(occ2015, index.family = "sorensen")
beta.multi(occ2025, index.family = "sorensen")

beta.multi.abund(mat2015, index.family = "bray")
beta.multi.abund(mat2025, index.family = "bray")

#### 2.7 Temporal beta ####

library(betapart)

# abundance based

temp_beta <- sites %>% select(site_name) %>% distinct() %>% 
  mutate(temp_beta=NA) %>% 
  mutate(temp_beta_sor=NA) %>% 
  mutate(temp_beta_sim=NA) %>% 
  mutate(temp_beta_sne=NA)

for (i in 1:nrow(temp_beta)){
  mat=rbind(mat2015[i,], mat2025[i,])
  temp_beta_i <- beta.pair.abund(mat)
  temp_beta[i, 2] <- temp_beta_i$beta.bray
}

# occurence based

occ2015<-ifelse(mat2015>0,1,0)
occ2025<-ifelse(mat2025>0,1,0)

for (i in 1:nrow(temp_beta)){
  mat=rbind(occ2015[i,], occ2025[i,])
  temp_beta_i <- beta.pair(mat, index.family = "sorensen")
  temp_beta[i, 3] <- temp_beta_i$beta.sor
  temp_beta[i, 4] <- temp_beta_i$beta.sim
  temp_beta[i, 5] <- temp_beta_i$beta.sne
}

#### 2.8 Climate change ####

library(Rchelsa) # Chelsa data extraction
library(terra) # working with raster data

# Selection of coordinates of interest:

coords <-  sites_resampled2025 %>% 
  select(replicate) %>% 
  distinct() %>% 
  left_join(data2015_raw %>% rename(replicate=UID)) %>% 
  rename(quadrat='Worm Quadrat', 
         site_name=siteID) %>% 
  filter(quadrat=="6") %>% 
  select(site_name, Longitude, Latitude)

# selection of variables of interest

bioclim <- c("bio01", "bio04", "bio12", "scd")

climate_data <- coords

for (i in bioclim){
  print(i) # to see where we are in the loop
  year_bioclimX <- getChelsa(i, coords = coords[,2:3], 
                             date = "1981-2010",
                             dataset = "chelsa-bioclim")
  year_bioclimX_t <- data.frame(t(year_bioclimX))
  rownames(year_bioclimX_t) <- NULL
  year_bioclimX_t <- data.frame(year_bioclimX_t[-1,])
  colnames(year_bioclimX_t) <- i
  year_bioclimX_t[,i] <- as.numeric(year_bioclimX_t[,i])
  climate_data <- cbind(climate_data, year_bioclimX_t)
}

richness_change <- richness %>% 
  pivot_wider(names_from = year, values_from=richness) %>% 
  mutate(richness_change=t1-t0) %>% 
  select(site_name, richness_change)

ab_change <- ab_per_site %>% 
  pivot_wider(names_from = year, values_from=tot_ab) %>% 
  mutate(ab_change=t1-t0) %>% 
  select(site_name, ab_change)


beta_climate <- climate_data %>% 
  left_join(temp_beta) %>% 
  left_join(richness_change) %>% 
  left_join(ab_change)

ggplot(beta_climate, aes(x=scd, y=temp_beta_sor))+
  geom_point()+ 
  geom_smooth(method='lm', formula= y~x)

ggplot(beta_climate, aes(x=bio01, y=richness_change))+
  geom_point()+ 
  geom_smooth(method='lm', formula= y~x)

ggplot(beta_climate, aes(x=bio01, y=ab_change))+
  geom_point()+ 
  geom_smooth(method='lm', formula= y~x)

