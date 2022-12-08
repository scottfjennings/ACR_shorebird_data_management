


# test data for checking that splitting code produces desired results

# requires psite_sbirds from shorebirds_start.R and sbird_lumpies from shorebirds_splitter_NEW.R
test_surveys <- expand.grid(site = levels(psite_sbirds$site),
                            date = c("2020-11-15", "2020-11-30", "2020-12-15","2020-12-30", "2021-01-15", "2021-01-30"))


make_truth <- function(zlump) {
  unsplit_sbirds = psite_sbirds
  #zlump <- c("PEEP")
  zlump <- c("PEEP", "DOSP", "YELL")
spp_group <- sbird_lumpies %>% 
  filter(long_lumpies %in% zlump) 
unlumpies <- unsplit_sbirds %>% 
  filter(species %in% spp_group$species, grepl("winter", season.year)) 

#set.seed(5)
spp_samples <- unlumpies %>% 
  select(site, species, truth = count) %>% 
  group_by(site, species) %>% 
  sample_n(size = 6) %>% 
  ungroup() %>% 
  arrange(site, species)

test_truth <- spp_samples %>% 
  mutate(date = rep(distinct(test_surveys, date)$date, nrow(spp_samples)/6)) %>% 
  arrange(site, date, species)
} 


lumps_truth <- make_truth() %>% 
  arrange(date, site)
 
#zlumps <- c("YELL", "PEEP", "DOSP")
#lumps_truth <- map_df(zlumps, make_truth) %>% 
#  arrange(date, site)

####################################################################################################################
####################################################################################################################




make_test_data <- function(zlump, group.mis.id.rate) {
  #zlump <- c("PEEP")
  spp_group <- sbird_lumpies %>% 
  filter(long_lumpies %in% zlump) %>%
  arrange(species) 
  #group.mis.id.rate = c(0.2, 0.8)
  #group.mis.id.rate = c(0.8)
 
# indicate what to do if single/fixed un-ID rate across species or varying un-ID rate across species   
if(length(group.mis.id.rate) == 1) {
  true_unlumpies <- lumps_truth %>% 
    filter(species %in% spp_group$species) %>% 
    mutate(sp.mis.id.rate = group.mis.id.rate)
} else {
  if(length(group.mis.id.rate) == 2) {
  sp.mis.id.rate = seq(from = group.mis.id.rate[1],
                             to = group.mis.id.rate[2], 
                             length.out =  nrow(spp_group))
  
spp_group <- cbind(spp_group, sp.mis.id.rate)
true_unlumpies <- lumps_truth %>% 
  filter(species %in% spp_group$species) %>% 
  full_join(., select(spp_group, species, sp.mis.id.rate))
  } else {
  print("group.mis.id.rate must have either length = 1 or 2")
  }
}

# do the actual allocation of truth to lumpies count
  #mis.id.rate = 0.8
test_data1 <- true_unlumpies %>% 
  mutate(lump = truth * sp.mis.id.rate,
         count = truth * (1 - sp.mis.id.rate))

# get un-ID individuals and combine into site X date lumpies
lump <- test_data1 %>% 
  select(date, site, lump) %>% 
  group_by(date, site) %>% 
  summarise(count = sum(lump)) %>% 
  mutate(species = zlump,
         sp.mis.id.rate = NA) %>% 
  select(date, site, species, count, sp.mis.id.rate) %>% 
  ungroup()
# get remaining pos-ID birds
obs <- test_data1 %>% 
  select(date, site, species, count, sp.mis.id.rate)%>% 
  ungroup()
# combine into a dataset that looks like the real data
test_data <- rbind(obs, lump) %>% 
  mutate(group.mis.id.rate = paste(group.mis.id.rate, collapse = "_"))
}



zlumps <- c("YELL", "PEEP", "DOSP") 

site_ns <- distinct(psite_sbirds, site, .keep_all = T) %>% 
  select(site, North_South_Code)
#------------------
mis.id.rate = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
zlumps.rep = rep(zlumps, length(mis.id.rate))
mis.id.rate.rep = rep(mis.id.rate, each = length(zlumps))
lumps.starter_fixd_unid <- data.frame(zlumps = zlumps.rep,
                            mis.id.rate = mis.id.rate.rep)
#------------------
lumps.starter_spvar_misid <- expand.grid(zlumps = zlumps,
                             mis.id.rate = list(c(0.1, 0.9), c(0.2, 0.8), c(0.3, 0.7), c(0.4, 0.6)))


#------------------
lumps_test_data_fixd_unid <- map2_df(lumps.starter_fixd_unid$zlumps, lumps.starter_fixd_unid$mis.id.rate, make_test_data) %>% 
  arrange(date, site, group.mis.id.rate) %>% 
  full_join(., site_ns, by = c("site")) %>% 
  mutate(season.year = "winter_2020")

#------------------
lumps_test_data_spvar_misid <- map2_df(lumps.starter_spvar_misid$zlumps, lumps.starter_spvar_misid$mis.id.rate, make_test_data)%>% 
  arrange(date, site, group.mis.id.rate) %>% 
  full_join(., site_ns, by = c("site")) %>% 
  mutate(season.year = "winter_2020")


############################################################################################################
############################################################################################################

# separate data generating functions
# hopefully not needed

make_test_data_spvar_misid <- function(zlump, spp.mis.id.rate.bounds) {
  #zlump <- c("PEEP")
  spp_group <- sbird_lumpies %>% 
  filter(long_lumpies %in% zlump)
  
  #spp.mis.id.rate.bounds = c(0.2, 0.8)
  spp.mis.id.rate.mult = seq(from = spp.mis.id.rate.bounds[1],
                             to = spp.mis.id.rate.bounds[2], 
                             length.out =  nrow(spp_group))
  
spp_group <- cbind(spp_group, spp.mis.id.rate.mult)
true_unlumpies <- lumps_truth %>% 
  filter(species %in% spp_group$species) %>% 
  full_join(., select(spp_group, species, group.mis.id.rate = spp.mis.id.rate.mult))

  #mis.id.rate = 0.8
test_data1 <- true_unlumpies %>% 
  mutate(lump = truth * group.mis.id.rate,
         count = truth * (1 - group.mis.id.rate))

lump <- test_data1 %>% 
  select(date, site, lump) %>% 
  group_by(date, site) %>% 
  summarise(count = sum(lump)) %>% 
  mutate(species = zlump) %>% 
  select(date, site, species, count) %>% 
  ungroup() %>% 
  mutate(mis.id.rate = NA)
  

obs <- test_data1 %>% 
  select(date, site, species, count, mis.id.rate = group.mis.id.rate)%>% 
  ungroup()

test_data <- rbind(obs, lump) %>% 
  mutate(spp.mis.id.rate.bounds = paste(spp.mis.id.rate.bounds, collapse = "_"))

}

peep_test_data <- make_test_data_spvar_misid("PEEP", c(0.2, 0.8))
zlumps <- c("YELL", "PEEP", "DOSP") 
lumps.starter <- expand.grid(zlumps = zlumps,
                             spp.mis.id.rate.bounds = list(c(0.1, 0.9), c(0.2, 0.8), c(0.3, 0.7), c(0.4, 0.6)))

site_ns <- distinct(psite_sbirds, site, .keep_all = T) %>% 
  select(site, North_South_Code)
lumps_test_data <- map2_df(lumps.starter$zlumps, lumps.starter$spp.mis.id.rate.bounds, make_test_data_spvar_misid)%>% 
  arrange(date, site, spp.mis.id.rate.bounds) %>% 
  full_join(., site_ns, by = c("site")) %>% 
  mutate(season.year = "winter_2020")

lumps_test_summary <- lumps_test_data %>% 
  group_by(species) %>% 
  summarise(mean.truth = mean(count))

