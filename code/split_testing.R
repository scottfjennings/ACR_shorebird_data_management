library(data.table)

lumps_test_data <- lumps_test_data_spvar_misid
gr.mis.id.rate <- distinct(lumps_test_data, group.mis.id.rate) %>% 
  as.list()
sp.mis.id.rate <- distinct(lumps_test_data, species,  sp.mis.id.rate) 


# split_tester() --------
split_tester <- function(zmis.id.rate) {
#zmis.id.rate = mis.id.rate[[1]][1]
unsplit_sbirds <<- lumps_test_data %>% 
  dplyr::filter(group.mis.id.rate == zmis.id.rate)
sp_mis_id_rates <- distinct(unsplit_sbirds, species,  sp.mis.id.rate) %>% 
  filter(!is.na(sp.mis.id.rate))
sbird_unlumped_all <- map_df(zlumps, sbird_unlumper) 
sbird_unlumped_all0 <- sbird_unlumped_all %>% 
  mutate(mis.id.rate = zmis.id.rate) %>% 
  full_join(., lumps_truth) %>% 
  full_join(., sp_mis_id_rates) %>% 
  mutate(estimated.truth = count + unlumped.count) %>% 
  select(date, site, mis.id.rate, sp.mis.id.rate, species, truth, estimated.truth, count, lumpy.count, lumpy.to.sp, unlumped.count, which.ratio, unlump.ratio, everything())
}

test_all <- map_df(mis.id.rate$group.mis.id.rate, split_tester)


test_all <- test_all %>% 
  ungroup() %>% 
  mutate(allocate.error = 100 * ((estimated.truth - truth)/truth)) %>% 
  mutate(allocate.error = ifelse(allocate.error == -Inf, -100, allocate.error),
         allocate.error = ifelse(allocate.error == Inf, 100, allocate.error))%>% 
  select(date, site, mis.id.rate, sp.mis.id.rate, allocate.error, species, truth, estimated.truth, count, lumpy.count, lumpy.to.sp, unlumped.count, which.ratio, unlump.ratio, everything())        


#-------------
ggplot(data = filter(test_all, mis.id.rate != 1, mis.id.rate != 0)) +
  geom_jitter(aes(x = mis.id.rate, y = allocate.error, color = lumpy.count))+
  facet_wrap(~which.ratio) +
  ylim(-200, 13000)

ggplot(data = filter(test_all, mis.id.rate != 1, mis.id.rate != 0, lumpy.count > 0)) +
  geom_freqpoly(aes(x = allocate.error, color = which.ratio))
     
ggplot(data = filter(test_all, mis.id.rate != 1, mis.id.rate != 0, lumpy.count > 0)) +
  geom_boxplot(aes(x = as.factor(mis.id.rate), y = allocate.error))
 
#-------------

ggplot(data = test_all_summary) +
  geom_point(aes(x = mis.id.rate, y = mean.allocate.error, color = which.ratio))

###################################################


full_tester <- function() {
# make a new simulated truth, sampling from the real data
lumps_truth <<- make_truth() %>% 
  arrange(date, site)

# make a testing data set from that truth
# splits truth by sequence of proportions of total birds that were unidentified
# same un-ID proportion across species
lumps_test_data <<- map2_df(lumps.starter_fixd_unid$zlumps, lumps.starter_fixd_unid$mis.id.rate, make_test_data) %>% 
  arrange(date, site, group.mis.id.rate) %>% 
  full_join(., site_ns, by = c("site")) %>% 
  mutate(season.year = "winter_2020")
# varying un-ID proportion across species
#lumps_test_data <<- map2_df(lumps.starter_spvar_misid$zlumps, lumps.starter_spvar_misid$mis.id.rate, make_test_data)%>% 
#  arrange(date, site, group.mis.id.rate) %>% 
#  full_join(., site_ns, by = c("site")) %>% 
#  mutate(season.year = "winter_2020")

# get estimated species abundances from test data
mis.id.rate <- distinct(lumps_test_data, group.mis.id.rate) %>% 
  as.list()

test_all <- map_df(mis.id.rate$group.mis.id.rate, split_tester)


test_all <- test_all %>% 
  ungroup() %>% 
  mutate(allocate.error = 100 * ((estimated.truth - truth)/truth)) %>% 
  mutate(allocate.error = ifelse(allocate.error == -Inf, -100, allocate.error),
         allocate.error = ifelse(allocate.error == Inf, 100, allocate.error),
         allocate.error = ifelse(truth == 0 & estimated.truth == 0 & is.na(allocate.error), 0, allocate.error))%>% 
  select(date, site, mis.id.rate, sp.mis.id.rate, allocate.error, species, truth, estimated.truth, count, lumpy.count, lumpy.to.sp, unlumped.count, which.ratio, unlump.ratio, everything())   

}

test_all2 <- full_tester()

# add result to the test results list
results = as.list(c(1:100))
system.time(
for(i in seq_along(results)) {
  results[[i]] <- full_tester()
}
)



#--------

test_summarizer <- function(test_result) {

  test_summary <- test_result %>% 
  filter(mis.id.rate != 0, mis.id.rate != 1, lumpy.count > 0) %>% 
  group_by(mis.id.rate, which.ratio) %>% 
  summarise(mean.allocate.error = mean(abs(allocate.error)),
            num.cases = n()) %>% 
  ungroup() %>% 
  arrange(mis.id.rate, match(which.ratio, c("site.date", "NS.date", "NS.3day", "NS.season", "bay.season", "no.split"))) %>% 
  group_by(mis.id.rate) %>% 
  mutate(rate.num.cases = sum(num.cases))
}

foo <- test_summarizer(zoof)

zoof <- results[[4]]
fooz <- t
summ_all <- map_df(results, test_summarizer)
#fooz <- summ_all[[2]]


grand_summ <- summ_all %>% 
  group_by(mis.id.rate, which.ratio) %>% 
  summarise(grand.mean.error = mean(mean.allocate.error))


grand_summ_wide <- grand_summ %>% 
  spread(which.ratio, grand.mean.error, -mis.id.rate) %>%
  ungroup() %>% 
  select(mis.id.rate, site.date, NS.date, NS.3day, NS.season, bay.season) %>% 
  arrange(as.factor(mis.id.rate))

write.csv(grand_summ_wide, "figures_output/splitting_error_fixdSpUnID.csv", row.names = F)

