
####################################################################################################################

# split_tester() --------
split_tester <- function(zspp.mis.id.rate.bounds) {
#zmis.id.rate = 0
unsplit_sbirds <<- lumps_test_data %>% 
  dplyr::filter(spp.mis.id.rate.bounds == zspp.mis.id.rate.bounds)
sbird_unlumped_all <- map_df(zlumps, sbird_unlumper) 
sbird_unlumped_all0 <- sbird_unlumped_all %>% 
  mutate(mis.id.rate = zmis.id.rate) %>% 
  full_join(., lumps_truth) %>% 
  mutate(estimated.truth = count + unlumped.count) %>% 
  select(date, site, mis.id.rate, species, truth, estimated.truth, count, lumpy.count, lumpy.to.sp, unlumped.count, which.ratio, unlump.ratio, everything())
}

test_all <- map_df(mis.id.rate, split_tester)


test_all <- test_all %>% 
  ungroup() %>% 
  mutate(allocate.error = 100 * ((estimated.truth - truth)/truth),
         allocate.error = ifelse(abs(truth-estimated.truth) <= 2, 0, allocate.error)) %>% 
  mutate(allocate.error = ifelse(allocate.error == -Inf, -100, allocate.error),
         allocate.error = ifelse(allocate.error == Inf, 100, allocate.error))%>% 
  select(date, site, mis.id.rate, allocate.error, species, truth, estimated.truth, count, lumpy.count, lumpy.to.sp, unlumped.count, which.ratio, unlump.ratio, everything())        


#--------

test_summarizer <- function(test_result) {
  test_summary <- test_result %>% 
  filter(mis.id.rate != 0, mis.id.rate != 1, lumpy.count > 0) %>% 
  group_by(mis.id.rate, which.ratio) %>% 
  summarise(mean.allocate.error = mean(allocate.error),
            num.cases = n()) %>% 
  ungroup() %>% 
  arrange(mis.id.rate, match(which.ratio, c("site.date", "NS.date", "NS.3day", "NS.season", "bay.season", "no.split"))) %>% 
  group_by(mis.id.rate) %>% 
  mutate(rate.num.cases = sum(num.cases))
}



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
lumps_test_data <<- map2_df(lumps.starter$zlumps, lumps.starter$mis.id.rate, make_test_data_fixd_misid) %>% 
  arrange(date, site, mis.id.rate) %>% 
  full_join(., site_ns, by = c("site")) %>% 
  mutate(season.year = "winter_2020")

# get estimated species abundances from test data

test_all <<- map_df(mis.id.rate, split_tester)

# calculate the error in allocating individuals
test_all <<- test_all %>% 
  ungroup() %>% 
  mutate(allocate.error = 100 * ((estimated.truth - truth)/truth),
         allocate.error = ifelse(abs(truth-estimated.truth) <= 2, 0, allocate.error)) %>% 
  mutate(allocate.error = ifelse(allocate.error == -Inf, -100, allocate.error),
         allocate.error = ifelse(allocate.error == Inf, 100, allocate.error))%>% 
  select(date, site, mis.id.rate, allocate.error, species, truth, estimated.truth, count, lumpy.count, lumpy.to.sp, unlumped.count, which.ratio, unlump.ratio)    

}

test_all2 <- full_tester()

# add result to the test results list
results = as.list(c(1:100))

for(i in seq_along(results)) {
  results[[i]] <- full_tester()
}

#zoof <- test_summarizer(results[[3]])

summ_all <- map(results, test_summarizer)
#fooz <- summ_all[[2]]

summ_all_df <- rbindlist(summ_all)

grand_summ <- summ_all_df %>% 
  group_by(mis.id.rate, which.ratio) %>% 
  summarise(grand.mean.error = mean(mean.allocate.error))


grand_summ_wide <- grand_summ %>% 
  spread(which.ratio, grand.mean.error, -mis.id.rate) %>%
  ungroup() %>% 
  select(mis.id.rate, site.date, NS.date, NS.3day, NS.season, bay.season)

write.csv(grand_summ_wide, "figures_output/splitting_error_fixdSpUnID.csv", row.names = F)

