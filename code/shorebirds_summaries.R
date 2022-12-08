
library(tidyverse)


sbirds.long.sites.tots <- read.csv("data_files/sbirds_unk_split_1989_2018.csv")




######################################################
spppp <- ungroup(sbirds.long.sites.tots) %>% distinct(species) %>% mutate(sbird = 1)
sbirds <- edit(spppp)
sbirds <- filter(sbirds, sbird ==1)
write.csv(sbirds, "data_files/sbird_spp_list.csv", row.names = F)



sbirds <- read.csv("data_files/sbird_spp_list.csv")

# by species
sbirds.winter.nums <- sbirds.long.sites.tots  %>%
  filter(species %in% sbirds$species, count >= 0) %>% 
  filter(species %in% sbirds$species) %>% 
  filter(grepl("winter", season.year)) %>% 
  group_by(season.year, date, species) %>% 
  summarise(tot.bay.count = sum(count)) 
  
sbirds.winter.summary <- sbirds.winter.nums %>% 
  ungroup() %>% 
  group_by(season.year, species) %>% 
  summarise(mean.seas.count = round(mean(tot.bay.count), 0),
            med.seas.count = round(median(tot.bay.count), 0))



wesa.lesa.dunl <- c("total.WESA", "total.LESA", "total.DUNL", "WESA", "LESA", "DUNL")
tot.wesa.lesa.dunl.sand <- c("total.WESA", "total.LESA", "total.DUNL", "total.SAND")

wesa.lesa.dunl.summ <- sbirds.winter.summary %>% 
  filter(species %in% wesa.lesa.dunl) %>% 
  ungroup()

# total shorebirds
sbirds.winter.nums.allspp <- sbirds.long.sites.tots  %>%
  filter((species %in% sbirds$species & !(species %in% tot.wesa.lesa.dunl.sand)), count >= 0) %>% 
  filter(grepl("winter", season.year)) %>% 
  group_by(season.year, date) %>% 
  summarise(tot.bay.count = sum(count)) 
  
sbirds.winter.summary.allspp <- sbirds.winter.nums.allspp %>% 
  ungroup() %>% 
  group_by(season.year) %>% 
  summarise(mean.seas.count = round(mean(tot.bay.count), 0),
            med.seas.count = round(median(tot.bay.count), 0)) %>% 
  mutate(species = "All") %>% 
  select(season.year, species, mean.seas.count, med.seas.count) %>% 
  ungroup()


wesa.lesa.dunl.all.summ <- rbind(wesa.lesa.dunl.summ, sbirds.winter.summary.allspp)

write.csv(wesa.lesa.dunl.all.summ, "data_files/wesa.lesa.dunl.all.summ.csv", row.names = F)
############################################
##
sbirds.year.ns.summ <- sbirds.long.sites.tots  %>% 
  filter(MONTH < 3 | MONTH >10) %>% 
  group_by(date, species, North_South_Code) %>% 
  summarise(date.sp.total = sum(count)) %>% 
  mutate(season = ifelse(month(date) > 10, year(date), year(date) - 1),
         early.late = ifelse(month(date) > 10, "early", "late")) %>% 
  group_by(season, early.late, species, North_South_Code) %>% 
  summarise(sp.mean = mean(date.sp.total)) %>% 
  arrange(season, species, North_South_Code, early.late)

##
sbirds.year.walker.giac.summ <- sbirds.long.sites  %>% 
  filter(MONTH > 2 | MONTH < 6) %>% 
  filter(SITE_ABBR %in% c("GIA", "GWE", "GWW", "GWT", "GWWW", "GWWM", "GWM", "GWMGWT", "WCD")) %>% 
  mutate(GiacWalk = ifelse(SITE_ABBR == "WCD", "walker", "giacomini")) %>% 
  group_by(date, species, GiacWalk) %>% 
  summarise(count = sum(count)) %>% 
  mutate(count = ifelse(is.na(count), 0, count))
  
wesa.giac.walk <- sbirds.year.walker.giac.summ %>% 
  filter(species == "WESA") %>% 
  arrange(GiacWalk, date)
 
write.csv(wesa.giac.walk, "WESA_Walker_Giacomini.csv", row.names = F)
 
  
 spp.freq <- sbirds.year.ns.summ %>% 
   filter(sp.mean > 0) %>% 
   ungroup() %>% 
   group_by(species) %>% 
   summarise(survey.freq = length(species))
  
  
  
sp.plotter <- function(sp){
  df <- filter(sbirds.year.ns.summ, species == sp)
  ggplot(data = df, aes(x = season, y = sp.mean)) +
    geom_line(aes(color = North_South_Code, linetype = early.late))+
    ggtitle(sp)
  
}


sp.plotter("MAGO")


# below here copied from what was shorebirds_start.r 5/22/2020
# not checked for function

###########################################

# summarise YElL and DOSP assigned to species

yell_dosp_spp <- sbirds %>% 
  filter(species %in% c("YELL", "GRYE", "LEYE", "DOSP", "LBDO", "SBDO"), season == "winter") %>% 
  group_by(season.year, date, species) %>% 
  summarise(total.count = sum(count),
            mean.count = mean(count),
            min.count = min(count),
            max.count = max(count)) %>% 
  ungroup()%>% 
  group_by(season.year, species) %>% 
  summarise(total.count = sum(total.count),
            mean.count = mean(mean.count),
            min.count = min(min.count),
            max.count = max(max.count)) %>% 
  ungroup()%>% 
  group_by(species) %>% 
  summarise(total.count = sum(total.count),
            mean.count = mean(mean.count),
            min.count = min(min.count),
            max.count = max(max.count)) %>% 
  ungroup() %>%  
  arrange(factor(species, levels = c("YELL", "GRYE", "LEYE", "DOSP", "LBDO", "SBDO")))

# ----

notes_checker <- sbirds_wide %>% 
  select(COUNT.AREA, MONTH, DAY, YEAR, notes = NOTES...OTHER.SPECIES) %>% 
  filter(notes != "") %>% 
  mutate(notes = as.character(notes))

notes_checker$notes <- tolower(notes_checker$notes)

foo_notes <- unique(unlist(strsplit(notes_checker$notes, " "))) %>% 
  data.frame() %>% 
  rename(notes = 1)
foo_notes2 <- foo_notes %>% 
  mutate(notes2 = as.numeric(as.character(notes))) %>% 
  filter(is.na(notes2))



foo_notes3 <- foo_notes %>% 
  mutate(notes2 = gsub("\\b\\d+\\b", "", notes)) %>% 
  mutate(notes3 = str_replace_all("[^[:alnum:]]", " "))

patterns <- c("count", "incomplete", "not ", "couldn't", "no ", "didn't", "double-count", "downpour", "incomplete", "missed", "missing")

notes_checker2 <- notes_checker %>% 
  filter(grepl(paste(patterns, collapse="|"), notes))

write.csv(notes_checker2, "notes_checker2.csv", row.names = F)



# ----

peep_summ <- filter(psite_sbirds, species %in% c("PEEP", "DUNL", "WESA", "LESA"), grepl("winter", season.year)) %>% 
  spread(species, count) %>% 
  mutate(sum.known = DUNL + WESA + LESA,
         peep.g.known = ifelse(sum.known < PEEP, 1, 0))

peep_summ_yr <- peep_summ %>%
  filter(PEEP >= 1) %>% 
  group_by(year(date)) %>% 
  summarise(surveys.w.peeps = n(),
            surveys.w.peep.g.known = sum(peep.g.known)) %>% 
  full_join(., peep_summ %>% 
              group_by(year(date)) %>%  
              summarise(num.survey = n())) %>% 
  full_join(., peep_summ %>% 
              select(date) %>% 
              distinct() %>%  
              group_by(year(date)) %>%  
              summarise(num.survey.dates = n())) %>% 
  full_join(., peep_summ %>%
              filter(PEEP > 0) %>% 
              group_by(year(date)) %>% 
              summarise(max.peep = max(PEEP),
                        mean.peep = round(mean(PEEP), 2),
                        sd.peep = round(sd(PEEP), 2))) %>% 
  rename(year = 1) %>% 
  arrange(year) %>% 
  mutate(surveys.w.peeps = ifelse(is.na(surveys.w.peeps), 0, surveys.w.peeps),
         surveys.w.peep.g.known = ifelse(is.na(surveys.w.peep.g.known), 0, surveys.w.peep.g.known)) %>% 
  select(year, num.survey.dates, num.survey, surveys.w.peeps, surveys.w.peep.g.known, max.peep, mean.peep, sd.peep) %>% 
  mutate_all(~replace(., is.na(.), 0))


write.csv(peep_summ_yr, "figures_output/peep_year_summary.csv", row.names = F, col.names = c("Year", "Number of winter survey dates", "Number of winter surveys", "Number of surveys with PEEPs", "Number of surveys  with more PEEPs than DUNL, WESA and LESA combined", "Maximum PEEP count (single survey) that year", "Mean PEEP count (for surveys with at least 1 PEEP)", "St. Dev. PEEP count (for surveys with at least 1 PEEP)"))

colSums(peep_summ_yr)

# ----

n_s_ratio_summ <- peep_summ %>% 
  group_by(date, North_South_Code) %>% 
  summarise(sum.peep = sum(PEEP),
            sum.dunl = sum(DUNL),
            sum.wesa = sum(WESA),
            sum.lesa = sum(LESA),
            total.known = sum(sum.known)) %>% 
  mutate(known.peep.ratio = total.known/sum.peep,
         dunl.known.ratio = sum.dunl/total.known,
         wesa.known.ratio = sum.wesa/total.known,
         lesa.known.ratio = sum.lesa/total.known) 

n_s_ratio_summ_long <- n_s_ratio_summ  %>% 
  select(date, North_South_Code, contains("ratio"))%>% 
  gather(which.ratio, zratio, -date, -North_South_Code) %>% 
  filter(zratio >= 0, zratio != Inf) %>% 
  group_by(North_South_Code, which.ratio) %>%
  summarise(mean.ratio = mean(zratio),
            sd.ratio = sd(zratio)) %>% 
  arrange(which.ratio)
  
write.csv(n_s_ratio_summ_long, "figures_output/north_south_ratio_summary.csv", row.names = F)


# ----

sbirds.peepl <- sbirds %>% 
  select(OBSERVERS) %>% 
  distinct() %>% 
  separate(OBSERVERS, sep = '[,&;]', into = c("name1", "name2", "name3", "name4", "name5", "name6", "name7", "name8", "name9", "name10", "name11", "name12"))


sbirds.peepl.long <- sbirds.peepl %>% 
  gather(foo, names) %>%
  select(names) %>% 
  mutate(names = tolower(names),
         names = trimws(names, which = c("both")),
         names = gsub("and", "&", names)) %>% 
  separate(names, sep = "&", into = c("name1", "name2", "name3", "name4", "name5", "name6", "name7", "name8", "name9")) 

,
         names = gsub(" ", ".", names)) 
%>% 
  distinct(names) %>% 
  arrange(names) %>% 
  separate(names, sep = "\\.", into = c("name1", "name2", "name3", "name4", "name5", "name6", "name7", "name8", "name9"))  %>% 
  separate(name1, sep = " ", into = c("name1a", "name1b", "name1c")) %>% 
  mutate(first.name = ifelse())
  
  
  
  gather(foo, names2)  %>% 
  distinct(names2)  %>% 
  separate(names, sep = "and", into = c("name1", "name2", "name3", "name4", "name5", "name6", "name7", "name8", "name9"))  %>% 
  gather(foo, names2) 

write.csv(sbirds.peepl.long, "sbirds.peepl.long.csv")
  

