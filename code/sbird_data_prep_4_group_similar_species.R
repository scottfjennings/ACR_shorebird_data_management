

# lump possitively IDed birds into groups ------
# doing this for the species that that are similar and that may not be accurately separated in the field 

# input data  can be allocaded_sbirds from sbird_data_prep_3_split_lumped_species.R
# or the saved RDS
# see below for reading RDS

# can change the species that get lumped in make_df4lumping depending on analysis needs

# packages, source ----
library(tidyverse)
library(lubridate)
library(here)
options(scipen = 999)

library(birdnames)
# load custom_bird_list from OneDrive if working on Scott's computer
# custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")
# load custom_bird_list from E:/ if working from the AVD
custom_bird_list <- readRDS("E:/TestFolderSJ/helper_data/custom_bird_list")


# define functions ----
sbird_groupies <- c("YELL", 
                    #"LEGP", 
                    "DOSP", 
                    "PHAL" 
                    #"TURN"
                    )
make_df4lumping <- function(sbird_groupies) {
  # this goes inside tally_lumped_spp_counts()


lumpies <- custom_bird_list %>% 
  filter(alpha.code %in% sbird_groupies) %>% 
  dplyr::select(alpha.code, group.spp) 

num_group_spp <- lumpies %>% 
  mutate(num.group.spp = str_count(group.spp, ',') + 1) %>% 
  summarise(max.num.spp = max(num.group.spp))

df4lumping <- lumpies %>% 
  separate(group.spp, into = paste("spp", 1:num_group_spp$max.num.spp[1], sep = "_")) %>% 
  pivot_longer(cols = contains("spp")) %>% 
  dplyr::select(long.lumpies = alpha.code, alpha.code = value) %>% 
  filter(!is.na(alpha.code))%>% 
  mutate(long.lumpies = as.character(long.lumpies))

}


tally_lumped_spp_counts <- function(allocated_sbirds) {
allocated_sbirds2 <- allocated_sbirds %>% 
  full_join(., make_df4lumping(sbird_groupies)) %>% 
  mutate(long.lumpies2 = ifelse(is.na(long.lumpies), alpha.code, long.lumpies)) %>% 
  group_by(date, site, long.lumpies2) %>% 
  mutate(allocated.count2 = sum(allocated.count)) %>% 
  arrange(date, site, long.lumpies2) %>% 
  ungroup()

allocated_sbirds3 <- allocated_sbirds2 %>% 
  dplyr::select(season.year, date, site, alpha.code = long.lumpies2, count = allocated.count2, North_South_Code) %>% 
  distinct()
}



# pipe functions together ----
# if coming from sbird_data_prep_3... don't need to reload allocated_birds
#allocated_sbirds <- readRDS("C:/Users/scott.jennings/Documents/Projects/shorebirds/shorebird_data_work/data_files/rds/sbirds_peep_lwsa_split") %>% 
shorebirds_for_analysis <- allocated_sbirds  %>% 
  dplyr::mutate(season = case_when(month(date) >= 3 & month(date) < 6 ~ "spring",
                                   month(date) > 7 & month(date) < 10 ~ "fall",
                                   month(date) >= 10 | month(date) < 3 ~ "winter",
                                   TRUE ~ NA)) %>% 
  dplyr::mutate(study.year = ifelse(month(date) < 3, year(date) - 1, year(date)),
                season.year = paste(season, study.year, sep = "_"))
  

shorebirds_for_analysis <- shorebirds_for_analysis %>% 
  tally_lumped_spp_counts()

# check which species were in allocated_sbirds but aren't in shorebirds_for_analysis
# this should be the constituent species in the groups listed in sbird_groupies
anti_join(allocated_sbirds, shorebirds_for_analysis, by = c("date", "site", "alpha.code")) %>% 
  filter(allocated.count > 0) %>% 
  group_by(alpha.code) %>% 
  summarise(n())

#write.csv(allocated_sbirds4, "data_files/sbirds4analysis.csv", row.names = F)
saveRDS(shorebirds_for_analysis, here("data/shorebirds_for_analysis"))



### testing NO RUN


old_sbirds <- read.csv("data_files/csv/sbirds4analysis.csv") %>% 
  mutate(date = as.Date(date),
         alpha.code = as.character(alpha.code)) %>% 
  filter(year(date) < 2019) %>% 
  fix_4letter_codes() %>% 
  droplevels()


