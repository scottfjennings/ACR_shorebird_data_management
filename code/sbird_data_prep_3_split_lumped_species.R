
library(tidyverse)
library(lubridate)
library(RODBC)
library(here)

library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

# start with sbirds_with_interpolated from sbird_data_prep_2_interpolate_missing_surveys.R
# or sbirds_date_parentsite from C:\Users\scott.jennings\Documents\Projects\shorebirds\shorebird_data_work\data_files\rds
#sbirds_with_interpolated <- readRDS("C:/Users/scott.jennings/Documents/Projects/shorebirds/shorebird_data_work/data_files/rds/sbirds_with_interpolated")


# define functions ----

sbirds_group_parentsite <- function(sbirds) {
  sbirds <- sbirds %>% 
    mutate(count = ifelse(is.na(count), 0, count)) %>% 
    group_by(date, alpha.code, North_South_Code, PARENT_SITE_ABBR) %>% 
    summarise(psite_count = sum(count)) %>%  
    ungroup() %>% 
    rename(site = PARENT_SITE_ABBR, count = psite_count)
}

# create grouping object                  
# this is called inside sbird_unlumper
make_sbird_lumpies <- function(sbird_groupies = c("LWSA", "PEEP")) {
# warning about missing pieces is expected and OK
  lumpies <- custom_bird_list %>% 
  filter(alpha.code %in% sbird_groupies) %>% 
  select(alpha.code, group.spp) 

num_group_spp <- lumpies %>% 
  mutate(num.group.spp = str_count(group.spp, ',') + 1) %>% 
  summarise(max.num.spp = max(num.group.spp))

lumpies_long <- lumpies %>% 
  separate(group.spp, into = paste("spp", 1:num_group_spp$max.num.spp[1], sep = "_")) %>% 
  pivot_longer(cols = contains("spp")) %>% 
  select(long.lumpies = alpha.code, alpha.code = value) %>% 
  filter(!is.na(alpha.code))%>% 
  mutate(long.lumpies = as.character(long.lumpies))
}
#sbird_lumpies <- make_sbird_lumpies()

# actual allocation 
# this does the actual calculation of ratios, determination of appropriate ratio, and allocation of undifferentiated birds to each species
# this works on one lumped group at a time (e.g. PEEP), which is input as zlump
# can then lump over multiple groups_to_split

sbird_unlumper <- function(zlump) {
 #zlump = "PEEP"

spp_group <- make_sbird_lumpies() %>% 
  filter(long.lumpies == zlump)

# all unIDed birds for the lumpy group
lumpies <- sbirds %>% 
  filter(alpha.code == zlump) %>% 
  select(date, site, lumpy.count = count)

# all possitively IDed birds for species in the lumpy group
alpha.code <- sbirds %>% 
  filter(alpha.code %in% spp_group$alpha.code)

un_lumpies <- full_join(alpha.code, lumpies, by = c("date", "site")) %>% 
  select(North_South_Code, site, date, alpha.code, count, lumpy.count)

# total possitively IDed birds at each site X date
un_lumpies <- un_lumpies %>% 
  arrange(date, site) %>% 
  group_by(date, site) %>% 
  mutate(site.date.sum.known = sum(count)) %>% 
  ungroup()  %>% 
  mutate(site.date.ratio = round(count/site.date.sum.known, 2)) 

# total possitively IDed birds by species and all in the group at each half of the bay X date
un_lumpies <- un_lumpies %>% 
  group_by(date, North_South_Code, alpha.code) %>% 
  mutate(NS.date.count = sum(count)) %>% 
  ungroup()  %>% 
  group_by(date, North_South_Code) %>% 
  mutate(NS.date.sum.known = sum(count)) %>% 
  ungroup() %>% 
  mutate(NS.date.ratio = round(NS.date.count/NS.date.sum.known, 2)) %>% 
  arrange(North_South_Code, date, alpha.code)

# total possitively IDed birds by species and all in the group for the whole bay X date
un_lumpies <- un_lumpies %>% 
  group_by(date, alpha.code) %>% 
  mutate(bay.date.count = sum(count)) %>% 
  ungroup()  %>% 
  group_by(date) %>% 
  mutate(bay.date.sum.known = sum(count)) %>% 
  ungroup() %>% 
  mutate(bay.date.ratio = round(bay.date.count/bay.date.sum.known, 2)) %>% 
  arrange(North_South_Code, date, alpha.code)
#---

un_lumpies_ratios <- un_lumpies %>% 
  #filter(lumpy.count > 0) %>% 
  select(North_South_Code, site, date, alpha.code, count, lumpy.count, contains("sum.known"), contains("ratio")) %>% 
  arrange(North_South_Code, date, site) %>% 
  mutate(unlump.ratio = NA,
         which.ratio = NA) %>% 
  mutate(unlump.ratio = ifelse(lumpy.count > 0 & lumpy.count < site.date.sum.known, 
                               site.date.ratio, unlump.ratio),
         which.ratio = ifelse(lumpy.count > 0 & lumpy.count < site.date.sum.known, 
                               "site.date", which.ratio)) %>%
  mutate(unlump.ratio = ifelse((lumpy.count > 0 & lumpy.count >= site.date.sum.known & lumpy.count < NS.date.sum.known),
                               NS.date.ratio, unlump.ratio),
         which.ratio = ifelse((lumpy.count > 0 & lumpy.count >= site.date.sum.known & lumpy.count < NS.date.sum.known), 
                               "NS.date", which.ratio))%>%
  mutate(unlump.ratio = ifelse((lumpy.count > 0 & lumpy.count >= NS.date.sum.known),
                               bay.date.ratio, unlump.ratio),
         which.ratio = ifelse((lumpy.count > 0 & lumpy.count >= NS.date.sum.known), 
                               "bay.date", which.ratio))

un_lumped <- un_lumpies_ratios %>% 
  mutate(unlumped.count = floor(lumpy.count * unlump.ratio),
         unlumped.count = ifelse(is.na(unlumped.count), 0, unlumped.count),
         lumpy.to.sp = paste(zlump, ".to.", alpha.code, sep = ""),
         which.ratio = ifelse(is.na(which.ratio), "no.split", which.ratio)) %>% 
  select(North_South_Code, site, date, alpha.code, count, lumpy.count, lumpy.to.sp, unlumped.count, which.ratio, unlump.ratio, everything())

}

# add unlumped birds to possitively IDed birds
add_allocated_sbirds <- function() {

unlumped_all_slim <- sbird_unlumped_all %>%
  filter(lumpy.count > 0) %>% 
  select(date, site, alpha.code, count = unlumped.count, lumpy.to.sp)

all_sbirds <- sbirds %>%
  filter(!alpha.code %in% groups_to_split) %>% 
  select(-North_South_Code) %>% 
  mutate(lumpy.to.sp = NA) %>% 
  rbind(., unlumped_all_slim) %>% 
  arrange(site, date, alpha.code)
# all_sbirds has a row for each species X site X date, plus extra rows for DUNL, WESA, LESA, SAND for the counts allocated to those species from PEEP and LWSA
all_sbirds_summ <- all_sbirds %>% 
  group_by(site, date, alpha.code) %>% 
  summarise(allocated.count = sum(count)) %>% 
  ungroup()
# all_sbirds_summ has the aggregated counts, so just a single row for each species X site X date
} 



# pipe functions together ----
# group by parent site to standardize across years

# sbirds <- sbirds_with_interpolated %>%
sbirds <- sbirds %>% 
  sbirds_group_parentsite() 
# summarize(sbirds, sum(count)) == summarize(sbirds_with_interpolated, sum(count))

groups_to_split <- c("LWSA", "PEEP")

sbird_unlumped_all <- map_df(groups_to_split, sbird_unlumper)
# sbird_unlumped_all shows the work of unlumping the "species" named in groups_to_split
# warning about missing pieces is ok
saveRDS(sbird_unlumped_all, here("data_files/rds/show_sbird_unlumping_work"))


# summarize numbers of split birds
sbird_unlumped_all <- readRDS("data_files/rds/show_sbird_unlumping_work")

splitting_summary <- sbird_unlumped_all %>% 
  group_by(alpha.code) %>% 
  summarise(total.pos.id = sum(count),
            total.from.lumpies = sum(lumpy.count)) %>% 
  mutate(prop.from.split = total.from.lumpies/total.pos.id)

# --
# add_allocated_sbirds combines the counts allocated from the "species" named in groups_to_split with those in all other species from the original count data
allocated_sbirds <- add_allocated_sbirds()
# add back in N/S field
allocated_sbirds <- allocated_sbirds %>% 
  left_join(., select(sbirds, site, North_South_Code) %>% distinct())


# allocated_sbirds has a single row for each species X site X date, and represents the data from birds IDed to species in the field and the birds IDed as PEEP or LWSA that have been allocated to DUNL, WESA, LESA, SAND based on the ratios of those possitively IDed species.
# the field allocated.count contains these total bird numbers (possitively ID and allocated)

saveRDS(allocated_sbirds, here("data_files/rds/sbirds_peep_lwsa_split"))
rm(sbird_unlumped_all, sbirds, sbirds_with_interpolated)
