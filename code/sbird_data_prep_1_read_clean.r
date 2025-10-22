

# this code creates a long version of the Shorebird access database

# count data are aggregated to the parent site level for consistency across years

# the result of this file is likely the starting point for any shorebird data analysis, but not that it still contains unsplit peeps and other lumped species (see sbird_data_prep_2_split_lumped_species.r)

# all this code should be able to be run at once

# last action is to save an RDS file, so this code only really needs to be run if there have been changes to the main access database.

# otherwise start with C:/Users/scott.jennings/Documents/Projects/shorebirds/shorebird_data_work/data_files/rds/sbirds_date_parentsite



# packages, source ----
library(tidyverse)
library(lubridate)
library(RODBC)
library(here)

library(birdnames)

# load custom_bird_list from OneDrive if working on Scott's computer
# custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")
# load custom_bird_list from E:/ if working from the AVD
custom_bird_list <- readRDS("E:/TestFolderSJ/helper_data/custom_bird_list")

source("E:/TestFolderSJ/helper_functions/ms_access_helper_functions.R")


source(here("code/sbird_data_prep_utilities.R"))
options(scipen = 999)


# some dates had bad weather, but data were collected anyway, data from these dates should be excluded from interpolation calculations done in sbird_data_prep_2....r and the splitting done in sbird_data_prep_3....r
exclude_dates <- as.Date(c("1990-01-04", "1990-02-12", "2010-01-18"))

# pipe all functions together to produce clean data ----

#sbird_data <- shorebird_from_access("SHOREBIRD") %>% 
sbird_data_start <- read_table_from_access("V:/Shorebirds_data/Shorebirds.mdb", "SHOREBIRD") 

# fix field names for easier coding in subsequent steps
sbird_data_wide <- sbird_data_start %>% 
  rename_all(list(~gsub(" ", ".", .))) %>% 
  rename_all(list(~gsub("&", "", .))) %>% 
  rename_all(list(~gsub("\\.\\.", ".", .)))  %>% 
  rename(SITE_ABBR = COUNT.AREA)%>% 
  mutate_if(is.factor, as.character)

# add date field
sbird_data_wide <- sbird_data_wide %>% 
  mutate(date = mdy(paste(MONTH, DAY, YEAR, sep = "-"))) 


# check for any records flagged to potentially exclude from analysis
filter(sbird_data_wide, Analysis_Flag == 1) %>% view()


# filter/fix problems
sbird_data_wide <- sbird_data_wide %>%    
  filter(!date %in% exclude_dates) %>%
  mutate(date = if_else(date == "2018-08-29", ymd("2018-08-28"), date))

# add season year field with winter seasons being assigned the year at the start of the winter
sbird_data_wide <- sbird_data_wide %>% 
  dplyr::mutate(season = case_when(MONTH >= 3 & MONTH < 6 ~ "spring",
                                   MONTH > 7 & MONTH < 10 ~ "fall",
                                   MONTH >= 10 | MONTH < 3 ~ "winter",
                                   TRUE ~ NA)) %>% 
  dplyr::mutate(study.year = ifelse(MONTH < 3, YEAR - 1, YEAR),
                season.year = paste(season, study.year, sep = "_")) %>% 
  dplyr::select(ID, SITE_ABBR, MONTH, DAY, YEAR, START.TIME, END.TIME, OBSERVERS, date, season.year, everything(), -season, -study.year)

# original has column for each species, this pivots to 1 column for spp name and one column for count
sbird_data <- sbird_data_wide  %>%
  dplyr::select(-ID, -MONTH, -DAY, -contains("NOTES"), -PROOFED, -contains("ENTERED"), -contains("Analysis")) %>% 
  pivot_longer(cols = -c(SITE_ABBR, YEAR, START.TIME, END.TIME, OBSERVERS, date, season.year),
    names_to = "species",
    values_to = "count"
  )
  
# replace negatives and NA values with 0
# as of Oct 2025 there aren't any negatives; not sure why this replacement was originally added. commenting out but keeping for now
# replacing NA at this stage is appropriate because they result from the pivoting long operation, not from anything that happened in the field
sbird_data <- sbird_data %>% 
 # mutate(count = ifelse(count < 0, 0, count)) %>% 
  mutate(count = replace_na(count, 0))

# fix alpha codes for updated taxonomy
# update_alpha() is from birdnames
sbird_data <- sbird_data %>% 
  rename(alpha.code = species) %>% 
  mutate(alpha.code = update_alpha(alpha.code)) 




# add survey site info from SHOREBIRD_SITES table
sbird_sites <- read_table_from_access("V:/Shorebirds_data/Shorebirds.mdb", "SHOREBIRD_SITES")
sbird_sites <- sbird_sites %>% 
  select(SITE_ABBR = SITE_CODE, SITE_NAME, PARENT_SITE_ABBR, North_South_Code) 

sbird_data <- sbird_data %>% 
  left_join(., sbird_sites, by = c("SITE_ABBR")) 


# run to here then check for missing surveys, other data problems
check_surveys <- sbird_data %>% 
  distinct(YEAR, season.year, date, SITE_ABBR) %>% 
  count(YEAR, season.year, SITE_ABBR) %>% 
  pivot_wider(id_cols = c(YEAR, season.year), names_from = SITE_ABBR, values_from = n)  %>%
  rowwise() %>%
  mutate(all.sites.surveyed = n_distinct(c_across(-c(YEAR, season.year)), na.rm = TRUE) <= 1) %>%
  ungroup() %>% 
  arrange(YEAR, season.year)

filter(check_surveys, all.sites.surveyed == FALSE)

# known funky stuff
# SPT and BRB combined into BRBSPT on 2014-08-25
# GWM and GWT combined into GWMGWT on 2015-02-05

# the result to here is a data frame with the the total number of each species detected on each day at each parent site;
# includes shorebird and raptor species
# this still has PEEP and LWSA counts


# can filter to just shorebirds now, or go to sbird_data_prep_3... to split PEEP and LWSA
# can also go to raptors_from_shorebirds to extract raptor data and process further



sbirds <- sbird_data %>% 
  bird_taxa_filter(keep_taxa = c("Charadriiformes", "YELL", "LEGP", "LWSA", "PEEP", "DOSP", "PHAL"))



saveRDS(sbirds, here("data/sbirds_date_parentsite"))

 
