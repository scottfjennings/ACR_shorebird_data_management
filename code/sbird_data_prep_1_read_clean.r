

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
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


options(scipen = 999)


# some dates had bad weather, but data were collected anyway, data from these dates should be excluded from interpolation calculations done in sbird_data_prep_2....r and the splitting done in sbird_data_prep_3....r
exclude_dates <- as.Date(c("1990-01-04", "1990-02-12", "2010-01-18"))

# define data reading, cleaning functions ----
# read directly from access, basic data cleaning
shorebird_from_access <- function(ztable){
 db <- here("data_files/Shorebirds.mdb")
 
 con2 <- odbcConnectAccess2007(db)
 
sqlTables(con2, tableType = 'TABLE')$TABLE_NAME

out_table <- sqlFetch(con2, ztable) 
 
close(con2)

return(out_table)
}

#sbirds_wide <- shorebird_from_access("SHOREBIRD")

#sbird_sites <- shorebird_from_access("SHOREBIRD_SITES")



#sbirds_sites <- read.csv("data_files/sbirds_sites.csv")%>% 
#  select(SITE_ABBR, PARENT_SITE_ABBR, North_South_Code)

#sbirds_wide=read.csv("data_files/SHOREBIRD.csv")

fix_sbird_field_names <- function(sbirds_wide) {
  sbirds_wide <- sbirds_wide %>%
    rename_all(list(~gsub(" ", ".", .))) %>% 
    rename_all(list(~gsub("&", "", .))) %>% 
    rename_all(list(~gsub("\\.\\.", ".", .)))  %>% 
    rename(SITE_ABBR = COUNT.AREA) %>% 
    mutate(date = mdy(paste(MONTH, DAY, YEAR, sep = "-"))) %>% 
    mutate_if(is.factor, as.character)
}


fix_sbird_problem_dates <- function(sbirds_wide) {
  sbirds_wide <- sbirds_wide %>%  
    filter(!date %in% exclude_dates) %>%
    mutate(date = if_else(date == "2018-08-29", ymd("2018-08-28"), date)) 
}


make_sbirds_longer <- function(sbirds_wide) {

sbirds_longer <- sbirds_wide %>%
  select(-contains("NOTES"), -PROOFED, -contains("ENTERED"), -Field1) %>% 
  gather(species, count, -date, -ID, -SITE_ABBR, -MONTH, -DAY, -YEAR, -START.TIME, -END.TIME, -OBSERVERS) %>% 
  select(SITE_ABBR, everything(), -ID, -MONTH, -DAY, -YEAR) %>% 
  mutate(count = as.numeric(count))
}
# filter to just shorebird spp, and the lumpies, with bird_taxa_filter() from R_general/utility_functions/bird_utility_functions.r

fix_negative_NA_counts <- function(df, negative_to = 0, na_to = 0) {
  df <- df %>% 
    mutate(count = ifelse(count < 0, negative_to, count)) %>% 
    mutate(count = ifelse(is.na(count), na_to, count))
}


sbirds_add_site_names <- function(sbirds) {
#sbird_sites <- shorebird_from_access("SHOREBIRD_SITES") %>% 
#  select(SITE_ABBR, PARENT_SITE_ABBR, North_South_Code) 

  sbird_sites <- read.csv("data_files/csv/sbirds_sites.csv")    
sbirds <- sbirds %>% 
  left_join(., sbird_sites, by = c("SITE_ABBR")) 
}



sbirds_assign_season <- function(sbirds) {
sbirds  <- sbirds %>% 
  dplyr::mutate(date = ymd(date)) %>% 
  dplyr::mutate(season = NA,
         season = ifelse(month(date) >= 3 & month(date) < 6, "spring", season),
         season = ifelse(month(date) > 7 & month(date) < 10, "fall", season),
         season = ifelse(month(date) >= 10, "winter", season),
         season = ifelse(month(date) < 3, "winter", season),
         study.year = ifelse(month(date) < 3, year(date) - 1, year(date)),
         season.year = paste(season, study.year, sep = "_")) %>% 
  dplyr::select(-season, -study.year)
  
}


# pipe all functions together to produce clean data ----

sbird_data <- shorebird_from_access("SHOREBIRD") %>% 
  fix_sbird_field_names() %>% #this adds date field
  fix_sbird_problem_dates() %>% 
  make_sbirds_longer() %>% 
  fix_negative_NA_counts() %>% 
  rename(alpha.code = species) %>% 
  mutate(alpha.code = update_alpha(alpha.code)) %>% # from birdnames
  sbirds_assign_season() %>% 
  sbirds_add_site_names() 



# run to here then check for missing surveys, other data problems

sbird_sites <- shorebird_from_access("SHOREBIRD_SITES") %>% 
  select(SITE_ABBR, PARENT_SITE_ABBR, North_South_Code)



# the result to here is a data frame with the the total number of each species detected on each day at each parent site;
# includes shorebird and raptor species
# this still has PEEP and LWSA counts


# can filter to just shorebirds now, or go to sbird_data_prep_2... to split PEEP and LWSA
# can also go to raptors_from_shorebirds to extract raptor data and process further



sbirds <- sbird_data %>% 
  bird_taxa_filter(keep_taxa = c("Charadriiformes", "YELL", "LEGP", "LWSA", "PEEP", "DOSP", "PHAL"))



saveRDS(sbirds, here("data_files/rds/sbirds_date_parentsite"))

 
