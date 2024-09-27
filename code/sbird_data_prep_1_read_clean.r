

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
custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/sbird_data_prep_utilities.R"))
options(scipen = 999)


# some dates had bad weather, but data were collected anyway, data from these dates should be excluded from interpolation calculations done in sbird_data_prep_2....r and the splitting done in sbird_data_prep_3....r
exclude_dates <- as.Date(c("1990-01-04", "1990-02-12", "2010-01-18"))

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
  dplyr::select(SITE_CODE, PARENT_SITE_ABBR, North_South_Code)



# the result to here is a data frame with the the total number of each species detected on each day at each parent site;
# includes shorebird and raptor species
# this still has PEEP and LWSA counts


# can filter to just shorebirds now, or go to sbird_data_prep_3... to split PEEP and LWSA
# can also go to raptors_from_shorebirds to extract raptor data and process further



sbirds <- sbird_data %>% 
  bird_taxa_filter(keep_taxa = c("Charadriiformes", "YELL", "LEGP", "LWSA", "PEEP", "DOSP", "PHAL"))



saveRDS(sbirds, here("data_files/rds/sbirds_date_parentsite"))

 
