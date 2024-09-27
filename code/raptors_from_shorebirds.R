
# extract raptor data from shorebirds database

# do some cleaning



# packages, source ----
library(tidyverse)
library(lubridate)
library(RODBC)
library(birdnames)


source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/R_general/utility_functions/bird_utility_functions.R")

custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")
# start is sbird_data from sbird_data_prep_2_interpolate...


# raptors ----
# uses sbird_data from sbird_data_prep_1_read_clean.R
filt_raptors <- sbird_data %>% 
  bird_taxa_filter(data_file = ., 
                   #join_taxa = c("alpha.code", "species"), 
                   keep_taxa = c("Accipitriformes", "Falconiformes", "Strigiformes", "SSCOHA")
                   #, drop_cols = c("Species.Number", "Scientific.Name", "order", "family", "subfamily", "genus", "species")
                   )  %>% 
  rename(species = alpha.code) %>% 
  full_join(., read.csv("data_files/csv/raptor_classes.csv")) 



saveRDS(filt_raptors, "data_files/rds/raptors4analysis")

write.csv(filt_raptors, "data_files/csv/raptors4analysis.csv", row.names = F)


rm(filt_raptors)

