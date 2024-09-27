
# packages, source ----
library(tidyverse)
library(lubridate)
library(RODBC)
library(here)

library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/sbird_data_prep_utilities.R"))
options(scipen = 999)


# need to combine all alpha codes and common names for any "waterbird" species into a single string that can be passed to grepl
alpha_common <- custom_bird_list %>% 
  bird_taxa_filter(keep_taxa = c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes", "Laridae", "Stercorariidae", "BEKI", "Ardeidae")) %>% 
  mutate(alpha.code = paste("\\b", alpha.code, "\\b", sep = ""),
         common.name = paste("\\b", common.name, "\\b", sep = ""),
         alpha.common = paste(tolower(alpha.code), tolower(common.name), sep = "|")) 

all_alpha_common <- alpha_common %>% 
  summarise(all.alpha.common = paste(alpha.common, collapse = "|"))



# some dates had bad weather, but data were collected anyway, data from these dates should be excluded from interpolation calculations done in sbird_data_prep_2....r and the splitting done in sbird_data_prep_3....r
exclude_dates <- as.Date(c("1990-01-04", "1990-02-12", "2010-01-18"))

# pipe all functions together to produce clean data ----

sbird_notes <- shorebird_from_access("SHOREBIRD") %>% 
  fix_sbird_field_names() %>% #this adds date field
  fix_sbird_problem_dates() %>% 
  select(date, SITE_ABBR, NOTES.OTHER.SPECIES) %>% 
  filter(!is.na(NOTES.OTHER.SPECIES))



# \\d+\\s*\\w+\\s*\\w+ finds 1+ digits, followed by 0+ spaces, followed by 1+ words, followed by 0+ spaces, followed by 1+ words (e.g. 400 greater scaup) 
# \\w+\\s*\\w+\\s*\\d+ finds followed by 1+ words, followed by 0+ spaces,followed by 1+ words, followed by 0+ spaces, followed by 1+ digits (e.g. greater scaup 400) 
# replacing any dashes/hyphens reduces the number of patterns to search for in the next step
#  If keep dashes/hyphens need to match more patterns: "\\d+\\s*\\w+\\s*\\w+|\\w+\\s*\\w+\\s*\\d+|\\w+\\s*\\w+\\s*\\-\\s*\\d+|\\d+\\s*-\\s*\\w+\\s*\\w+")) 

  
extracted_wbirds <- sbird_notes %>% 
  # first step is some cleanup
  mutate(NOTES.OTHER.SPECIES = tolower(NOTES.OTHER.SPECIES),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "kingfisher", "beki"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "elegan tern", "elegant tern"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "raven", "cora"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "b. bran", "bran"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "br. pelican", "brpe"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "brants", "brant"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "swans", "swan"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "gulls", "gull"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "buffleheads|buffelheads", "bufflehead"),
         NOTES.OTHER.SPECIES = str_replace_all(NOTES.OTHER.SPECIES, "\u00ad|\u2013|\u2014|-", " "))  %>% 
  # next step is to extract any pairings of word and numbers (see pattern note above)
  mutate(spp = str_extract_all(NOTES.OTHER.SPECIES, "\\d+\\s*\\w+\\s*\\w+|\\w+\\s*\\w+\\s*\\d+"))  %>% 
  separate(spp, into = paste("spp.", seq(1, 20), sep = ""), sep = ", ") %>% 
  pivot_longer(cols = contains("spp."), values_to = "extracted.data", names_to = "spp.num") %>% 
  filter(!is.na(extracted.data), extracted.data != "character(0)") %>% 
  # and finally look for waterbird names in those extracted word/number pairings
  mutate(extracted.data = str_replace_all(extracted.data, "c\\(|\\)|", ""),
         extracted.data = gsub("\"", "", extracted.data),
         extracted.data = trimws(extracted.data),
#         notes.has.spp = grepl(all_alpha_common$all.alpha.common, NOTES.OTHER.SPECIES),
         extracted.spp = str_extract(extracted.data, all_alpha_common$all.alpha.common),
         notes.has.spp = grepl(all_alpha_common$all.alpha.common, extracted.spp),
#         extracted.number = ifelse(!is.na(extracted.spp), str_extract_all(extracted.data, "(\\d)+"), NA),
extracted.number = str_extract_all(extracted.data, "(\\d)+"),
extracted.number = as.character(extracted.number),
         missed.spp = is.na(extracted.spp) & notes.has.spp == TRUE)


zz <- filter(extracted_wbirds, date == "1991-10-31")

#filter(extracted_wbirds, !is.na(extracted.spp)) %>%  select(date, SITE_ABBR, NOTES.OTHER.SPECIES, extracted.spp, extracted.number) %>% view()


write.csv(extracted_wbirds, here("data_files/csv/waterbirds_from_sbirdnotes_check.csv"), row.names = FALSE)



extracted_wbirds2 <- filter(extracted_wbirds, !is.na(extracted.spp)) %>% 
  select(date, SITE_ABBR, NOTES.OTHER.SPECIES, extracted.spp, extracted.number) %>% 
  mutate(extracted.number = as.numeric(extracted.number))

write.csv(extracted_wbirds2, here("data_files/csv/waterbirds_from_sbirdnotes.csv"), row.names = FALSE)
