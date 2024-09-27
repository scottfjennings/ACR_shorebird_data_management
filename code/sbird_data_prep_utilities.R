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
  dplyr::select(-contains("NOTES"), -PROOFED, -contains("ENTERED")) %>% 
  gather(species, count, -date, -ID, -SITE_ABBR, -MONTH, -DAY, -YEAR, -START.TIME, -END.TIME, -OBSERVERS) %>% 
  dplyr::select(SITE_ABBR, everything(), -ID, -MONTH, -DAY, -YEAR) %>% 
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


