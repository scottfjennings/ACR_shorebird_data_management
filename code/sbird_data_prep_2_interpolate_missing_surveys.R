

# this is an optional step in shorebird data management
# 
# takes sbird_data from sbird_data_prep_1...


interpolate_exclude_dates <- c("2010-11-12", 
                               "1990-01-04", 
                               "1990-02-12", 
                               "2018-08-29"
                               ) # some of these excluded by other means, but keeping here too
#
# define functions for checking whether sites were visited on each date -----

make_siteXdate_checker_wide <- function(sbirds) {

  woo <- table(sbirds$SITE_ABBR, sbirds$date) %>% data.frame() %>% 
    mutate(Freq = Freq / sbirds %>% distinct(alpha.code) %>% nrow())
wider_woo <- woo %>% 
  pivot_wider(id_cols = Var2, names_from = Var1, values_from = Freq) %>% 
  mutate(num.sites = select(., -contains("Var2")) %>% rowSums(na.rm = TRUE)) %>% 
  rename(date = Var2) %>% 
  mutate(date = ymd(date)) %>% 
  sbirds_assign_season() %>% 
  group_by(season.year) %>% 
  mutate(max.season.year.sites = max(num.sites)) %>% 
  ungroup() %>% 
  mutate(too.few.sites = ifelse(num.sites == max.season.year.sites, F, T))
}

make_siteXdate_checker_long <- function(missing_siteXdate_wide) {
 
  season_year_goal_sites <- missing_siteXdate_wide %>% 
  group_by(season.year) %>% 
  filter(num.sites == max(num.sites)) %>% 
  ungroup() %>% 
  select(-date, -num.sites, -max.season.year.sites, -too.few.sites) %>% 
  distinct()

season_year_goal_sites_long <- season_year_goal_sites %>% 
  pivot_longer(cols = (-season.year), names_to = "SITE_ABBR", values_to = "should.been.surveyed") 

missing_siteXdate_long <- missing_siteXdate_wide %>% 
  select(-num.sites, -max.season.year.sites, -too.few.sites) %>% 
  pivot_longer(cols = c(-date, -season.year), names_to = "SITE_ABBR", values_to = "surveyed")

dates_sites_goals <- full_join(season_year_goal_sites_long, missing_siteXdate_long) %>% 
  mutate(site.missed = ifelse(surveyed == 0 & should.been.surveyed > 0, T, F)) %>% 
  filter(should.been.surveyed > 0) %>% 
  select(season.year, date, SITE_ABBR, everything())

  
}

# pipe functions together ----

all_siteXdate <- sbird_data %>% 
  filter(!as.character(date) %in% interpolate_exclude_dates) %>% 
  make_siteXdate_checker_wide() %>% 
  make_siteXdate_checker_long()


# site.missed == T indicates sites that were missed when that site was surveyed on other dates that season.year
# filter(all_siteXdate, site.missed == T) %>% nrow()

# NO RUN, checking sites further ----
all_parentsiteXdate <- sbird_data %>% 
  filter(!as.character(date) %in% interpolate_exclude_dates) %>%
  group_by(date, PARENT_SITE_ABBR, alpha.code) %>%
  summarise(count = sum(count)) %>% 
  rename(SITE_ABBR = PARENT_SITE_ABBR) %>% 
  make_siteXdate_checker_wide()

foo <- all_siteXdate %>% 
  filter(grepl("winter", season.year)) %>% 
  select(-date, -surveyed, -site.missed) %>% 
  distinct() %>% 
  pivot_wider(id_cols = season.year, names_from = SITE_ABBR, values_from = should.been.surveyed)

# --- now filling count for those problem dates X sites ----

# interpolate data for missing surveys by mean and median of data for when those sites were counted that season.year ----
interpolate_missing_site_dates <- function(all_siteXdate, sbird_df) {
  # get the dates that had missing sites
  missed_siteXdate <- all_siteXdate %>% 
  filter(site.missed == T) %>%
  select(season.year, date, SITE_ABBR) %>% 
  distinct() 
# get good data for the season.year X sites that had missing dates
good_data_for_interpo <- missed_siteXdate %>% 
  select(season.year, SITE_ABBR) %>% 
  left_join(., sbird_data, by = c("season.year", "SITE_ABBR"))
# summarize that good data    
good_data_interpolated <- good_data_for_interpo %>% 
  group_by(season.year, SITE_ABBR, alpha.code) %>% 
  summarize(med.count = median(count),
            mean.count = mean(count)) %>% 
  mutate(count = NA)
# fill interpolated data in to missing
interp_sbirds <- full_join(missed_siteXdate, good_data_interpolated, by = c("season.year", "SITE_ABBR")) %>% 
  mutate(count.type = "interpolated")
# combine with the good data to allow evaluation of the interpolation process
interp_counted_sbirds <- good_data_for_interpo %>% 
  select(season.year, SITE_ABBR, date, alpha.code, count) %>% 
  mutate(count.type = "counted",
         med.count = NA,
         mean.count = NA) %>% 
  rbind(., interp_sbirds) %>% 
  arrange(season.year, alpha.code, date)

}

interpolated_sbirds <- interpolate_missing_site_dates(all_siteXdate, sbird_data)


# add interpolated birds back to full data ----
add_interpolated <- function(interpolated_sbirds, sbird_data) {
  interp_only <- interpolated_sbirds %>% 
    filter(count.type == "interpolated")   %>% 
    select(season.year, SITE_ABBR, date, alpha.code, med.count) %>% 
    rename(count = med.count) %>% 
    sbirds_add_site_names() %>% 
    mutate(START.TIME = NA,
           END.TIME = NA,
           OBSERVERS = "interpolated data")
  
  sbird_with_interpo <- rbind(interp_only, sbird_data)

}

sbirds_with_interpolated <- add_interpolated(interpolated_sbirds, sbird_data) %>% 
  arrange(date, SITE_ABBR)

# check to see that everything was added:
# if surveyed > 0 and !=1, then some species are missing from that date X survey
all_siteXdate_interpo <- sbirds_with_interpolated %>% 
  make_siteXdate_checker_wide() %>% 
  make_siteXdate_checker_long()


filter(all_siteXdate_interpo, site.missed == T) %>% nrow()

# a few remaining GWMGWT surveys is ok, just double check that if GWMGWT was surveyed, GWM and GWT were not surveyed:
filter(all_siteXdate_interpo, GWMGWT == 1 & GWM == 0 & GWT == 0)


foo <- anti_join(sbirds_with_interpolated, sbird_data, by = c("season.year", "date", "SITE_ABBR"))

saveRDS(sbirds_with_interpolated, "data_files/rds/sbirds_with_interpolated")
rm(all_siteXdate_interpo, all_siteXdate, sbird_data, interpolated_sbirds)
# can now proceed to either sbird_data_prep_3_split... or raptors_from_shorebirds


## summarize interpolation

interpolated_summary_by_spp <- sbirds_with_interpolated %>% 
  mutate(data.type = ifelse(OBSERVERS == "interpolated data", "interpolated", "counted"),
         data.type = ifelse(is.na(data.type), "counted", data.type)) %>% 
  group_by(alpha.code, data.type) %>% 
  summarize(total.count = sum(count)) %>% 
  ungroup()

interpolated_summary_by_spp_wide <- interpolated_summary_by_spp %>% 
  pivot_wider(id_cols = alpha.code, names_from = data.type, values_from = total.count) %>% 
  mutate(interp.count.ratio = interpolated/counted)



interpolated_summary_spp_combined <- sbirds_with_interpolated %>% 
  mutate(data.type = ifelse(OBSERVERS == "interpolated data", "interpolated", "counted"),
         data.type = ifelse(is.na(data.type), "counted", data.type)) %>% 
  group_by(data.type) %>% 
  summarize(total.count = sum(count)) %>% 
  ungroup()

interpolated_summary_spp_combined_wide <- interpolated_summary_spp_combined %>% 
  pivot_wider(names_from = data.type, values_from = total.count) %>% 
  mutate(alpha.code = "all species combined",
         interp.count.ratio = interpolated/counted)


interpolated_summary_wide <- rbind(interpolated_summary_spp_combined_wide, interpolated_summary_by_spp_wide) %>% 
  mutate(interp.count.ratio = round(interp.count.ratio, 3))

write.csv(interpolated_summary_wide, "figures_output/data_summary_tables/interpolation_summary.csv", row.names = F)


## additional checking ---- NO RUN ----
# how many total birds did this add for each species
compare_added_by_interp_method <- function(interpolated_sbirds) {
total_interp_added <- interpolated_sbirds %>% 
  sbirds_add_site_names()%>% 
  group_by(season.year, SITE_ABBR, alpha.code) %>% 
  summarise(total.interp.added.mean = sum(mean.count, na.rm = T) %>% round(., 0),
            total.interp.added.med = sum(med.count, na.rm = T) %>% round(., 0))

counted_vals_string <- interpolated_sbirds %>% 
  filter(count.type == "counted") %>% 
  group_by(season.year, SITE_ABBR, alpha.code) %>% 
  summarise(counted.string = paste(count, collapse=", "))

total_interp_added_counted <- full_join(total_interp_added, counted_vals_string)
}

total_interp_added_counted <- compare_added_by_interp_method(interpolated_sbirds)


filter(total_interp_added_counted, total.interp.added.mean >= 100) %>% 
  write.csv("figures_output/data_summary_tables/shorebirds_added_by_interpolation.csv", row.names = F)
