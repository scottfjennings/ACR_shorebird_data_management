

# tools and products to visualize the most important results from this paper, for popular audiences

# packages, source ----
library(tidyverse)
library(lubridate)
library(MASS)
library(here)

library(birdnames)
custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

zyear = 2021

zspp_list <- c("All", "DUNL", "WESA", "MAGO", "LESA", "SAND", "WILL", "DOSP", "BBPL", "BLTU", "YELL", "SEPL", "KILL", "SPSA")

# read, combine data ----
sbirds <- readRDS(here("data_files/rds/shorebirds_for_analysis"))%>% 
  rename(section = North_South_Code)

rapt <- readRDS(here("data_files/rds/raptors4analysis")) %>%     
  mutate(date = as.Date(date)) %>% 
  rename(section = North_South_Code) %>% 
  filter(grepl("winter", season.year), bird.hunter == 1, count >= 0) %>% 
  #filter(grepl("winter", season.year), species %in% c("PEFA", "MERL"), count >= 0, season.year != "winter_2019") %>% 
  group_by(date, section) %>% 
  summarise(tot.count = sum(count)) %>% 
  ungroup() %>% 
  mutate(year = ifelse(month(date) < 6, year(date)-1, year(date))) %>% 
  group_by(year, section) %>% 
  summarise(mean.bird.hunters = mean(tot.count),
            sd.bird.hunters = sd(tot.count)) %>% 
  ungroup() 

# rain from C:/Users/scott.jennings/Documents/Projects/general_data_sources/Rainfall/get_PRISM.R
rain <- read.csv("C:/Users/scott.jennings/Documents/Projects/general_data_sources/Rainfall/derived_data/tomales_watershed_mean_month_rain.csv") %>% 
  mutate(year = ifelse(month < 7, year - 1, year)) %>% 
  filter(month <= 2 | month > 7) %>% 
  group_by(year) %>% 
  summarise(seas.rain.mm = sum(mean.rain),
            seas.rain.mm = round(seas.rain.mm)) %>% 
  filter(year > 1988)


# annual estimate each species
analysis_table <- sbirds %>% 
  group_by(season.year, date, section, alpha.code) %>% 
  summarize(total.sbirds = sum(count)) %>% 
  ungroup() %>% 
  group_by(season.year, section, alpha.code) %>% 
  summarise(mean.total.sbirds = floor(mean(total.sbirds)),
            sd.total.sbirds = sd(total.sbirds),
            max.total.sbirds = max(total.sbirds),
            p75.total.sbirds = quantile(total.sbirds, probs = c(0.75)),
            num.surveys = n()) %>% 
  ungroup() %>%
  bind_rows(., sbirds %>% 
              group_by(season.year, date, section) %>% 
              summarize(total.sbirds = sum(count)) %>% 
              ungroup() %>% 
              group_by(season.year, section) %>% 
              summarise(mean.total.sbirds = floor(mean(total.sbirds)),
                        sd.total.sbirds = sd(total.sbirds),
                        max.total.sbirds = max(total.sbirds),
                        p75.total.sbirds = quantile(total.sbirds, probs = c(0.75)),
                        num.surveys = n()) %>%
              ungroup() %>% 
              mutate(alpha.code = "All")) %>% 
  separate(season.year, c("season", "year"), remove = F) %>% 
  mutate(year = as.numeric(year),
         giac.dummy = ifelse(year < 2009, 0,
                             ifelse(section == "S", 1, 0))) %>% 
  full_join(., rain) %>% 
  full_join(rapt) %>% 
  filter(year <= zyear)


mean_preds <- analysis_table %>% 
  group_by(section) %>% 
  summarise(seas.rain.mm = mean(seas.rain.mm, na.rm = TRUE),
            mean.bird.hunters = mean(mean.bird.hunters, na.rm = TRUE))

znewdat <- analysis_table %>% 
  distinct(year, section, giac.dummy) %>% 
  full_join(mean_preds)


# fit a general model, extract model predictions ----

big_mod_preds <- function(zspp) {
  sbirds_winter <- analysis_table %>% 
    filter(season == "winter", alpha.code == zspp)
  
big_mod <- glm.nb(floor(p75.total.sbirds) ~ poly(year, 2) * section + giac.dummy + seas.rain.mm + mean.bird.hunters, data = sbirds_winter, maxit = 100)

ilink <- family(big_mod)$linkinv
 all_best_pred = predict(big_mod, znewdat, se.fit=TRUE, type='link') %>% 
  data.frame() %>% 
  cbind(znewdat) %>% 
  ungroup() %>% 
  mutate(predicted = ilink(fit),
         lci = ilink(fit - (1.96 * se.fit)),
         uci = ilink(fit + (1.96 * se.fit)),
         alpha.code = zspp)
}

zz <- big_mod_preds("All")


all_preds <- map_df(zspp_list, big_mod_preds)




ztext <- spp_pred %>% 
  filter(year == zyear) %>% 
  mutate(predicted = round(predicted, 0),
         section = ifelse(section == "N", "North", "South"),
         sect.text = paste("an estimated", predicted, "birds in the", section, "section")) %>% 
  summarise(out.text = paste(sect.text, collapse = " and ")) %>% 
  mutate(out.spp = ifelse(zspp == "All", "all shorebirds combined", translate_bird_names(zspp, "alpha.code", "common.name")),
         out.text = paste("For ", out.spp, " in ", zyear, ", there were ", out.text, ".", sep = "")) %>% 
  dplyr::select(out.text)

sbird_out <- list(zplot = sbird_mod_plotter(spp_pred),
                  ztext = ztext,
                  all.pred = dplyr::select(spp_pred, year, section, predicted, lci, uci, alpha.code))


# convert estimates to percent change from 1989-2018 ----
all_est_lean <- all_preds %>% 
  dplyr::select(alpha.code, year, section, predicted, lci, uci) %>% 
  group_by(alpha.code, year) %>% 
  summarise(bay.pred = sum(predicted),
            bay.lci = sum(lci),
            bay.uci = sum(uci)) %>% 
  ungroup()

all_est_per_change <- all_est_lean %>% 
  filter(year %in% c(1989, 2018, zyear)) %>% 
  rename(predicted = bay.pred,
         lci = bay.lci,
         uci = bay.uci)



trimmed_ests_bay <- all_est_per_change %>% 
  rename(Year = year) %>% 
  mutate(predicted.form = formatC(predicted, big.mark = ",", format = "f", digits = 0),
           lci.form = formatC(lci, big.mark = ",", format = "f", digits = 0),
           uci.form = formatC(uci, big.mark = ",", format = "f", digits = 0),
           text4output = paste(predicted.form, " (", lci.form, "-", uci.form, ")", sep = "")) 

per_change_bay <- trimmed_ests_bay %>%  
  mutate(predicted = round(predicted, 3),
         Year = paste("yr", Year, sep = "")) %>% 
  select(Year, alpha.code, predicted) %>% 
  tidyr::spread(Year, predicted) %>% 
  mutate(change = round(yr2018, 0) - round(yr1989, 0),
         per.change = 100*(change/round(yr1989, 0)),
         per.change = round(per.change, 1)) %>% 
  select(alpha.code, per.change)

mod_ests_wider2_bay <- trimmed_ests_bay %>% 
  mutate(Year = paste("yr", Year, sep = "")) %>% 
  select(Year, species, text4output) %>% 
  tidyr::spread(Year, text4output)

mod_ests_wider_bay <- full_join(per_change_bay, mod_ests_wider2_bay) %>% 
  bird_taxa_filter() %>% 
  mutate(common.name = ifelse(alpha.code == "all", "All shorebirds", common.name)) %>% 
  arrange(per.change) %>% 
  dplyr::select(common.name, yr1989, yr2018, per.change) 


percent_change <- rbind(mod_ests_wider_bay %>% 
  filter(common.name == "All shorebirds"), 
  mod_ests_wider_bay %>% 
  filter(common.name != "All shorebirds") %>% 
  arrange(per.change)) %>%
  mutate(common.name = factor(common.name, levels = rev(c("All shorebirds", "Western Sandpiper", "Black-bellied Plover", "Killdeer", "Semipalmated Plover", "Dunlin", "Spotted Sandpiper", "Sanderling", "Dowitcher spp.", "Black Turnstone", "Willet", "Marbled Godwit", "Least Sandpiper", "Snowy Plover", "Yellowlegs spp."))))

saveRDS(percent_change, "popular_communication/pop_comm_data/percent_change")

rm(all_est_lean, all_est_98_18, trimmed_ests_bay, per_change_bay, mod_ests_wider2_bay, mod_ests_wider_bay, percent_change)

# estimates for best models for effect of local variables ----

rapt_preds <- readRDS("analysis_objects/best_model_estimates_with_interpolated_amke_4local/all_rapt_spp_best_est_amke_4local") %>% 
  bird_taxa_filter(drop_cols = c("species.number", "order", "family", "subfamily", "genus", "species")) %>% 
  rename(species = alpha.code) %>% 
  get_median_year() %>% 
  mutate(section = ifelse(section == "N", "North", "South"))
   
saveRDS(rapt_preds, "popular_communication/pop_comm_data/rapt_preds")


# rain

rain_preds <- readRDS("analysis_objects/best_model_estimates_with_interpolated_amke_4local/all_rain_spp_best_est_amke_4local")%>% 
  bird_taxa_filter(drop_cols = c("species.number", "order", "family", "subfamily", "genus", "species")) %>% 
  rename(species = alpha.code) %>% 
  get_median_year() %>% 
  mutate(section = ifelse(section == "N", "North", "South")) %>% 
  mutate(common.name = ifelse(is.na(common.name) & species == "all", "All species combined", common.name)) %>% 
  group_by(year, common.name, seas.rain.mm) %>% 
  summarise(bay.pred = sum(predicted)) %>% 
  ungroup() %>% 
  rename(predicted = bay.pred)

 rain_pred_groups <- filter(rain_preds, seas.rain.mm == 600) %>% 
   distinct(common.name, predicted) %>% 
  mutate(pred.group = ifelse(predicted <= 100, 1, 
                              ifelse(predicted > 100 & predicted <= 1000, 2, 3)),
         pred.group.name = ifelse(predicted <= 100, "Abundance <100", 
                              ifelse(predicted > 100 & predicted <= 1000, "Abundance 100-1000", "Abundance >1000")),
         pred.group.name = factor(pred.group.name, levels = c("Abundance >1000", "Abundance 100-1000", "Abundance <100"))) %>% 
   select(-predicted)

 
 rain_preds <- full_join(rain_preds, rain_pred_groups, by = c("common.name"))

 saveRDS(rain_preds, "popular_communication/pop_comm_data/rain_preds")

  
  