


library(tidyverse)
library(birdnames)
library(here)

custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

options(scipen = 999)


# this has the lumped species split, with species headed with 'total.'
sbirds <- readRDS(here("data_files/rds/shorebirds_for_analysis")) 



# calculate median number of winter birds for each species and for all spp combined ----
make_med_sbirds_bysection <- function() {
all_plot_df <- sbirds %>% 
  filter(grepl("winter_", season.year), !is.na(count), count >= 0) %>% 
  ungroup() %>% 
  group_by(season.year, date, North_South_Code) %>% 
  summarise(date.total.all.spp = sum(count)) %>% 
  group_by(season.year, North_South_Code) %>%
  summarise(med.count = median(date.total.all.spp)) %>% 
  mutate(alpha.code = "all") %>% 
  select(season.year, alpha.code, North_South_Code, med.count) %>% 
  ungroup()

spp_plot_df <- sbirds %>% 
  filter(grepl("winter_", season.year), !is.na(count), count >= 0) %>% 
  ungroup() %>% 
  group_by(season.year, date, alpha.code, North_South_Code) %>% 
  summarise(date.total.each.spp = sum(count)) %>% 
  group_by(season.year, alpha.code, North_South_Code) %>%
  summarise(med.count = median(date.total.each.spp))  %>% 
  ungroup()%>% 
  mutate(alpha.code = as.character(alpha.code))

median_sbirds <- rbind(all_plot_df, spp_plot_df)
return(median_sbirds)
}
median_sbirds_section <- make_med_sbirds_bysection() %>% 
  bird_taxa_filter(keep_taxa = c("Charadriiformes", "all", "YELL", "LEGP", "DOSP", "PHAL")) %>%
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         common.name = as.character(common.name),
         common.name = ifelse(alpha.code == "all", "All shorebirds", common.name),
         common.name = ifelse(alpha.code == "DOSP", "Dowitcher", common.name),
         common.name = ifelse(alpha.code == "PHAL", "Phalarope", common.name),
         common.name = ifelse(alpha.code == "YELL", "Yellowlegs", common.name)) %>% 
  separate(season.year, c("season", "year"), remove = F) %>% 
  mutate(year = as.numeric(year))


make_med_sbirds <- function() {
all_plot_df <- sbirds %>% 
  filter(grepl("winter_", season.year), !is.na(count), count >= 0) %>% 
  ungroup() %>% 
  group_by(season.year, date) %>% 
  summarise(date.total.all.spp = sum(count)) %>% 
  group_by(season.year) %>%
  summarise(med.count = median(date.total.all.spp)) %>% 
  mutate(alpha.code = "all") %>% 
  select(season.year, alpha.code, med.count)

spp_plot_df <- sbirds %>% 
  filter(grepl("winter_", season.year), !is.na(count), count >= 0) %>% 
  ungroup() %>% 
  group_by(season.year, date, alpha.code) %>% 
  summarise(date.total.each.spp = sum(count)) %>% 
  group_by(season.year, alpha.code) %>%
  summarise(med.count = median(date.total.each.spp)) %>% 
  mutate(alpha.code = as.character(alpha.code)) %>% 
  ungroup()

median_sbirds <- rbind(all_plot_df, spp_plot_df)
return(median_sbirds)
}
median_sbirds <- make_med_sbirds() %>% 
  #bird_taxa_filter(keep_taxa = c("Charadriiformes", "all", "YELL", "LEGP", "DOSP", "PHAL")) %>%
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         common.name = as.character(common.name),
         common.name = ifelse(alpha.code == "all", "All shorebirds", common.name),
         common.name = ifelse(alpha.code == "DOSP", "Dowitcher", common.name),
         common.name = ifelse(alpha.code == "PHAL", "Phalarope", common.name),
         common.name = ifelse(alpha.code == "YELL", "Yellowlegs", common.name)) %>% 
  separate(season.year, c("season", "year"), remove = F) %>% 
  mutate(year = as.numeric(year))

filt_winter_sbirds <- filter(median_sbirds, alpha.code %in% c("DUNL", "WESA", "LESA", "all"))

ggplot(data = filt_winter_sbirds) +
  geom_point(data = filter(filt_winter_sbirds, year < 2021), aes(x = year, y = med.count, color = alpha.code)) +
  stat_smooth(data = filter(filt_winter_sbirds, year < 2021), aes(x = year, y = med.count, color = alpha.code)) + 
  geom_point(data = filter(filt_winter_sbirds, year == 2021), aes(x = year, y = med.count, color = alpha.code), size = 4) +
  theme_classic() +
  theme(legend.position=c(0.9, 0.9),
        legend.justification = c(1,1),
        panel.background = element_blank()) +
  scale_color_discrete(breaks=c("all", "DUNL", "LESA", "WESA"),
                      labels=c("All shorebirds", "Dunlin", "Least Sandpiper", "Western Sandpiper")) +
  scale_x_continuous(limits = c(1989, 2021), breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))  +
  labs(y = "Median number of individuals",
       x = "",
       main = "Winter shorebirds on Tomales Bay",
       size = "",
       color = "")


ggsave("figures_output/Winter_All_DUNL_LESA_WESA_2021.png", width = 5, height = 5, units = "in", dpi = 300)

# plot with line for all shorebirds combined plus lines for 1 or more species ----

zspp <- c("all", "DUNL", "WESA")

 ggplot(data = filter(median_sbirds, alpha.code %in% zspp)) +
    geom_point(aes(x = year, y = med.count, colour = common.name)) +
    stat_smooth(aes(x = year, y = med.count, colour = common.name)) +
    theme(legend.title=element_blank(),
        legend.position=c(0.9, 0.99),
        legend.justification = c(1,1),
        panel.background = element_blank()) + 
    ylab("Median number of individuals") +
    scale_x_continuous(limits = c(1989, 2018), breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
    xlab("")
  
ggsave(paste("figures_output/Winter_shorebirds_All_DUNL_WESA1200.png", sep = ""), width = 5, height = 2.5, units = "in", dpi = 1200)

######################
## all shorebirds, y section

all_wint_med <- median_sbirds_section %>% 
  filter(alpha.code == "all")



ggplot(data = all_wint_med) +
  geom_point(aes(x = year, y = med.count, color = section)) +
  stat_smooth(aes(x = year, y = med.count, color = section)) + 
  theme_classic() +
  theme(legend.title=element_blank(),
        legend.position=c(0.8, 0.9),
        legend.justification = c(1,1),
        panel.background = element_blank()) +
  ylab("Total shorebirds")  +
  scale_color_discrete(breaks=c("N", "S"),
                      labels=c("North", "South"))+
  scale_x_continuous(limits = c(1989, 2020), breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  xlab("") 
+
  ggtitle("Winter shorebirds on Tomales Bay")

ggsave("figures_output/Winter_All.png", width = 5, height = 5, units = "in", dpi = 300)

############################


sbird_plotter_1orAll <- function(zspp) {
 
  if(zspp == "all") {
    plot_df <- median_sbirds %>% 
      ungroup() %>% 
      group_by(year) %>% 
      summarize(count = sum(med.count)) %>% 
      mutate(alpha.code = "all")
  } else {
    plot_df <- median_sbirds  %>% 
      group_by(year, alpha.code) %>% 
      summarise(count = sum(med.count)) %>% 
      filter(alpha.code == zspp)
  }

 ggplot(data = plot_df) +
   geom_point(aes(x = year, y = count), colour = section) +
  stat_smooth(aes(x = year, y = count), colour = "black") + 
   theme_classic() +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
  ylab("Number of individuals") +
  scale_x_continuous(limits = c(1989, 2020), breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  xlab("")
ggsave(paste("figures_output/Winter_shorebirds_", zspp, ".jpg", sep = ""), width = 8, height = 4, units = "in")
}
zspp = c("all", "DUNL", "LESA", "MAGO", "WESA", "SAND")
map(zspp, sbird_plotter_1orAll)

sbird_plotter_1orAll("all")

############################

dunl_test <- winter_sbirds %>% 
  filter(alpha.code == "DUNL") %>%
  ungroup() %>% 
  select(-season.year, -alpha.code) %>% 
  gather(varb, count, -year)

ggplot(data = dunl_test)+
  geom_point(aes(x = year, y = count, color = varb)) +
  stat_smooth(aes(x = year, y = count, color = varb)) + 
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
  ylab("Number of individuals") +
  scale_x_continuous(limits = c(1989, 2018), breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  xlab("")


############################
all_winter_sbirds <- sbirds_use %>% 
  filter(grepl("winter_", season.year), count >= 0) %>% 
  group_by(date) %>% 
  summarise(tot.sbirds.date = sum(count))%>% 
  full_join(., select(sbirds_use, date, season.year)%>% 
              filter(grepl("winter_", season.year)) %>% 
              distinct()) %>% 
  group_by(season.year) %>% 
  summarise(mean.count = mean(tot.sbirds.date, na.rm = TRUE),
            med.count = median(tot.sbirds.date, na.rm = TRUE)) %>% 
  mutate(year = gsub("winter_", "", season.year),
         year = as.numeric(year))


ggplot(data = all_winter_sbirds) +
  geom_point(aes(x = year, y = mean.count)) +
  stat_smooth(aes(x = year, y = mean.count), color = "black") + 
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
  ylab("Number of individuals") +
  scale_x_continuous(limits = c(1989, 2018), breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  xlab("") +
  ylim(c(0, 15000)) +
  ggtitle("Winter shorebirds on Tomales Bay")


ggsave("figures_output/Winter_shorebirds_allSpp.jpg", width = 8, height = 4, units = "in")


###############################################
# trends by site

seas_year_summ <- sbirds %>% 
  group_by(season.year, site, alpha.code) %>% 
  summarize(seas.yr.mean = mean(count)) %>% 
  ungroup() %>% 
  separate(season.year, c("season", "year"), remove = F) %>% 
  mutate(year = as.numeric(year))


ggplot(data = filter(seas_year_summ, season == "winter", alpha.code == "WILL")) +
  geom_point(aes(x = year, y = seas.yr.mean)) +
  stat_smooth(aes(x = year, y = seas.yr.mean)) +
  facet_wrap(~site) +
  ylab("Mean abundance") +
  xlab("")+
  ggtitle("WILL, winter")
  
ggsave("figures_output/WILL_winter_bysite.jpg", height = 6, width = 8)
