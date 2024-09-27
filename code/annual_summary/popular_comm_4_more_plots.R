

library(tidyverse)

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/analysis_utility_functions.R")
source("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/code_shorebird_trend_analysis/analysis_sbirds_0_sbird_util_functions.R")

options(scipen = 999)

modeled_spp <- data.frame(species = c("all", "DUNL", "WESA", "MAGO", "LESA", "SAND", "WILL", "DOSP", "BBPL", "BLTU", "YELL", "SEPL", "KILL", "SPSA", "SNPL"))


dat_est_lean <- readRDS("popular_communication/pop_comm_data/dat_est_lean")


# trend for all species combined
dat_est_lean %>% 
  filter(species == "all") %>% 
  group_by(year) %>% 
  summarise(baywide.predicted = sum(predicted)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = baywide.predicted)) +
  geom_line(aes(x = year, y = baywide.predicted))+
  ylab("") +
  ylim(0, 16000) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5))+
  theme_bw() +
  theme(text = element_text(size=16, face="bold"),
    axis.text = element_text(colour="black"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )


ggsave("popular_communication/standalone_figures/all_spp_baywide_trend.png", height = 5, width = 6, bg = "transparent", dpi = 300)




# species proportions in first year of study ----
spp_1989 <- filter(dat_est_lean, year == 1989, species != "all")
all_1989 <- filter(dat_est_lean, year == 1989, species == "all")

spp_1989 %>% 
  mutate(total.predicted = sum(predicted),
         spp.prop = 100 * (predicted/total.predicted)) %>% 
  arrange(-spp.prop) %>% 
  mutate(cum.prop = cumsum(spp.prop)) %>% view()


spp_1989 %>% 
  mutate(spp.prop = 100 * (predicted/all_1989$predicted)) %>% 
ggplot() +
  geom_col(aes(x = spp.prop, y = reorder(common.name, spp.prop))) +
  ylab("") +
  xlab("% of total") +
  #ggtitle("1989 Tomales Bay shorebirds") +
  theme_bw() +
  xlim(0, 70) +
  geom_text(aes(x = spp.prop, y = reorder(common.name, spp.prop), label = round(predicted, 0)), hjust = -0.5)+
  theme(text = element_text(size=16, face="bold"),
    axis.text = element_text(colour="black"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

ggsave("popular_communication/standalone_figures/spp_percentages_1989.png", height = 5, width = 6, bg = "transparent", dpi = 300)

# species proportions in last year of study ----
spp_2018 <- filter(dat_est_lean, year == 2018, species != "all")
all_2018 <- filter(dat_est_lean, year == 2018, species == "all")


spp_2018 %>% 
  mutate(spp.prop = 100 * (predicted/all_2018$predicted),
         common.name = as.factor(common.name)) %>% 
  mutate(common.name = factor(common.name, levels = c("Dunlin",
                              "Western Sandpiper",
                              "Least Sandpiper",
                              "Marbled Godwit",
                              "Sanderling",
                              "Willet",
                              "Black-bellied Plover",
                              "Dowitcher spp.",
                              "Semipalmated Plover",
                              "Black Turnstone",
                              "Killdeer",
                              "Yellowlegs spp.",
                              "Snowy Plover",
                              "Spotted Sandpiper"))) %>% 
ggplot() +
  geom_col(aes(x = spp.prop, y = fct_reorder(common.name, desc(common.name)))) +
  ylab("") +
  xlab("% of total") +
  #ggtitle("1989 Tomales Bay shorebirds") +
  theme_bw() +
  xlim(0, 70) +
  geom_text(aes(x = spp.prop, y = fct_reorder(common.name, desc(common.name)), label = round(predicted, 0)), hjust = -0.5)+
  theme(text = element_text(size=16, face="bold"),
    axis.text = element_text(colour="black"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

ggsave("popular_communication/standalone_figures/spp_percentages_2018.png", height = 5, width = 6, bg = "transparent", dpi = 300)


# rain effect plot ----


rain_spp <- c("all",
              "DUNL", 
              "LESA", 
              "SAND", 
              "WILL", 
              "DOSP",
              "BBPL", 
              "YELL", 
              "KILL",
              "SNPL")


get_rain_estimates <- function(zspp) {
# rain preictions made in output_5_local_effects_plot.R
#zspp <- zspp %>% as.factor() %>% droplevels() %>% as.character()
if(zspp == "all") {
  sp_name = "All shorebirds"
} else {
sp_name <- bird_taxa_filter(data.frame(species = zspp))$common.name[1]
}
#-- bandaid for SAND, 9/11/2020 get_median_year stopped working for SAND, can't figure out why
if(zspp == "SAND") {
  zpred <- read_rain_preds(zspp) %>% 
    filter(year == 2013)
} else {
zpred <- read_rain_preds(zspp) %>% 
  get_median_year() 
}
  
  
zpred <- zpred %>% 
  rename(Year = year, Section = section) %>% 
  mutate(Section = ifelse(Section == "N", "North", "South"),
         spp = sp_name)
}



rain_estimates <- map_df(rain_spp, get_rain_estimates)

baywide_rain_est <- rain_estimates %>% 
  group_by(spp, seas.rain.mm) %>% 
  summarise(baywide.predicted = sum(predicted),
            baywide.lci = sum(lci),
            baywide.uci = sum(uci)) %>% 
  ungroup() %>% 
  group_by(spp) %>% 
  mutate(percent.change.rain = baywide.predicted/max(baywide.predicted))
  

baywide_rain_est %>% 
  filter(spp != "All shorebirds") %>% 
ggplot(aes(x = (seas.rain.mm/10), y = percent.change.rain * 100, color = spp)) +
  geom_line(size = 2) +  
  theme_bw() +
  theme(legend.title=element_blank()) +
  xlab("Cumulative rainfall (cm)") +
  ylab("% of maximum abundance")+
  theme(text = element_text(size=16, color = "black", face="bold"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

ggsave("popular_communication/standalone_figures/rain_effect.png", height = 5, width = 8, bg = "transparent", dpi = 300)


# raptor effect plot ----
rapt_spp <- c("SAND", "WILL", "KILL")

read_rapt_preds <- function(zspp) {
  
preds <- readRDS(paste("analysis_objects/best_model_estimates_with_interpolated_amke_4local/", zspp, "_best_est_amke_4local", sep = "")) %>% 
    filter(seas.rain.mm == 600)

  return(preds)
}

rapt_preds <- map_df(rapt_spp, read_rapt_preds)

rapt_preds %>% 
  bird_taxa_filter() %>%
  filter(year == 2005) %>% 
  group_by(common.name, mean.bird.hunters) %>% 
  summarise(baywide.rapt = sum(mean.bird.hunters),
            baywide.predicted = sum(predicted),
            baywide.lci = sum(lci),
            baywide.uci = sum(uci)) %>% 
  ungroup() %>% 
  group_by(common.name) %>% 
  mutate(percent.change.rapt = baywide.predicted/max(baywide.predicted))%>% view()
ggplot(aes(x = baywide.rapt, y = percent.change.rapt * 100, color = common.name)) +
  geom_line(size = 2) +
  ylim(0, 100) +
  scale_x_continuous(breaks = seq(2, 20, by = 2)) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  xlab("Number of bird hunting raptors") +
  ylab("% of maximum abundance")+
  theme(text = element_text(size=16, color = "black", face="bold"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
+
  facet_wrap(~section, scales = "free_x")

ggsave("popular_communication/standalone_figures/raptor_effect.png", height = 5, width = 8, bg = "transparent", dpi = 300)


# giacomini effect


giac_spp <- c("DUNL", "DOSP", "BBPL", "LESA", "YELL")
up_down_spp <- c("DUNL", "DOSP", "BBPL")

current_file_mod = "with_interpolated_amke"
current_file_mod_short = "_amke"

read_giac_est <- function(zspp) {
zpred <- readRDS(paste("analysis_objects/best_model_estimates_", current_file_mod, "/", zspp, "_best_est", current_file_mod_short, sep = ""))   
   
zpred <- zpred %>% 
  rename(Year = year, Section = section) %>% 
  mutate(Section = ifelse(Section == "N", "North", "South"))

}

giac_ests <- map_df(giac_spp, read_giac_est)



giac_ests <- giac_ests %>% 
  filter(Section == "South") %>% 
  group_by(species) %>% 
  mutate(per.max = 100 * predicted/max(predicted)) %>% 
  bird_taxa_filter()


giac_ests %>% 
  mutate(up.down = ifelse(alpha.code %in% up_down_spp, "ayes", "no")) %>% 
ggplot() +
  geom_line(aes(x = Year, y = per.max, color = common.name), size = 2) +
  ylim(0, 100) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  ylab("% of maximum abundance")+
  theme(text = element_text(size=16, color = "black", face="bold"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) +
  facet_wrap(~up.down, ncol = 1)+ 
theme(
  strip.background = element_blank(),
  strip.text.x = element_blank()
) +
  geom_vline(xintercept = 2008.5, size = 2, linetype = "dashed")

ggsave("popular_communication/standalone_figures/giac_effect.png", height = 5, width = 8, bg = "transparent", dpi = 300)

