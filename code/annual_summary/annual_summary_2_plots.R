

library(plotly)
library(htmlwidgets)
library(cowplot)
library(flextable)
library(gridExtra)
library(webshot)
library(magick)
#source("code_shorebird_trend_analysis/analysis_sbirds_0_sbird_util_functions.R")

# overal change in raw numbers ----
zspp = "all"  
sbirds_winter <- sbird_data_reader(zspp, "winter") %>% 
  group_by(year) %>% 
  summarise(total.shorebirds = sum(p75.total.sbirds))

ggplot(sbirds_winter, aes(x = year, y = total.shorebirds)) +
  stat_smooth(se = F, aes(color = "red", size = 2))  +
  geom_point(aes(size = 2)) +
  theme_classic() +
  theme(text = element_text(size=20, face= "bold", colour= "black"),
        legend.position = "none") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_x_continuous(limits = c(1989, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  ylab("") +
  xlab("") +
  ggtitle("Total Shorebirds")

#ggsave("popular_communication/popular_comm_figs/total_shorebird_year_wide.png", height = 8, width = 12, bg = "transparent")

#
#### percent change plot ----

# using dat_est_lean from popular_comm_1_prep_data.r
# which is saved here:
# include full path so this works when sourced in popular_comm_3_output.RMD
per_change_df <- readRDS("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/pop_comm_data/percent_change")

#levs <- table4plot %>% 
#  mutate(zlevs = paste(common.name, collapse = ", "))



per_change_plotter <- function(per_change_df) {
   zplot <-  ggplot(data = per_change_df, aes(x = per.change, y = common.name,
                    text = paste(common.name, "\n",
                                 "% change: ", per.change, "\n",
                                 "Estimated abundance (95% CI) \n",
                                 "1989: ", yr1989, "\n",
                                 "2018: ", yr2018, sep = ""))) +
    #geom_segment(aes(x = -100, y = common.name, xend = ifelse(per.change < 0, per.change, 0), yend = common.name), color = 'gray85') +
    geom_segment(aes(x = -100, y = common.name, xend = max(per.change), yend = common.name), color = 'gray85') +
  geom_point(aes(color = per.change, size = 3)) +
  geom_segment(aes(x = per.change, y = common.name, xend = 0, yend = common.name, color = per.change)) +
  geom_vline(xintercept = 0)  +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("")
   return(zplot)
}

# zback_color = "gray50" # color to match slide background fro docent training slides
zback_color = "white"
all_others <- per_change_df  %>% 
  filter(!common.name %in% c("Yellowlegs spp.")) %>% 
  per_change_plotter() +
  scale_x_continuous(breaks = c(-100, -50, 0, 50)) +
  scale_color_gradient2(low = "red", mid = "gray75", high = "blue")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  +
  theme(text = element_text(size=20, face= "bold", colour= "black")) +
  theme(panel.background = element_rect(fill = zback_color, colour = NA),  
        plot.background = element_rect(fill = zback_color, colour = NA)) 



yell <- per_change_df %>%
  filter(common.name == "Yellowlegs spp.") %>% 
  mutate(common.name = "Yellowlegs spp.") %>% 
  per_change_plotter() +
  scale_x_continuous(breaks = c(-100, 0, 100, 500, 1000)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(text = element_text(size=20, face= "bold", colour= "black")) +
  theme(panel.background = element_rect(fill = zback_color, colour = NA),  
        plot.background = element_rect(fill = zback_color, colour = NA)) 


title <- ggdraw() + 
  draw_label(
    "Percent change in wintering shorebird abundance, \n Tomales Bay, CA, 1989 to 2018",
    fontface = 'bold',
    size = 16)

foot <- ggdraw() + 
  draw_label(
    "hover over points for more information\n*note different scale for Yellowlegs",
    size = 10,
    x = 0.25,
    vjust = -3)

#per_cow_head_foot <- plot_grid(title, all_others, yell, foot, align = "v", nrow = 4, rel_heights = c(1/5, 1, 1/5, 1/6))

#per_cow_head <- plot_grid(title, all_others, yell, align = "v", nrow = 3, rel_heights = c(1/5, 1, 1/5))


#ggsave2("popular_communication/popular_comm_figs/condor20_135__graphical_abstract.png", per_cow_head, dpi = 1200, width = 8, height = 8)


# per_cow <- make_cow_plot(all_others, yell)
#ggsave2("popular_communication/popular_comm_figs/overall_percent_change.png", per_cow, height = 8, width = 6, units = "in")

# per_cow_notitle <- plot_grid(all_others, yell, align = "v", nrow = 2, rel_heights = c(1, 1/5))

 # ggsave2("popular_communication/popular_comm_figs/overall_percent_change_notitle.png", per_cow_notitle, height = 8, width = 8, units = "in",  bg = "transparent")



# -- plotly??
make_everyone_plotly <- function(zall_others, zyell) {
all_others_plotly <- ggplotly(all_others, tooltip = "text") 
yell_plotly <- ggplotly(yell, tooltip = "text")

everyone_plotly <- subplot(all_others_plotly, yell_plotly, margin = 0.04, nrows = 2, heights = c(0.9, 0.1)) %>% 
  layout(title = "Percent change in shorebird abundance, Tomales Bay, CA, 1989 to 2018",
         font = list(size = 16)) #%>%
  #add_annotations(text="*note different scale for Yellowlegs", xref="paper", yref="paper", x=0.1, xanchor="left", y=-0.07, yanchor="top", legendtitle=FALSE, showarrow=FALSE, align = "left") 
return(everyone_plotly)
}

# everyone_plotly <- make_everyone_plotly(all_others, yell)


#everyone_plotly %>% htmlwidgets::saveWidget("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/percent_change.html")
  
  

# plot all species together, facetted y abundance group ----
# dat_est_lean <- readRDS("popular_communication/pop_comm_data/dat_est_lean")

# glitchy - facets end up different widths after ggplotly()
make_trend_plotly <- function(zdat_est_lean) {
trend_plot_all <- zdat_est_lean %>% 
  #filter(species == zspp) %>% 
  ggplot(aes(x = year, y = p75.total.sbirds, group = common.name,
             text = paste(common.name, ", ", year, "\n",
                          "Actual abundance: ", floor(p75.total.sbirds), "\n",
                          "Estimated abundance: ", floor(predicted), "\n",
                          "Cumulative rain (cm): ", seas.rain.cm, "\n",
                          "Raptor abundance: ", round(mean.bird.hunters, 1), sep = ""))) +
  #geom_point(aes(color = common.name)) +
  geom_line(aes(x = year, y = predicted, color = common.name)) +
  #geom_ribbon(aes(x = year, ymin=lci,ymax=uci),alpha=0.3) +
  theme_classic() +
  #theme(legend.title=element_blank(),
   #     legend.position = "none") +
  scale_x_continuous(limits = c(1989, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("Year") +
  ylab("Shorebird abundance") + 
  facet_wrap(~abund.group.name, scales = "free") +
  scale_color_discrete(name = "\n Click species name \n to hide/show")
  
trend_plotly <- ggplotly(trend_plot_all, tooltip = "text") %>% 
  layout(margin = list(t = 50, b = 50, l = 50, r = 50))
return(trend_plotly)
}

# trend_plotly <- make_trend_plotly(dat_est_lean)


# plot trends for abundance groups separately ----


pop_plotter_abund_group <- function(zdat_est_lean, zabund.group, include.points = F) {

  
pop_plot <- zdat_est_lean %>% 
  filter(abund.group == zabund.group) %>% 
  ggplot(aes(x = year, y = p75.total.sbirds, group = common.name,
             text = paste(common.name, ", ", year, "\n",
                          "Counted abundance: ", floor(p75.total.sbirds), "\n",
                          "Modelled abundance: ", floor(predicted), "\n",
                          "Cumulative rain (cm): ", seas.rain.cm, "\n",
                          "Raptor abundance: ", round(mean.bird.hunters, 1), sep = "")))+
  geom_line(aes(x = year, y = predicted, color = common.name)) +
  #geom_ribbon(aes(x = year, ymin=lci,ymax=uci),alpha=0.3) +
  theme_classic() +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits = c(1989, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("Year") +
  ylab("Shorebird abundance")

if(include.points == T) {
  pop_plot <- pop_plot +
  geom_point(aes(color = common.name), alpha = 0.3) 
} else {
  pop_plot <- pop_plot
}

return(pop_plot)
}




pop_plotlyer <- function(zpop_plot, zannotate = F) {
pop_plotly <- ggplotly(zpop_plot, tooltip = "text") %>% 
  layout(margin = list(t = 50, b = 50, l = 50, r = 100),
         legend=list(y=0.9, yanchor="top" )) 

# this optional to add annotation to the plot
# first choice is to have this info in the doc text
if(zannotate == T) {
pop_plotly <- pop_plotly %>%
  add_annotations(text="Click species name \n to hide/show", xref="paper", yref="paper",
                  x=1.02, xanchor="left",
                  y=0.9, yanchor="bottom",    # Same y as legend below
                  legendtitle=TRUE, showarrow=FALSE, align = "left") %>%
  add_annotations(text="Dots are counted \n abundance", xref="paper", yref="paper",
                  x=1.02, xanchor="left",
                  y=0.4, yanchor="top",    # Same y as legend below
                  legendtitle=FALSE, showarrow=FALSE, align = "left")  %>%
  add_annotations(text="Lines are modelled \n abundance", xref="paper", yref="paper",
                  x=1.02, xanchor="left",
                  y=0.3, yanchor="top",    # Same y as legend below
                  legendtitle=FALSE, showarrow=FALSE, align = "left") 
} else {
  pop_plotly <- pop_plotly 
}



return(pop_plotly)
}


# l100_plot <- pop_plotter_abund_group(1, include.points = T) + ggtitle("Max abundance <100") + theme(legend.position = c(0.6, 0.8)) + xlab("") 

#l100_plotly <- pop_plotter_abund_group(1, include.points = T) %>% 
#   pop_plotlyer()

  #ggsave("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/pop_plot_l100.png", l100_plot, height = 6, width = 4)

#  htmlwidgets::saveWidget("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/pop_plot_l100.html")
  
  
# b100_1000_plot <- pop_plotter_abund_group(2) + ggtitle("Max abundance 100-1000") + theme(legend.position = c(0.7, 0.8)) + xlab("") + ylim(0, 1000)

# b100_1000_plotly <- pop_plotter_abund_group(2, include.points = T) %>% pop_plotlyer()

  #ggsave("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/pop_plot_100_1000.png", b100_1000_plot, height = 6, width = 4)

#  htmlwidgets::saveWidget("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/pop_plot_l100.html")

# g1000_plot <- pop_plotter_abund_group(3) + ggtitle("Max abundance >1000") + theme(legend.position = c(0.7, 0.8)) + xlab("") 

# g1000_plotly <- pop_plotter_abund_group(3, include.points = T) %>% pop_plotlyer()

  #ggsave("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/pop_plot_100_1000.png", b100_1000_plot, height = 6, width = 4)

#  htmlwidgets::saveWidget("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/pop_plot_l100.html")


# all_trend_plotly <- subplot(g1000_plotly, b100_1000_plotly, l100_plotly)%>% layout(annotations = list(list(x = 0.1 , y = 1.05, text = "Max. abundance >1000", showarrow = F, xref='paper', yref='paper'), list(x = 0.5 , y = 1.05, text = "Max. abundance 100-1000", showarrow = F, xref='paper', yref='paper'), list(x = 0.9 , y = 1.05, text = "Max. abundance <100", showarrow = F, xref='paper', yref='paper'), list(x = 0.5 , y = -0.15, text = "Year", showarrow = F, xref='paper', yref='paper')))

#  all_trend_plotly %>% htmlwidgets::saveWidget("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/all_trend_plotly.html")

# local effects plots ----
#rapt_preds <- readRDS("popular_communication/pop_comm_data/rapt_preds")


rapt_effect_plotly <- function(zrapt_preds, zspp) {
rapt_effect_plot <- zrapt_preds %>% 
  filter(species == zspp) %>% 
  ggplot(aes(x = mean.bird.hunters, y = predicted, group = interaction(common.name, section),
             text = paste(common.name, "\n",
                          section, " section of bay", "\n",
                          "Modelled abundance: ", floor(predicted), "\n",
                          "Raptor abundance: ", round(mean.bird.hunters, 1), sep = "")))+
  geom_line(aes(x = mean.bird.hunters, y = predicted, linetype = section)) +
  theme_classic() +
  theme(legend.title=element_blank()) + 
  xlab("Mean raptor abundance") +
  ylab("Modeled abundance") 

rapt_effect_plotly <- ggplotly(rapt_effect_plot, tooltip = "text")  

return(rapt_effect_plotly)
}

# rapt_plotly <- rapt_effect_plotly(rapt_effect)

rapt_effect_bay_plotly <- function(zrapt_preds) {
rapt_pred_bay <- zrapt_preds %>% 
  filter(mean.bird.hunters <= 11) %>% 
  group_by(year, common.name, mean.bird.hunters) %>% 
  summarise(mean.bird.hunters.bay = sum(mean.bird.hunters),
            pred.bay = sum(predicted)) %>% 
  ungroup() %>% 
  select(-mean.bird.hunters) %>% 
  rename(mean.bird.hunters = mean.bird.hunters.bay, predicted = pred.bay)

rapt_pred_bay_plot <- rapt_pred_bay %>% 
  ggplot(aes(x = mean.bird.hunters, y = predicted, group = common.name,
             text = paste(common.name, "\n",
                          "Modelled abundance: ", floor(predicted), "\n",
                          "Raptor abundance: ", round(mean.bird.hunters, 1), sep = "")))+
  geom_line(aes(x = mean.bird.hunters, y = predicted)) +
  theme_classic() +
  theme(legend.title=element_blank()) +
  xlab("Raptor abundance") +
  ylab("Modeled abundance") +
  facet_wrap(~common.name, scales = "free")

rapt_effect_bay_plotly <- ggplotly(rapt_pred_bay_plot, tooltip = "text")  
return(rapt_effect_bay_plotly)
}


# rapt_effect_plotly %>% htmlwidgets::saveWidget("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/rapt_effects_plot.html")


#for (i in 1:length(rapt_effect_plotly$x$data)){
#    if (!is.null(rapt_effect_plotly$x$data[[i]]$name)){
#        rapt_effect_plotly$x$data[[i]]$name =  gsub("\\(","", rapt_effect_plotly$x$data[[i]]$name)
#        rapt_effect_plotly$x$data[[i]]$name =  gsub(",1\\)","", rapt_effect_plotly$x$data[[i]]$name)
#        rapt_effect_plotly$x$data[[i]]$name =  gsub(",",", ", rapt_effect_plotly$x$data[[i]]$name)
#    }
#}




# rain_preds <- readRDS("popular_communication/pop_comm_data/rain_preds")


make_rain_effect_plotly <- function(zrain_preds) {
rain_effect_plot <- zrain_preds %>% 
  ggplot(aes(x = seas.rain.mm/10, y = predicted, group = common.name,
             text = paste(common.name, "\n",
                          "Modeled abundance: ", floor(predicted), "\n",
                          #"Cumulative rain (cm): ", seas.rain.cm, "\n",
                          "Rainfall (cm): ", seas.rain.mm/10, sep = "")))+
  #geom_point(aes(x = mean.bird.hunters, y = predicted, color = common.name)) +
  geom_line(aes(x = seas.rain.mm/10, y = predicted, color = common.name)) +
  #geom_ribbon(aes(x = year, ymin=lci,ymax=uci),alpha=0.3) +
  theme_classic() +
  theme(legend.title=element_blank()) +
  #theme(axis.text.y=element_text(vjust=5, hjust=-1)) +
  xlab("Rainfall (cm)") +
  ylab("Modeled abundance") +
  facet_wrap(~pred.group.name, scales = "free")


rain_effect_plotly <- ggplotly(rain_effect_plot, tooltip = "text") 
return(rain_effect_plotly)
}


# rain_effect_plotly %>%htmlwidgets::saveWidget("C:/Users/scott.jennings/Documents/Projects/shorebirds/2019_20_shorebird_trend_analysis/popular_communication/popular_comm_figs/rain_effects_plot.html")
