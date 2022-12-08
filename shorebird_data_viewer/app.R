#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

sbirds=read.csv("data/SHOREBIRD20190311.csv")

sbirds.long <- sbirds %>% 
  gather(species, count, -ID, -COUNT.AREA, -MONTH, -DAY, -YEAR, -START.TIME, -END.TIME, -OBSERVERS) %>% 
  mutate(date = mdy(paste(MONTH, DAY, YEAR, sep = "-"))) %>% 
  select(SITE_ABBR = COUNT.AREA, everything(), -ID) %>% 
  mutate(count = as.numeric(count))


sbirds.sites=read.csv("data/SHOREBIRD_SITES.csv")%>% 
  select(-ID)



sbirds.long.sites <- sbirds.long %>% 
  full_join(sbirds.sites, by = c("SITE_ABBR")) %>% 
  select(MONTH, DAY, YEAR, species, count, date, SITE_ABBR, North_South_Code) %>% 
  filter(!is.na(North_South_Code)) %>% 
  filter(!(species %in% c("ENTERED.BY", "NOTES...OTHER.SPECIES", "PROOFED")), !is.na(species)) %>% 
  arrange(date, species, North_South_Code) %>% 
  mutate(season = NA,
         season = ifelse(MONTH > 3 & MONTH < 6, "spring", season),
         season = ifelse(MONTH > 6 & MONTH < 10, "fall", season),
         season = ifelse(MONTH >= 10, "winter", season),
         season = ifelse(MONTH <= 3, "winter", season),
         study.year = ifelse(MONTH < 6, YEAR - 1, YEAR),
         season.year = paste(season, study.year, sep = "_"))


  sbirds.year.ns.summ <- sbirds.long.sites  %>%
    filter(MONTH < 3 | MONTH >10) %>% 
    group_by(date, species, North_South_Code) %>%
    summarise(date.sp.total = sum(count)) %>%
    mutate(season = ifelse(month(date) > 10, year(date), year(date) - 1),
           early.late = ifelse(month(date) > 10, "early", "late"),
           date.sp.total = ifelse(is.na(date.sp.total), 0, date.sp.total),
           date.sp.total = ifelse(is.nan(date.sp.total), 0, date.sp.total),
           date.sp.total = ifelse(date.sp.total < 0, 0, date.sp.total)) %>%
    group_by(season, early.late, species, North_South_Code) %>%
    summarise(sp.mean = mean(date.sp.total),
              sp.sd = sd(date.sp.total)) %>%
    arrange(season, species, North_South_Code, early.late)
  
  
####################3
ui = fluidPage(div(style = "background-color: skyblue;",
                   
                   fluidRow(column(7, offset = 1, h1("Explore numbers of wintering shorebirds on Tomales Bay.", 
                                                     style = "font-family: 'Source Sans Pro';"))),
                   
                   
                   
                   fluidRow(column(3, offset = 1, 
                                   div(style = "height:70px", 
                                       selectInput(inputId = "species", 
                                                   label = "Select a species", 
                                                   choices = sort(unique(sbirds.year.ns.summ$species)))))),
                   
                   fluidRow(column(12,
                                   plotOutput("SbirdPlot")))))


## the server part  
server = function(input, output) {
  
observe({

  output$SbirdPlot <-  renderPlot({
    zspecies = input$species
  

  fooz1 <- sbirds.year.ns.summ %>% 
    filter(species == zspecies)
  
   ggplot(data = fooz1, aes(x = season, y = sp.mean)) +
    geom_line(aes(color = North_South_Code, linetype = early.late))+
    ggtitle(zspecies)
  
  })
  

})  



  
}



shinyApp(ui, server)
