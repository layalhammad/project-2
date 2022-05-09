library(tidyverse)
library(dplyr)
library(zoo)
library(maps)
library(ggplot2)
library(magrittr)
library(rvest)
library(RColorBrewer)
library(ggiraph)
library(shiny)
library(plotly)
library(crosstalk)
library(data.table)

#loading dataset of annual causes of death
death_causes = read.csv("DeathCauses.csv")

summary(death_causes)


#Changing Types of columns to facilitate analysis

death_causes$Causes.name = as.factor(death_causes$Causes.name)

death_causes$Entity = as.factor(death_causes$Entity)

death_causes$Code = as.factor(death_causes$Code)

#Dealing with NA, applying interpolation as it reflects a better estimate than avg

which(colSums(is.na(death_causes))>0)
 
death_causes = death_causes %>% group_by(Entity, Causes.name) %>% mutate(Deaths = ifelse(is.na(Death.Numbers), round(na.approx(Death.Numbers)) , Death.Numbers))


#Dealing with NA, for countries that do not have any value for the full category, NA here represents 0, which means that the country does not have this category in their census
death_causes$Deaths[is.na(death_causes$Deaths)] = 0


## excluding America(aggregated), Zaire(is congo now, insuffiecient data), and Macau(insufficient data) from dataset 

death_causes = death_causes %>% filter(Entity != 'America' & Entity != 'Zaire' & Entity != 'Macau')



## total number of deaths per entity


dfig <- crosstalk::SharedData$new(death_causes)$data() %>%
  group_by(Entity, Year) %>%
  summarise(Total = sum(Deaths)) %>%
  mutate(year = as.factor(Year))


fig1 = dfig %>% plot_ly(x = ~Total, y = ~Entity, color = ~year, type = 'bar',
        orientation = 'h', marker = list(color = 'rgb(158,202,225)',
                     line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% layout(title = "Annual Total Deaths",
                       xaxis = list(title = "Deaths"),
                       yaxis = list(categoryorder = "total ascending",title = "Country"))







## total number of deaths per cause


dfig2 <- crosstalk::SharedData$new(death_causes)$data() %>%
  group_by(Causes.name, Year) %>%
  summarise(Total = sum(Deaths)) %>% filter(Year %in% c(2015,2016, 2017, 2018,2019)) %>% 
  filter(Causes.name %in% c('Cardiovascular diseases', 'Neoplasms', 'Chronic respiratory diseases', 'Digestive diseases', 'Lower respiratory infections', 'Neonatal disorders', "Alzheimer's disease and other dementias", 'Diabetes mellitus')) %>% 
mutate(year = as.factor(Year)) 


fig2 = dfig2 %>% plot_ly(x = ~Total, y = ~Causes.name, color = ~year, type = 'bar',
                 orientation = 'h' ) %>% layout(title = "Causes of Deaths",
                                      xaxis = list(title = "Deaths"),  yaxis = list(categoryorder = "total ascending",title = "Cause"))
                                                                                                                
fig2

## total number of deaths per cause top 5 

dfig3 <- crosstalk::SharedData$new(death_causes)$data() %>%
  group_by(Causes.name, Year) %>%
  summarise(Total = sum(Deaths)) %>% 
  mutate(year = as.factor(Year)) %>% filter(Causes.name %in% c('Cardiovascular diseases', 'Neoplasms', 'Chronic respiratory diseases', 'Digestive diseases', 'Lower respiratory infections', 'Neonatal disorders', "Alzheimer's disease and other dementias", 'Diabetes mellitus'))


dfig3 %>% plot_ly(x = ~year, y = ~Total, color = ~Causes.name, type = 'scatter', mode = 'lines') %>% layout(title = "Causes of Deaths",
                                                                                                                  xaxis = list(title = "Year"),
                                                                                                                  yaxis = list(categoryorder = "total ascending",title = "Number of Deaths"))

# graphing percentage of deaths for top 10 categories
perdeaths = death_causes %>% filter(Year %in% c(2015,2016, 2017, 2018,2019)) %>% group_by(Causes.name) %>%
  summarise(numberofdeaths = sum(Deaths)) %>% mutate(perc = round(numberofdeaths/sum(numberofdeaths)*100, digits = 2)) %>% arrange(desc(perc)) %>% head(10)   

        
            
graph1 = perdeaths %>% plot_ly(x = ~Causes.name, y = ~perc, color = ~Causes.name,text = ~perc, textposition = 'top', insidetextfont = list(size=10, color = 'black'), type = 'bar') %>% layout(title = "Percentage of Deaths",
                                                                                            xaxis = list(categoryorder = "total descending",title = "Cause of Death"), yaxis = list(title = "percentage"))


graph1


##tried working on an interactive map on shiny app but faced technical issues
# adopted from https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# 
# world_data = ggplot2::map_data('world')
# world_data = fortify(world_data)
# head(world_data)
# 
# world_data["ISO3"] <- death_causes$Code[match(world_data$region, death_causes$Entity)]
# 
# 
# worldMaps <- function(death_causes, world_data, country, year, cause ){
#   
#   # Function for setting the aesthetics of the plot
#   my_theme <- function () { 
#     theme_bw() + theme(axis.title = element_blank(),
#                        axis.text = element_blank(),
#                        axis.ticks = element_blank(),
#                        panel.grid.major = element_blank(), 
#                        panel.grid.minor = element_blank(),
#                        panel.background = element_blank(), 
#                        legend.position = "bottom",
#                        panel.border = element_blank(), 
#                        strip.background = element_rect(fill = 'white', colour = 'white'))
#   }
#   
#   # Select only the data that the user has selected to view
#   plotdf <- death_causes[death_causes$Causes.name == cause & death_causes$Entity == country & death_causes$Year == year,]
#   plotdf <- plotdf[!is.na(plotdf$Code), ]
#   
#   # Add the data the user wants to see to the geographical world data
#   world_data['Country'] <- rep(country, nrow(world_data))
#   world_data['Year'] <- rep(year, nrow(world_data))
#   world_data['Causes,name'] <- rep(cause, nrow(world_data))
#   world_data['Deaths'] <- plotdf$Deaths[match(world_data$ISO3, plotdf$Code)]
#   
#   # Create caption with the data source to show underneath the map
#   capt <- paste0("Source: https://ourworldindata.org/causes-of-death")
#   
#   # Specify the plot for the world map
#   library(RColorBrewer)
#   library(ggiraph)
#   g <- ggplot() + 
#     geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
#                              aes(x = long, y = lat, fill = Deaths, group = group, 
#                                  tooltip = sprintf("%s<br/>%s", ISO3, Deaths))) + 
#     scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
#     labs(fill = country, color = country, title = NULL, x = NULL, y = NULL, caption = capt) + 
#     my_theme()
#   
#   return(g)
# }
# 
# 
# 
# # # Define the UI
#  ui = fluidPage(
# #   
# #   # App title
#    titlePanel("Causes of Death"),
# #   
# #   # Sidebar layout with input and output definitions
#    sidebarLayout(
# #     
# #     # Sidebar panel for inputs 
#      sidebarPanel(
# #       
# #       # First input: Type of data
#        sliderInput(inputId = "year",
#                   label = "Choose year:",
#                   min = 1990,
#                   max = 2019,
#                   value = 2019),
#        
# #       # Second input (choices depend on the choice for the first input)
#        uiOutput("secondSelection"),
# #       
#        uiOutput("thirdSelection")
# #       
#     ),
# #     
# #     # Main panel for displaying outputs
#     mainPanel(
#       
# #       # Hide errors
#        tags$style(type = "text/css",
#                   ".shiny-output-error { visibility: hidden; }",
#                   ".shiny-output-error:before { visibility: hidden; }"),
#        
# #       # Output: interactive world map
#        girafeOutput("distPlot")
#       
#      )
#    )
#  )
# 
# 
#  # Define the server
#  server = function(input, output) {
# #   
# #   # Create the interactive world map
#    output$distPlot <- renderGirafe({
#     ggiraph(code = print(worldMaps(df, world_data, input$country, input$year, input$cause)))
#    })
#                           
# 
#    
#    output$secondSelection <- renderUI({
#      choice_second <- as.list(as.character(death_causes$Causes.name[which(death_causes$Year == input$year)]))
#      selectInput(inputId = "cause", choices = choice_second,
#                  label = "Choose cause:")
# 
#    })
#  }
#    
#    
# 
# 
# # # Finally, we can run our app by either clicking "Run App" in the top of our RStudio IDE, or by running
# shinyApp(ui = ui, server = server)
