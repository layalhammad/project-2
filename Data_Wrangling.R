library(tidyverse)
library(dplyr)
library(zoo)
library(maps)
library(ggplot2)
library(magrittr)
library(rvest)
library(RColorBrewer)
library(ggiraph)

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
death_causes %>% group_by(Entity, Year) %>% summarize(Total = sum(Deaths))
## total number of deaths per cause
death_causes %>% group_by(Causes.name, Year) %>% summarize(Total = sum(Deaths))



# adopted from https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/

world_data = ggplot2::map_data('world')
world_data = fortify(world_data)
head(world_data)

world_data["ISO3"] <- death_causes$Code[match(world_data$region, death_causes$Entity)]



worldMaps <- function(death_causes, world_data, country, cause , year){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- death_causes[death_causes$Year == year & death_causes$Entity == country & death_causes$Causes.name == cause,]
  plotdf <- plotdf[!is.na(plotdf$Code), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['Entity'] <- rep(country, nrow(world_data))
  world_data['Year'] <- rep(year, nrow(world_data))
  world_data['Causes.name'] <- rep(cause, nrow(world_data))
  world_data['Deaths'] <- plotdf$Deaths[match(world_data$ISO3, plotdf$Code)]
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: https://ourworldindata.org/causes-of-death")
  
  # Specify the plot for the world map

  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Deaths))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    labs(fill = country, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}



