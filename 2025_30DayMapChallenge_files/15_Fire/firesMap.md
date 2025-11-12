---
title: "Fire - 30DayMapChallenge2025 - AEET"
author: 'Sonia Illanas'
format: pdf
date: 11/15/2025 (mm/dd/yyyy)
editor: visual
---

## 30DayMapChallenge 2025

This year the Ecoinformatics group (AEET group) has joined the 30DaysMapChallenge initiative that takes place every November. Among the topics of this year, the theme for November 15th is: *Fire*.

## Fire

The aim of this work was to present the distribution of fires over the last decade in Spain, at the smallest administrative level (municipalities). 

To do so, you may need:

1. To download a fire polygon vector layer.
I used the the EFFIS (European Forest Fire Information System) which updates the status of ongoing fires across Europe **daily**. In their own words: *"it provides near real-time and historical information on forest fires."* 
This is open data (available, public and free) that can be accessed and downloaded from [here](https://forest-fire.emergency.copernicus.eu/applications/data-and-services). 
However, feel free to use any other repository you prefer.

2. To download a polygon vector layer: 
Since the spatial resolution used in this example are municipalities in Spain, I downloaded the data from [here](https://centrodedescargas.cnig.es/CentroDescargas/limites-municipales-provinciales-autonomicos). Be aware that any other spatial resolutions, such as grid, provinces, counties in other countries, national boundaries, could be also used depending on your needs!


First at all: load the layers you are interested:


```r 
library(tidyverse) # data management
library(sf)        # vector spatial data 
library(tmap)      # plots
library(viridis)   # palette
```

```r 
setwd("F:/IREC/PhD/D/Docs/Sonia/1_Colaboraciones/202511_MapChallengeAEET/2025_30DayMapChallenge/2025_30DayMapChallenge_files/")
```

```r
fires<-st_read("./data/effis_layer/modis.ba.poly.shp", quiet=TRUE) %>% st_transform(3035)
firesES<-fires %>% filter(COUNTRY=="ES")  #used country code to select the country you're interested 

muni<-st_read("./data/lineas_limite/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp", quiet=TRUE) %>% st_transform(3035)
muni$area_ha<-st_area(muni)/10000 # calculate municipalities area in ha
attributes(muni$area_ha)<-NULL  # remove ha attribute from the col
```

Intersect the fire layer with the spatial polygon layer that you're interested, in our case Spanish municipalities:

```r
# it could take some time to do it depending on your computer, be patient!
intersections <- st_intersection(muni, firesES)
intersections<-intersections %>% mutate(area_ha.fire=st_area(geometry)/10000) 
attributes(intersections$area_ha.fire)<-NULL
```

Group fires per municipality and sum number of fires and surface burnt in each one:

```r
fires<-intersections %>% as_tibble() %>% mutate(count=1) %>% 
                          group_by(NATCODE, NAMEUNIT) %>% 
                          summarize(area_ha.fire=sum(area_ha.fire),
                                    n.fires=sum(count))

```

Join the information of fires from the intersection with the municipalities by their ID codes:

```r
# ID code = NATCODE & NAMEUNIT
fires2<-muni %>% left_join(fires, by=c("NATCODE", "NAMEUNIT"))

fires2<- fires2 %>% rename(area_ha.tm=area_ha) %>% 
                    mutate(n.fires=replace_na(n.fires, 0),          
                           area_ha.fire=replace_na(area_ha.fire, 0),
                           perc.burnt=area_ha.fire/area_ha.tm*100)

```

Save results in a geopackge (.gpkg) or any other vector layer format:
```r
st_write(fires2, "15Fires.gpkg", layer="FiresTM")
```

Plot results
```r
pal_inferno_black <- c("black", inferno(10)[2:10])
prov<-st_read("./data/lineas_limite/SHP_ETRS89/recintos_provinciales_inspire_peninbal_etrs89/recintos_provinciales_inspire_peninbal_etrs89.shp",
              quiet=TRUE) %>% st_transform(3035)
              
tmap_mode("plot") 
p1<-
tm_shape(fires2) +
  tm_fill("n.fires",
          fill.scale = tm_scale_intervals(breaks = 
                                            c(0, 1, 10, 50, 100, 150, 200, 250, 300),  
                                           values  = pal_inferno_black), 
          fill_alpha = 0.9,
          fill.legend = tm_legend(title = "Number of fires \n per municipality \n",
                                  frame = FALSE)
              ) +
  tm_legend(frame = FALSE, 
            position = c("right", "bottom"), 
            height = 11.3,
            title.size = 0.8,
            title.color = "white",
            text.size = 0.6, 
            text.color = "white",
            bg.color = "black", 
            item.space = 1.2)+
  # tm_title(text = "                                  Fires over the last decade in Spain (2016–2025)",
  #          color="white", 
  #          fontface = "bold",
  #          position = c("center", "top"), 
  #          size = 1.1)+
  tm_layout(bg.color = "black",
            frame = FALSE,
            legend.frame = FALSE,
            outer.bg.color = "black"
            ) +
  # tm_credits("Data from Copernicus", 
  #            position = c("center", "bottom"), 
  #            color = "white")+
tm_shape(prov)+
    tm_borders(col="white")
# p1 
```

```r
p2<-
tm_shape(fires2) +
  tm_fill("perc.burnt",
          fill.scale = tm_scale_intervals(breaks = 
                                            c(0, 1, 3, 5, 10, 20, 40, 80, 120),  
                                           values = pal_inferno_black), 
          fill_alpha = 0.9,
          fill.legend = tm_legend(title = "Burnt area of each \n municipality (%) \n",
                                  frame = FALSE)
              ) +
  tm_legend(frame = FALSE, 
            position = c("right", "bottom"), 
            height = 11.3,
            title.size = 0.8,
            title.color = "white",
            text.size = 0.6, 
            text.color = "white",
            bg.color = "black", 
            item.space = 1.2)+
  # tm_title(text = "                                  Fires over the last decade in Spain (2016–2025)",
  #          color="white", 
  #          fontface = "bold",
  #          position = c("center", "top"), 
  #          size = 1.1)+
  tm_layout(bg.color = "black",
            frame = FALSE,
            legend.frame = FALSE, 
            outer.bg.color = "black"
            ) +
  # tm_credits("Data from Copernicus", 
  #            position = c("center", "bottom"), 
  #            color = "white")+
tm_shape(prov)+
    tm_borders(col="white")

# p2
```

```r
library(grid)
library(gridExtra)
g1 <- tmap_grob(p1)
g2 <- tmap_grob(p2)

library(ggplotify)
ggobj1 <- as.ggplot(g1)
ggobj2 <- as.ggplot(g2)

library(patchwork)
combined<-(ggobj1 | ggobj2) + 
          plot_annotation(title = "Fires over the last decade in Spain (2016–2025)",
                          caption = "Data from EFFIS - Copernicus", 
                          theme = theme(plot.title = 
                                          element_text(colour = "white",
                                                       size=14, 
                                                       face = "bold",
                                                       hjust=0.5), 
                                        plot.caption = 
                                          element_text(colour = "white",
                                                       size=10, 
                                                       face = "italic",
                                                       hjust=0.5), 
                                        plot.background = 
                                          element_rect(fill = "black", 
                                                       color = NA)
                                        )
                  )
combined
```
![15Fire_CombinedMap](https://github.com/ecoinfAEET/2025_30DayMapChallenge/blob/15_Fire/2025_30DayMapChallenge_files/15_Fire/15_fires.png)<!-- -->

```r
# save results in a
ggsave(filename = "15_fires.png", # filename of the plot
        plot = combined,           # plot to save
        width = 30, height = 15,   # dimensions
        units = "cm",              # dimension units
        dpi = 300)
```

Happy coding! :)
