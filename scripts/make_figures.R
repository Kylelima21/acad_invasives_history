## Plotting of Invasives work started by N. Fisichelli March 2019
## Revised by K. Lima June 2023


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(sf)
library(rgdal)
library(leaflet)
library(ggplot2)
library(cowplot)




#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

T1 <- tibble(read.csv('data/SummaryData22.csv', header = TRUE))

S1 <- tibble(read.csv('data/SpeciesData22ecl.csv', header=TRUE)) %>% 
  filter(Species %in%  c("ALLPET", "BERTHU", "CELORB",
                         "CIRSPP", "FALJAP", "FRAALN",
                         "HERMAN", "LONSPP", "LYTSAL"))




#------------------------------------------------#
####            Plotting/Figures              ####
#------------------------------------------------#

### Ended up creating a QGIS map instead
## Read in state polygons and format for plotting
lyr <- ogrListLayers("data/st_us.kml")
mykml <- lapply(lyr, function(i) readOGR("data/st_us.kml", i))
names(mykml) <- lyr


## Read in park points
parks <- st_read("data/Analysis_Parks_39/Analysis_Parks_39.shp") 


## Plot Maine map with Leaflet
me_map <- leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(maxZoom = 9)) %>% 
  leaflet::addPolygons(data = mykml$MAINE, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`NEW HAMPSHIRE`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$VERMONT, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$MASSACHUSETTS, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`NEW YORK`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`PENNSYLVANIA`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`NEW JERSEY`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`MARYLAND`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`WEST VIRGINIA`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`OHIO`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`VIRGINIA`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`MICHIGAN`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`KENTUCKY`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`NORTH CAROLINA`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`TENNESSEE`, weight = 0.5, opacity = 1, color = "white", 
                       fillOpacity = 0) %>% 
  leaflet::addCircleMarkers(data = parks, radius = 4, weight = 1, opacity = 1, color = "black",
                            fillColor = "orange", fillOpacity = 100)


## View map
me_map


#------------------------------------------------#
####            Plotting/Figures              ####
#------------------------------------------------#


### 4 panel figure - Sites Species Hours Herbicide
## Species
species <- T1 %>% 
  ggplot(aes(x = Year, y = Species)) +
  geom_point(size = 1) + 
  geom_line() + 
  labs(y = "Species (no.)") +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.4, 0, 0.5, "cm")))

## Sites
sites <- T1 %>% 
  ggplot(aes(x = Year, y = Sites)) +
  geom_point(size = 1) + 
  geom_line() + 
  labs(y = "Sites (no.)") +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.4, 0, 0.5, "cm")))

## Hours
hours <- T1 %>% 
  ggplot(aes(x = Year, y = Hours)) +
  geom_point(size = 1) + 
  geom_line() + 
  labs(y = "Hours") +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.4, 0, 0.5, "cm")))

## Herbicide
herbs <- T1 %>% 
  ggplot(aes(x = Year, y = Herbicide)) +
  geom_point(size = 1) + 
  geom_line() + 
  labs(y = "Herbicide (oz)") +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.4, 0, 0.5, "cm")))

## Create 4 panel figure
plot_grid(species, sites, hours, herbs, nrow = 2, labels = c('a)', 'b)', 'c)', 'd)'), align = "h", label_size = 16)

## Save
ggsave(paste0("outputs/four_panel_figure_", str_replace_all(today(), "-", ""), ".pdf"),
       height = 6, width = 9, units = "in", dpi = 700)




### Average hours per site
## Calculate and plot
T1 %>%
  mutate(HoursSite = Hours/Sites) %>% 
  ggplot(aes(x = Year, y = HoursSite)) +
  geom_point(size = 1.2) +
  geom_line() +
  labs(y = "Average hours per site") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")))

## Save
ggsave(paste0("outputs/avg_hours_", str_replace_all(today(), "-", ""), ".pdf"),
       height = 4, width = 6, units = "in", dpi = 700)



### Create herbicide per site
## Calculate and plot
T1 %>%
  mutate(HerbSite = Herbicide/Sites) %>% 
  ggplot(aes(x = Year, y = HerbSite)) +
  geom_point(size = 1.2) +
  geom_line() +
  labs(y = "Average herbicide (oz) per site") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")))

## Save
ggsave(paste0("outputs/avg_herbicide_", str_replace_all(today(), "-", ""), ".pdf"),
       height = 4, width = 6, units = "in", dpi = 700)



### Species by Year
## Calculate and plot
T1 %>% 
  ggplot(aes(x = Year, y = Species)) +
  geom_point(size = 1.2) +
  geom_line() +
  labs(y = "Species per year") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")))

## Save
ggsave(paste0("outputs/species_year_", str_replace_all(today(), "-", ""), ".pdf"),
       height = 4, width = 6, units = "in", dpi = 700)



### Herbicide use by Year 
T1 %>% 
  ggplot(aes(x = Year, y = Herbicide)) +
  geom_point(size = 1.2) +
  geom_line() +
  labs(y = "Annual herbicide (oz) useage") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")))

## Save
ggsave(paste0("outputs/annual_herbicide_", str_replace_all(today(), "-", ""), ".pdf"),
       height = 4, width = 6, units = "in", dpi = 700)



### Facet wrapped species by hours
## Plot
S1 %>% 
  ggplot(aes(Year, Hours, group = ScientificName),
       stat = "identity")+
  geom_point(size = 0.5) +
  geom_line() + 
  labs(x = "Year", y = "Hours") +
  facet_wrap(~ScientificName, ncol = 3) + #, scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(color = "black", size = "8"),
        axis.title = element_text(color = "black", size = "10"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) 

## Save
ggsave(paste0("outputs/species_hours_", str_replace_all(today(), "-", ""), ".pdf"),
              height = 4, width = 6, units = "in", dpi = 700)



### Facet wrapped species by herbicide
## Plot
S1 %>% 
  ggplot(aes(Year, Herbicide, group = ScientificName),
       stat="identity") +
  geom_point(size = 0.5) +
  geom_line() +
  labs(x = "Year", y = "Herbicide (oz)") +
  facet_wrap(~ScientificName, ncol = 3) + #, scales = "free_y")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(color = "black", size = "8"),
        axis.title = element_text(color = "black", size = "10"),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm"))) 

## Save
ggsave(paste0("outputs/species_herbicide_", str_replace_all(today(), "-", ""), ".pdf"),
       height = 4, width = 6, units = "in", dpi = 700)



