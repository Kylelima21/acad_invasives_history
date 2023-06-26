## Plotting of Invasives work started by N. Fisichelli March 2019
## Revised by K. Lima June 2023


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
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



