# Tidy Tuesday: 2021 Week 15
# 6 April 2021: Deforestation

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

# Explore drivers in Brazil
library(extrafont)
library(scales)
library(tidyverse)

br <- tuesdata$brazil_loss

br_plot <- br %>%
  transmute(year= year, 
            agriculture= commercial_crops + pasture + tree_plantations_including_palm, 
            natural =  natural_disturbances + fire, 
            extraction = selective_logging + mining, 
            infrastructure = other_infrastructure + roads + small_scale_clearing + flooding_due_to_dams) %>%
  pivot_longer(names_to= "driver", values_to= "loss", cols=-c(year)) %>%
  ggplot(aes(year, loss, color=driver))+
  geom_line(linetype= "dashed") +
  geom_point(aes(shape= driver), size= 2.5) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_x_continuous(expand=c(0.02,0.02))+
  scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#D55E00"))+
  labs(title='Deforestation losses in Brazil 2001-2013',
       subtitle='Deforestation for agricultural use declined to one third between 2004 and 2010',
       y='Annual forest loss (ha)',x='Year', 
       caption='Data Source: Our World in Data\n Created by: bbauer') +
  theme(legend.position = c(0.9, 0.8), 
        plot.title=element_text(colour ='grey30', size=15),
        plot.subtitle=element_text(colour ='grey30', size=10),
        panel.background = element_rect(fill= 'white'),
        panel.grid.major=element_line(colour='grey95', linetype=2))

ggsave('figs/2021W15_brazil.png', br_plot)

# Explore uses of soy in Europe
soy <- tuesdata$soybean_use %>%
  pivot_longer(cols= c(human_food, animal_feed, processed), 
               names_to = "use", 
               values_to= "tonne")

library(sf)
library(tmap)
library(spData)
data(world)

eu <- filter(world, continent == "Europe")

# Figure out which country names from world don't match names in the soy dataset
setdiff(unique(eu$name_long), unique(soy$entity))

# change to be able to match all countries that are in
eu_n <- eu %>%
  mutate(name = as.factor(name_long), 
         name = fct_recode(name,
                             Russia = "Russian Federation", 
                             Czechia=  "Czech Republic")) 

# join 
eu_soy <- inner_join(eu_n, soy, by= c("name" = "entity")) %>%
  # NB year where pop data is from is 2014 but soy data only goes to 2013.
  filter(year==2013) %>%
  mutate(use_per_cap = tonne/pop * 1000) 

# Check the CRS
st_crs(eu_soy)

tm_shape(eu_soy, 
         # create a bounding box to focus on geographical Europe
         # I use bboxfinder.com to get the values
         bbox = st_bbox(c(xmin = -13, xmax = 55, ymin = 35, ymax = 71), crs = st_crs(4326)))+
  tm_polygons(col= "use_per_cap", style= "kmeans", 
              legend.format=list(fun=function(x) formatC(x, digits=2, format="f")), 
              title= "", 
              n= 5, 
              border.alpha = 0.2) +
  tm_facets(by = "use", nrow = 1, 
            free.coords = FALSE, 
            free.scales.fill = TRUE
            )+
  tm_legend(position= c("right", "top"), 
            text.size= 1, 
            bg.color = "white", bg.alpha=1, 
            frame="gray50")+
  tm_layout(panel.labels = c("Direct animal feed", "Human food", "Processed"), 
            panel.label.size= 2, 
            main.title= "Annual use of soy in European countries, 2013 (kg/pp)")
