
# #################################
#
# Program: china_on_map.R
#
# Author: Waveley Qiu
# 
# Date: 2023-04-10
#
# Description: map identifying China
#
# ###################################


library(tidyverse)
library(geojsonsf)
library(sf)
#install.packages("devtools")
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

miller <- "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# change worldmap json file path as needed
json_file <- 
  readr::read_file("task_files/code/map/world-highres2.geo.json")

world_sf <- geojson_sf(json_file)
world_sf <- world_sf %>% st_set_crs(miller) 


select_sf <- 
  world_sf %>% 
  filter(name %in% c("China")) %>%
  janitor::clean_names() %>%
  mutate(
    label = "Here it is!"
  )

st_crs(world_sf)
study_map <- 
  ggplot() + 
  geom_sf(data = world_sf, fill = "grey", colour = NA) +
  geom_sf(data = select_sf, mapping = aes(fill = name), colour = NA) + 
  geom_sf_label_repel(data = select_sf, 
                      aes(label = label),
                      force = 550,
                      seed = 20,
                      family = "serif"
  ) +
  coord_sf(crs = miller) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = NA),
        plot.background = element_rect(fill = "#FFFFFF", color = NA),
        text = element_text(family = "serif"),
        legend.position = c(1, 0.2),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        strip.text = element_text(family = "serif")
  ) +
  scale_fill_brewer(palette = "PuBu") +
  theme(legend.position = "none")
study_map