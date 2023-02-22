library(tidyverse)
library(sf)
library(stars)
library(ggspatial)
library(cowplot)
library(rnaturalearth)
library(metR)
rm(list = ls())

# read data ---------
bathyEmod <-
  read_csv('data/Mean depth in multi colour (no land).csv', skip = 1) %>%
  mutate(m = m * -1) %>% 
  rename(y = 1, x = 2)

esc_d <- read_csv('data/gorguel_density_data_clean.csv')

coords <- read_csv('data/site_coords.csv')


# Murcia coast map----------
bbox <- 
  st_bbox(c(
    xmin = -1.3,    xmax = 0.7,
    ymin = 37.5,
    ymax = 37.8
  ))

murcia <- 
  read_sf('C:/Users/javiera/OneDrive - Cawthron/UA/blue_food_consumption/data/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>% 
  dplyr::filter(Texto_Alt == "Murcia") %>% 
  st_transform (4326) %>% 
  st_crop(bbox) 

sites_map <- 
  ggplot() +
  geom_contour(
    data = bathyEmod,
    aes(x, y, z = m),
    color = 'gray70',
    breaks = c(25, 50, 100)
  ) +
  # geom_text_contour(
  #   data = bathyEmod,
  #   aes(x, y, z = m),
  #   stroke = 0.2,
  #   breaks = c(25, 50, 100),
  #   size = 2.5,
  #   color = "gray70",
  #   check_overlap = T,
  #   label.placer = label_placer_random()
  # ) +
  geom_sf(data = murcia, fill = 'gray60', color = "white") +
  geom_point(data = coords, aes(X, Y), size = 2) +
  ggrepel::geom_label_repel(
    data = coords,
    aes(X, Y, label = localidad),
    color = 1 ,
    size = 2.5,
    fill = "white"
  ) +
  geom_point(aes(y = 37.570215, x  = -0.873569),
             size = 3,
             color = 2) +
  labs(x = NULL, y = NULL) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering,
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  annotation_scale(location = "br", width_hint = 0.15) +
  theme_minimal() +
  coord_sf(
    xlim = c(-1.2, -0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  scale_y_continuous(breaks = c(37.55, 37.60))

sites_map


iberian_map <- 
ggplot() + 
  geom_sf(data = ne_countries(country = 'spain', scale = "large", returnclass = "sf"), fill = 'gray60', color = "white") +
  geom_sf(data = ne_countries(country = 'portugal', scale = "large", returnclass = "sf"), fill = 'gray60', color = "white") +
  geom_sf(data = ne_countries(country = 'france', scale = "large", returnclass = "sf"), fill = 'gray60', color = "white") +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    panel.border = element_rect(color = 'gray90', fill= NA)
  ) +
  geom_point(aes(y= 37.570215, x  = -0.873569), size = 2, color =2) +
  coord_sf(
    xlim = c(-10,4.5),
    ylim = c(35.5, 44.3),
    expand = FALSE
  )


fig_map <- 
  cowplot::ggdraw() +
  draw_plot(sites_map) +
  draw_plot(
    iberian_map,
    x = 0.8,
    y = .75,
    width = .2,
    height = .2
  )

fig_map

ggsave(plot = fig_map,
       device = 'png',
       filename = "figures/fig_1.png",
       width = 8, 
       height = 6,
       bg = "white",
       dpi = 600)


ggsave(plot = fig_map,
       device = 'svg',
       filename = "figures/fig_1.svg",
       width = 8,
       height = 6,
       bg = "white",
       dpi = 600)

