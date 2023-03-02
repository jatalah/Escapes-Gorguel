library(tidyverse)
library(sf)
rm(list = ls())


# read data----------
coords <- read_csv('data/coords.csv')

preds <- 
  read_csv("data/glmmTMB_preds.csv") %>% 
  mutate(month = fct_relevel(month, c("July","August"))) 

murcia <- 
  read_sf('C:/Users/javiera/OneDrive - Cawthron/UA/blue_food_consumption/data/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>% 
  dplyr::filter(Texto_Alt == "Murcia") %>% 
  st_transform (4326) %>% 
  st_crop(bbox) 


coast <- 
  read_sf('C:/Users/javiera/OneDrive - Cawthron/UA/blue_food_consumption/data/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>% 
  st_crop(bbox) 

# set geo limits------------ 
bbox <- 
  st_bbox(c(
    xmin = -1.3,
    xmax = 0.7,
    ymin = 37.5,
    ymax = 37.65
  ))

# add the farm location int o the plot----
farm_loc <- 
  tibble(
    x = rep(37.570215, 3),
    y = rep(-0.873569, 3),
    month = c("July", "August", "September")
  ) %>% 
  mutate(month = fct_relevel(month, "July", "August"))

# plot the predictions-----
ggplot() +
  geom_sf(
    data = murcia,
    fill = 'gray60',
    color = "gray60",
    linewidth = 2
  ) +
  geom_raster(data = preds, aes(round(X, 10), round(Y, 10), fill = preds_glmmTMB)) +
  geom_point(data = farm_loc, aes(y = 37.570215, x  = -0.873569),
             size = 2,
             color = 2) +
  scale_fill_viridis_c(
    trans = "log10",
    name = Predicted ~ density ~ (fish ~ 100 ~ m ^ -2),
    option = "D",
    
  ) +
  theme_minimal(base_size = 7) +
  facet_wrap(~ month, ncol = 1) +
  coord_sf(
    xlim = c(-1.2, -0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  scale_y_continuous(breaks = c(37.55, 37.60)) +
  labs(x = NULL, y = NULL) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(.4, 'cm')
  )


# save figure 3------
ggsave(
  last_plot(),
  filename = "figures/figure_3.pdf",
  width = 90,
  height = 120,
  bg = "white",
  units = "mm",
  dpi = 300
)

ggsave(
  last_plot(),
  filename = "figures/figure_3.png",
  width = 90,
  height = 120,
  bg = "white",
  units = "mm",
  dpi = 300
)
