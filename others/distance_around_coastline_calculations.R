library(raster)
library(fasterize)
library(sf)
esc_d <- 
  read_csv('data/gorguel_density_data_clean.csv') %>% 
  # filter(density < 500) %>% 
  mutate(month = fct_relevel(month, c("July","August"))) 

murcia_coast <- read_sf("data/murcia_coast.shp")
murcia <- 
  read_sf('C:/Users/javiera/OneDrive - Cawthron/UA/blue_food_consumption/data/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>% 
  dplyr::filter(Texto_Alt == "Murcia") %>% 
  st_transform (4326) %>% 
  st_crop(
    xmin = -1.3,
    xmax = 0.7,
    ymin = 37.5,
    ymax = 37.8
  ) 


# coords <-
#   read_csv('data/coords.csv') %>%
#   add_case(X = -0.87685249,
#            Y = 37.57722501,
#            localidad = "Escape") %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326)
# coords_sf <- st_as_sf(coords, coords = c("X", "Y"), crs = 4326)

transect_coords <- 
esc_d %>% 
  distinct(ID, X, Y) %>% 
  add_case(X = -0.87685249,
           Y = 37.57722501,
           ID = "Escape")

transect_coords_sf <- 
  transect_coords %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4326)

library(mapview)
mapview(list(transect_coords_sf, murcia))

r <- raster(extent(murcia), nrows = 1e3, ncols = 1e3)
rtas <- fasterize(summarize(murcia), r)

plot(rtas)
rtas_pts <- rtas
xy <- st_coordinates(transect_coords_sf)
icell <- cellFromXY(rtas, xy)
rtas_pts[icell[97]] <- 2

d <- gridDistance(rtas_pts, origin = 2,
                  omit = 1)/1000

d[icell] %>% as_tibble() %>% bind_cols(transect_coords_sf) %>% view

new_dat_test <- 
murcia_coast %>% 
  st_as_sfc() %>% 
  st_bbox() %>% 
  st_make_grid(
    cellsize = .02, # pixel size
    what = "centers"
  ) %>%
  st_as_sf() %>% 
  st_intersection(murcia_coast) %>% 
  st_coordinates() %>% 
  as_tibble()
