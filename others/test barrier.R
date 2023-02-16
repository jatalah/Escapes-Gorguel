murcia_coast2 <- 
  s2::s2_buffer_cells(
    s2::s2_geog_from_wkb(st_as_binary(st_geometry(murcia)), check = FALSE),
    distance = 2.5e3,
    max_cells = 1e4
  ) %>% 
  st_as_sf() %>% 
  st_difference(., murcia) %>% 
  st_crop(
    xmin = -1.23,
    xmax = 0.7,
    ymin = 37.5,
    ymax = 37.649
  ) %>% 
  st_set_crs(4326) %>% 
  st_as_sfc() %>% 
  st_cast("POLYGON") %>% 
  .[1] %>% # remove Mar menor portion
  st_as_sf()
plot(murcia_coast2)


murcia_coast3 <- 
  s2::s2_buffer_cells(
    s2::s2_geog_from_wkb(st_as_binary(st_geometry(murcia)), check = FALSE),
    distance = 1e3,
    max_cells = 1e4
  ) %>% 
  st_as_sf() %>% 
  st_difference(., murcia) %>% 
  st_crop(
    xmin = -1.23,
    xmax = 0.7,
    ymin = 37.5,
    ymax = 37.649
  ) %>% 
  st_set_crs(4326) %>% 
  st_as_sfc() %>% 
  st_cast("POLYGON") %>% 
  .[1] %>% # remove Mar menor portion
  st_as_sf()

library(mapview)
mapview(list(murcia_coast3, murcia_coast))
plot(murcia_coast)
# y <- 
# s2::s2_buffer_cells(
#   s2::s2_geog_from_wkb(st_as_binary(st_geometry(murcia_coast2)), check = FALSE),
#   distance = 1e4,
#   max_cells = 1e4
# ) %>% st_as_sf()

x <- st_difference(murcia_coast2, murcia_coast) %>% st_union(murcia)

