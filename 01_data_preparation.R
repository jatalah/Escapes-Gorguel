library(readxl)
library(tidyverse)
library(janitor)
library(lubridate)
library(bbl)
library(fuzzyjoin)
theme_set(theme_minimal())
select <- dplyr::select

rm(list = ls())

# density data prep ----
esc <- 
  read_excel("data/Escape Gorguel.xlsx", skip = 2, n_max = 1000) %>% 
  clean_names() %>% 
  drop_na(fecha) %>% 
  rename(density = dens_dlab_100m2) %>% 
  mutate(fecha = as_date(fecha),
         distance = abs(distancia_localidad_km),
         orientation = if_else(distancia_localidad_km>0, "E", "W"), 
         day = day(fecha),
         mes = fct_relevel(mes, c("Julio","Agosto")),
         month = fct_recode(mes, July = "Julio", August = "Agosto", September = "Septiembre"),
         num_mes = as.numeric(mes))

# size data preparation ------

size <- 
esc %>% 
  pivot_longer(cols = dlab_10:dlab_65, names_to = 'size_code', values_to = 'number') %>% 
  mutate(size = str_extract(size_code, "(\\d)+") %>% as.numeric,
         month = fct_recode(mes, July = "Julio", August = "Agosto", September = "Septiembre")) %>% 
  dplyr::select(fecha, mes:distancia_localidad_km, distance, day, num_mes, month, orientation, density, size, number) %>% 
  write_csv('data/size_data.csv')

# convert frequency data into raw data----
size_raw <- 
  bbl::freq2raw(data = size %>% 
                  dplyr::select(localidad, toponimia, month, sitio , distance, transecto , size), freq = size$number) %>% 
  mutate(loc_dist = fct_reorder(paste0(localidad, " (", distance," km)"), distance),
         month = fct_relevel(month, "July","August")) 

size_raw %>%
  mutate(month =fct_reorder(loc_dist, distance))

write_csv(size_raw, 'data/size_data_raw.csv')


# add locations and transect coords------------- 
locations <- 
  st_read("data/Zero Inflated escape.kml", quiet = T) %>% 
  st_centroid()

coord <-
stringdist_join(
  st_coordinates(locations) %>% bind_cols(ID = locations$Name) %>% slice(-(1:8)),
  esc %>% transmute(ID = paste0(toponimia," ", transecto)) %>% distinct(), 
  by = "ID",
  mode = "left",
  ignore_case = FALSE,
  method = "jw",
  max_dist = 3,
  distance_col = "dist"
) %>% 
  group_by(ID.x) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  dplyr::select(ID.y, X, Y) %>%
  rename(ID = "ID.y")

esc_d <- 
  esc %>% 
  mutate(ID = paste0(toponimia," ", transecto)) %>% 
  dplyr::select(fecha, ID, mes:distancia_localidad_km, distance, day, num_mes, month, orientation, density) %>% 
  left_join(coord, by = "ID") %>% 
  write_csv('data/gorguel_density_data_clean.csv')
