# load dependencies
library(raster)
library(downloader)
library(sf)
library(mapview)
library(RStoolbox)

coords <- read_csv('data/coords.csv')
coords_sf <- st_as_sf(coords, coords = c("X", "Y"), crs = 4326)

xmin = -1.3
xmax = - 0.65
ymin = 37.5
ymax = 37.8

getbathymetry <-
  function (name = "emodnet__mean",
            xmin = -1.3,
            xmax = -0.6,
            ymin = 37.5,
            ymax = 37.8) {
    bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")
    
    con <-
      paste(
        "https://ows.emodnet-bathymetry.eu/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=",
        name,
        "&crs=EPSG:4326&BBOX=",
        bbox,
        "&format=image/tiff&interpolation=nearest&resx=0.00208333&resy=0.00208333",
        sep = ""
      )
    
    print(con)
    
    stop
    nomfich <- paste(name, "img.tiff", sep = "_")
    nomfich <- tempfile(nomfich)
    download(con, nomfich, quiet = TRUE, mode = "wb")
    img <- raster(nomfich)
    img <- -1 * img # reverse the scale (below sea level = positive)
    img[img == 0] <- NA
    img[img < 0] <- 0
    names(img) <- paste(name)
    return(img)
  }

# get the bathymetry data for the MPA
bathy_img <- getbathymetry(name = "emodnet:mean", xmin, xmax, ymin, ymax)
write_rds(bathy_img, "data/bathy_img.rds")
plot(bathy_img)

sf_use_s2.(FALSE)
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

murcia_coast <- 
s2::s2_buffer_cells(
  s2::s2_geog_from_wkb(st_as_binary(st_geometry(murcia)), check = FALSE),
  distance = 2e3,
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

plot(murcia_coast)
st_write(murcia_coast, "data/murcia_coast.shp", append=FALSE)
read_sf("data/murcia_coast.shp")

coastal_grid <- 
  st_make_grid(murcia_coast, cellsize = .01, square = FALSE) %>%
  st_intersection(murcia_coast) %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble()


mapview(list(murcia_coast, coords_sf))

# murcia_coast_extent <- extent(bbox(sf::as_Spatial(murcia_coast)))

## crop and mask
murcia_bathy_cropped <- crop(bathy_img, extent(murcia_coast)) %>% mask(murcia_coast)
writeRaster(murcia_bathy_cropped, filename='data/murcia_bathy_cropped.grd', overwrite=TRUE)

raster('data/murcia_bathy_cropped.grd')

shallow_muric <- murcia_bathy_cropped[] > 50
# murcia_bathy_cropped[murcia_bathy_cropped[] > 50 ] = NA
## Check that it worked
mapview(murcia_bathy_cropped)

library(RStoolbox)
murcia_bathy_cropped_df <- 
  fortify(murcia_bathy_cropped, maxpixels = 9e9) %>% #9e9
  drop_na() %>% 
  as_tibble() %>% 
  dplyr::rename(depth = "emodnet.mean",
                X = "x",
               Y = "y")
write_csv(murcia_bathy_cropped_df, "data/murcia_bathy_cropped_df.csv")

xmin = -1.3,
xmax = -0.6,
ymin = 37.5,
ymax = 37.8

study_map_bathy <- 
ggplot() + 
  geom_raster(data = murcia_bathy_cropped_df, aes(X,Y,fill = depth)) +
  scale_fill_viridis_b(option = 'D') +
  geom_contour(data = murcia_bathy_cropped_df, aes(X,Y,z = depth), color = 'gray50') +
  geom_sf(data = murcia, fill = 'gray90') +
  coord_sf(xlim = c(-1.3, -0.6), ylim = c(37.5, 37.68), expand = FALSE) +
  geom_point(data = coords, aes(X,Y), color = 'red') +
  ggrepel::geom_label_repel(data = coords, aes(X,Y, label = localidad), color =1 , size = 2.5) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

study_map_bathy

ggsave(plot = study_map_bathy,
       device = 'png',
       filename = "figures/study_map_bathy.png",
       width = 8, 
       height = 3,
       dpi = 600)

write_csv(murcia_bathy_cropped_df, "data/murcia_bathy_cropped.csv")

meta <- 
  extract(bathy_img, coords[,2:3]) %>% 
  as_tibble() %>%
  dplyr::rename(depth = 'value') %>% 
  bind_cols(coords)

meta <- 
coords %>% 
  mutate(depth = rnorm(8,2,1.5))


esc_d <- 
  read_csv('data/gorguel_density_data_clean.csv') %>% 
  mutate(month = fct_relevel(month, c("July","August"))) %>% 
  left_join(meta) %>% 
  write_csv('data/gorguel_density_data_clean_depth.csv')

