library(dplyr)
library(tidyverse)
library(tidyr)
library(tidync)

### --- Bayesian inference --- ###
library(INLA)
library(splancs)
library(inlabru)

### --- maps --- ###
library(lattice)
library(fields)
library(plotKML)
library(ggplot2)
library(ggpolypath)
library(ggpubr)
library(lwgeom)
library(raster)

### --- spatial objects --- ###
library(raster)
library(rgdal)
library(sp)
library(sf)
library(geoR)
library(spatial)

### --- colors --- ####
library(viridis)
library(scico)





### --- function to convert matrix to raster --- ####
rotate <- function(x)(apply(x,2,rev))

matrix_to_raster <- function(m, proj.grid.mat = proj.grid.mat)
{
  raster(rotate(t(m)), 
         xmn = min(proj.grid.mat$lattice$loc[,1]), 
         xmx = max(proj.grid.mat$lattice$loc[,1]), 
         ymn = min(proj.grid.mat$lattice$loc[,2]), 
         ymx = max(proj.grid.mat$lattice$loc[,2]), 
         crs = proj4string(SpP))   
}



########## Data ##############################

load("C:/Users/laixa/Dropbox/ThinkInAzul/data/processed-data/1. data copernicus and buffer from Delta to Cartagena.RData")

cca <- read_sf("C:/Users/laixa/Dropbox/ThinkInAzul/data/original-data/Shapefiles España/ComunidadesAutonomas_ETRS89_30N/Comunidades_Autonomas_ETRS89_30N.shp") 
peninsula_crop <-
  cca %>% 
  filter(Texto_Alt != "Canarias" & Texto_Alt != "Illes Balears") %>% 
  st_transform (4326)  %>% 
  st_crop(
    xmin = -1.5,
    xmax = 1.38,
    ymin = 41.2,
    ymax = 37.54
  ) %>% 
  st_combine() %>% 
  st_sf()

## rete the border polygon

border <- as.data.frame(buffer_costa[[1]][[1]][[1]]) %>%
  st_as_sf(coords = c("V1", "V2"), crs = 4326, agr = "constant")
border_poly <- as_Spatial(border)

### Convert the SpatialPointsDataFrame to SpatialPolygons
(Sr1 = Polygon(border_poly@coords))
(Srs1 = Polygons(list(Sr1), "s1"))
(SpP = SpatialPolygons(list(Srs1), 1:1, proj4string = crs("+proj=longlat +datum=WGS84 +no_defs"))) 
plot(SpP)#, col = 3:3, pbg="white", add=T) 
SpP ### can not write as shapefile

### Convert the SpatialPolygons to SpatialPolygonsDataFrame
#shape_pol <- SpatialPolygonsDataFrame(SpP, match.ID=F, data= data.frame(x=border_poly@coords[,1], y=border_poly@coords[,1]))
#shape_pol ### can be write as shapefile
#plot(shape_pol, col = 4, add=T)


## Copernicus data

SST_medi_0 <- SST_medi_Delta_Cartagena %>%
  #filter(between(lon, -0.3, 0.3)) %>%
  #filter(between(lat, 38.0, 38.5)) %>% 
  dplyr::mutate(year = lubridate::year(time_day), 
                month = lubridate::month(time_day), 
                day = lubridate::day(time_day)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

#ggplot() +
#  geom_sf(data = buffer_costa, fill = "lightblue", col = "transparent")+
#  geom_sf(data = SST_medi_0, aes(color = analysed_sst)) #+
#  #scale_color_scico(palette = "lajolla", name = "K", na.value = "gray99",
#  #                  limits=lim.SST)

SST_medi_1 <- SST_medi_0 %>%
  filter(month %in% c(9,10,11)) %>%
  group_by(year,lon,lat) %>%
  summarize(range = max(max(analysed_sst)-min(analysed_sst))   ) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") 

SST_medi_2 <- SST_medi_1 %>%
  group_by(lon,lat) %>%
  summarize(range_max = max(range)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") 



################## spde mesh etc #######################

SST_medi <- SST_medi_2

# Variograma
  SST_geo <- data.frame(lon=SST_medi$lon,lat=SST_medi$lat,data=SST_medi$range_max)
  coords <- SST_geo[,c("lon","lat")]
  
  SST_geo <- as.geodata(SST_geo)
  
  # Eliminacion de tendencias
  SST_ls<-with(SST_medi, surf.ls(3,lon,lat,range_max))
  SST_sin<-SST_medi$range_max-predict(SST_ls,SST_medi$lon,SST_medi$lat)
  SST2_geo<-SST_geo
  SST2_geo$data<-SST_sin
  plot.geodata(SST2_geo)
  
  #variograma de la variabilidad residual tras eliminar la tendencia observada
  #par(mfrow=c(1,1))
  plot(variog(SST2_geo,uvec=seq(0,2,0.05),messages=F),pch=20)


# Mesh
  
  
  # Mesh
  borinla_1 <- inla.sp2segment(SpP) #map_sp)
  max.edge = diff(range(coords[,1]))/20 # 15/20/25 com més gran el denominador més petits els triangles 
  bound.outer = diff(range(coords[,1]))/5 # 
  
  mesh <- inla.mesh.2d(boundary  = borinla_1,  
                       max.edge  = c(1, 5) * max.edge,
                       cutoff    = max.edge/5, 
                       #min.angle = 50
                       offset    = c(max.edge, bound.outer)
  )
  
  #borinla_1 <- inla.sp2segment(SpP) #map_sp)
  #max.edge = diff(range(coords[,1]))/20 # 15/20/25 com més gran el denominador més petits els triangles 
  #bound.outer = diff(range(coords[,1]))/5 # 
  
  #mesh <- inla.mesh.2d(loc.domain = coords,
  #                     boundary  = borinla_1,  
  #                     max.edge  = c(1, 5) * max.edge,
  #                     cutoff    = max.edge/5, 
  #                     #min.angle = 50
  #                     offset    = c(max.edge, bound.outer)
  #)

  ggplot() +
    geom_sf(data = buffer_costa, fill = "lightblue", col = "transparent")+
    geom_polygon(data = SpP, aes(x = long, y = lat))+
    geom_sf(data = SST_medi, aes(color = range_max)) +
    gg(mesh)  

    
# spde

  size <- min(c(diff(range(coords$lon)), diff(range(coords$lat))))
  range0 <- 0.7 #size/2 # ~ default
  
  spde <- inla.spde2.pcmatern(
    mesh = mesh, 
    prior.range = c(range0, 0.5),
    prior.sigma = c(1, 0.01))

  
# indexs
  
  k = length(unique(SST_medi$order))
  iset <- inla.spde.make.index('i', n.spde = spde$n.spde,
                               n.group = k)
  lengths(iset)

# Projection matrix
  
  SST_medi_1$order <- SST_medi_1$year-(min(SST_medi_1$year)-1)
  SST_medi <- SST_medi_1
  
  A <- inla.spde.make.A(mesh, 
                        loc = cbind(SST_medi$lon, SST_medi$lat),
                        group = SST_medi$order)

  Ap <- inla.spde.make.A(mesh = mesh, loc = dp_1[,1:2], group = dp_1[,3])
  
# Prediction data
  
  bb <- st_bbox(SpP)
  x <- seq(bb$xmin - 1, bb$xmax + 1, length.out = 100)
  y <- seq(bb$ymin - 1, bb$ymax + 1, length.out = 300)
  dp <- as.matrix(expand.grid(x, y))
  plot(dp, asp = 1)
  
  p <- st_as_sf(data.frame(x = dp[, 1], y = dp[, 2]),
                coords = c("x", "y")
  )
  st_crs(p) <- st_crs(4326)
  ind <- st_intersects(buffer_costa, p)
  dp <- dp[ind[[1]], ]
  plot(dp, asp = 1)
  
  dp_1 <- cbind(dp, 1)
  
  for(i in 2:k){
    dp_2 <- cbind(dp, i)
    dp_1 <- rbind(dp_1, dp_2)
  }
  
  
  

  
  
########### Spatio-temporal Continuous prediction ########################

  # Stack and fitting  
  
  stk.e <- inla.stack(
    tag = "est",
    data = list(y = SST_medi$range),
    A = list(1, A),
    effects = list(data.frame(b0 = rep(1, nrow(SST_medi))), 
                   i = iset)
  )
  
  stk.p <- inla.stack(
    tag = "pred",
    data = list(y = NA),
    A = list(1, Ap),
    effects = list(data.frame(b0 = rep(1, nrow(dp_1))), 
                   i = iset)
  )
  
  stk.full <- inla.stack(stk.e, stk.p)

 ### --- fitting the model --- ###
  
    h.spec <- list(rho = list(prior = 'pc.cor1', param = c(0, 0.9)))
    # PC prior on the autoreg. param.
    prec.prior <- list(prior = 'pc.prec', param = c(1, 0.01))
    
    formula.1 <- y ~ -1 + b0 + 
      f(i, model = spde, group = i.group, 
        control.group = list(model = 'iid')) #, hyper = h.spec))

    tictoc::tic()
    res <- inla(formula.1,
                      data              = inla.stack.data(stk.full), 
                      family            = "gamma" ,
                      control.compute   = list(dic              = TRUE,
                                               cpo              = TRUE, 
                                               waic             = TRUE, 
                                               return.marginals = TRUE), 
                      control.predictor = list(A       = inla.stack.A(stk.full), 
                                               compute = TRUE),
                      control.family = list(hyper = list(prec = prec.prior)), 
                      control.fixed = list(expand.factor.strategy = 'inla'),
                      #control.fixed     = list(prec.intercept = 1),
                      #control.inla=list(strategy = "laplace"),
                      verbose           = TRUE,
                      num.threads       = 2)
  
    
    tictoc::toc()
    saveRDS(res, "../results/11. INLA Dana models results/model_pred temporal range0 0.7 prob 0.5 var range_max.rds")
  
  
# Results
    
    summary(res)

    list_marginals <- list(
      "b0" = res$marginals.fixed$b0,
      "precision Gaussian obs" =
        res$marginals.hyperpar$"Precision for the Gaussian observations",
      "range" = res$marginals.hyperpar$"Range for s",
      "stdev" = res$marginals.hyperpar$"Stdev for s",
      "rho" = res$marginals.hyperpar$"GroupRho for s"
    )
    
    
    marginals <- data.frame(do.call(rbind, list_marginals))
    sapply(list_marginals, nrow)
    marginals$parameter <- rep(names(list_marginals)[1:2],
                               times = c(43,43)
    )  
    
    library(ggplot2)
    ggplot(marginals, aes(x = x, y = y)) + geom_line() +
      facet_wrap(~parameter, scales = "free") +
      labs(x = "", y = "Density") + theme_bw()
    
    
    
# Mapping predictions
    
    index <- inla.stack.index(stack = stk.full, tag = "pred")$data
    
    dp_1 <- data.frame(dp_1)
    names(dp_1) <- c("x", "y", "time")
    
    dp_1$pred_mean <- res$summary.fitted.values[index, "mean"]
    dp_1$pred_ll <- res$summary.fitted.values[index, "0.025quant"]
    dp_1$pred_ul <- res$summary.fitted.values[index, "0.975quant"]
    dp_1$pred_sd <- res$summary.fitted.values[index, "sd"]
    
    library(reshape2)
    dpm <- melt(dp_1,
                id.vars = c("x", "y", "time"),
                measure.vars = c("pred_mean", "pred_ll", "pred_ul", "pred_sd")
    )
    head(dpm)
    
    #ggplot(buffer_costa) + geom_sf() + coord_sf(datum = NA) +
    #  geom_tile(data = dpm, aes(x = x, y = y, fill = value)) +
    #  labs(x = "", y = "") +
    #  facet_wrap(variable ~ time) +
    #  scale_fill_viridis("PM2.5") +
    #  theme_bw()
    
    dpm_41 <- dpm %>%
      filter(time == 41)
    
    g1 <- ggplot(buffer_costa) + geom_sf() + coord_sf(datum = NA) +
      geom_tile(data = dpm_41[dpm_41$variable=="pred_mean",], aes(x = x, y = y, fill = value)) +
      geom_sf(data = peninsula_crop) +
      labs(x = "", y = "") +
      scale_fill_viridis("mean") +
      theme_minimal() 
      
    
    g2 <- ggplot(buffer_costa) + geom_sf() + coord_sf(datum = NA) +
      geom_tile(data = dpm_41[dpm_41$variable=="pred_sd",], aes(x = x, y = y, fill = value)) +
      geom_sf(data = peninsula_crop) +
      labs(x = "", y = "") +
      scale_fill_viridis("sd") +
      theme_minimal() 
      
    
    # g <-# 
    ggarrange(g1,g2,ncol=2)
    