### --- Bayesian inference --- ###
library(INLA)
library(inlabru)
library(sf)
library(sp)


esc_d <- 
  read_csv('data/gorguel_density_data_clean.csv') %>% 
  mutate(month = fct_relevel(month, c("July","August")),
         density = density + 1e-3) 


esc_d_sp <- 
  esc_d %>% st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  as(., 'Spatial')

plot(esc_d_sp)


murcia_coast_sp <- st_read("data/murcia_coast.shp") %>%  st_as_sfc() %>% as(., 'Spatial')

plot(murcia_coast_sp)
# Mesh
borinla_1 <- inla.sp2segment(murcia_coast_sp)

mesh <- inla.mesh.2d(boundary  = borinla_1,  
                     max.edge  = c(0.1, 0.1))


plot(mesh)

mesh$n

ggplot() + gg(mesh) + gg(esc_d_sp, color = 2) + theme_minimal()

A <- inla.spde.make.A(mesh, loc = coordinates(esc_d_sp))
dim(A)

spde <- inla.spde2.matern(mesh, alpha = 2)


library(spaMM)

spamm_fit <- 
  fitme(log10(density + 1e-3)~
        month + IMRF(1|X+Y, model= spde),
      family = gaussian(), data=esc_d)


w.index <- inla.spde.make.index(
  name = 'w',
  n.spde = spde$n.spde,
  n.group = 1,
  n.repl = 1)


Xm <- model.matrix(~ -1 , data = esc_d)



StackFit <- inla.stack(
  tag = "Fit",
  data = list(y = esc_d$density),
  A = list(1, A),
  effects = list(
    Intercept = rep(1, N),
    w = w.index))




constr <- FALSE
mdl <- density ~ spat(map = coordinates, model = spde) + Intercept
fit <- bru(mdl, family = "gaussian", data = esc_d_sp)


fit_INLA <- inlabru::bru(
  components = density ~ field(map = coordinates, model = spde) +
    Intercept + month,
  data = esc_d_sp, family = "gaussian")
