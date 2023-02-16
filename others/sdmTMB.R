library(sdmTMB)
library(sf)
library(INLA)
library(raster)
library(RStoolbox)
library(geosphere)
rm(list = ls())

# read data --------------
esc_d <- 
  read_csv('data/gorguel_density_data_clean.csv') %>% 
  # filter(density < 500) %>% 
  mutate(month = fct_relevel(month, c("July","August")))

# coast shape file-----------
murcia_coast <- read_sf("data/murcia_coast.shp")

bbox <-
st_bbox(c(
xmin = -1.3,
xmax = 0.7,
ymin = 37.5,
ymax = 37.65
))

murcia <-
read_sf('C:/Users/javiera/OneDrive - Cawthron/UA/blue_food_consumption/data/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>%
dplyr::filter(Texto_Alt == "Murcia") %>%
st_transform (4326) 

# define the mesh
# mesh <- make_mesh(esc_d, c("X", "Y"), n_knots = 90, type = "cutoff_search")
mesh <- make_mesh(esc_d, c("X", "Y"), n_knots = 95, type = "kmeans")
plot(mesh)
bspde <-
  add_barrier_mesh(
    spde_obj = mesh,
    barrier_sf = murcia,
    plot = TRUE,
    range_fraction = .1
  )

plot(bspde)
bspde$mesh$n

mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]

ggplot(murcia) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 2, color  = "darkblue", pch = 21) +
  geom_sf(data = mesh_df_land, size = 2, colour = "darkgreen", pch = 21) +
  theme_minimal() + 
  coord_sf(
    xlim = c(-1.2,-0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  )


ggplot() + 
  inlabru::gg(bspde$mesh) +
  geom_point(data = esc_d, aes(X, Y), color = "darkred") +
  geom_sf(data = murcia) +
  coord_sf(
    xlim = c(-1.2,-0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(x= NULL, y = NULL)


# Moran index ----
library(ape)
inv_dist <- as.matrix(dist(esc_d[,c("X","Y")]))
diag(inv_dist) <- 0
Moran.I(esc_d$density, inv_dist)

# fit the model----------
fit <- 
  sdmTMB(
  # density ~ s(distance, k = 5) + month,
  density ~ distance * month,
  family = tweedie(link = "log"),
  mesh = bspde,
  spatial = "on",
  data = esc_d
)
AIC(fit)

# model results -------
fit
tidy(fit, conf.int = F, exponentiate = T) 
tidy(fit, conf.int = F, exponentiate = T)
tidy(fit, conf.int = F, exponentiate = T,   effects = "ran_pars")

# write_csv("data/sdmTMB_model_table.csv")

tidy(fit, effects = "ran_pars", conf.int = TRUE)

sanity(fit)

# partial response plots ------
visreg::visreg(fit,
               xvar = "distance",
               by = "month",
               scale = "response"
               gg = TRUE) +
  theme_minimal() +
  geom_point(size = 2)

plot(ggeffects::ggeffect(fit))

# model validation----
# rq_res <- residuals(fit)
# qqnorm(rq_res);qqline(rq_res)

simulate(fit, nsim = 300) %>% 
  dharma_residuals(fit)

s_fit <- simulate(fit, nsim = 1e3)
pred_fixed <- fit$family$linkinv(predict(fit)$est_non_rf)
r_pois <- DHARMa::createDHARMa(
  simulatedResponse = s_fit,
  observedResponse = esc_d$density,
  fittedPredictedResponse = pred_fixed
)
plot(r_pois)

DHARMa::testResiduals(r_pois)
DHARMa::testZeroInflation(r_pois)


# observed vs predicted
predict(fit, newdata = esc_d, type = "response") %>% 
  as_tibble() %>% 
  ggplot(aes(est, density)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, lty = 2, color = 2) +
  theme_minimal() +
  tune::coord_obs_pred()



# prediction grid data 
new_dat <-
  murcia_coast %>%
  st_bbox() %>%
  st_make_grid(
    cellsize = .001, # pixel size
    what = "centers"
  ) %>%
  st_intersection(murcia_coast) %>%
  st_coordinates() %>%
  as_tibble() %>%
  expand_grid(month = c('July',"August","September")) %>%
  mutate(distm(c(X = -0.87685249, Y = 37.57722501),.[,1:2]) %>% t() %>% as_tibble()/1000) %>%
  rename(distance = V1) %>%
  mutate(month = fct_relevel(month, c("July","August"))) %>%
  drop_na()

# get predictions---------
p <- predict(fit, newdata = new_dat, type = "response") 

# write_csv(p, "data/sdmTMB_preds.csv")

ggplot() + 
  geom_sf(data = murcia, fill = 'gray60', color = "gray60", linewidth = 2) +
  geom_raster(data = p, aes(round(X,10), round(Y,10), fill = est)) +
  scale_fill_viridis_c(
    # trans = "log10",
    name = "Density",
    option = "H",
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ month, ncol = 1) +
  coord_sf(
    xlim = c(-1.2,-0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  scale_y_continuous(breaks = c(37.55, 37.60)) +
  labs(x = "Longitude", y = "Latitude")

# random factor estimate ------
ggplot() + 
  geom_sf(data = murcia, fill = 'gray60', color = "gray60", linewidth = 2) +
  geom_raster(data = p, aes(round(X,10), round(Y,10), fill = exp(est_rf))) +
  scale_fill_viridis_c(
    # trans = "log10",
    name = "Density",
    option = "H",
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ month, ncol = 1) +
  coord_sf(
    xlim = c(-1.2,-0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  scale_y_continuous(breaks = c(37.55, 37.60)) +
  labs(x = "Longitude", y = "Latitude")

# non-random factor estimate ------
ggplot() + 
  geom_sf(data = murcia, fill = 'gray60', color = "gray60", linewidth = 2) +
  geom_raster(data = p, aes(round(X,10), round(Y,10), fill = exp(est_non_rf))) +
  scale_fill_viridis_c(
    # trans = "log10",
    name = "Density",
    option = "H",
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ month, ncol = 1) +
  coord_sf(
    xlim = c(-1.2,-0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  scale_y_continuous(breaks = c(37.55, 37.60)) +
  labs(x = "Longitude", y = "Latitude")




# time_varying models----
fit2 <- sdmTMB(
  density ~ 0 + month,
  data = esc_d,
  # time_varying = ~ distance,
  mesh = bspde,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "month",
  spatiotemporal = "iid",
  silent = FALSE
)

AIC(fit)
AIC(fit2)
# get predictions---------
p2 <- predict(fit2, newdata = new_dat, type = "response") 

# write_csv(p, "data/sdmTMB_preds.csv")

ggplot() + 
  geom_sf(data = murcia, fill = 'gray60', color = "gray60", linewidth = 2) +
  geom_raster(data = p2, aes(round(X,10), round(Y,10), fill = est)) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "Density",
    option = "H",
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ month, ncol = 1) +
  coord_sf(
    xlim = c(-1.2,-0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  scale_y_continuous(breaks = c(37.55, 37.60)) +
  labs(x = "Longitude", y = "Latitude")

ggplot() + 
  geom_sf(data = murcia, fill = 'gray60', color = "gray60", linewidth = 2) +
  geom_raster(data = p2, aes(round(X,10), round(Y,10), fill = est_non_rf)) +
  scale_fill_viridis_c(
    # trans = "log10",
    name = "Density",
    option = "H",
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ month, ncol = 1) +
  coord_sf(
    xlim = c(-1.2,-0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  scale_y_continuous(breaks = c(37.55, 37.60)) +
  labs(x = "Longitude", y = "Latitude")

ggplot() + 
  geom_sf(data = murcia, fill = 'gray60', color = "gray60", linewidth = 2) +
  geom_raster(data = p2, aes(round(X,10), round(Y,10), fill = omega_s)) +
  scale_fill_viridis_c(
    # trans = "log10",
    name = "Density",
    option = "H",
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ month, ncol = 1) +
  coord_sf(
    xlim = c(-1.2,-0.65),
    ylim = c(37.5, 37.65),
    expand = FALSE
  ) +
  scale_y_continuous(breaks = c(37.55, 37.60)) +
  labs(x = "Longitude", y = "Latitude")
