# percentage of zeros----
filter(esc_d, density==0) %>% nrow/nrow(esc_d) *100

esc_d %>% count(localidad, mes, sitio) 

esc_d %>% tabyl(mes, localidad)


esc_d %>% distinct(toponimia, distance)

esc_d %>% distinct(localidad, distance)

esc_d %>% tabyl(mes, sitio)

esc_d %>% count(toponimia, mes)

esc_d %>% tabyl(mes, transecto)

esc_d %>% tabyl(sitio, transecto)

esc_d %>% tabyl(toponimia, transecto)

esc_d %>% tabyl(toponimia, localidad)

esc_d %>% tabyl(toponimia, localidad)

esc_d %>% tabyl(localidad, sitio)

esc_d %>% distinct(localidad, distance)

summary(esc_d$density)

# Data exploration 
ggplot(esc_d, aes(density)) +
  geom_histogram() +
  scale_x_log10()

# Data exploration 
ggplot(esc_d, aes(density)) +
  geom_histogram() +
  scale_x_continuous(trans = "log1p")

# density by date and sitio ---------
ggplot(esc_d, aes(month, density)) +
  geom_point(alpha = .5, size = 3, aes(color = factor(sitio)), position = position_jitter(width = .1)) +
  scale_y_continuous(trans = "log1p", expand = c(0, Inf), ) +
  facet_wrap(~localidad) 



# ggplot(esc_d, aes(abs_dist, density, color = mes)) +
#   geom_point(alpha = .5) +
#   geom_smooth(se = F, span = .45) +
#   facet_wrap(~orientation) +
#   scale_y_continuous(trans = "log1p")

dist_plot <- 
  ggplot(esc_d, aes(distance, density, color = month, group = month)) +
  scale_y_continuous(trans = "log1p", breaks = c(0,10, 50, 250, 1000) ) +
  geom_point(alpha = .4, position = position_jitter(width = .5)) +
  geom_smooth(stat = 'summary', fun.data = mean_se, alpha = .2) +
  # stat_summary(fun.data = mean_se, geom = 'point', shape = 2) +
  facet_wrap(~orientation, scales = 'fixed') +
  theme(legend.position = 'bottom') +
  labs(x = 'Distance (m)' , y = Individuals~per~100~m^2, parse = T)

dist_plot

ggsave(dist_plot, filename = 'figures/dist_plot.png', width = 6, height = 4)

# 
# ggplot(esc_d, aes(abs_dist, density, color = mes, group = mes)) +
#   scale_y_continuous(trans = "log1p") +
#   stat_summary(fun.data = mean_se, geom = 'line') +
#   stat_summary(fun.data = mean_se, geom = 'point') +
#   stat_summary(fun.data = mean_se, geom = 'errorbar') +
#   facet_wrap(~orientation, scales = 'free_x') +
#   theme(legend.position = 'bottom') +
#   labs(x = 'Distancia (m)' , y = Individuals~per~100~m^2, parse = T)

# distancia y direccion
ggplot(esc_d, aes(distance, density, color = mes, group = mes)) +
  scale_y_continuous(trans = "log1p") +
  stat_summary(fun.data = mean_se) +
  stat_summary(fun.data = mean_se, geom = 'line') +
  facet_wrap(~orientation, scales = 'free_x') +
  theme(legend.position = 'bottom') +
  labs(x = 'Distance (m)' , y = Nuumber~of~fish~per~100~m^2, parse = T) +
  scale_color_discrete(name = NULL)

# distance ----
ggplot(esc_d, aes(distance, density, color = month, group = month)) +
  scale_y_continuous(trans = "log1p", breaks = c(0,10, 50, 250, 1000) ) +
  geom_point(alpha = .4, position = position_jitter(width = .5)) +
  geom_smooth(stat = 'summary', fun.data = mean_se, alpha = .2) +
  theme(legend.position = 'bottom') +
  labs(x = 'Distance (m)' , y = Individuals~per~100~m^2, parse = T)


ggplot(esc_d, aes(distance, density, color = mes, group = mes)) +
  scale_y_continuous(trans = "log1p") +
  stat_summary(fun.data = mean_se) +
  stat_summary(fun.data = mean_se, geom = 'line') +
  # facet_wrap(~orientation, scales = 'free_x') +
  theme(legend.position = 'bottom') +
  labs(x = 'Distance (m)' , y = Number~of~fish~per~100~m^2, parse = T) +
  scale_color_discrete(name = NULL)


# Size ==----
size %>%
  ggplot(aes(size, number, fill = month)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(trans = 'log1p')


# maps------
library(sf)
library(leaflet)
murcia <- 
  read_sf('C:/Users/javiera/OneDrive - Cawthron/UA/Consumo/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>% 
  filter(Texto_Alt == "Murcia")

locations <- st_read("data/coords_gorguel.kml")

ggplot() +
  geom_sf(data = murcia) +
  geom_sf(data = locations, size = 3) 

leaflet(locations) %>%
  setView(lng = -1,
          lat = 37.6,
          zoom = 11) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers()


m1 <-
  glmmTMB(
    density ~ distance * month + orientation + (1|localidad:sitio),
    zi =  ~ distance + month,
    family = tweedie(link = "log"),
    data = esc_d
  )

# * Zero-inflation tests after fitting the model are crucial to see if you have zero-inflation. Just because there are a lot of zeros doesn't mean you have zero-inflation, see Warton, D. I. (2005). Many zeros does not mean zero inflation: comparing the goodness-of-fit of parametric models to multivariate abundance data. Environmetrics 16(3), 275-289.



tab_model(m1)
summary(m1)

tidy(m1)
glance(m1)
pred_d <- augment(m1)

ggplot(pred_d, aes(.fitted, .resid, color = distance)) +
  geom_point() +
  facet_wrap(~orientation)


ggplot(pred_d, aes(.fitted, density)) +
  geom_point()


# Models were fitted using a Tweedie error distribution which is more robust to overdispersed and zero-rich data than other members of the exponential distribution family (e.g. poisson,gamma; Shono, 2008). Residual plots were examined to check model assumptions. Models were compared using the Akaike's InformationCriterion (AIC) optimized for small samples sizes (AICc), with themost parsimonious model selected based on the lowest AICc (Δ AICc≤2) and least number of predictor variables (Burnham & Anderson,2002).

# Model validation ------
library(DHARMa)
testDispersion(m1)
simulationOutput <- simulateResiduals(fittedModel = m1, plot = F)
plot(simulationOutput)
testZeroInflation(simulationOutput)



### Size data
# compute mean size
mean_size <- 
  size_raw %>% 
  group_by(month) %>% 
  summarise(size = mean(size)) 

ggplot(size_raw, aes(size, fill = localidad)) +
  geom_histogram(bins = 10) +
  labs(x = "Fish total length (cm)", y = "Number of fish") +
  geom_vline(data = mean_size, aes(xintercept = size, group = month), lty = 2) +
  facet_wrap(~ month, scales = 'free_y')

size_raw %>% 
  group_by(localidad, month) %>% 
  count()

ggplot(size_raw, aes(month, size)) +
  geom_boxplot() 

library(ggfortify)
size_lm <- lm(log(size) ~ month*localidad, size_raw)
# autoplot(size_lm)
plot(size_lm)

ggplot(size_raw, aes(month, size)) +
  geom_boxplot() +
  facet_wrap(~loc_dist, scales = 'free_y', ncol = 1) 

ggplot(size_raw, aes(month, size, color = fct_reorder(loc_dist, distance) , group = fct_reorder(loc_dist, distance) )) +
  stat_summary(fun.data = mean_sdl,   position = position_dodge(width = .7)) +
  scale_color_discrete(name = NULL) +
  labs(x = NULL, y = "Total fish length (cm)" )


library(fishmethods)

clus_res <- 
  clus.lf(group=size$month,haul=paste(size$transecto,size$toponimia),
          len=size$size, number=size$number, 
          binsize=5,resamples=100)

clus_res$results %>% kable


ggplot(size_raw, aes(month, size)) +
  geom_boxplot() +
  facet_wrap(~factor(localidad) )
