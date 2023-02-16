library(tidyverse)
library(spaMM)
library(geosphere)
library(DHARMa)

esc_d <- 
  read_csv('data/gorguel_density_data_clean.csv') %>% 
  # filter(density < 500) %>% 
  mutate(month = fct_relevel(month, c("July","August"))) 

# murcia_bathy_cropped_df <- read_csv("data/murcia_bathy_cropped.csv")


# Prediction data
murcia_coast <- read_sf("data/murcia_coast.shp")
plot(murcia_coast)

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


dim(new_dat)

# new_dat <- 
#   expand_grid(read_csv("data/murcia_bathy_cropped.csv"), month = c('July',"August","September")) %>% 
#   mutate(month = fct_relevel(month, c("July","August"))) %>% 
#   mutate(distm(c(X = -0.87685249, Y = 37.57722501),.[,1:2]) %>% t() %>% as_tibble()/1000) %>% 
#   rename(distance = V1)

# hist(new_dat$distance)

# fit the model
# m_spamm <-
#   fitme(density + 0.1 ~ month + Matern(1|X + Y),
#         data = esc_d,
#         family = Gamma(log)) # this take a bit of time
# 
# # model summary
# summary(m_spamm)

m_spamm1 <-
  fitme(density + 1 ~ distance * month + Matern(1|X + Y),
        data = esc_d,
        family = Gamma(log),
        control.glm=list(maxit=100)) 

summary(m_spamm1)

# m_spamm2 <-
#   fitme(round(density,0) ~ distance * month + Matern(1|X + Y),
#         data = esc_d,
#         family = spaMM::negbin(),
#         control.glm=list(maxit=100)) 

# summary(m_spamm2)

# # LRT(m_spamm1, m_spamm2)
# AIC(m_spamm1)
# AIC(m_spamm2)

# observed vs predicted
predict(m_spamm1, newdata = esc_d, type = "response") %>% 
  data.frame() %>% 
  rename(pred = ".") %>% 
  bind_cols(esc_d) %>% 
  as_tibble() %>% 
  ggplot(aes(pred, density)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, lty = 2, color = 2) +
  theme_minimal() +
  tune::coord_obs_pred()



# predictions of the spamm model
preds_spamm <-
  new_dat %>%
  bind_cols(as_tibble(predict(m_spamm1, newdata = new_dat, type = "response")))

ggplot(preds_spamm) +
  geom_raster(aes(round(X,10), round(Y,10), fill =  V1)) +
  # geom_contour(aes(X,Y, z =  V1), colour = "gray60") +
  scale_fill_viridis_c(
    # trans = "log10",
    name = "Density",
    option = "G",
    labels = scales::comma_format(accuracy = 0.1),
    end = .9
  ) +
  theme_void() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ month, ncol = 1)


ggsave(last_plot(),
       filename = "figures/spamm_spatial_predictions.png", bg = "white",
       width = 6, height = 4)


# Using the glmmTMB library that allow tweedie error distribution-------
library(glmmTMB)

esc_d$pos <- numFactor(scale(esc_d$X), scale(esc_d$Y)) 
# then create a dummy group factor to be used as a random term
# esc_d$IDD <- factor(rep(1, nrow(esc_d)))
# fit the model

spa_model <-
  glmmTMB(
    density ~ month * distance + mat(pos + 0 | ID),
    family = tweedie(link = "log"),
    data = esc_d
  )

res_tmb <- summary(spa_model)
res_tmb$coefficients$cond %>% as_tibble(rownames = "Term") %>% janitor::clean_names()

# model validation 
sims <- simulateResiduals(spa_model)
plot(sims)
testZeroInflation(sims)

sims2  <- recalculateResiduals(sims, group = esc_d$ID)
plot(sims2)
# testSpatialAutocorrelation(sims2, x = esc_d$X, y= esc_d$Y)

ranef(spa_model) %>% as_tibble()

predict(spa_model, newdata = esc_d, type = "response") %>% 
  data.frame() %>% 
  rename(pred = ".") %>% 
  bind_cols(esc_d) %>% 
  as_tibble() %>% 
  ggplot(aes(pred, density)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = 2) +
  theme_minimal() +
  geom_smooth(method = lm) +
  tune::coord_obs_pred()


# predictions to all locations------
new_dat$pos <- numFactor(((new_dat$X - mean(esc_d$X)) / sd(esc_d$X)), ((new_dat$Y - mean(esc_d$Y)) / sd(esc_d$Y)))
# then create a dummy group factor to be used as a random term
new_dat$ID <- factor(rep(1, nrow(new_dat)))

# predict in slices of 1000 predictions to speed up computation
preds <- 
  new_dat %>%
  mutate(set = cut_interval(1:nrow(new_dat), 20, labels = c(1:20))) %>%
  group_by(set) %>%
  nest() %>%
  mutate(preds = map(
    data,
    ~ predict(
      spa_model,
      newdata = .x,
      allow.new.levels = TRUE,
      type = "response"
    )
  ))


new_dat_preds <- 
new_dat %>%
  bind_cols(preds %>%
              select(preds) %>%
              unnest(cols = preds) %>%
              ungroup() %>%
              select(preds))

write_csv(new_dat_preds, "data/new_dat_preds.csv")


# plot the predictions 
ggplot(new_dat) +
  geom_raster(aes(X, Y, fill =  preds)) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "Density",
    option = "G",
    labels = c(0, .1, 1, 10, 50),
    breaks = c(0, .1, 1, 10, 50),
    end = .9
  ) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom") +
  facet_wrap( ~ month, ncol = 1)


ggsave(last_plot(),
       filename = "figures/glmmtmb_spatial_predictions.png", bg = "white",
       width = 6, height = 4)
