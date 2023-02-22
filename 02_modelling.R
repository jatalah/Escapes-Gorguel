library(glmmTMB)
library(tidyverse)
library(DHARMa)
library(broom.mixed)

rm(list = ls())

esc_d <- 
  read_csv('data/gorguel_density_data_clean.csv') %>%
  mutate(month = fct_relevel(month, c("July","August"))) 


# fit zero-infalted model
zi_model <-
  glmmTMB(
    density ~ distance * month + orientation + (1|localidad:sitio),
    zi =  ~ distance + month,
    family = tweedie(link = "log"),
    data = esc_d
  )


testZeroInflation(zi_model, plot = F) %>% tidy()

# The test showed that the expected distribution of zeros is not larger than the observed values, thus there is no need to incorporate zero-inflation in the model.

glance(zi_model) 


# fir model without zero inflation---
m1 <-
  glmmTMB(
    density ~ distance * month + orientation + (1|localidad:sitio),
    family = tweedie(link = "log"),
    data = esc_d
  )

# model selection 
drop1(m1, trace = F) %>% 
  tidy() 


# The lowest AIC value (716.97) was for the model with the distance and month interaction and no effect of orientation (density ~ distance * month).

# All AIC values were lower than that for the zero-inflated model.

# Model validation --------------
# fit final model
final_model <-
  glmmTMB(
    density ~ distance * month  + (1|localidad:sitio),
    family = tweedie(link = "log"),
    data = esc_d
  )

simulationOutput <- simulateResiduals(fittedModel = final_model, plot = F)
plot(simulationOutput)

# There are no residual patterns or over-dispersion. The residuals test identified one outlier that could be potentially removed and some mild deviation that is not of concern.

summary(final_model)

tidy(final_model, exponentiate = T)

# Interpret the interaction 
exp(-0.18722 +  0.1036) # 0.91
exp(-0.18722 + 0.11065) # 0.92

exp(-0.18722)*exp(0.10364)
0.829*1.11

exp(-0.18722)*exp(0.11065)
0.829*1.12
