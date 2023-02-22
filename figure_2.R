library(tidyverse)

# read data----------
esc_d <- 
  read_csv('data/gorguel_density_data_clean.csv') %>%
  mutate(month = fct_relevel(month, c("July","August"))) 

#data summaries------------
dd <-
  esc_d %>%
  group_by(distance, orientation, month) %>%
  summarise(
    mean = mean(density),
    sd = sd(density),
    n = n(),
    se = sd / sqrt(n),
    .groups = 'drop'
  ) %>% 
  add_row(
    distance = rep(0, 3),
    orientation = rep("W", 3),
    month = c("July", "August", "September"),
    mean = c(114, 08, 0.0333),
    se = c(44.7, 0.711, 0.0225)
  ) %>% 
  mutate(distance = if_else(distance == 0.7, 0, distance),
         lower = mean - se,
         upper = mean + se) %>% 
  arrange(distance)

# sketch plot-------------
ggplot(dd, aes(distance, mean, color = month)) +
  geom_point(
    # position = position_jitter(width = .5, seed = 123),
    size = 1,
    alpha = .8
  ) +
  geom_errorbar(
    aes(ymin = lower , ymax = upper),
    width = 0.1,
    # position = position_jitter(width = .5, seed = 123),
    alpha = .8
  ) +
  geom_line(linewidth = .2) +
  facet_wrap(~ fct_rev(orientation), scales = 'fixed') +
  scale_y_continuous(
    trans = 'sqrt',
    breaks = c(0, 10, 25, 50, 100, 150),
    labels = c(0, 10, 25, 50, 100, 150)
  ) +
  theme_bw(base_size = 7) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = c(.88, .8),
    legend.key.size = unit(.3, 'cm'),
    legend.background = element_rect(fill = "transparent"),
    panel.border = element_rect(color = "gray30"),
    axis.ticks = element_line(color =  "gray30")
  ) +
  labs(x = 'Distance from escape (km)' ,
       y = Density ~ (fish ~ 100 ~ m ^ -2),
       parse = T) +
  scale_color_tableau(name = NULL)

last_plot()


# save figure 2--------------
ggsave(
  last_plot(),
  filename = "figures/figure_2.png",
  width = 90,
  height = 40,
  bg = "white",
  units = "mm",
  dpi = 300
)

ggsave(
  last_plot(),
  filename = "figures/figure_2.pdf",
  width = 90,
  height = 40,
  bg = "white",
  units = "mm",
  dpi = 300
)
