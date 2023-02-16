library(tidyverse)

# read data----------
esc_d <- 
  read_csv('data/gorguel_density_data_clean.csv') %>%
  mutate(month = fct_relevel(month, c("July","August"))) 

#data summaries------------
esc_d %>% 
  group_by(distance, month) %>% 
  summarise(
    mean = mean(density),
    sd = sd(density),
    n = n(),
    se = sd / sqrt(n),
    .groups = 'drop'
  ) %>% 
  arrange(month) %>% 
  dplyr::filter(month == "August")


# sketch plot-------------
ggplot(esc_d, aes(distance, density, color = month, group = month)) +
  scale_y_continuous(trans = "log1p", breaks = c(0,10,100,1000), labels = c(0,10,100,1000)) +
  geom_point(alpha = .2,
             position = position_jitter(width = .5),
             size = 1) +
  stat_summary(fun.data = "mean_se",
               size = .1) +
  stat_summary(fun.data = mean_se,
               geom = 'line',
               linewidth = .1) +
  facet_wrap(~ fct_rev(orientation), scales = 'fixed') +
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


# save figure 2--------------
ggsave(
  last_plot(),
  filename = "figures/figure_2.pdf",
  width = 90,
  height = 40,
  bg = "white",
  units = "mm",
  dpi = 300
)

ggsave(
  last_plot(),
  filename = "figures/figure_2.png",
  width = 90,
  height = 40,
  bg = "white",
  units = "mm",
  dpi = 300
)
