library(tidyverse)

size_raw <-
  read_csv('data/size_data_raw.csv', show_col_types = F) %>%
  mutate(month = fct_relevel(month, "July", "August"),
         loc_dist = fct_reorder(loc_dist, distance)) %>% 
  filter(size<=30)

ggplot(size_raw,
       aes(
         month,
         size,
         color = loc_dist ,
         group = loc_dist
       )) +
  stat_summary(fun.data = mean_sdl,   position = position_dodge(width = .7)) +
  scale_color_tableau(name = NULL) +
  labs(x = NULL, y = "Total fish length (cm)") +
  theme_bw(base_size = 7) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = c(.2, .8),
    legend.key.size = unit(.3, 'cm'),
    legend.background = element_rect(fill = "transparent"),
    panel.border = element_rect(color = "gray30"),
    axis.ticks = element_line(color =  "gray30")
  ) 

ggsave(
  last_plot(),
  filename = "figures/figure_4.pdf",
  width = 90,
  height = 60,
  bg = "white",
  units = "mm",
  dpi = 300
)


ggsave(
  last_plot(),
  filename = "figures/figure_4.png",
  width = 90,
  height = 60,
  bg = "white",
  units = "mm",
  dpi = 300
)


size_raw %>%
  group_by(distance, month) %>%
  summarise(
    mean = mean(size),
    min = min(size),
    max = max(size),
    sd = sd(size),
    n = n(),
    se = sd / sqrt(n),
    .groups = 'drop'
  ) %>%
  arrange(month)