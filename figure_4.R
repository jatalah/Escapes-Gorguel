library(tidyverse)
rm(list = ls())

sites <- 
  read_csv('data/gorguel_density_data_clean.csv') %>% 
  distinct(orientation, localidad)

size_raw <-
  read_csv('data/size_data_raw.csv', show_col_types = F) %>%
  left_join(sites) %>%
  dplyr::filter(size <= 30) %>%
  mutate(
    month = fct_relevel(month, "July", "August"),
    # loc_dist = fct_reorder(loc_dist, distance),
    loc_dist = fct_relevel(
      loc_dist,
      "Azohía (45.3 km)",
      "Portus (26.4 km)",
      "Calacortina (12.6 km)",
      "Escombreras (6.4 km)",
      "Gorguel (0.7 km)",
      "Portman (4.4 km)",
      "Atamaría (10.1 km)"
    ),
    loc_dist = fct_recode(loc_dist, `Cala Cortina (12.6 km)` = "Calacortina (12.6 km)"),
    loc_dist2 = if_else(orientation == "E", distance,-1 * distance),
    loc_dist3 = fct_reorder(paste0(localidad, " ", distance," km ", orientation), loc_dist2),
  )

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


# Histogram option----
ggplot(size_raw,
       aes(
         size,
         fill = loc_dist3,
         color = loc_dist3
       )) +
  geom_histogram(bins = 12, alpha = .70, na.rm = T) +
  facet_grid(month ~ loc_dist3, scales = "fixed", labeller = label_wrap_gen(width=10)) +
  scale_fill_tableau(name = NULL) +
  scale_color_tableau(name = NULL) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(breaks = seq(10,30,10)) +
  labs(x = "Total fish length (cm)", y = "Count") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none") 



ggsave(
  last_plot(),
  filename = "figures/figure_4_histogram.png",
  width = 180,
  height = 70,
  bg = "white",
  units = "mm",
  dpi = 300
)

# relative frequency -----
ggplot(size_raw,
       aes(
         size,
         fill = month,
         color = month
       )) +
  geom_histogram(aes(y = ..density..), binwidth = 2, alpha = 0.5, position = position_dodge(), bins = 10) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_tableau(name = NULL) +
  scale_color_tableau(name = NULL) +
  labs(x = "Total fish length (cm)", y = NULL) +
  theme_minimal(base_size = 8) 

ggplot(size_raw,
       aes(
         size,
         fill = month,
         color = month
       )) +
  geom_histogram(aes(y = ..density..), binwidth = 2, alpha = 0.5, position = position_dodge(), bins = 10) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_tableau(name = NULL) +
  scale_color_tableau(name = NULL) +
  labs(x = "Total fish length (cm)", y = NULL) +
  theme_minimal(base_size = 8) 


ggplot(size_raw,
       aes(
         size,
         fill = loc_dist3,
         color = loc_dist3
       )) +
  geom_histogram(aes(y = ..density..), bins = 12, alpha = .70, na.rm = T) +
  facet_grid(month ~ loc_dist3, scales = "fixed", labeller = label_wrap_gen(width=10)) +
  scale_fill_tableau(name = NULL) +
  scale_color_tableau(name = NULL) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(10,30,10)) +
  labs(x = "Total fish length (cm)") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none") 


ggplot(size_raw,
       aes(
         size,
         fill = loc_dist2,
       )) +
  geom_histogram(bins = 12, na.rm = T) +
  facet_grid(month ~ loc_dist3, scales = "fixed", labeller = label_wrap_gen(width=10)) +
  # colorspace::scale_fill_continuous_divergingx(palette = 'RdGy', mid = 0.7) +
  scale_fill_gradient2(
    low = "blue", 
    mid = "gray", 
    high = "darkred", 
    midpoint = .7
  ) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(breaks = seq(10,30,10)) +
  labs(x = "Total fish length (cm)", y = "Count") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none") 


# data summaries------
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
