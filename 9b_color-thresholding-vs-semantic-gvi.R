library(extrafont)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(broom)
library(MetBrewer)

setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34
# ordered_countries = c("All", "Chile", "Netherlands", "Nigeria", "Singapore", "USA")
ordered_cities = c("All", "Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_cities_metadata = c("Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")

# load dataset and preprocessing ----
df_gvi_threshold <- read.csv("data/labels/processed/all_imgs_gvi_threshold.csv")

df_singleSVI_multiPar <- read.csv("data/labels/processed/singleSVI_multiPar_qscores.csv")
df_singleSVI_multiPar <- df_singleSVI_multiPar[df_singleSVI_multiPar$Question == "green",]

df_singleSVI_multiPar$SVI_from <- factor(df_singleSVI_multiPar$SVI_from, levels = ordered_cities)

# merge with metadata and gvi ----
df_gvi_comparisons <- df_singleSVI_multiPar%>%
  left_join(df_gvi_threshold, by='uuid')

# plot 1 ----
# green_view_index_threshold_pos uses a green thresholding of
# diff1 = green - red > 0
# diff2 = green - blue > 0
# the GVI masked is diff1 x diff2
p_gvi_comparisons_pos <- df_gvi_comparisons %>%
  ggplot(aes(x=green_view_index, y=green_view_index_threshold_pos)) +
  geom_point() +
  geom_smooth(method=lm) +
  stat_cor(method = "pearson", 
           label.x = 0.05,  # Adjust x position of the correlation text
           label.y = 0,  # Adjust y position of the correlation text
           size = 8,
           aes(label = ..r.label..)) +
  # scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  # scale_x_continuous(breaks = NULL) +
  # coord_cartesian(ylim = c(0, 1), xlim = c(0, 0.6)) + # Force exact dimensions
  theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(
      hjust = 0.5,
      size = 34,
      margin = margin(b = 8)
    ),
  ) +
  facet_wrap(~ SVI_from, nrow = 1) +
  labs(x = "Semantic GVI", y = "Color threshold GVI", title = "SVI from")
p_gvi_comparisons_pos

# plot 2 ----
# green_view_index_threshold_75 uses a green thresholding of
# diff1 = green - red > 0
# diff2 = green - blue 
# diff = diff1 x diff2 > otsu threshold
p_gvi_comparisons_75 <- df_gvi_comparisons %>%
  ggplot(aes(x=green_view_index, y=green_view_index_threshold_75)) +
  geom_point() +
  geom_smooth(method=lm) +
  stat_cor(method = "pearson", 
           label.x = 0.05,  # Adjust x position of the correlation text
           label.y = 0,  # Adjust y position of the correlation text
           size = 8,
           aes(label = ..r.label..)) +
  # scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  # scale_x_continuous(breaks = NULL) +
  coord_cartesian(ylim = c(0, 0.3), xlim = c(0, 0.6)) + # Force exact dimensions
  theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(
      hjust = 0.5,
      size = 34,
      margin = margin(b = 8)
    ),
  ) +
  facet_wrap(~ SVI_from, nrow = 1) +
  labs(x = "Color threshold GVI", y = "Semantic GVI", title = "SVI from")
p_gvi_comparisons_75
