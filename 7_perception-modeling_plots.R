library(extrafont)
library(tidyverse)
library(MetBrewer)

# global variables ----
setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34

# load datasets and preprocessing ----
df_permutation <- read.csv("data/model/rf_permutation_importance.csv")
df_conditional_permutation <- read.csv("data/model/rf_conditional_permutation_importance.csv")

# rename features
df_permutation$feature[df_permutation$feature == "green_view_index"] <- "Green View Index (GVI)"
df_permutation$feature[df_permutation$feature == "spatial_entropy"] <- "Spatial entropy"
df_permutation$feature[df_permutation$feature == "svi_from"] <- "SVI from"
df_permutation$feature[df_permutation$feature == "shannon_entropy"] <- "Shannon entropy"
df_permutation$feature[df_permutation$feature == "sky_view_index"] <- "Sky View Index"
df_permutation$feature[df_permutation$feature == "participants_from"] <- "Participants from"
df_permutation$feature[df_permutation$feature == "safe"] <- "Safe"
df_permutation$feature[df_permutation$feature == "beautiful"] <- "Beautiful"
df_permutation$feature[df_permutation$feature == "live nearby"] <- "Live nearby"
df_permutation$feature[df_permutation$feature == "wealthy"] <- "Wealthy"
df_permutation$feature[df_permutation$feature == "depressing"] <- "Depresing"
df_permutation$feature[df_permutation$feature == "cycle"] <- "Cycle"
df_permutation$feature[df_permutation$feature == "walk"] <- "Walk"
df_permutation$feature[df_permutation$feature == "boring"] <- "Boring"
df_permutation$feature[df_permutation$feature == "lively"] <- "Lively"

df_conditional_permutation$feature[df_conditional_permutation$feature == "green_view_index"] <- "Green View Index (GVI)"
df_conditional_permutation$feature[df_conditional_permutation$feature == "spatial_entropy"] <- "Spatial entropy"
df_conditional_permutation$feature[df_conditional_permutation$feature == "svi_from"] <- "SVI from"
df_conditional_permutation$feature[df_conditional_permutation$feature == "shannon_entropy"] <- "Shannon entropy"
df_conditional_permutation$feature[df_conditional_permutation$feature == "sky_view_index"] <- "Sky View Index"
df_conditional_permutation$feature[df_conditional_permutation$feature == "participants_from"] <- "Participants from"
df_conditional_permutation$feature[df_conditional_permutation$feature == "safe"] <- "Safe"
df_conditional_permutation$feature[df_conditional_permutation$feature == "beautiful"] <- "Beautiful"
df_conditional_permutation$feature[df_conditional_permutation$feature == "live nearby"] <- "Live nearby"
df_conditional_permutation$feature[df_conditional_permutation$feature == "wealthy"] <- "Wealthy"
df_conditional_permutation$feature[df_conditional_permutation$feature == "depressing"] <- "Depresing"
df_conditional_permutation$feature[df_conditional_permutation$feature == "cycle"] <- "Cycle"
df_conditional_permutation$feature[df_conditional_permutation$feature == "walk"] <- "Walk"
df_conditional_permutation$feature[df_conditional_permutation$feature == "boring"] <- "Boring"
df_conditional_permutation$feature[df_conditional_permutation$feature == "lively"] <- "Lively"


# plot ----
p_permutation <- df_permutation %>% 
  arrange(importance_mean) %>%  # Sort by importance_mean in descending order
  mutate(feature = factor(feature, levels = feature)) %>% # preserve order 
  ggplot(aes(x = feature, y = importance_mean)) +
  geom_bar(stat="identity", fill="green", alpha=0.5) +
  geom_pointrange(
    aes(
      x=feature, 
      ymin=importance_mean - importance_std, 
      ymax=importance_mean + importance_std
    ), 
    colour = "darkgreen", 
    alpha = 0.9, 
    size = 2,
    linewidth = 4
  ) +
  coord_flip() +
  theme_minimal(base_family = font_family,  base_size = 34) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    legend.position = "bottom",
    legend.title = element_text(family = font_family),
    legend.direction = "horizontal",
  ) + 
  labs(x = "Feature",
       y = "Peformance drop (MSE)",
       title = NULL)
p_permutation
  
ggsave(
  "permutation_feature_importance.png",
  plot = p_permutation,
  path = "img/",
  height = 7,
  width = 20,
  scale = 1,
  dpi = 300,
)

# conditional plot ----
p_permutation <- df_conditional_permutation %>% 
  arrange(importance_mean) %>%  # Sort by importance_mean in descending order
  mutate(feature = factor(feature, levels = feature)) %>% # preserve order 
  ggplot(aes(x = feature, y = importance_mean)) +
  geom_bar(stat="identity", fill="green", alpha=0.5) +
  geom_pointrange(
    aes(
      x=feature, 
      ymin=importance_mean - importance_std, 
      ymax=importance_mean + importance_std
    ), 
    colour = "darkgreen", 
    alpha = 0.9, 
    size = 2,
    linewidth = 4
  ) +
  coord_flip() +
  theme_minimal(base_family = font_family,  base_size = 34) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    legend.position = "bottom",
    legend.title = element_text(family = font_family),
    legend.direction = "horizontal",
  ) + 
  labs(x = "Feature",
       y = "Peformance drop (MSE)",
       title = NULL)
p_permutation

ggsave(
  "conditional_permutation_feature_importance.png",
  plot = p_permutation,
  path = "img/",
  height = 7,
  width = 20,
  scale = 1,
  dpi = 300,
)

