library(extrafont)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(MetBrewer)

# global variables ----
setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34

#window_relative_sizes <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.8, 0.85, 0.90, 0.95, 1.00)
window_relative_sizes <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.8, 0.85, 0.90, 0.95)

# load dataset ----
df_entropy_relative_all <- read.csv("data/labels/processed/entropy_metrics_relative.csv")

# plot entropy vs relative window size  ----

# filter out images with 0 entropy
df_entropy_relative_filtered <- df_entropy_relative_all[rowSums(df_entropy_relative_all[,-1] > 0, na.rm = TRUE) > 0, ]

# convert to long format
df_long <- df_entropy_relative_filtered %>%
  pivot_longer(-uuid, names_to = "WindowSize", values_to = "Entropy") %>%
  mutate(WindowSize = as.numeric(gsub("spatial_", "", WindowSize)) / 100)  # Convert names to numeric window sizes

# Compute mean entropy for each window size
mean_entropy_relative <- df_long %>%
  group_by(WindowSize) %>%
  summarise(MeanEntropy = mean(Entropy, na.rm = TRUE))

max_point <- mean_entropy_relative %>%
  filter(MeanEntropy == max(MeanEntropy)) %>%
  pull(WindowSize) %>%
  first()  

# Plot all individual entropy curves with transparency
p <- df_long %>%
  ggplot(aes(x = WindowSize, y = Entropy, group = uuid)) +
  geom_line(alpha = 1, color = "gray") +  # Individual lines with transparency
  stat_summary(
    fun = mean, 
    geom = "line", aes(group = 1), 
    color = "darkgreen", size = 5) +
  geom_vline(xintercept = max_point, linetype = "dashed", color = "black", size = 1) +
  scale_y_continuous(
    breaks = seq(0, 1.00, by = 0.25),
    limits = c(0, 1.0)
  ) +
  scale_x_continuous(
    # show increments of 0.25 from 0 but also show 0.1, and the max_point intercept
    breaks = sort(unique(c(seq(0, 1, by = 0.25), max_point, 0.1, 0.95))),
    limits = c(0.10, 0.95)
  ) +
  theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
  ) + 
  labs(x = "Relative window size (fraction of image)", 
           y = "Spatial entropy")
p

ggsave(
  "entropy_relative_window_sensitivity.png",
  plot = p,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

