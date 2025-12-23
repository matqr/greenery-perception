library(extrafont)
library(tidyverse)
library(ggtext)
library(ggdist)
library(glue)
library(rstatix)
library(patchwork)
library(MetBrewer)

# global variables ----
setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34
ordered_countries = c("All", "Chile", "Netherlands", "Nigeria", "Singapore", "USA")
ordered_cities = c("All", "Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")

# load datasets and preprocessing ----
df_depths <- read.csv("data/depth/qscores_by_q1q3_median_depth.csv")
df_depths_single <- df_depths[df_depths$grouping == 'SVI_from',]
df_depths_multi <- df_depths[df_depths$grouping == 'participants_from',]
df_depths_location <- df_depths[df_depths$grouping == 'location_pairs',]

df_depths_location$SVI_from <- factor(df_depths_location$SVI_from, levels=ordered_cities)
df_depths_location$participants_from <- factor(df_depths_location$participants_from, levels=rev(ordered_countries))

# TODO: perform analysis for single and multi

# perform Mann-Whitney U test between quantile distributions ----
mann_whitney_results <- df_depths_location %>%
  group_by(SVI_from, participants_from) %>%
  summarise(
    # Sample sizes
    n_Q1 = sum(quantile == "Q1"),
    n_Q3 = sum(quantile == "Q3"),
    
    # Descriptive statistics
    median_Q1 = median(Score[quantile == "Q1"], na.rm = TRUE),
    median_Q3 = median(Score[quantile == "Q3"], na.rm = TRUE),
    mean_Q1 = mean(Score[quantile == "Q1"], na.rm = TRUE),
    mean_Q3 = mean(Score[quantile == "Q3"], na.rm = TRUE),
    
    # Mann-Whitney U test
    mann_whitney_test = list(wilcox.test(
      Score[quantile == "Q1"], 
      Score[quantile == "Q3"],
      alternative = "two.sided"  # Change to "greater" or "less" if you have a directional hypothesis
    )),
    
    # Extract results
    p_value = map_dbl(mann_whitney_test, ~ .x$p.value),
    w_statistic = map_dbl(mann_whitney_test, ~ .x$statistic),
    
    .groups = "drop"
  ) %>%
  # Remove the list column to clean up
  select(-mann_whitney_test) %>%
  
  # Apply multiple comparisons correction (FDR)
  mutate(
    p_adjusted = p.adjust(p_value, method = "fdr"),
    
    # Add significance indicators
    significance = case_when(
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""
    ),
    
    # Calculate effect size (r = Z/sqrt(N))
    # First need to convert W to Z-score approximation
    n_total = n_Q1 + n_Q3,
    expected_w = (n_Q1 * n_total) / 2,
    z_score = (w_statistic - expected_w) / sqrt((n_Q1 * n_Q3 * n_total) / 12),
    effect_size_r = abs(z_score) / sqrt(n_total),
    
    # Effect size interpretation
    effect_magnitude = case_when(
      effect_size_r < 0.1 ~ "negligible",
      effect_size_r < 0.3 ~ "small",
      effect_size_r < 0.5 ~ "medium",
      TRUE ~ "large"
    ),
    
    # Direction of effect (which group has higher values)
    direction = ifelse(median_Q1 > median_Q3, "Q1 > Q3", "Q3 > Q1")
  )

# plot ----

# TODO: perform analysis for single and multi

# significance "*" position
star_positions <- df_depths_location %>%
  group_by(SVI_from, participants_from) %>%
  summarise(
    max_x = max(Score, na.rm = TRUE),
    # max_x = max(mean_depth_hist, na.rm = TRUE),
    max_y = 1.0,  # Position at top of density plots
    .groups = "drop"
  ) %>%
  # Join with your Mann-Whitney results
  left_join(mann_whitney_results %>% 
              select(SVI_from, participants_from, significance), 
            by = c("SVI_from", "participants_from")) %>%
  # Only keep rows with significance markers
  filter(significance != "")

custom_colors <- c("Q1" = "#404040", "Q3" = "#f4a582")
p_location <- df_depths_location %>% 
  ggplot(aes(x = Score, fill = quantile)) +
  stat_halfeye(
    aes(color=quantile),
    fill_type = "segments", 
    alpha = 0.7,
    point_size = 7,
    interval_size = 15,
    point_interval = median_qi) +
  # Add significance stars
  geom_text(
    data = star_positions,
    aes(x = max_x * 0.9, y = 0.5, label = significance),
    inherit.aes = FALSE,  # Don't inherit fill/color aesthetics
    size = 12,  # Adjust size as needed
    color = "black",
    fontface = "bold",
    hjust = 1,
    vjust = 1
  ) +
  scale_fill_manual(
    values = custom_colors,
    labels = c("Q1" = "≤ Q1", "Q3" = "≥ Q3"),
    name = "Quartiles of lowest (≤ Q1) and highest (≥ Q3) median depth values in SVI"
  ) +
  scale_color_manual(
    values = custom_colors,
    labels = c("Q1" = "≤ Q1", "Q3" = "≥ Q3"),
    name = "Quartiles of lowest (≤ Q1) and highest (≥ Q3) median depth values in SVI"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.00, by = 0.5),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 5),
    limits = c(0, 10)
  ) +
  theme_minimal(base_family = font_family,  base_size = 34) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    legend.position = "bottom",
    legend.title = element_text(family = font_family),
    legend.direction = "horizontal",
    # Bold facet labels
    strip.text = element_text(face = "bold", family = font_family),
  ) + 
  facet_grid(
    participants_from ~ SVI_from,  # Format: rows ~ columns
    switch = "both",
    labeller = labeller(
      SVI_from = label_value,  # Show only values, no variable names
      participants_from = label_value
    )
  ) +
  labs(x = expression(atop("Q scores", bold("SVI from"))),
       y = expression(bold("Participants from"), ("Density")),
       title = NULL)
p_location

ggsave(
  "qscore_dist_bydepth_location.png",
  plot = p_location,
  path = "img/",
  height = 25,
  width = 20,
  scale = 1,
  dpi = 300,
)

