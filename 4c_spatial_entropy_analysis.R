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
not_needed_cols <- c("Question", "Num_comparisons", "Relabelled.Name", "source", "orig_id")
ordered_countries <- c("All", "Chile", "Netherlands", "Nigeria", "Singapore", "USA")
ordered_cities <- c("All", "Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_cities_metadata = c("Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")

# load datasets and preprocessing ----
df_entropy_relative_all <- read.csv("data/labels/processed/spatial_entropy_relative_metrics.csv")

df_singleSVI_multiPar <- read.csv("data/labels/processed/singleSVI_multiPar_qscores.csv")
df_multiSVI_singlePar <- read.csv("data/labels/processed/multiSVI_singlePar_qscores.csv")

df_singleSVI_multiPar <- df_singleSVI_multiPar[df_singleSVI_multiPar$Question == "green",]
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$Question == "green",]

df_singleSVI_multiPar$SVI_from <- factor(df_singleSVI_multiPar$SVI_from, levels=ordered_cities)
df_multiSVI_singlePar$participants_from <- factor(df_multiSVI_singlePar$participants_from, levels=rev(ordered_countries))

df_metadata <- read.csv("data/svi/metadata.csv")
df_metadata$city <-factor(df_metadata$city, levels = ordered_cities_metadata) 

# filter images with green score <= Q1 and >= Q3 for single and multi SVI ----
filter_q1 <- function(df, split_col) {
  df <- df %>% 
    group_by(across(all_of(split_col))) %>%
    filter(Score <= quantile(Score, 0.25)) %>%
    ungroup()
  return(df)
}

filter_q3 <- function(df, split_col) {
  df <- df %>% 
    group_by(across(all_of(split_col))) %>%
    filter(Score >= quantile(Score, 0.75)) %>%
    ungroup()
  return(df)
}

# single SVI multi participant ratings
df_single_q1 <- filter_q1(df_singleSVI_multiPar, "SVI_from")
df_single_q3 <- filter_q3(df_singleSVI_multiPar, "SVI_from")

# multi SVI single participant ratings
df_multi_q1 <- filter_q1(df_multiSVI_singlePar, "participants_from")
df_multi_q3 <- filter_q3(df_multiSVI_singlePar, "participants_from")

# location pair ratings (svi from and participants from)
df_par_from_svi_from <- df_multiSVI_singlePar %>% 
  left_join(
    df_metadata %>%
      select(Image.number, city) %>%
      rename(SVI_from = city),
    by = c("Image" = "Image.number")
  )

df_par_from_svi_from <- df_par_from_svi_from[df_par_from_svi_from$participants_from != 'All',]
df_par_from_svi_from_q1 <- filter_q1(df_par_from_svi_from, c("SVI_from", "participants_from"))
df_par_from_svi_from_q3 <- filter_q3(df_par_from_svi_from, c("SVI_from", "participants_from"))

# merge with spatial entropy ----
merge_spatial_entropy <- function(df, quantile) {
  df_new <- df %>% # merge with entropy metrics,
    left_join(df_entropy_relative_all, by='uuid') %>%
    mutate(
      quantile = quantile
    )
  return(df_new)
}

# single SVI multi participant ratings
df_single_entropy_q1 <- merge_spatial_entropy(df_single_q1, "Q1")
df_single_entropy_q3 <- merge_spatial_entropy(df_single_q3, "Q3")

# multi SVI single participant ratings
df_multi_entropy_q1 <- merge_spatial_entropy(df_multi_q1, "Q1")
df_multi_entropy_q3 <- merge_spatial_entropy(df_multi_q3, "Q3")

# location pair ratings (svi from and participants from)
df_par_from_svi_from_entropy_q1 <- merge_spatial_entropy(df_par_from_svi_from_q1, "Q1")
df_par_from_svi_from_entropy_q3 <- merge_spatial_entropy(df_par_from_svi_from_q3, "Q3")

# combine q1 and q3 dataframes  ----
combine_q1_q3 <- function(df_q1, df_q3, split_col) {
  df_new <- rbind(df_q1, df_q3) %>%
    # filter selected columns
    select("uuid", "Image", all_of(split_col), "spatial_045", "quantile") %>%
    # change to long format
    pivot_longer( 
      cols = -c("uuid", "Image", all_of(split_col), "quantile"),
      names_to = "metric",
      values_to = "value"
    )
  return(df_new)
}

df_single_entropy_q1q3 <- combine_q1_q3(df_single_entropy_q1, df_single_entropy_q3, "SVI_from")
df_multi_entropy_q1q3 <- combine_q1_q3(df_multi_entropy_q1, df_multi_entropy_q3, "participants_from")
df_par_from_svi_from_entropy_q1q3 <- combine_q1_q3(df_par_from_svi_from_entropy_q1, df_par_from_svi_from_entropy_q3, c("SVI_from", "participants_from"))

# sample sizes within groups and quantiles ----
counts <- df_single_entropy_q1q3 %>% count(SVI_from, quantile)
cat("Group counts:\n")
print(counts)
cat("Minimum count:", min(counts$n), "\n") # n >= 20

counts <- df_multi_entropy_q1q3 %>% count(participants_from, quantile)
cat("Group counts:\n")
print(counts)
cat("Minimum count:", min(counts$n), "\n") # n >= 30

counts <- df_par_from_svi_from_entropy_q1q3 %>% count(SVI_from, participants_from, quantile)
cat("Group counts:\n")
print(counts)
cat("Minimum count:", min(counts$n), "\n") # n >= 14

# perform Mann-Whitney U test between quantile distributions ----

# TODO: perform for single and multi

mann_whitney_results <- df_par_from_svi_from_entropy_q1q3 %>%
  group_by(SVI_from, participants_from) %>%
  summarise(
    # Sample sizes
    n_Q1 = sum(quantile == "Q1"),
    n_Q3 = sum(quantile == "Q3"),
    
    # Descriptive statistics
    median_Q1 = median(value[quantile == "Q1"], na.rm = TRUE),
    median_Q3 = median(value[quantile == "Q3"], na.rm = TRUE),
    mean_Q1 = mean(value[quantile == "Q1"], na.rm = TRUE),
    mean_Q3 = mean(value[quantile == "Q3"], na.rm = TRUE),
    
    # Mann-Whitney U test
    mann_whitney_test = list(wilcox.test(
      value[quantile == "Q1"], 
      value[quantile == "Q3"],
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
# TODO: update with single and multi

# significance "*" position
star_positions <- df_par_from_svi_from_entropy_q1q3 %>%
  group_by(SVI_from, participants_from) %>%
  summarise(
    max_x = max(value, na.rm = TRUE),
    max_y = 1.0,  # Position at top of density plots
    .groups = "drop"
  ) %>%
  left_join(mann_whitney_results %>% 
              select(SVI_from, participants_from, significance), 
            by = c("SVI_from", "participants_from")) %>%
  # Only keep rows with significance markers
  filter(significance != "")

# plot with significance indicators ----
sample_size <- df_par_from_svi_from_entropy_q1q3 %>%
  group_by(SVI_from, participants_from, quantile) %>%
  count(name = "n")

custom_colors <- c("Q1" = "lightgreen", "Q3" = "darkgreen")  # Example: sea green and peru
p_location <- df_par_from_svi_from_entropy_q1q3 %>% 
  ggplot(aes(x = value, fill = quantile)) +
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
    name = "Quartiles of least (≤ Q1) and most (≥ Q3) perceived green SVI"
  ) +
  scale_color_manual(
    values = custom_colors,
    labels = c("Q1" = "≤ Q1", "Q3" = "≥ Q3"),
    name = "Quartiles of least (≤ Q1) and most (≥ Q3) perceived green SVI"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.00, by = 0.5),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1.00, by = 0.5),
    limits = c(0, 0.95)
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
  labs(x = expression(atop("Spatial entropy at relative window size of 0.45", bold("SVI from"))),
       y = expression(atop(bold("Participants from"), "Density")),
       title = NULL)
p_location

ggsave(
  "spatial_entropy_location.png",
  plot = p_location,
  path = "img/",
  height = 25,
  width = 20,
  scale = 1,
  dpi = 300,
)


