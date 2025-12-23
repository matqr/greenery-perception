library(extrafont)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(corrplot)
library(MetBrewer)

# global variables ----
setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34
min_samples <- 30
col_lim <- c(-1, 1)
color_palette <- colorRampPalette(c("#404040",
                                    "#bababa",
                                    "#ffffff", 
                                    "#f4a582",
                                    "#CA0020"))
cols_single = c("Image", "Question", "Score", "SVI_from", "green_view_index")
cols_multi = c("Image", "Question", "Score", "participants_from", "green_view_index")
ordered_countries = c("All", "Chile", "Netherlands", "Nigeria", "Singapore", "USA")
ordered_cities = c("All", "Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_cities_metadata = c("Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_countries_metadata = c("Chile", "Netherlands", "Nigeria", "Singapore", "USA")

# load dataset and preprocessing ----
df_singleSVI_multiPar <- read.csv("data/labels/processed/singleSVI_multiPar_qscores.csv")
df_singleSVI_multiPar$SVI_from <- factor(df_singleSVI_multiPar$SVI_from, levels=ordered_cities)
df_singleSVI_multiPar <- df_singleSVI_multiPar[,cols_single]
df_singleSVI_multiPar <- df_singleSVI_multiPar[df_singleSVI_multiPar$Question %in% c("lively", "green"),]

df_multiSVI_singlePar <- read.csv("data/labels/processed/multiSVI_singlePar_qscores.csv")
# remove 'All', not used on this analysis
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$participants_from != 'All',]
df_multiSVI_singlePar$participants_from <- factor(df_multiSVI_singlePar$participants_from, levels=ordered_countries)
df_multiSVI_singlePar <- df_multiSVI_singlePar[,cols_multi]
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$Question %in% c("lively", "green"),]

df_metadata <- read.csv("data/svi/metadata.csv")
df_metadata$city <-factor(df_metadata$city, levels = ordered_cities_metadata) 
# normalizing Qscores values ----
# first convert to same scale, QScores are [0,10] and GVI is [0,1]
df_singleSVI_multiPar$Score_scaled <- df_singleSVI_multiPar$Score / 10
df_multiSVI_singlePar$Score_scaled <- df_multiSVI_singlePar$Score / 10

# add column svi location for multiSVI_singlePar ----
df_par_from_svi_from <- df_multiSVI_singlePar %>%
  left_join(
    df_metadata %>%
    select(Image.number, city) %>%
    rename(SVI_from = city),
    by = c("Image" = "Image.number")
  )

# reshape data for correlation analysis ----
# separate the data for 'lively' and 'green' questions
df_lively <- df_par_from_svi_from %>% 
  filter(Question == "lively") %>%
  select(Image, Score, participants_from, SVI_from)

df_green <- df_par_from_svi_from %>% 
  filter(Question == "green") %>%
  select(Image, Score, participants_from, SVI_from)

# join the two datasets by Image and grouping variables
df_combined <- inner_join(
  df_lively, 
  df_green, 
  by = c("Image", "participants_from", "SVI_from"),
  suffix = c("_lively", "_green")
)

# calculate correlations for each participants_from and SVI_from combination ----
cor_test_function <- function(x, y) {
  # Function to calculate correlation and p-value
  test_result <- cor.test(x, y, use = "complete.obs")
  return(c(correlation = test_result$estimate, 
           p_value = test_result$p.value,
           lower_ci = test_result$conf.int[1],
           upper_ci = test_result$conf.int[2]))
}

corr_matrix <- df_combined %>%
  group_by(participants_from, SVI_from) %>%
  summarize(
    correlation = cor(Score_lively, Score_green, use = "complete.obs"),
    p_value = cor.test(Score_lively, Score_green, use = "complete.obs")$p.value,
    n = n(),
    .groups = "drop"
  ) %>%
  # Filter out groups with insufficient data for correlation
  filter(n >= min_samples) %>%
  # Add significance indicators
  mutate(
    significance = case_when(
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # Create a label combining correlation and significance
    label = paste0(sprintf("%.2f", correlation), significance),
    # add font face column for bold when significant
    fontface = ifelse(p_value < 0.05, "bold", "plain")
  )

# heatmap with ggplot ----
p_heatmap_matrix <- corr_matrix %>%
  ggplot(aes(x = SVI_from, y = participants_from, fill = correlation)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label, fontface = fontface), color = "black", size = 10) +
  scale_fill_gradientn(colours = color_palette(200),
                       limits = col_lim,
                       name = "Pearson\nCorrelation") +
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
    legend.key.height = unit(2, "cm"),  # Make colorbar longer
    legend.key.width = unit(1, "cm"),  # Adjust width if needed
  ) + 
  # coord_equal() + # Make tiles square
  labs(x = "SVI from", y = "Participants from")
p_heatmap_matrix

# save figure ----
ggsave(
  "green-lively-matrix-paired-locations.png",
  plot = p_heatmap_matrix,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

# scatter plot of correlations ----
## theme ----
pearson_corr_theme <- theme_minimal(base_family = font_family, base_size = base_size) +
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
    # Bold facet labels
    strip.text = element_text(face = "bold", family = font_family),
  )

## Calculate R^2 for each facet group ----
calculate_cubic_r2 <- function(data) {
  if(nrow(data) < 4) return(data.frame(r2 = NA, p_value = NA, label_x = 0.05, label_y = 0))
  
  model <- lm(Score_lively ~ Score_green + I(Score_green^2) + I(Score_green^3), data = data)
  model_summary <- summary(model)
  r2 <- model_summary$r.squared
  
  # Get overall model p-value from F-statistic
  f_stat <- model_summary$fstatistic
  p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  
  data.frame(
    r2 = r2,
    p_value = p_value,
    label_x = 0.05,
    label_y = 0
  )
}

r2_data <- df_combined %>%
  group_by(participants_from, SVI_from) %>%
  do(calculate_cubic_r2(.)) %>%
  ungroup()

## actual plot ----
df_combined$participants_from <- factor(df_combined$participants_from, levels=rev(ordered_countries_metadata))

sample_size <- df_combined %>%
  group_by(SVI_from, participants_from) %>%
  count(name = "n")

p_location_scatter <- df_combined %>%
  ggplot(aes(x=Score_green, y=Score_lively)) +
  geom_point() +
  # geom_smooth(method=lm) +
  # stat_cor(method = "pearson",
           # label.x = 0.05,  # Adjust x position of the correlation text
           # label.y = 0,  # Adjust y position of the correlation text
           # size = 8,
           # aes(label = paste0("R^2 = ", round(..r..^2, 2)))) +
  geom_smooth(method="lm", formula = y ~ x + I(x^2) + I(x^3)) +
  geom_text(data = r2_data,
            aes(x = label_x, y = label_y, 
                label = paste0("R² = ", round(r2, 2), 
                               ifelse(p_value < 0.05 & !is.na(p_value), "*", "")),
                fontface = ifelse(p_value < 0.05 & !is.na(p_value), "bold", "plain")),
            size = 8,  # Convert from stat_cor size to geom_text size
            hjust = 0, vjust = 0) +
  scale_y_continuous(breaks = seq(0, 10, by = 5)) +
  scale_x_continuous(breaks = seq(0, 10, by = 5)) +
  coord_cartesian(ylim = c(0, 10), xlim = c(0, 10)) + # Force exact dimensions
  pearson_corr_theme + 
  facet_grid(
    participants_from ~ SVI_from,  # Format: rows ~ columns
    switch = "both",
    labeller = labeller(
      SVI_from = label_value,  # Show only values, no variable names
      participants_from = label_value
    )
  ) +
  labs(
    x = expression(atop("Green perception Q scores", bold("SVI from"))),
    y = expression(atop(bold("Participants from"), "Lively perception Q scores")),
    title = NULL)
p_location_scatter

ggsave(
  "green-lively-scatter-paired-locations.png",
  plot = p_location_scatter,
  path = "img/",
  height = 20,
  width = 20,
  scale = 1,
  dpi = 300,
)

