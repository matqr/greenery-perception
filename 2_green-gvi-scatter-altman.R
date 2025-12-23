library(extrafont)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(broom)
library(MetBrewer)

# global variables ----
setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34
min_samples <- 30
ordered_countries = c("All", "Chile", "Netherlands", "Nigeria", "Singapore", "USA")
ordered_cities = c("All", "Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_cities_metadata = c("Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_countries_metadata = c("Chile", "Netherlands", "Nigeria", "Singapore", "USA")

# load datasets and preprocessing ----
df_singleSVI_multiPar <- read.csv("data/labels/processed/singleSVI_multiPar_qscores.csv")
df_multiSVI_singlePar <- read.csv("data/labels/processed/multiSVI_singlePar_qscores.csv")

df_singleSVI_multiPar <- df_singleSVI_multiPar[df_singleSVI_multiPar$Question == "green",]
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$Question == "green",]

df_singleSVI_multiPar$SVI_from <- factor(df_singleSVI_multiPar$SVI_from, levels = ordered_cities)
df_multiSVI_singlePar$participants_from <- factor(df_multiSVI_singlePar$participants_from, levels = ordered_countries)

df_metadata <- read.csv("data/svi/metadata.csv")
df_metadata$city <-factor(df_metadata$city, levels = rev(ordered_cities_metadata)) 

# min-max normalise GVI [0, 1]
df_singleSVI_multiPar$green_view_index_unnorm <- df_singleSVI_multiPar$green_view_index
df_multiSVI_singlePar$green_view_index_unnorm <- df_multiSVI_singlePar$green_view_index

df_singleSVI_multiPar$green_view_index <- (df_singleSVI_multiPar$green_view_index - min(df_singleSVI_multiPar$green_view_index)) / (max(df_singleSVI_multiPar$green_view_index) - min(df_singleSVI_multiPar$green_view_index))
df_multiSVI_singlePar$green_view_index <- (df_multiSVI_singlePar$green_view_index - min(df_multiSVI_singlePar$green_view_index)) / (max(df_multiSVI_singlePar$green_view_index) - min(df_multiSVI_singlePar$green_view_index))

# add column svi location for multiSVI_singlePar
df_par_from_svi_from <- df_multiSVI_singlePar %>% 
  left_join(
    df_metadata %>%
      select(Image.number, city) %>%
      rename(SVI_from = city),
    by = c("Image" = "Image.number")
  )

df_par_from_svi_from <- df_par_from_svi_from[df_par_from_svi_from$participants_from != 'All',]
df_par_from_svi_from$SVI_from <- factor(df_par_from_svi_from$SVI_from, levels=ordered_cities_metadata)
df_par_from_svi_from$participants_from <- factor(df_par_from_svi_from$participants_from, levels=rev(ordered_countries_metadata))

# Filter dataframes to have a more than 30 samples per grouping
df_singleSVI_multiPar <- df_singleSVI_multiPar %>%
  group_by(SVI_from) %>%
  filter(n() >= min_samples) %>%
  ungroup()

df_multiSVI_singlePar <- df_multiSVI_singlePar %>%
  group_by(participants_from) %>%
  filter(n() >= min_samples) %>%
  ungroup()

df_par_from_svi_from <- df_par_from_svi_from %>%
  group_by(SVI_from, participants_from) %>%
  filter(n() >= min_samples) %>%
  ungroup()
# => all existing groups have n>30 (images with at least four pairwise comparisons)

# scaling Qscores values ----
# first convert to same scale, QScores are [0,10] and GVI is [0,1]
df_singleSVI_multiPar$Score_scaled <- df_singleSVI_multiPar$Score / 10
df_multiSVI_singlePar$Score_scaled <- df_multiSVI_singlePar$Score / 10
df_par_from_svi_from$Score_scaled <- df_par_from_svi_from$Score / 10

# (green area) q score (>0.5) and GVI (>0.3) ----
# highqscore and medium GVI
threshold_gvi <- 0.3
threshold_qscore <- 0.5
threshold_gvi_low <- 0.02

# no grouping is needed for filtering as the thresholds are absolute and dont depend on groups values (unlike quantile groups)
df_single_threshold_green <- df_singleSVI_multiPar[df_singleSVI_multiPar$Score_scaled > threshold_qscore & df_singleSVI_multiPar$green_view_index < threshold_gvi,]
df_multi_threshold_green <- df_multiSVI_singlePar[df_multiSVI_singlePar$Score_scaled > threshold_qscore & df_multiSVI_singlePar$green_view_index < threshold_gvi,]
df_par_from_svi_from_threshold_green <- df_par_from_svi_from[df_par_from_svi_from$Score_scaled > threshold_qscore & df_par_from_svi_from$green_view_index < threshold_gvi,]

## count remaining datapoints ----
### SVI_from ----
# total rows
single_counts_total <- df_singleSVI_multiPar %>%
  group_by(SVI_from) %>%
  summarise(total_rows = n())
# remaining rows
single_counts_filtered <- df_single_threshold_green %>%
  group_by(SVI_from) %>%
  summarise(retained_rows = n())
# calculate %
single_retention_rates <- single_counts_total %>%
  left_join(single_counts_filtered, by = "SVI_from") %>%
  mutate(
    retained_rows = ifelse(is.na(retained_rows), 0, retained_rows),
    retention_percentage = (retained_rows / total_rows) * 100
  )

### participant_from ----
# total rows
multi_counts_total <- df_multiSVI_singlePar %>%
  group_by(participants_from) %>%
  summarise(total_rows = n())
# remaining rows
multi_counts_filtered <- df_multi_threshold_green %>%
  group_by(participants_from) %>%
  summarise(retained_rows = n())
# calculate %
multi_retention_rates <- multi_counts_total %>%
  left_join(multi_counts_filtered, by = "participants_from") %>%
  mutate(
    retained_rows = ifelse(is.na(retained_rows), 0, retained_rows),
    retention_percentage = (retained_rows / total_rows) * 100
  )

#### images that are consistent within green regions ----
n_groups <- df_multi_threshold_green %>%
  filter(participants_from != "All") %>%
  pull(participants_from) %>%
  n_distinct()

# Filter for common images (present in all non-"All" groups)
filtered_rows <- df_multi_threshold_green %>%
  filter(participants_from != "All") %>%
  group_by(Image) %>%
  filter(n_distinct(participants_from) == n_groups) %>%
  ungroup()

# Count by city
city_counts <- filtered_rows %>%
  count(city, name = "n_rows") %>%
  arrange(desc(n_rows))

### location pair ----
# total rows
par_from_counts_total <- df_par_from_svi_from %>%
  group_by(SVI_from, participants_from) %>%
  summarise(total_rows = n())
# remaining rows
par_from_counts_filtered <- df_par_from_svi_from_threshold_green %>%
  group_by(SVI_from, participants_from) %>%
  summarise(retained_rows = n())
# calculate %
par_from_retention_rates <- par_from_counts_total %>%
  left_join(par_from_counts_filtered, by = c("SVI_from","participants_from")) %>%
  mutate(
    retained_rows = ifelse(is.na(retained_rows), 0, retained_rows),
    retention_percentage = (retained_rows / total_rows) * 100
  )

# (yellow area) q score (<0.5) and (<0.02)  ----
# low qscore and low gvi
df_single_threshold_yellow <- df_singleSVI_multiPar[df_singleSVI_multiPar$Score_scaled < threshold_qscore & df_singleSVI_multiPar$green_view_index < threshold_gvi_low,]
df_multi_threshold_yellow <- df_multiSVI_singlePar[df_multiSVI_singlePar$Score_scaled < threshold_qscore & df_multiSVI_singlePar$green_view_index < threshold_gvi_low,]
df_par_from_svi_from_threshold_yellow <- df_par_from_svi_from[df_par_from_svi_from$Score_scaled < threshold_qscore & df_par_from_svi_from$green_view_index < threshold_gvi_low,]

## count remaining datapoints ----
### SVI_from ----
# total rows
single_counts_total <- df_singleSVI_multiPar %>%
  group_by(SVI_from) %>%
  summarise(total_rows = n())
# remaining rows
single_counts_filtered <- df_single_threshold_yellow %>%
  group_by(SVI_from) %>%
  summarise(retained_rows = n())
# calculate %
single_retention_rates <- single_counts_total %>%
  left_join(single_counts_filtered, by = "SVI_from") %>%
  mutate(
    retained_rows = ifelse(is.na(retained_rows), 0, retained_rows),
    retention_percentage = (retained_rows / total_rows) * 100
  )

### participants_from ----
# total rows
multi_counts_total <- df_multiSVI_singlePar %>%
  group_by(participants_from) %>%
  summarise(total_rows = n())
# remaining rows
multi_counts_filtered <- df_multi_threshold_yellow %>%
  group_by(participants_from) %>%
  summarise(retained_rows = n())
# calculate %
multi_retention_rates <- multi_counts_total %>%
  left_join(multi_counts_filtered, by = "participants_from") %>%
  mutate(
    retained_rows = ifelse(is.na(retained_rows), 0, retained_rows),
    retention_percentage = (retained_rows / total_rows) * 100
  )

### location pair ----
# total rows
par_from_counts_total <- df_par_from_svi_from %>%
  group_by(SVI_from, participants_from) %>%
  summarise(total_rows = n())
# remaining rows
par_from_counts_filtered <- df_par_from_svi_from_threshold_yellow %>%
  group_by(SVI_from, participants_from) %>%
  summarise(retained_rows = n())
# calculate %
par_from_retention_rates <- par_from_counts_total %>%
  left_join(par_from_counts_filtered, by = c("SVI_from","participants_from")) %>%
  mutate(
    retained_rows = ifelse(is.na(retained_rows), 0, retained_rows),
    retention_percentage = (retained_rows / total_rows) * 100
  )

# correlation scatter plots ----
pearson_corr_theme <- theme_minimal(base_family = font_family, base_size = base_size * 2) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(
      hjust = 0.5,
      size = base_size * 2,
      margin = margin(b = 8)
    ),
    # Bold facet labels
    strip.text = element_text(face = "bold", family = font_family),
  )

## For presentation: overall correlation ----
df_overall <- df_singleSVI_multiPar[df_singleSVI_multiPar$SVI_from == 'All',]
p_overall <- df_overall %>%
  ggplot(aes(x=green_view_index, y=Score_scaled)) +
  # geom_rect(aes(xmin = -Inf, xmax = threshold_gvi_low,
  #               ymin = -Inf, ymax = threshold_qscore),
  #           fill = "orange", alpha = 0.01) +
  # # Add shaded rectangle for upper left quadrant
  # geom_rect(aes(xmin = -Inf, xmax = threshold_gvi, 
  #               ymin = threshold_qscore, ymax = Inf),
  #           fill = "lightgreen", alpha = 0.01) +
  geom_point(size=5) +
  geom_abline(linetype = "dashed", color = "red", size = 5) +
  # horizontal cut-off
  # geom_hline(yintercept = threshold_qscore, linetype = "dashed", color = "gray60", size = 1) +
  # vertical cut-off
  # geom_vline(xintercept = threshold_gvi, linetype = "dashed", color = "gray60", size = 1) +
  geom_smooth(method=lm) +
  stat_cor(method = "pearson", 
           label.x = 0.05,  # Adjust x position of the correlation text
           label.y = 0,  # Adjust y position of the correlation text
           size = 16,
           aes(label = ..r.label..)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) + # Force exact dimensions
  pearson_corr_theme + 
  labs(x = "Normalised Green View Index (GVI)", y = "Green perception\nQ Score", title = NULL)
p_overall

ggsave(
  "green-gvi-overall-corr-presentation.png",
  plot = p_overall,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

## overall correlation ----
pearson_corr_theme <- theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(
      hjust = 0.5,
      size = base_size,
      margin = margin(b = 8)
    ),
    # Bold facet labels
    strip.text = element_text(face = "bold", family = font_family),
  )

df_overall <- df_singleSVI_multiPar[df_singleSVI_multiPar$SVI_from == 'All',]
p_overall <- df_overall %>%
  ggplot(aes(x=green_view_index, y=Score_scaled)) +
  # geom_rect(aes(xmin = -Inf, xmax = threshold_gvi_low,
  #               ymin = -Inf, ymax = threshold_qscore),
  #           fill = "orange", alpha = 0.01) +
  # # Add shaded rectangle for upper left quadrant
  # geom_rect(aes(xmin = -Inf, xmax = threshold_gvi, 
  #               ymin = threshold_qscore, ymax = Inf),
  #           fill = "lightgreen", alpha = 0.01) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red", size = 1) +
  # horizontal cut-off
  # geom_hline(yintercept = threshold_qscore, linetype = "dashed", color = "gray60", size = 1) +
  # vertical cut-off
  # geom_vline(xintercept = threshold_gvi, linetype = "dashed", color = "gray60", size = 1) +
  geom_smooth(method=lm) +
  stat_cor(method = "pearson", 
           label.x = 0.05,  # Adjust x position of the correlation text
           label.y = 0,  # Adjust y position of the correlation text
           size = 8,
           aes(label = ..r.label..)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) + # Force exact dimensions
  pearson_corr_theme + 
  labs(x = "Normalised Green View Index (GVI)", y = "Green perception Q Score", title = NULL)
p_overall

ggsave(
  "green-gvi-overall-corr.png",
  plot = p_overall,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

## single and multi plots ----
p_singleSVI <- df_singleSVI_multiPar %>%
  ggplot(aes(x=green_view_index, y=Score_scaled)) +
  geom_rect(aes(xmin = -Inf, xmax = threshold_gvi_low,
                ymin = -Inf, ymax = threshold_qscore),
            fill = "orange", alpha = 0.01) +
  # Add shaded rectangle for upper left quadrant
  geom_rect(aes(xmin = -Inf, xmax = threshold_gvi, 
                ymin = threshold_qscore, ymax = Inf),
            fill = "lightgreen", alpha = 0.01) +
  geom_point() +
  # horizontal cut-off
  geom_hline(yintercept = threshold_qscore, linetype = "dashed", color = "gray60", size = 1) +
  # vertical cut-off
  geom_vline(xintercept = threshold_gvi, linetype = "dashed", color = "gray60", size = 1) +
  geom_smooth(method=lm) +
  stat_cor(method = "pearson", 
           label.x = 0.05,  # Adjust x position of the correlation text
           label.y = 0,  # Adjust y position of the correlation text
           size = 8,
           aes(label = ..r.label..)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  scale_x_continuous(breaks = NULL) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 0.6)) + # Force exact dimensions
  pearson_corr_theme + 
  facet_wrap(~ SVI_from, nrow = 1) +
  labs(x = NULL, y = NULL, title = expression(bold("SVI from")))
p_singleSVI

p_multiSVI <- df_multiSVI_singlePar %>%
  ggplot(aes(x=green_view_index, y=Score_scaled)) +
  geom_rect(aes(xmin = -Inf, xmax = threshold_gvi_low,
                ymin = -Inf, ymax = threshold_qscore),
            fill = "orange", alpha = 0.01) +
  geom_rect(aes(xmin = -Inf, xmax = threshold_gvi, 
                ymin = threshold_qscore, ymax = Inf),
            fill = "lightgreen", alpha = 0.01) +
  geom_point() +
  # horizontal cut-off
  geom_hline(yintercept = threshold_qscore, linetype = "dashed", color = "gray60", size = 1) +
  # vertical cut-off
  geom_vline(xintercept = threshold_gvi, linetype = "dashed", color = "gray60", size = 1) +
  geom_smooth(method=lm) +
  stat_cor(method = "pearson", 
           label.x = 0.05,  # Adjust x position of the correlation text
           label.y = 0,  # Adjust y position of the correlation text
           size = 8,
           aes(label = ..r.label..)) +
           # sig_labels +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  scale_x_continuous(
    breaks = sort(unique(c(seq(0, 1, by = 0.5), threshold_gvi))),
    ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 0.6)) + # Force exact dimensions
  pearson_corr_theme + 
  facet_wrap(~ participants_from, nrow = 1) +
  labs(x = NULL, y = NULL, title = expression(bold("Participants from")))
p_multiSVI

### merge  correlation plots into one figure ----
figure_corr <- ggarrange(p_singleSVI, 
                    p_multiSVI, 
                    font.label = list(size = 26),
                    nrow = 2,
                    align = "v",
                    heights = c(1, 1.1)
)

figure_corr <- annotate_figure(
  figure_corr,
  left = text_grob("Perception Q scores", rot = 90, size = base_size),
  bottom = text_grob("Green View Index (GVI)", size = base_size)
)

ggsave(
  "green-gvi-scatter-corr.png",
  plot = figure_corr,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

## location pair plot ----
p_location_scatter <- df_par_from_svi_from %>%
  ggplot(aes(x=green_view_index, y=Score_scaled)) +
  geom_rect(aes(xmin = -Inf, xmax = threshold_gvi_low,
                ymin = -Inf, ymax = threshold_qscore),
            fill = "orange", alpha = 0.01) +
  geom_rect(aes(xmin = -Inf, xmax = threshold_gvi, 
                ymin = threshold_qscore, ymax = Inf),
            fill = "lightgreen", alpha = 0.01) +
  geom_point() +
  # horizontal cut-off
  geom_hline(yintercept = threshold_qscore, linetype = "dashed", color = "gray60", size = 1) +
  # vertical cut-off
  geom_vline(xintercept = threshold_gvi, linetype = "dashed", color = "gray60", size = 1) +
  geom_smooth(method=lm) +
  stat_cor(method = "pearson", 
           label.x = 0.05,  # Adjust x position of the correlation text
           label.y = 0,  # Adjust y position of the correlation text
           size = 8,
           aes(label = ..r.label..)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  scale_x_continuous(
    breaks = sort(unique(c(seq(0, 1, by = 0.5), threshold_gvi))),
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 0.6)) + # Force exact dimensions
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
    x = expression(atop("Green View Index (GVI)", bold("SVI from"))),
    y = expression(atop(bold("Participants from"), "Perception Q scores")), 
    title = NULL)

p_location_scatter

ggsave(
  "green-gvi-scatter-corr_location.png",
  plot = p_location_scatter,
  path = "img/",
  height = 20,
  width = 20,
  scale = 1,
  dpi = 300,
)

# over/under estimation analysis ----
## match precision level ----
df_singleSVI_multiPar$Score_scaled <- round(df_singleSVI_multiPar$Score_scaled, 4)
df_singleSVI_multiPar$green_view_index <- round(df_singleSVI_multiPar$green_view_index, 4)

df_multiSVI_singlePar$Score_scaled <- round(df_multiSVI_singlePar$Score_scaled, 4)
df_multiSVI_singlePar$green_view_index <- round(df_multiSVI_singlePar$green_view_index, 4)

df_par_from_svi_from$Score_scaled <- round(df_par_from_svi_from$Score_scaled, 4)
df_par_from_svi_from$green_view_index <- round(df_par_from_svi_from$green_view_index, 4)

## calculate difference subjective vs objective ----
df_singleSVI_multiPar$diff <- df_singleSVI_multiPar$Score_scaled - df_singleSVI_multiPar$green_view_index
df_multiSVI_singlePar$diff <- df_multiSVI_singlePar$Score_scaled - df_multiSVI_singlePar$green_view_index
df_par_from_svi_from$diff <- df_par_from_svi_from$Score_scaled - df_par_from_svi_from$green_view_index

## Wilcoxon Signed-Rank ----
# Statistical test: Wilcoxon Signed-Rank (test against median of 0)
# not assuming normal distribution of differences for subgroups
# diff > 0: subjective tends to give higher values
# diff < 0: objective tends to give higher values

### single ----
summary_stats <- df_singleSVI_multiPar %>%
  group_by(SVI_from) %>%
  summarise(
    median_diff = median(diff, na.rm = TRUE),
    mean_diff = mean(diff, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

wilcox_test_results_singleSVI_multiPar <- df_singleSVI_multiPar %>%
  group_by(SVI_from) %>%
  wilcox_test(diff ~ 1, mu = 0) %>%
  ungroup() %>%
  # Add the descriptive statistics
  left_join(summary_stats, by = c("SVI_from")) %>%
  mutate(
    direction = case_when(
      p < 0.05 & median_diff > 0 ~ "subjective > objective",
      p < 0.05 & median_diff < 0 ~ "subjective < objective",
      p >= 0.05 ~ "Not significant"
    )
  )


### multi ----
summary_stats <- df_multiSVI_singlePar %>%
  group_by(participants_from) %>%
  summarise(
    median_diff = median(diff, na.rm = TRUE),
    mean_diff = mean(diff, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

wilcox_test_results_multiSVI_singlePar <- df_multiSVI_singlePar %>%
  group_by(participants_from) %>%
  wilcox_test(diff ~ 1, mu = 0) %>%
  ungroup() %>%
  # Add the descriptive statistics
  left_join(summary_stats, by = c("participants_from")) %>%
  mutate(
    direction = case_when(
      p < 0.05 & median_diff > 0 ~ "subjective > objective",
      p < 0.05 & median_diff < 0 ~ "subjective < objective",
      p >= 0.05 ~ "Not significant"
    )
  )

### location ----
summary_stats <- df_par_from_svi_from %>%
  group_by(SVI_from, participants_from) %>%
  summarise(
    median_diff = median(diff, na.rm = TRUE),
    mean_diff = mean(diff, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

wilcox_test_results_location <- df_par_from_svi_from %>%
  group_by(SVI_from, participants_from) %>%
  wilcox_test(diff ~ 1, mu = 0) %>%
  ungroup() %>%
  # Add the descriptive statistics
  left_join(summary_stats, by = c("SVI_from", "participants_from")) %>%
  mutate(
    direction = case_when(
      p < 0.05 & median_diff > 0 ~ "subjective > objective",
      p < 0.05 & median_diff < 0 ~ "subjective < objective",
      p >= 0.05 ~ "Not significant"
    )
  )

# => All comparisons are significant, we reject the null hypothesis of the differences
# => subjective values are consistently higher (this is statistically significant)

# Bland-Altman plot, or difference plots ----
## mean between measurements ----
df_singleSVI_multiPar$mean <- (df_singleSVI_multiPar$Score_scaled + df_singleSVI_multiPar$green_view_index) / 2
df_multiSVI_singlePar$mean <- (df_multiSVI_singlePar$Score_scaled + df_multiSVI_singlePar$green_view_index) / 2
df_par_from_svi_from$mean <- (df_par_from_svi_from$Score_scaled + df_par_from_svi_from$green_view_index) / 2

## descriptive statistics ----
stats_single <- df_singleSVI_multiPar %>%
  group_by(SVI_from) %>%
  summarise(
    mean_diff = mean(diff),
    sd_diff = sd(diff),
    upper_limit = mean_diff + 2*sd_diff,
    lower_limit = mean_diff - 2*sd_diff
  )

stats_multi <- df_multiSVI_singlePar %>%
  group_by(participants_from) %>%
  summarise(
    mean_diff = mean(diff),
    sd_diff = sd(diff),
    upper_limit = mean_diff + 2*sd_diff,
    lower_limit = mean_diff - 2*sd_diff
  )

stats_par_from_svi_from <- df_par_from_svi_from %>%
  group_by(SVI_from, participants_from) %>%
  summarise(
    mean_diff = mean(diff),
    sd_diff = sd(diff),
    upper_limit = mean_diff + 2*sd_diff,
    lower_limit = mean_diff - 2*sd_diff
  )

## themes ----
bland_altman_colours <- scale_color_manual(values = c("Mean" = "darkgreen", 
                                                      "1.96xSD" = "#d47400",
                                                      "Zero line" = "darkgray"))
bland_altman_theme <- theme_minimal(base_family = font_family, base_size = base_size) +
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
    legend.position = "bottom",
    legend.title = element_blank(),
    # Bold facet labels
    strip.text = element_text(face = "bold", family = font_family),
  )

fixed_y_axis <- scale_y_continuous(
  breaks = seq(-0.75, 0.75, by = 0.25), 
  limits = c(-0.75, 0.9),
)

## single and multi plots ----
sample_size <- df_singleSVI_multiPar %>%
  group_by(SVI_from) %>%
  count(name = "n")

p_diff_single <- df_singleSVI_multiPar %>%
  ggplot(aes(x = mean, y = diff), alpha = 0.15) +
  geom_point() +
  geom_hline(data = stats_single, 
             aes(yintercept = mean_diff, color="Mean"), 
             linetype = "solid",
             size = 1.5) +
  geom_hline(data = stats_single, 
             aes(yintercept = upper_limit, linetype="1.96xSD", color="1.96xSD"), 
             linetype = "dashed",
             size = 1.5) +
  geom_hline(data = stats_single, 
             aes(yintercept = lower_limit, linetype="1.96xSD", color="1.96xSD"), 
             linetype = "dashed",
             size = 1.5) +
  geom_hline(aes(yintercept = 0, linetype = "Zero line", color = "Zero line"),
             linetype = "dotdash",
             size = 1.5) +
  geom_text(
    data = wilcox_test_results_singleSVI_multiPar,
    aes(
      x = 0.75,
      y = 0.75, 
      label = case_when(
        p < 0.05 ~ "*",
        TRUE ~ ""
      )
    ),
    inherit.aes = FALSE,  # Don't inherit fill/color aesthetics
    size = 12,  # Adjust size as needed
    color = "black",
    fontface = "bold",
    hjust = 1,
    vjust = 1
  ) +
  bland_altman_colours +
  fixed_y_axis + 
  scale_x_continuous(breaks = NULL) +
  bland_altman_theme + 
  facet_wrap(~ SVI_from, nrow = 1) +
  labs(x = NULL, y = NULL, title = expression(bold("SVI from")))
p_diff_single

sample_size <- df_multiSVI_singlePar %>%
  group_by(participants_from) %>%
  count(name = "n")

p_diff_multi <- df_multiSVI_singlePar %>%
  ggplot(aes(x = mean, y = diff), alpha = 0.15) +
  geom_point() +
  geom_hline(data = stats_multi, 
             aes(yintercept = mean_diff, color="Mean"), 
             linetype = "solid",
             size = 1.5) +
  geom_hline(data = stats_multi, 
             aes(yintercept = upper_limit, color = "1.96xSD"), 
             linetype = "dashed",
             size = 1.5) +
  geom_hline(data = stats_multi, 
             aes(yintercept = lower_limit, color = "1.96xSD"), 
             linetype = "dashed",
             size = 1.5) +
  geom_hline(aes(yintercept = 0, color = "Zero line"), 
             linetype = "dotdash",
             size = 1.5) +
  geom_text(
    data = wilcox_test_results_multiSVI_singlePar,
    aes(
      x = 0.75,
      y = 0.75, 
      label = case_when(
        p < 0.05 ~ "*",
        TRUE ~ ""
      )
    ),
    inherit.aes = FALSE,  # Don't inherit fill/color aesthetics
    size = 12,  # Adjust size as needed
    color = "black",
    fontface = "bold",
    hjust = 1,
    vjust = 1
  ) +
  bland_altman_colours + 
  fixed_y_axis +
  scale_x_continuous(breaks = seq(0, 1, by = 0.3)) +
  coord_cartesian(ylim = c(-0.25, 0.75)) + # Force exact dimensions
  bland_altman_theme +
  facet_wrap(~ participants_from, nrow = 1) +
  labs(x = NULL, y = NULL, title = expression(bold("Participants from")))
p_diff_multi

### merge all correlation plots into one figure ----
figure_bland <- ggarrange(p_diff_single, 
                          p_diff_multi, 
                          font.label = list(size = 26),
                          nrow = 2,
                          align = "v",
                          common.legend = TRUE,
                          legend = "bottom",
                          heights = c(1, 1.1)
)

figure_bland <- annotate_figure(
  figure_bland,
  left = text_grob("Green perception Q scores - normalized GVI", rot = 90, size = base_size),
  bottom = text_grob("Mean of green perception Q scores and normalized GVI", size = base_size)
)

ggsave(
  "bland-altman.png",
  plot = figure_bland,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

## location pair plot ----
sample_size <- df_par_from_svi_from %>%
  group_by(SVI_from, participants_from) %>%
  count(name = "n")

p_diff_par_from_svi_from <- df_par_from_svi_from %>%
  ggplot(aes(x = mean, y = diff), alpha = 0.15) +
  geom_point() +
  geom_hline(data = stats_par_from_svi_from, 
             aes(
               yintercept = mean_diff, 
               color="Mean",
               group = interaction(SVI_from, participants_from)
               ), 
             linetype = "solid",
             size = 1.5) +
  geom_hline(data = stats_par_from_svi_from, 
             aes(
               yintercept = upper_limit, 
               linetype="1.96xSD", 
               color="1.96xSD",
               group = interaction(SVI_from, participants_from)
               ), 
             linetype = "dashed",
             size = 1.5) +
  geom_hline(data = stats_par_from_svi_from, 
             aes(
               yintercept = lower_limit, 
               linetype="1.96xSD", 
               color="1.96xSD",
               group = interaction(SVI_from, participants_from)
               ), 
             linetype = "dashed",
             size = 1.5) +
  geom_hline(aes(yintercept = 0, linetype = "Zero line", color = "Zero line"),
             linetype = "dotdash",
             size = 1.5) +
  # Add significance stars
  geom_text(
    data = wilcox_test_results_location,
    aes(
      x = 0.75,
      y = 0.75, 
      label = case_when(
        p < 0.05 ~ "*",
        TRUE ~ ""
        )
    ),
    inherit.aes = FALSE,  # Don't inherit fill/color aesthetics
    size = 12,  # Adjust size as needed
    color = "black",
    fontface = "bold",
    hjust = 1,
    vjust = 1
  ) +
  bland_altman_colours +
  fixed_y_axis + 
  bland_altman_theme + 
  theme(
    strip.placement.y = "outside",
    strip.placement.x = "outside",
    strip.background.y = element_rect(fill = "white", color = NA),
    strip.background.x = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.2, "lines"),
    axis.text.y = element_text(hjust = 1),  # Ensure right alignment of y-axis text
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
  labs(
    x = expression(atop("Mean of green perception Q scores and normalised GVI", bold("SVI from"))),
    y = expression(atop(bold("Participants from"), "Green perception Q scores - normalised GVI")), 
    title = NULL)
p_diff_par_from_svi_from

ggsave(
  "bland-altman-paired-location.png",
  plot = p_diff_par_from_svi_from,
  path = "img/",
  height = 25,
  width = 20,
  scale = 1,
  dpi = 300,
)

