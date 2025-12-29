library(extrafont)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(MetBrewer)
library(reshape2)
library(corrplot)
library(patchwork)

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

# load dataset and preprocessing ----
df_singleSVI_multiPar <- read.csv("data/labels/processed/singleSVI_multiPar_qscores.csv")
df_singleSVI_multiPar$SVI_from <- factor(df_singleSVI_multiPar$SVI_from, levels=ordered_cities)
df_singleSVI_multiPar <- df_singleSVI_multiPar[,cols_single]
df_singleSVI_multiPar <- df_singleSVI_multiPar[df_singleSVI_multiPar$Question == "green",]

df_multiSVI_singlePar <- read.csv("data/labels/processed/multiSVI_singlePar_qscores.csv")
# remove 'All', not used on this analysis
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$participants_from != 'All',]
df_multiSVI_singlePar$participants_from <- factor(df_multiSVI_singlePar$participants_from, levels=ordered_countries)
df_multiSVI_singlePar <- df_multiSVI_singlePar[,cols_multi]
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$Question == "green",]

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

corr_matrix <- df_par_from_svi_from %>%
  group_by(participants_from, SVI_from) %>%
  summarize(correlation = cor(Score_scaled, green_view_index, use = "complete.obs"), 
            p_value = cor.test(Score_scaled, green_view_index, use = "complete.obs")$p.value,
            n = n(), 
            .groups = "drop") %>%
  # Filter out groups with insufficient data for correlation
  filter(n >= min_samples) %>%
  # Add significance indicators
  mutate(
    significance = case_when(
      p_value < 0.05 ~ "*",
      # p_value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    # Create a label combining correlation and significance
    label = paste0(sprintf("%.2f", correlation), significance),
    # add font face column for bold when significant
    fontface = "bold"
    # fontface = ifelse(p_value < 0.05, "bold", "plain")
  )

# heatmap with ggplot ----
p_heatmap_matrix <- corr_matrix %>%
  ggplot(aes(x = SVI_from, y = participants_from, fill = correlation)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label, fontface = fontface), color = "black", size = 10) +
  scale_fill_gradientn(colours = color_palette(200),
                       limits = col_lim,
                       name = "Pearson\nCorrelation",
                       guide = "none") +
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
  labs(x = "SVI from", y = "Participants from")
p_heatmap_matrix

# save figure ----
ggsave(
  "green-GVI-matrix-paired-locations.png",
  plot = p_heatmap_matrix,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

# plot with summaries ----
## compute summary column (row-wise summary) ----
corr_all_svi <- df_multiSVI_singlePar %>%
  group_by(participants_from) %>%
  summarize(
    correlation = cor(Score_scaled, green_view_index, use = "complete.obs"),
    p_value     = cor.test(Score_scaled, green_view_index, use = "complete.obs")$p.value,
    n           = n(),
    .groups = "drop"
  ) %>%
  filter(n >= min_samples) %>%
  mutate(
    significance = case_when(p_value < 0.05 ~ "*", TRUE ~ ""),
    label   = paste0(sprintf("%.2f", correlation), significance),
    fontface = "bold"
    # fontface = ifelse(p_value < 0.05, "bold", "plain")
  ) %>%
  mutate(SVI_from = "Everywhere")   # fake column name for plotting

## compute summary row (column-wise summary) ----
corr_all_par <- df_singleSVI_multiPar[df_singleSVI_multiPar$SVI_from != 'All',] %>%
  group_by(SVI_from) %>%
  summarize(
    correlation = cor(Score_scaled, green_view_index, use = "complete.obs"),
    p_value     = cor.test(Score_scaled, green_view_index, use = "complete.obs")$p.value,
    n           = n(),
    .groups = "drop"
  ) %>%
  filter(n >= min_samples) %>%
  mutate(
    significance = case_when(p_value < 0.05 ~ "*", TRUE ~ ""),
    label   = paste0(sprintf("%.2f", correlation), significance),
    fontface = "bold"
    # fontface = ifelse(p_value < 0.05, "bold", "plain")
  ) %>%
  mutate(participants_from = "Everywhere")

## compute overall summary (grand average) ----
corr_overall <- df_singleSVI_multiPar[df_singleSVI_multiPar$SVI_from == 'All',] %>%
  summarize(
    correlation = cor(Score_scaled, green_view_index, use = "complete.obs"),
    p_value     = cor.test(Score_scaled, green_view_index, use = "complete.obs")$p.value,
    n           = n()
  ) %>%
  mutate(
    significance = case_when(p_value < 0.05 ~ "*", TRUE ~ ""),
    label   = paste0(sprintf("%.2f", correlation), significance),
    fontface = "bold",
    # fontface = ifelse(p_value < 0.05, "bold", "plain"),
    SVI_from = "All",
    participants_from = "All"
  )


## plot summary column ----
p_summary_col <- corr_all_svi %>%
  ggplot(aes(x = SVI_from, y = participants_from, fill = correlation)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label, fontface = fontface), color = "black", size = 10) +
  scale_fill_gradientn(colours = color_palette(200),
                       limits = col_lim,
                       name = "Pearson\nCorrelation") +
  theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(4, 4, 4, 4),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(1, "cm"),
  )

## plot summary row ----
p_summary_row <- corr_all_par %>%
  ggplot(aes(x = SVI_from, y = participants_from, fill = correlation)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label, fontface = fontface), color = "black", size = 10) +
  scale_fill_gradientn(colours = color_palette(200),
                       limits = col_lim,
                       name = "Pearson\nCorrelation",
                       guide = "none") +
  theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(4, 4, 4, 4),
  )

## plot overall summary cell ----
p_summary_overall <- corr_overall %>%
  ggplot(aes(x = SVI_from, y = participants_from, fill = correlation)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label, fontface = fontface), color = "black", size = 10) +
  scale_fill_gradientn(colours = color_palette(200),
                       limits = col_lim,
                       name = "Pearson\nCorrelation",
                       guide = "none") +
  theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(4, 4, 4, 4),
  )

## combine all plots ----
design <- "
AB
CD
"
p_combined <- p_summary_row + p_summary_overall + 
  p_heatmap_matrix + p_summary_col +
  plot_layout(design = design, widths = c(5, 1.15), heights = c(1.15, 5))

p_combined

## save figure ----
ggsave(
  "green-GVI-matrix-paired-locations-extended_v2.png",
  plot = p_combined,
  path = "img/",
  height = 10,
  width = 21,
  scale = 1,
  dpi = 300,
)

# presentation plots ----
p_combined_presentation <- p_combined

# simply double the geom_text size, which is the second layer
for(i in 1:4) {
  p_combined_presentation[[i]]$layers[[2]]$aes_params$size <- 20 # originally 10
}
p_combined_presentation

ggsave(
  "green-GVI-matrix-paired-locations-extended-presentation.png",
  plot = p_combined_presentation,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

