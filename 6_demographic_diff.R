library(extrafont)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MetBrewer)

# global variables ----
setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34
ordered_countries = c("All", "Chile", "Netherlands", "Nigeria", "Singapore", "USA")
ordered_cities = c("All", "Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_cities_metadata = c("Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")

# load datasets and preprocessing ----
df_singleSVI_multiPar <- read.csv("data/labels/processed/singleSVI_multiPar_qscores.csv")
df_multiSVI_singlePar <- read.csv("data/labels/processed/multiSVI_singlePar_qscores.csv")

df_singleSVI_multiPar <- df_singleSVI_multiPar[df_singleSVI_multiPar$Question == "green",]
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$Question == "green",]

df_singleSVI_multiPar$SVI_from <- factor(df_singleSVI_multiPar$SVI_from, levels = ordered_cities)
df_multiSVI_singlePar$participants_from <- factor(df_multiSVI_singlePar$participants_from, levels = ordered_countries)

df_metadata <- read.csv("data/svi/metadata.csv")
df_metadata$city <-factor(df_metadata$city, levels = rev(ordered_cities_metadata)) 

# normalizing Qscores values ----
# first convert to same scale, QScores are [0,10] and GVI is [0,1]
df_singleSVI_multiPar$Score_scaled <- df_singleSVI_multiPar$Score / 10
df_multiSVI_singlePar$Score_scaled <- df_multiSVI_singlePar$Score / 10

# high q score low GVI analysis ----
threshold_gvi <- 0.3
threshold_qscore <- 0.5
threshold_gvi_low <- 0.02

# filter data with high q score (>0.5) and med gvi (>0.3) (green) ----
df_single_threshold_green <- df_singleSVI_multiPar[df_singleSVI_multiPar$Score_scaled > threshold_qscore & df_singleSVI_multiPar$green_view_index < threshold_gvi,]
df_multi_threshold_green <- df_multiSVI_singlePar[df_multiSVI_singlePar$Score_scaled > threshold_qscore & df_multiSVI_singlePar$green_view_index < threshold_gvi,]

# filter data with high q score (<0.5) and med gvi (<0.02) (yellow) ----
df_single_threshold_yellow <- df_singleSVI_multiPar[df_singleSVI_multiPar$Score_scaled < threshold_qscore & df_singleSVI_multiPar$green_view_index < threshold_gvi,]
df_multi_threshold_yellow <- df_multiSVI_singlePar[df_multiSVI_singlePar$Score_scaled < threshold_qscore & df_multiSVI_singlePar$green_view_index < threshold_gvi,]

# remove 'All', not used  ----
df_multi_threshold_green <- df_multi_threshold_green[df_multi_threshold_green$participants_from != 'All',]
df_multi_threshold_green$participants_from <- factor(df_multi_threshold_green$participants_from, levels = rev(ordered_countries))

df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$participants_from != 'All',]
df_multiSVI_singlePar$participants_from <- factor(df_multiSVI_singlePar$participants_from, levels = rev(ordered_countries))


# add SVI_from feature ----
df_multi_threshold_green_svi_from <- df_multi_threshold_green %>%
  left_join(
    df_metadata %>%
      select(Image.number, city) %>%
      rename(SVI_from = city),
    by = c("Image" = "Image.number")
  )

df_multiSVI_singlePar_svi_from <- df_multiSVI_singlePar %>%
  left_join(
    df_metadata %>%
      select(Image.number, city) %>%
      rename(SVI_from = city),
    by = c("Image" = "Image.number")
  )

df_multi_threshold_green_svi_from$SVI_from <- factor(df_multi_threshold_green_svi_from$SVI_from, levels = ordered_cities)

df_multi_threshold_yellow <- df_multi_threshold_yellow[df_multi_threshold_yellow$participants_from != 'All',]
df_multi_threshold_yellow$participants_from <- factor(df_multi_threshold_yellow$participants_from, levels = rev(ordered_countries))

df_multi_threshold_yellow_svi_from <- df_multi_threshold_yellow %>%
  left_join(
    df_metadata %>%
      select(Image.number, city) %>%
      rename(SVI_from = city),
    by = c("Image" = "Image.number")
  )
df_multi_threshold_yellow_svi_from$SVI_from <- factor(df_multi_threshold_yellow_svi_from$SVI_from, levels = ordered_cities)

# find images that have been rated in all location pairs ----
find_images_most_combinations <- function(df) {
  # Count in how many unique combinations each UUID appears
  uuid_combination_counts <- df %>%
    distinct(uuid, participants_from, SVI_from) %>%
    group_by(uuid) %>%
    summarise(num_combinations = n(), .groups = "drop") %>%
    arrange(desc(num_combinations))
  
  # Find the maximum number of combinations
  max_combinations <- max(uuid_combination_counts$num_combinations)
  
  # Get the UUIDs that appear in the maximum number of combinations
  top_uuids <- uuid_combination_counts %>%
    filter(num_combinations == max_combinations)
  
  # Filter the original dataframe to keep these top UUIDs
  filtered_df <- df %>%
    filter(uuid %in% top_uuids$uuid)
  
  # Prepare results
  results <- list(
    filtered_dataframe = filtered_df,
    top_uuids = top_uuids$uuid,
    max_combinations = max_combinations
  )
  
  return(results)
}

images_in_green <- find_images_most_combinations(df_multi_threshold_green_svi_from)
images_in_yellow <- find_images_most_combinations(df_multi_threshold_yellow_svi_from)
images_all <- find_images_most_combinations(df_multiSVI_singlePar_svi_from)

# preprocessing for plotting ----
# manual/visual confirmation from the two variables above
img_to_filter <- images_all[["top_uuids"]]
df_plot <- df_multiSVI_singlePar_svi_from[df_multiSVI_singlePar_svi_from$uuid %in% img_to_filter,]

# select two pairs with similar GVI 

# original pair
# img_ams = "e97e0042-0ca3-47a2-a514-7422cb60e6a6"
# img_abu = "38ba29ee-7e23-4ad8-addc-0f90afc7c38e"

# latest pair
img_ams <- "9708104d-fe08-4ccc-a1fa-96e421b99f24" # GVI 0.04
img_abu <- "82eddd58-1254-4d42-b145-f3699b459a8d" # GVI 0.03

# df_plot <- df_multi_threshold_yellow_svi_from[df_multi_threshold_yellow_svi_from$uuid == img_ams | df_multi_threshold_yellow_svi_from$uuid == img_abu,]
df_plot <- df_multiSVI_singlePar_svi_from[df_multiSVI_singlePar_svi_from$ uuid == img_ams | df_multiSVI_singlePar_svi_from$uuid == img_abu,]

df_plot$participants_from <- factor(df_plot$participants_from, levels = ordered_countries)

# heatmap plot for better viz v2 (mean diff)----
# a similar heatmap but the color divergence is in regards to the avg of both values by participants_from
df_heatmap <- df_plot %>%
  group_by(participants_from) %>%
  mutate(
    avg = (Score_scaled[uuid == img_abu] +
      Score_scaled[uuid == img_ams]) / 2)
df_heatmap$dif <- df_heatmap$Score_scaled - df_heatmap$avg

color_palette <- colorRampPalette(c("#404040",
                                    "#bababa",
                                    "#ffffff", 
                                    "#f4a582",
                                    "#CA0020"))
col_lim <- c(-0.12, 0.12)
p_heatmap <- df_heatmap %>%
  ggplot(aes(x = uuid, y = participants_from, fill = dif)) +
  geom_tile(color = "white", linewidth = 1, height = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Score_scaled)), color = "black", size = 10) +
  scale_fill_gradientn(colours = color_palette(200),
                       limits = col_lim,
                       name = NULL) +
  theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Add this line instead
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(
      hjust = 0.5,
      size = 34,
      margin = margin(b = 8)
    ),
    legend.key.height = unit(1.5, "cm"),  # Make colorbar longer
    legend.key.width = unit(3, "cm"),  # Adjust width if needed
    legend.position = "bottom"
  ) + 
  coord_equal() + # Make tiles square 
  labs(x = NULL, y = "Participants from")
p_heatmap

ggsave(
  "demographic-diff-rating-matrix-v2.png",
  plot = p_heatmap,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)
