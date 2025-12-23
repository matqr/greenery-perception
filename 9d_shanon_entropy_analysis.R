library(extrafont)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(entropy)
library(rstatix)

# global variables ----
setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34
not_needed_cols <- c("Question", "Num_comparisons", "Relabelled.Name", "source", "orig_id")
ordered_countries = c("All", "Chile", "Netherlands", "Nigeria", "Singapore", "USA")
ordered_cities = c("All", "Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_cities_metadata = c("Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")

# load datasets and preprocessing ----
df_entropy_all <- read.csv("data/labels/processed/shannon_entropy_metrics.csv")

df_singleSVI_multiPar <- read.csv("data/labels/processed/singleSVI_multiPar_qscores.csv")
df_multiSVI_singlePar <- read.csv("data/labels/processed/multiSVI_singlePar_qscores.csv")

df_singleSVI_multiPar <- df_singleSVI_multiPar[df_singleSVI_multiPar$Question == "green",]
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$Question == "green",]

df_singleSVI_multiPar$SVI_from <- factor(df_singleSVI_multiPar$SVI_from, levels=ordered_cities)
df_multiSVI_singlePar$participants_from <- factor(df_multiSVI_singlePar$participants_from, levels=ordered_countries)

df_metadata <- read.csv("data/svi/metadata.csv")
df_metadata$city <-factor(df_metadata$city, levels = ordered_cities_metadata) 

# filter images with green score <= Q1 and >= Q3 for single and multi SVI ----
filter_q1 <- function(df, split_col) {
  df <- df[df$Question == 'green',] %>%
    group_by(.data[[split_col]]) %>%
    filter(Score <= quantile(Score, 0.25)) %>%
    ungroup()
  return(df)
}

filter_q3 <- function(df, split_col) {
  df <- df[df$Question == 'green',] %>%
    group_by(.data[[split_col]]) %>%
    filter(Score >= quantile(Score, 0.75)) %>%
    ungroup()
  return(df)
}

df_single_q1 <- filter_q1(df_singleSVI_multiPar, "SVI_from")
df_single_q3 <- filter_q3(df_singleSVI_multiPar, "SVI_from")
df_multi_q1 <- filter_q1(df_multiSVI_singlePar, "participants_from")
df_multi_q3 <- filter_q3(df_multiSVI_singlePar, "participants_from")

# remove not needed columns ----
df_single_q1[not_needed_cols] <- NULL
df_single_q3[not_needed_cols] <- NULL
df_multi_q1[not_needed_cols] <- NULL
df_multi_q3[not_needed_cols] <- NULL
df_singleSVI_multiPar[not_needed_cols] <- NULL

# merge with entropy ----
merge_shanon_entropy <- function(df, quantile) {
  df_new <- df %>% # merge with entropy metrics,
    left_join(df_entropy_all, by='uuid') %>%
    mutate(
      quantile = quantile
    )
  return(df_new)
}

df_single_entropy <- df_singleSVI_multiPar %>%
  left_join(df_entropy_all, by='uuid')
df_single_entropy_q1 <- merge_shanon_entropy(df_single_q1, "Q1")
df_single_entropy_q3 <- merge_shanon_entropy(df_single_q3, "Q3")
df_multi_entropy_q1 <- merge_shanon_entropy(df_multi_q1, "Q1")
df_multi_entropy_q3 <- merge_shanon_entropy(df_multi_q3, "Q3")

# combine q1 and q3 dataframes  ----
combine_q1_q3 <- function(df_q1, df_q3, split_col) {
  df_new <- rbind(df_q1, df_q3) %>%
    # filter selected columns
    select("uuid", "Image", split_col, "green_view_index", "entropy", "quantile") %>%
    # change to long format
    pivot_longer(
      cols = -c("uuid", "Image", split_col, "quantile", "green_view_index"),
      names_to = "metric",
      values_to = "value"
    )
  return(df_new)
}

df_single_entropy_q1q3 <- combine_q1_q3(df_single_entropy_q1, df_single_entropy_q3, "SVI_from")
df_multi_entropy_q1q3 <- combine_q1_q3(df_multi_entropy_q1, df_multi_entropy_q3, "participants_from")

# add column svi location for multiSVI_singlePar and remove 'All' ----
df_par_from_svi_from_q1q3 <- df_multi_entropy_q1q3 %>%
  left_join(
    df_metadata %>%
      select(Image.number, city) %>%
      rename(SVI_from = city),
    by = c("Image" = "Image.number")
  )

df_par_from_svi_from_q1q3 <- df_par_from_svi_from_q1q3[df_par_from_svi_from_q1q3$participants_from != 'All',]

# compute Welch's anova test between entropy in q1 and q3 ----

# ANOVA function 
entropy_welch_test_by_group <- function(data, split_col, location_pair, min_samples) {
  if(location_pair == FALSE) {
    # Split by the specified column
    groups <- split(data, data[[split_col]])    
  }
  else {
    # Create an interaction column for grouping
    data <- data %>%
      mutate(split_group = interaction(!!!syms(split_col), sep = "_"))
    
    # Split by the newly created interaction column
    groups <- split(data, data$split_group)
  }
  
  # Perform Welch's ANOVA for each group
  results <- lapply(names(groups), function(group) {
    group_data <- groups[[group]] %>%
      filter(metric == "entropy")
    
    # Check if the group has at least 'min_size' unique quantile values with multiple observations
    valid_group <- group_data %>%
      group_by(quantile) %>%
      filter(n() > min_samples) %>%
      ungroup()
    
    if (n_distinct(valid_group$quantile) < 2) {
      return(NULL)  # Skip this group if not enough valid observations
    }
    
    # Perform Welch's ANOVA test
    test_result <- group_data %>%
      welch_anova_test(value ~ quantile)
    
    if(location_pair == FALSE) {
      # Add the group column information
      test_result[[split_col]] <- group
    }
    else {
      test_result$split_group <- group
    }
    
    return(test_result)
  })
  
  # Combine results into a single data frame
  do.call(rbind, results)
}

welch_results_single <- entropy_welch_test_by_group(df_single_entropy_q1q3, "SVI_from", FALSE, 30)
welch_results_multi <- entropy_welch_test_by_group(df_multi_entropy_q1q3, "participants_from", FALSE, 30)
welch_results_location <- entropy_welch_test_by_group(df_par_from_svi_from_q1q3, c("participants_from", "SVI_from"), TRUE, 12)

# format p-values for plotting and ensure factor ordering----
format_p_values <- function(welch_results, split_col) {
  df_new <- welch_results %>%
    select(split_col, p) %>%
    mutate(
      x_pos = "entropy",
      y_pos = 3.2,
      # Add significance stars
      significance = case_when(
        p < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  return(df_new)
}

stat_data_single <- format_p_values(welch_results_single, "SVI_from")
stat_data_multi <- format_p_values(welch_results_multi, "participants_from")
stat_data_location <- format_p_values(welch_results_location, "split_group")

# break `split_group` into two columns
stat_data_location <- stat_data_location %>%
  separate(split_group, into = c("participants_from", "SVI_from"), sep = "_", remove = TRUE)

stat_data_single$SVI_from <- factor(stat_data_single$SVI_from, levels = ordered_cities)
stat_data_multi$participants_from <- factor(stat_data_multi$participants_from, levels = ordered_countries)
stat_data_location$SVI_from <- factor(stat_data_location$SVI_from, levels = ordered_cities)
stat_data_location$participants_from <- factor(stat_data_location$participants_from, levels = ordered_countries)
df_par_from_svi_from_q1q3$participants_from <- factor(df_par_from_svi_from_q1q3$participants_from, levels = rev(ordered_countries))

# plot with significance indicators ----
p_single <- df_single_entropy_q1q3 %>%
  ggplot(aes(y = value, x = metric)) +
  geom_violin(aes(fill = quantile), position = "dodge", alpha = 0.5) +
  # Add means as points
  stat_summary(aes(group = quantile), fun = mean, geom = "point", 
               position = position_dodge(width = 0.9), size = 3, color = "black") +
  # Add significance annotations
  geom_text(data = stat_data_single, aes(x = x_pos, y = y_pos, label = significance),
            inherit.aes = FALSE, size = 8) +
  scale_fill_manual(values = c("Q1" = "lightgreen", "Q3" = "darkgreen")) +
  scale_y_continuous(
    # breaks = seq(0, 1.00, by = 0.25),
    limits = c(1.7, 3.2)
  ) +
  scale_x_discrete(breaks = NULL) +
  facet_wrap(~ SVI_from, nrow = 1) +
  theme_minimal(base_family = font_family, base_size = base_size) +
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
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(y = NULL,
       x = NULL,
       title = "SVI from")
p_single

p_multi <- df_multi_entropy_q1q3 %>%
  ggplot(aes(y = value, x = metric)) +
  geom_violin(aes(fill = quantile), position = "dodge", alpha = 0.5) +
  # Add means as points
  stat_summary(aes(group = quantile), fun = mean, geom = "point", 
               position = position_dodge(width = 0.9), size = 3, color = "black") +
  # Add significance annotations
  geom_text(data = stat_data_multi, aes(x = x_pos, y = y_pos, label = significance),
            inherit.aes = FALSE, size = 8) +
  scale_fill_manual(values = c("Q1" = "lightgreen", "Q3" = "darkgreen")) +
  scale_y_continuous(
    # breaks = seq(0, 1.00, by = 0.25),
    limits = c(1.7, 3.2)
  ) +
  scale_x_discrete(breaks = NULL) +
  facet_wrap(~ participants_from, nrow = 1) +
  theme_minimal(base_family = font_family, base_size = base_size) +
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
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(y = NULL,
       x = "Quartiles of least (Q1) and most (Q3) perceived green SVI",
       title = "Participants from")
p_multi

# merge both plots into one figure ----
figure_bland <- ggarrange(p_single, 
                          p_multi, 
                          font.label = list(size = 26),
                          nrow = 2,
                          align = "v",
                          common.legend = TRUE,
                          legend = "bottom",
                          heights = c(1.2, 1.2)
)

figure_bland <- annotate_figure(
  figure_bland,
  left = text_grob("Shanon entropy", rot = 90, size = base_size)
)

# save figure ----
ggsave(
  "shannon_entropy.png",
  plot = figure_bland,
  path = "img/",
  height = 10,
  width = 20,
  scale = 1,
  dpi = 300,
)

# plot with location pairs ----
p_location <- df_par_from_svi_from_q1q3 %>%
  # Join with significance data to determine which groups should have higher transparency
  left_join(
    stat_data_location %>% 
      select(participants_from, SVI_from, significance) %>%
      mutate(is_significant = significance == "*"),
    by = c("participants_from", "SVI_from")
  ) %>%
  ggplot(aes(y = value, x = metric)) +
  geom_violin(aes(fill = quantile, alpha = is_significant), position = "dodge") +
  # Add means as points
  stat_summary(aes(group = quantile), fun = mean, geom = "point", 
               position = position_dodge(width = 0.9), size = 3, color = "black") +
  # Add significance annotations
  geom_text(data = stat_data_location, 
            aes(x = x_pos, y = y_pos, label = significance),
            inherit.aes = FALSE, size = 16) +
  scale_fill_manual(values = c("Q1" = "lightgreen", "Q3" = "darkgreen"),
                    labels = c("Q1" = "<= Q1", "Q3" = ">= Q3")) +
  # Define alpha values for significant vs non-significant
  scale_alpha_manual(values = c("FALSE" = 0.3, "TRUE" = 0.8), guide = "none") +
  scale_y_continuous(
    # breaks = seq(0, 1.00, by = 0.25),
    limits = c(1.7, 3.2)
  ) +
  scale_x_discrete(breaks = NULL) +
  facet_grid(
    participants_from ~ SVI_from,  # Format: rows ~ columns
    switch = "both",
    labeller = labeller(
      SVI_from = label_value,  # Show only values, no variable names
      participants_from = label_value
    )
  ) +
  theme_minimal(base_family = font_family, base_size = base_size) +
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
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(x = "Quartiles of least (Q1) and most (Q3) perceived green SVI\nSVI from",
       y = "Participants from\nShannon entropy",
       title = NULL)
p_location

ggsave(
  "shannon_entropy_location.png",
  plot = p_location,
  path = "img/",
  height = 25,
  width = 20,
  scale = 1,
  dpi = 300,
)

