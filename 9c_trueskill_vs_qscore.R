library(extrafont)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(MetBrewer)

setwd("Developer/paper__svi-greenery/")
bg_color <- "white"
font_family <- "Fira Sans"
base_size <- 34
ordered_countries = c("All", "Chile", "Netherlands", "Nigeria", "Singapore", "USA")
ordered_cities = c("All", "Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")
ordered_cities_metadata = c("Santiago", "Amsterdam", "Abuja", "Singapore", "San Francisco")

# load dataset ----
df_singleSVI_multiPar <- read.csv("data/labels/processed/singleSVI_multiPar_qscores_trueskill.csv")
df_multiSVI_singlePar <- read.csv("data/labels/processed/multiSVI_singlePar_qscores_trueskill.csv")

df_singleSVI_multiPar <- df_singleSVI_multiPar[df_singleSVI_multiPar$Question == "green" |  df_singleSVI_multiPar$Question == "beautiful",]
df_multiSVI_singlePar <- df_multiSVI_singlePar[df_multiSVI_singlePar$Question == "green" | df_multiSVI_singlePar$Question == "beautiful",]

df_singleSVI_multiPar$SVI_from <- factor(df_singleSVI_multiPar$SVI_from, levels = ordered_cities)
df_multiSVI_singlePar$participants_from <- factor(df_multiSVI_singlePar$participants_from, levels = ordered_countries)

df_metadata <- read.csv("data/svi/metadata.csv")
df_metadata$city <-factor(df_metadata$city, levels = ordered_cities_metadata)

df_par_from_svi_from <- df_multiSVI_singlePar %>%
  left_join(
    df_metadata %>%
      select(Image.number, city) %>%
      rename(SVI_from = city),
    by = c("Image" = "Image.number")
  )

df_par_from_svi_from <- df_par_from_svi_from[df_par_from_svi_from$participants_from != 'All',]
df_par_from_svi_from$participants_from <- factor(df_par_from_svi_from$participants_from, levels = rev(ordered_countries))

# corr test ----
correlation_analysis <- df_par_from_svi_from %>%
  group_by(SVI_from, participants_from) %>%
  summarise(
    n = n(),
    correlation = cor(Score, TrueSkill_score, use = "complete.obs"),
    cor_test_p = cor.test(Score, TrueSkill_score)$p.value,
    .groups = "drop"
  )#

# plot theme ----
dist_theme <- theme_minimal(base_family = font_family, base_size = base_size) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 20, 4, 4),
    plot.title = element_text(
      hjust = 0.5,
      size = 34,
      margin = margin(b = 8)
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing.x = unit(3, "lines"),
    # Bold facet labels
    strip.text = element_text(face = "bold", family = font_family),
  )

# singleSVI plot ----
p_dist_single <- df_singleSVI_multiPar %>%
  pivot_longer(cols = c(Score, TrueSkill_score), 
               names_to = "Variable", 
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +
  geom_density(adjust = 1.5, alpha = 0.5) +
  dist_theme + 
  scale_x_continuous(breaks = seq(0, 10, by = 2.5)) +
  facet_wrap(~ SVI_from, nrow = 1) +
  labs(x = NULL, y = NULL, title = "SVI from") +
  scale_fill_manual(values = c(Score = "lightgreen", TrueSkill_score = "darkgreen"))
p_dist_single

# multiSVI plot ----
p_dist_multi <- df_multiSVI_singlePar %>%
  pivot_longer(cols = c(Score, TrueSkill_score), 
               names_to = "Variable", 
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +
  geom_density(adjust = 1.5, alpha = 0.5) +
  dist_theme + 
  scale_x_continuous(breaks = seq(0, 10, by = 2.5)) +
  facet_wrap(~ participants_from, nrow = 1) +
  labs(x = NULL, y = NULL, title = "Participants from") +
  scale_fill_manual(values = c(Score = "lightgreen", TrueSkill_score = "darkgreen"))
p_dist_multi

# pair locations plot ----
p_location <- df_par_from_svi_from %>%
  pivot_longer(cols = c(Score, TrueSkill_score), 
               names_to = "Variable", 
               values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +
  geom_density(adjust = 1.5, alpha = 0.5) +
  geom_text(
    data = correlation_analysis, 
    aes(x = 5, y = 0.02, label = paste0("R=", round(correlation, 2))),
    inherit.aes = FALSE, size = 8
  ) +
  dist_theme + 
  scale_x_continuous(breaks = seq(0, 10, by = 5)) +
  facet_grid(
    participants_from ~ SVI_from, # Format: rows ~ columns
    switch = "both",
    labeller = labeller(
      SVI_from = label_value,  # Show only values, no variable names
      participants_from = label_value
      )
  ) +
  labs(x = "SVI from", y = "Participants from") +
  scale_fill_manual(
    values = c(Score = "darkgreen", TrueSkill_score = "#d47400"),
    labels = c(Score = "Q score", TrueSkill_score = "TrueSkill")
    )
p_location

# save location pairs figure ----
ggsave(
  "Qscore-trueskill-dist.png",
  plot = p_location,
  path = "img/",
  height = 20,
  width = 20,
  scale = 1,
  dpi = 300,
)


# QQ plot for par locations ----
p_location_qq <- df_par_from_svi_from %>%
  group_by(participants_from, SVI_from, Question) %>%
  reframe(
    score_quantiles = quantile(Score, probs = seq(0.01, 0.99, 0.01), na.rm = TRUE),
    trueskill_quantiles = quantile(TrueSkill_score, probs = seq(0.01, 0.99, 0.01), na.rm = TRUE)
  ) %>%
  ggplot(aes(x = score_quantiles, y = trueskill_quantiles)) +
  geom_point(aes(color = Question), alpha = 0.6, size = 4) +
  geom_abline(aes(color = "Perfect agreement", slope = 1, intercept = 0), linewidth = 2, alpha = 0.7) +
  scale_color_manual(
    name = "Legend",
    values = c("green" = "darkgreen",  "beautiful" = "#d47400", "Perfect agreement" = "black"),
    labels = c("green" = "Green", "beautiful" = "Beautiful", "Perfect agreement" = "Perfect agreement line")
  ) +
  dist_theme + 
  facet_grid(
    participants_from ~ SVI_from, # Format: rows ~ columns
    switch = "both",
    labeller = labeller(
      SVI_from = label_value,  # Show only values, no variable names
      participants_from = label_value
    )
  ) +
  scale_x_continuous(breaks = seq(0, 10, by = 2.5), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2.5),  limits = c(0, 10)) +
  labs(
    x = expression(atop("Q score quantiles", bold("SVI from"))),
    y = expression(atop(bold("Participants from"), "Trueskill quantiles")),
  )
p_location_qq

# save qqp plots ----
ggsave(
  "Qscore-trueskill-qq.png",
  plot = p_location_qq,
  path = "img/",
  height = 20,
  width = 20,
  scale = 1,
  dpi = 300,
)

# QQ plot for par locations (only greenery) ----
p_location_qq <- df_par_from_svi_from[df_par_from_svi_from$Question =='green',] %>%
  group_by(participants_from, SVI_from, Question) %>%
  reframe(
    score_quantiles = quantile(Score, probs = seq(0.01, 0.99, 0.01), na.rm = TRUE),
    trueskill_quantiles = quantile(TrueSkill_score, probs = seq(0.01, 0.99, 0.01), na.rm = TRUE)
  ) %>%
  ggplot(aes(x = score_quantiles, y = trueskill_quantiles)) +
  geom_point(aes(color = Question), alpha = 0.6, size = 4) +
  geom_abline(aes(color = "Perfect agreement", slope = 1, intercept = 0), linewidth = 2, alpha = 0.7) +
  scale_color_manual(
    name = "Legend",
    values = c("green" = "darkgreen", "Perfect agreement" = "black"),
    labels = c("green" = "Green perceptual indicator", "Perfect agreement" = "Perfect agreement line")
  ) +
  dist_theme + 
  facet_grid(
    participants_from ~ SVI_from, # Format: rows ~ columns
    switch = "both",
    labeller = labeller(
      SVI_from = label_value,  # Show only values, no variable names
      participants_from = label_value
    )
  ) +
  scale_x_continuous(breaks = seq(0, 10, by = 2.5), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2.5),  limits = c(0, 10)) +
  labs(
    x = expression(atop("Q score quantiles", bold("SVI from"))),
    y = expression(atop(bold("Participants from"), "Trueskill quantiles")),
  )
p_location_qq

# save qqp plots (only greenery) ----
ggsave(
  "Qscore-trueskill-qq-greenery.png",
  plot = p_location_qq,
  path = "img/",
  height = 20,
  width = 20,
  scale = 1,
  dpi = 300,
)

