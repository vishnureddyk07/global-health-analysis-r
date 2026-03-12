# ============================================================
# Script: 05_visualizations.R
# Author: Hamsika | Branch: hamsika-viz
# Project: Global Health Analysis using R
# ============================================================

library(tidyverse)
library(ggcorrplot)
library(viridis)
library(plotly)
library(maps)

# Create output folder if it doesn't exist
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

# Load data
df <- read_csv("data/cleaned/life_expectancy_cleaned.csv")

# ── PLOT 1: Histogram — Life Expectancy Distribution ────────
p1 <- ggplot(df, aes(x = life_expectancy)) +
  geom_histogram(fill = "#2E86AB", color = "white", bins = 30) +
  labs(
    title   = "Distribution of Life Expectancy (2000–2015)",
    x       = "Life Expectancy (years)",
    y       = "Count",
    caption = "Source: WHO Life Expectancy Dataset"
  ) +
  theme_minimal()

ggsave("outputs/plots/plot1_life_expectancy_histogram.png",
       plot = p1, width = 8, height = 5, dpi = 300)
cat("✅ Plot 1 saved\n")

# ── PLOT 2: Boxplot — Developed vs Developing ───────────────
p2 <- ggplot(df, aes(x = status, y = life_expectancy, fill = status)) +
  geom_boxplot() +
  labs(
    title   = "Life Expectancy: Developed vs Developing Countries",
    x       = "Country Status",
    y       = "Life Expectancy (years)",
    caption = "Source: WHO Life Expectancy Dataset"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/plots/plot2_developed_vs_developing.png",
       plot = p2, width = 8, height = 5, dpi = 300)
cat("✅ Plot 2 saved\n")

# ── PLOT 3: Scatter — GDP vs Life Expectancy ────────────────
p3 <- ggplot(df, aes(x = gdp, y = life_expectancy, color = status)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title   = "GDP vs Life Expectancy",
    x       = "GDP per Capita",
    y       = "Life Expectancy (years)",
    caption = "Source: WHO Life Expectancy Dataset"
  ) +
  theme_minimal()

ggsave("outputs/plots/plot3_gdp_vs_life_expectancy.png",
       plot = p3, width = 8, height = 5, dpi = 300)
cat("✅ Plot 3 saved\n")

# ── PLOT 4: Correlation Heatmap ──────────────────────────────
numeric_df  <- df %>% select(where(is.numeric))
cor_matrix  <- cor(numeric_df, use = "complete.obs")

p4 <- ggcorrplot(cor_matrix,
                 hc.order = TRUE,
                 type     = "lower",
                 lab      = TRUE,
                 lab_size = 2.5,
                 title    = "Correlation Heatmap of Health Indicators")

ggsave("outputs/plots/plot4_correlation_heatmap.png",
       plot = p4, width = 12, height = 10, dpi = 300)
cat("✅ Plot 4 saved\n")

# ── PLOT 5: Time Series — Life Expectancy Trend ─────────────
avg_by_year <- df %>%
  group_by(year, status) %>%
  summarise(avg_life = mean(life_expectancy, na.rm = TRUE), .groups = "drop")

p5 <- ggplot(avg_by_year, aes(x = year, y = avg_life, color = status)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title   = "Life Expectancy Trend 2000–2015",
    x       = "Year",
    y       = "Average Life Expectancy (years)",
    caption = "Source: WHO Life Expectancy Dataset"
  ) +
  theme_minimal()

ggsave("outputs/plots/plot5_life_expectancy_trend.png",
       plot = p5, width = 8, height = 5, dpi = 300)
cat("✅ Plot 5 saved\n")

# ── PLOT 6: Top 10 Countries (2015) ─────────────────────────
top10 <- df %>%
  filter(year == 2015) %>%
  arrange(desc(life_expectancy)) %>%
  head(10)

p6 <- ggplot(top10, aes(x = reorder(country, life_expectancy),
                        y = life_expectancy,
                        fill = life_expectancy)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c() +
  labs(
    title   = "Top 10 Countries by Life Expectancy (2015)",
    x       = "Country",
    y       = "Life Expectancy (years)",
    caption = "Source: WHO Life Expectancy Dataset"
  ) +
  theme_minimal()

ggsave("outputs/plots/plot6_top10_countries.png",
       plot = p6, width = 8, height = 5, dpi = 300)
cat("✅ Plot 6 saved\n")

# ── PLOT 7: Bottom 10 Countries (2015) ──────────────────────
bottom10 <- df %>%
  filter(year == 2015) %>%
  arrange(life_expectancy) %>%
  head(10)

p7 <- ggplot(bottom10, aes(x = reorder(country, life_expectancy),
                           y = life_expectancy,
                           fill = life_expectancy)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "C") +
  labs(
    title   = "Bottom 10 Countries by Life Expectancy (2015)",
    x       = "Country",
    y       = "Life Expectancy (years)",
    caption = "Source: WHO Life Expectancy Dataset"
  ) +
  theme_minimal()

ggsave("outputs/plots/plot7_bottom10_countries.png",
       plot = p7, width = 8, height = 5, dpi = 300)
cat("✅ Plot 7 saved\n")

cat("\n🎉 All 7 plots saved to outputs/plots/\n")