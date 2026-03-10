# ============================================
# 02_eda.R
# Author: Vishnu
# Day 2 - Exploratory Data Analysis
# ============================================

# Set library path
.libPaths("C:/Users/vishnu reddy/AppData/Local/R/win-library/4.5")

# Load libraries
library(tidyverse)

# Load cleaned data
df <- read_csv("data/cleaned/life_expectancy_cleaned.csv")

# ---- Step 1: Summary statistics ----
cat("=== SUMMARY STATISTICS ===\n")
summary(df$life_expectancy)
summary(df$gdp)
summary(df$schooling)

# ---- Step 2: Average life expectancy by status ----
cat("\n=== AVG LIFE EXPECTANCY: Developed vs Developing ===\n")
df %>%
  group_by(status) %>%
  summarise(
    avg_life = round(mean(life_expectancy, na.rm = TRUE), 1),
    min_life = min(life_expectancy, na.rm = TRUE),
    max_life = max(life_expectancy, na.rm = TRUE),
    count = n()
  ) %>%
  print()

# ---- Step 3: Top 10 countries by life expectancy (2015) ----
cat("\n=== TOP 10 COUNTRIES (2015) ===\n")
df %>%
  filter(year == 2015) %>%
  arrange(desc(life_expectancy)) %>%
  select(country, life_expectancy, gdp, schooling) %>%
  head(10) %>%
  print()

# ---- Step 4: Bottom 10 countries by life expectancy (2015) ----
cat("\n=== BOTTOM 10 COUNTRIES (2015) ===\n")
df %>%
  filter(year == 2015) %>%
  arrange(life_expectancy) %>%
  select(country, life_expectancy, gdp, schooling) %>%
  head(10) %>%
  print()

# ---- Step 5: Average life expectancy per year ----
cat("\n=== LIFE EXPECTANCY TREND OVER YEARS ===\n")
df %>%
  group_by(year) %>%
  summarise(avg_life = round(mean(life_expectancy, na.rm = TRUE), 1)) %>%
  print()

# ---- Step 6: Correlation with life expectancy ----
cat("\n=== CORRELATION WITH LIFE EXPECTANCY ===\n")
df %>%
  select(life_expectancy, gdp, schooling, 
         total_expenditure, adult_mortality,
         income_composition_of_resources) %>%
  cor(use = "complete.obs") %>%
  round(2) %>%
  print()

cat("\n✅ EDA Complete!\n")
