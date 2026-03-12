# ============================================================
# Script: 02_eda.R
# Author: Vishnu | Branch: vishnureddy-eda
# Project: Global Health Analysis using R
# ============================================================
 
.libPaths("C:/Users/vishnu reddy/AppData/Local/R/win-library/4.5")
 
library(tidyverse)
library(janitor)
library(ggcorrplot)
 
# ---- Step 1: Load Cleaned Data ----
df <- read_csv("data/cleaned/life_expectancy_cleaned.csv")
cat("✅ Cleaned data loaded successfully!\n")
 
# ---- Step 2: Basic Structure ----
cat("\n📌 Dataset Dimensions:\n")
dim(df)
 
cat("\n📌 Column Names:\n")
colnames(df)
 
cat("\n📌 Data Types:\n")
glimpse(df)
 
# ---- Step 3: Summary Statistics ----
cat("\n📊 Summary Statistics:\n")
summary(df)
 
# ---- Step 4: Missing Value Analysis ----
cat("\n🔍 Missing Values Per Column:\n")
missing_values <- df %>%
  summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(everything(),
               names_to  = "column",
               values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))
 
print(missing_values)
 
total_missing <- sum(is.na(df))
cat("Total Missing Values:", total_missing, "\n")
cat("Missing Percentage:", round(total_missing / (nrow(df) * ncol(df)) * 100, 2), "%\n")
 
# ---- Step 5: Unique Value Counts ----
cat("\n📌 Unique Countries:", n_distinct(df$country), "\n")
cat("📌 Year Range:", min(df$year, na.rm = TRUE), "to", max(df$year, na.rm = TRUE), "\n")
cat("📌 Country Status Groups:\n")
print(table(df$status))
 
# ---- Step 6: Life Expectancy Summary by Status ----
cat("\n📊 Life Expectancy by Country Status:\n")
df %>%
  group_by(status) %>%
  summarise(
    count    = n(),
    mean_le  = round(mean(life_expectancy, na.rm = TRUE), 2),
    median_le = round(median(life_expectancy, na.rm = TRUE), 2),
    min_le   = round(min(life_expectancy, na.rm = TRUE), 2),
    max_le   = round(max(life_expectancy, na.rm = TRUE), 2),
    sd_le    = round(sd(life_expectancy, na.rm = TRUE), 2)
  ) %>%
  print()
 
# ---- Step 7: Life Expectancy Summary by Year ----
cat("\n📊 Average Life Expectancy by Year:\n")
df %>%
  group_by(year) %>%
  summarise(avg_life_expectancy = round(mean(life_expectancy, na.rm = TRUE), 2)) %>%
  arrange(year) %>%
  print()
 
# ---- Step 8: Top and Bottom 5 Countries (2015) ----
cat("\n🏆 Top 5 Countries by Life Expectancy (2015):\n")
df %>%
  filter(year == 2015) %>%
  arrange(desc(life_expectancy)) %>%
  select(country, status, life_expectancy) %>%
  head(5) %>%
  print()
 
cat("\n⚠️ Bottom 5 Countries by Life Expectancy (2015):\n")
df %>%
  filter(year == 2015) %>%
  arrange(life_expectancy) %>%
  select(country, status, life_expectancy) %>%
  head(5) %>%
  print()
 
# ---- Step 9: Correlation Matrix (Numeric Columns) ----
cat("\n📊 Correlation Matrix (Top Correlations with Life Expectancy):\n")
numeric_df <- df %>% select(where(is.numeric))
cor_matrix <- cor(numeric_df, use = "complete.obs")
 
cor_with_le <- cor_matrix["life_expectancy", ] %>%
  sort(decreasing = TRUE) %>%
  round(3)
 
print(cor_with_le)
 
# ---- Step 10: Save EDA Summary to File ----
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
 
sink("outputs/eda_summary.txt")
cat("============================================================\n")
cat("EDA SUMMARY REPORT — Global Health Analysis using R\n")
cat("Author: Vishnu | Branch: vishnureddy-eda\n")
cat("============================================================\n\n")
 
cat("Dataset Dimensions:\n")
cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n\n")
 
cat("Unique Countries:", n_distinct(df$country), "\n")
cat("Year Range:", min(df$year, na.rm = TRUE), "to", max(df$year, na.rm = TRUE), "\n\n")
 
cat("Country Status Distribution:\n")
print(table(df$status))
 
cat("\nMissing Values:\n")
print(missing_values)
 
cat("\nLife Expectancy by Status:\n")
df %>%
  group_by(status) %>%
  summarise(
    mean_le   = round(mean(life_expectancy, na.rm = TRUE), 2),
    median_le = round(median(life_expectancy, na.rm = TRUE), 2),
    sd_le     = round(sd(life_expectancy, na.rm = TRUE), 2)
  ) %>%
  print()
 
cat("\nTop Correlations with Life Expectancy:\n")
print(cor_with_le)
sink()
 
cat("\n✅ EDA summary saved to outputs/eda_summary.txt\n")
cat("🎉 EDA Complete!\n")