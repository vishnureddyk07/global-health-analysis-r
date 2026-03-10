# ============================================
# 01_data_wrangling.R
# Author: Vishnu
# Day 1 - Load, Explore and Clean the Data
# ============================================

# Set library path
.libPaths("C:/Users/vishnu reddy/AppData/Local/R/win-library/4.5")

# Load libraries
library(tidyverse)
library(janitor)

# ---- Step 1: Load the dataset ----
df <- read_csv("data/raw/Life Expectancy Data.csv")

# ---- Step 2: First look ----
head(df)
dim(df)
colnames(df)

# ---- Step 3: Clean column names ----
df <- df %>% clean_names()
colnames(df)

# ---- Step 4: Check missing values ----
missing_values <- df %>%
  summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "column",
               values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

print(missing_values)

# ---- Step 5: Basic checks ----
cat("Total Countries:", n_distinct(df$country), "\n")
cat("Year Range:", min(df$year), "to", max(df$year), "\n")
cat("Total Rows:", nrow(df), "\n")
cat("Total Columns:", ncol(df), "\n")

# ---- Step 6: Save cleaned data ----
write_csv(df, "data/cleaned/life_expectancy_cleaned.csv")
cat("Cleaned data saved successfully!\n")