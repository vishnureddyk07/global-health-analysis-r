<<<<<<< HEAD
# ============================================
# 03_statistical_analysis.R
# Author: Vishnu
# Day 4 - Statistical Analysis
# ============================================

.libPaths("C:/Users/vishnu reddy/AppData/Local/R/win-library/4.5")

library(tidyverse)

# Load cleaned data
df <- read_csv("data/cleaned/life_expectancy_cleaned.csv")

# ---- Step 1: Pearson Correlation ----
cat("=== PEARSON CORRELATION WITH LIFE EXPECTANCY ===\n")
numeric_df <- df %>% select_if(is.numeric) %>% select(-year)
cor_results <- cor(numeric_df, use = "complete.obs")
print(round(cor_results["life_expectancy", ], 2))

# ---- Step 2: T-Test ----
cat("\n=== T-TEST: Developed vs Developing ===\n")
developed <- df %>% filter(status == "Developed") %>% pull(life_expectancy)
developing <- df %>% filter(status == "Developing") %>% pull(life_expectancy)

t_result <- t.test(developed, developing)
print(t_result)
cat("P-value:", t_result$p.value, "\n")
if(t_result$p.value < 0.05) {
  cat("Result: SIGNIFICANT difference exists between developed and developing countries\n")
} else {
  cat("Result: No significant difference found\n")
}

# ---- Step 3: ANOVA ----
cat("\n=== ANOVA TEST ===\n")
anova_result <- aov(life_expectancy ~ status, data = df)
summary(anova_result)

# ---- Step 4: Spearman Correlation ----
cat("\n=== SPEARMAN CORRELATION (GDP vs Life Expectancy) ===\n")
spearman <- cor.test(df$gdp, df$life_expectancy, 
                     method = "spearman", 
                     use = "complete.obs")
print(spearman)

cat("\n✅ Statistical Analysis Complete!\n")

=======
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(car)
df <- read_csv("C:\\Users\\ASTALAKSHMI\\Documents\\global-health-analysis-r\\data\\cleaned\\life_expectancy_cleaned.csv")
head(df)
str(df)
summary(df)
numeric_df <- df %>% select_if(is.numeric)
head(numeric_df)
cor_matrix <- cor(numeric_df, use = "complete.obs")
print(cor_matrix)
cor_with_life <- cor(numeric_df, numeric_df$life_expectancy,
                     use = "complete.obs")

print(cor_with_life)
developed <- df %>% filter(status == "Developed") %>%
             pull(life_expectancy)

developing <- df %>% filter(status == "Developing") %>%
              pull(life_expectancy)

t_result <- t.test(developed, developing)

print(t_result)
anova_result <- aov(life_expectancy ~ status, data = df)

summary(anova_result)
cat("T-test p-value:", t_result$p.value, "\n")
cat("If p < 0.05: significant difference exists\n")
>>>>>>> 2c34c06e01e9f778d53a720684652359bff4b6b5
