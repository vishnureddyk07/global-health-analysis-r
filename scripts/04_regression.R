# ============================================================
# Script: 04_regression.R
# Author: Mohan | Branch: mohan-regression
# Project: Global Health Analysis using R
# ============================================================

.libPaths("C:/Users/vishnu reddy/AppData/Local/R/win-library/4.5")

library(tidyverse)
library(janitor)

# ---- Step 1: Load Cleaned Data ----
df <- read_csv("data/cleaned/life_expectancy_cleaned.csv")
cat("✅ Cleaned data loaded successfully!\n")

# ---- Step 2: Prepare Data ----
# Remove rows where life_expectancy is missing
df_clean <- df %>%
  filter(!is.na(life_expectancy)) %>%
  select(life_expectancy, adult_mortality, infant_deaths,
         alcohol, percentage_expenditure, bmi,
         under_five_deaths, polio, total_expenditure,
         diphtheria, hiv_aids, gdp, population,
         income_composition_of_resources, schooling, status) %>%
  mutate(status = as.factor(status)) %>%
  drop_na()

cat("✅ Data prepared for regression!\n")
cat("Rows used for modeling:", nrow(df_clean), "\n")

# ---- Step 3: Simple Linear Regression ----
# Life Expectancy ~ Schooling
cat("\n📊 Simple Linear Regression: Life Expectancy ~ Schooling\n")
simple_model <- lm(life_expectancy ~ schooling, data = df_clean)
summary(simple_model)

# ---- Step 4: Multiple Linear Regression ----
cat("\n📊 Multiple Linear Regression: Life Expectancy ~ All Predictors\n")
multi_model <- lm(life_expectancy ~ adult_mortality +
                    infant_deaths +
                    alcohol +
                    bmi +
                    hiv_aids +
                    gdp +
                    income_composition_of_resources +
                    schooling +
                    status,
                  data = df_clean)
summary(multi_model)

# ---- Step 5: Model Performance Metrics ----
cat("\n📈 Model Performance Metrics:\n")

# Simple model metrics
simple_r2 <- summary(simple_model)$r.squared
simple_adj_r2 <- summary(simple_model)$adj.r.squared
simple_rmse <- sqrt(mean(simple_model$residuals^2))

cat("\nSimple Linear Regression (Life Expectancy ~ Schooling):\n")
cat("  R-Squared      :", round(simple_r2, 4), "\n")
cat("  Adj R-Squared  :", round(simple_adj_r2, 4), "\n")
cat("  RMSE           :", round(simple_rmse, 4), "\n")

# Multiple model metrics
multi_r2 <- summary(multi_model)$r.squared
multi_adj_r2 <- summary(multi_model)$adj.r.squared
multi_rmse <- sqrt(mean(multi_model$residuals^2))

cat("\nMultiple Linear Regression (All Predictors):\n")
cat("  R-Squared      :", round(multi_r2, 4), "\n")
cat("  Adj R-Squared  :", round(multi_adj_r2, 4), "\n")
cat("  RMSE           :", round(multi_rmse, 4), "\n")

# ---- Step 6: Coefficients Table ----
cat("\n📋 Multiple Regression Coefficients:\n")
coef_table <- as.data.frame(summary(multi_model)$coefficients)
coef_table <- round(coef_table, 4)
print(coef_table)

# ---- Step 7: Identify Most Important Predictors ----
cat("\n🔑 Most Significant Predictors (p < 0.05):\n")
coef_df <- as.data.frame(summary(multi_model)$coefficients)
colnames(coef_df) <- c("Estimate", "Std_Error", "t_value", "p_value")
significant <- coef_df %>%
  filter(p_value < 0.05) %>%
  arrange(p_value)
print(significant)

# ---- Step 8: Save Results ----
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

sink("outputs/regression_summary.txt")
cat("============================================================\n")
cat("REGRESSION ANALYSIS REPORT — Global Health Analysis using R\n")
cat("Author: Mohan | Branch: mohan-regression\n")
cat("============================================================\n\n")

cat("Simple Linear Regression (Life Expectancy ~ Schooling):\n")
cat("  R-Squared :", round(simple_r2, 4), "\n")
cat("  Adj R-Squared :", round(simple_adj_r2, 4), "\n")
cat("  RMSE :", round(simple_rmse, 4), "\n\n")

cat("Multiple Linear Regression (All Predictors):\n")
cat("  R-Squared :", round(multi_r2, 4), "\n")
cat("  Adj R-Squared :", round(multi_adj_r2, 4), "\n")
cat("  RMSE :", round(multi_rmse, 4), "\n\n")

cat("Significant Predictors (p < 0.05):\n")
print(significant)
sink()

cat("\n✅ Regression summary saved to outputs/regression_summary.txt\n")
cat("🎉 Regression Analysis Complete!\n")