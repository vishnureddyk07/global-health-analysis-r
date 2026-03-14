<<<<<<< HEAD
# ============================================
# 04_regression.R
# Author: Vishnu
# Day 4 - Multiple Linear Regression
# ============================================

.libPaths("C:/Users/vishnu reddy/AppData/Local/R/win-library/4.5")

library(tidyverse)

# Load cleaned data
df <- read_csv("data/cleaned/life_expectancy_cleaned.csv")

# ---- Step 1: Prepare data ----
=======
library(tidyverse)
library(car)
df<-read_csv("C:\\Users\\ASTALAKSHMI\\Documents\\global-health-analysis-r\\data\\cleaned\\life_expectancy_cleaned.csv")
head(df)
>>>>>>> 2c34c06e01e9f778d53a720684652359bff4b6b5
df_clean <- df %>%
  select(life_expectancy, gdp, schooling,
         total_expenditure, adult_mortality,
         income_composition_of_resources) %>%
  drop_na()
<<<<<<< HEAD

cat("Rows used for regression:", nrow(df_clean), "\n")

# ---- Step 2: Build regression model ----
model <- lm(life_expectancy ~ gdp + schooling +
              total_expenditure + adult_mortality +
              income_composition_of_resources,
            data = df_clean)

# ---- Step 3: View results ----
cat("\n=== REGRESSION MODEL RESULTS ===\n")
summary(model)

# ---- Step 4: R-squared ----
cat("\n=== MODEL PERFORMANCE ===\n")
cat("R-squared:", round(summary(model)$r.squared, 3), "\n")
cat("Adjusted R-squared:", round(summary(model)$adj.r.squared, 3), "\n")

# ---- Step 5: Which variables are significant ----
cat("\n=== SIGNIFICANT PREDICTORS (p < 0.05) ===\n")
coef_table <- summary(model)$coefficients
significant <- coef_table[coef_table[,4] < 0.05, ]
print(round(significant, 4))

cat("\n✅ Regression Analysis Complete!\n")
=======
head(df_clean)
model <- lm(life_expectancy ~ gdp + schooling +
            total_expenditure + adult_mortality +
            income_composition_of_resources,
            data = df_clean)
summary(model)
cat("R-squared:", summary(model)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model)$adj.r.squared, "\n")
plot(model, which = 1)
>>>>>>> 2c34c06e01e9f778d53a720684652359bff4b6b5
