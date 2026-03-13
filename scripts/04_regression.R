library(tidyverse)
library(car)
df<-read_csv("C:\\Users\\ASTALAKSHMI\\Documents\\global-health-analysis-r\\data\\cleaned\\life_expectancy_cleaned.csv")
head(df)
df_clean <- df %>%
  select(life_expectancy, gdp, schooling,
         total_expenditure, adult_mortality,
         income_composition_of_resources) %>%
  drop_na()
head(df_clean)
model <- lm(life_expectancy ~ gdp + schooling +
            total_expenditure + adult_mortality +
            income_composition_of_resources,
            data = df_clean)
summary(model)
cat("R-squared:", summary(model)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model)$adj.r.squared, "\n")
plot(model, which = 1)
