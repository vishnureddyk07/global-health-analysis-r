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
