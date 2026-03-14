# ============================================
# 05_visualizations.R
# Author: Vishnu
# Day 3 - All Visualizations
# ============================================

.libPaths("C:/Users/vishnu reddy/AppData/Local/R/win-library/4.5")

library(tidyverse)
library(ggcorrplot)

# Load cleaned data
df <- read_csv("data/cleaned/life_expectancy_cleaned.csv")

# Create plots folder if not exists
dir.create("plots", showWarnings = FALSE)

# ---- PLOT 1: Life Expectancy Distribution ----
p1 <- ggplot(df, aes(x = life_expectancy)) +
  geom_histogram(bins = 30, fill = "#2E86AB", color = "white") +
  labs(title = "Distribution of Life Expectancy",
       x = "Life Expectancy (years)", y = "Count") +
  theme_minimal()
ggsave("plots/01_life_expectancy_distribution.png", p1)
cat("Plot 1 saved!\n")

# ---- PLOT 2: Developed vs Developing Boxplot ----
p2 <- ggplot(df, aes(x = status, y = life_expectancy, fill = status)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#2E86AB", "#E84855")) +
  labs(title = "Life Expectancy: Developed vs Developing",
       x = "Country Status", y = "Life Expectancy") +
  theme_minimal()
ggsave("plots/02_developed_vs_developing.png", p2)
cat("Plot 2 saved!\n")

# ---- PLOT 3: GDP vs Life Expectancy ----
p3 <- ggplot(df, aes(x = gdp, y = life_expectancy, color = status)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("#2E86AB", "#E84855")) +
  labs(title = "GDP vs Life Expectancy",
       x = "GDP per capita", y = "Life Expectancy") +
  theme_minimal()
ggsave("plots/03_gdp_vs_life_expectancy.png", p3)
cat("Plot 3 saved!\n")

# ---- PLOT 4: Schooling vs Life Expectancy ----
p4 <- ggplot(df, aes(x = schooling, y = life_expectancy, color = status)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("#2E86AB", "#E84855")) +
  labs(title = "Schooling vs Life Expectancy",
       x = "Years of Schooling", y = "Life Expectancy") +
  theme_minimal()
ggsave("plots/04_schooling_vs_life_expectancy.png", p4)
cat("Plot 4 saved!\n")

# ---- PLOT 5: Life Expectancy Trend Over Years ----
avg_by_year <- df %>%
  group_by(year, status) %>%
  summarise(avg_life = mean(life_expectancy, na.rm = TRUE), .groups = "drop")

p5 <- ggplot(avg_by_year, aes(x = year, y = avg_life, color = status)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#2E86AB", "#E84855")) +
  labs(title = "Life Expectancy Trend 2000-2015",
       x = "Year", y = "Average Life Expectancy") +
  theme_minimal()
ggsave("plots/05_life_expectancy_trend.png", p5)
cat("Plot 5 saved!\n")

# ---- PLOT 6: Top 10 Countries 2015 ----
top10 <- df %>%
  filter(year == 2015) %>%
  arrange(desc(life_expectancy)) %>%
  head(10)

p6 <- ggplot(top10, aes(x = reorder(country, life_expectancy),
                        y = life_expectancy, fill = life_expectancy)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#85C1E9", high = "#1A5276") +
  labs(title = "Top 10 Countries by Life Expectancy (2015)",
       x = "Country", y = "Life Expectancy") +
  theme_minimal()
ggsave("plots/06_top10_countries.png", p6)
cat("Plot 6 saved!\n")

# ---- PLOT 7: Bottom 10 Countries 2015 ----
bottom10 <- df %>%
  filter(year == 2015) %>%
  arrange(life_expectancy) %>%
  head(10)

p7 <- ggplot(bottom10, aes(x = reorder(country, life_expectancy),
                           y = life_expectancy, fill = life_expectancy)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#E74C3C", high = "#F1948A") +
  labs(title = "Bottom 10 Countries by Life Expectancy (2015)",
       x = "Country", y = "Life Expectancy") +
  theme_minimal()
ggsave("plots/07_bottom10_countries.png", p7)
cat("Plot 7 saved!\n")

# ---- PLOT 8: Correlation Heatmap ----
numeric_df <- df %>% select_if(is.numeric) %>% select(-year)
cor_matrix <- cor(numeric_df, use = "complete.obs")

png("plots/08_correlation_heatmap.png", width = 1200, height = 1000)
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 2.5,
           title = "Correlation Heatmap — All Variables")
dev.off()
cat("Plot 8 saved!\n")

# ---- PLOT 9: Adult Mortality vs Life Expectancy ----
p9 <- ggplot(df, aes(x = adult_mortality, y = life_expectancy, color = status)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#2E86AB", "#E84855")) +
  labs(title = "Adult Mortality vs Life Expectancy",
       x = "Adult Mortality Rate", y = "Life Expectancy") +
  theme_minimal()
ggsave("plots/09_mortality_vs_life_expectancy.png", p9)
cat("Plot 9 saved!\n")

# ---- PLOT 10: Income Composition vs Life Expectancy ----
p10 <- ggplot(df, aes(x = income_composition_of_resources,
                      y = life_expectancy, color = status)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#2E86AB", "#E84855")) +
  labs(title = "Income Composition vs Life Expectancy",
       x = "Income Composition of Resources", y = "Life Expectancy") +
  theme_minimal()
ggsave("plots/10_income_vs_life_expectancy.png", p10)
cat("Plot 10 saved!\n")

cat("\n✅ All 10 plots saved in plots/ folder!\n")
