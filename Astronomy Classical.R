# Install and load necessary packages
# If you don't have these packages installed, uncomment the install.packages() lines below
# install.packages("tidyverse") # For data manipulation and ggplot2 for plotting
# install.packages("corrplot")  # For visualizing correlation matrices
# install.packages("psych")     # For descriptive statistics (optional, but useful)
# install.packages("car")       # For Levene's test

library(tidyverse)
library(corrplot)
library(psych) # For describe() function
library(car)   # For Levene's test
library(ggplot2)
library(dplyr)

# --- 1. Data Loading and Initial Inspection ---

# Load the dataset
# Make sure 'xwas_agn_clean.csv' is in your R working directory,
# or provide the full path to the file.
data <- read_excel("C:/Users/SUBHRAJIT SAHA/Downloads/xwas_agn_clean.xlsx")

# Display the structure of the data
cat("--- Data Structure (str()) ---\n")
str(data)

# Display summary statistics for all columns
cat("\n--- Summary Statistics (summary()) ---\n")
summary(data)

# Display the first few rows of the data
cat("\n--- First 6 Rows (head()) ---\n")
head(data)

# Identify numerical and categorical columns
# Based on your data, 'Class' is categorical, others are mostly numerical
numerical_cols <- c("RA_deg", "Dec_deg", "z", "Flux_0p5_4p5", "Log_Lx", "Bmag", "Rmag", "BR_color")
categorical_cols <- c("Class")

# Convert 'Class' to a factor
data$Class <- as.factor(data$Class)

# --- 2. Univariate Analysis ---

cat("\n--- Univariate Analysis: Plots and Tables ---\n")

# A. Descriptive Statistics for Numerical Variables
cat("\n--- Descriptive Statistics for Numerical Variables (psych::describe) ---\n")
# The 'describe' function from the 'psych' package provides more detailed descriptive stats
# It automatically handles NA values by default.
describe(data[, numerical_cols])

# B. Frequency Table for Categorical Variable ('Class')
cat("\n--- Frequency Table for 'Class' ---\n")
table(data$Class)
prop.table(table(data$Class)) # Proportions

# C. Histograms for Numerical Variables
cat("\n--- Histograms for Numerical Variables ---\n")
# Loop through numerical columns to create histograms
for (col in numerical_cols) {
  # Skip if the column is entirely NA or has very few non-NA values
  if (sum(!is.na(data[[col]])) < 2) {
    message(paste("Skipping histogram for", col, "due to insufficient non-NA data."))
    next
  }
  p <- ggplot(data, aes_string(x = col)) +
    geom_histogram(binwidth = diff(range(data[[col]], na.rm = TRUE))/30, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme_minimal()
  print(p)
}

# D. Density Plots for Numerical Variables
cat("\n--- Density Plots for Numerical Variables ---\n")
for (col in numerical_cols) {
  if (sum(!is.na(data[[col]])) < 2) {
    message(paste("Skipping density plot for", col, "due to insufficient non-NA data."))
    next
  }
  p <- ggplot(data, aes_string(x = col)) +
    geom_density(fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = paste("Density Plot of", col), x = col, y = "Density") +
    theme_minimal()
  print(p)
}

# E. Box Plots for Numerical Variables (to visualize distribution and outliers)
cat("\n--- Box Plots for Numerical Variables ---\n")
for (col in numerical_cols) {
  if (sum(!is.na(data[[col]])) < 2) {
    message(paste("Skipping box plot for", col, "due to insufficient non-NA data."))
    next
  }
  p <- ggplot(data, aes_string(y = col)) +
    geom_boxplot(fill = "lightcoral", color = "black") +
    labs(title = paste("Box Plot of", col), y = col) +
    theme_minimal() +
    coord_flip() # Make it horizontal for better readability
  print(p)
}

# F. Bar Plot for Categorical Variable ('Class')
cat("\n--- Bar Plot for 'Class' ---\n")
p_class_bar <- ggplot(data, aes(x = Class, fill = Class)) +
  geom_bar() +
  labs(title = "Distribution of Object Classes", x = "Class", y = "Count") +
  theme_minimal()
print(p_class_bar)

# G. Quantile-Quantile (Q-Q) Plots for Numerical Variables (to check for normality)
cat("\n--- Q-Q Plots for Numerical Variables (Checking for Normality) ---\n")
for (col in numerical_cols) {
  if (sum(!is.na(data[[col]])) < 2) {
    message(paste("Skipping Q-Q plot for", col, "due to insufficient non-NA data."))
    next
  }
  # Create a data frame for ggplot
  qq_data <- data.frame(sample = sort(na.omit(data[[col]])))
  p <- ggplot(qq_data, aes(sample = sample)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste("Q-Q Plot of", col), x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  print(p)
}


# --- 3. Bivariate Analysis ---

cat("\n--- Bivariate Analysis: Plots and Tables ---\n")

# A. Scatter Plots (Numerical vs. Numerical)
cat("\n--- Scatter Plots (Numerical vs. Numerical) ---\n")

# Example 1: Log_Lx vs. z, colored by Class
p_lx_z_class <- ggplot(data, aes(x = z, y = Log_Lx, color = Class)) +
  geom_point(alpha = 0.6) +
  labs(title = "Log_Lx vs. Redshift (z) by Class", x = "Redshift (z)", y = "Log(Lx)") +
  theme_minimal()
print(p_lx_z_class)

# Example 2: Flux_0p5_4p5 vs. Log_Lx
p_flux_lx <- ggplot(data, aes(x = Flux_0p5_4p5, y = Log_Lx)) +
  geom_point(alpha = 0.6) +
  labs(title = "Flux_0p5_4p5 vs. Log_Lx", x = "Flux (0.5-4.5 keV)", y = "Log(Lx)") +
  theme_minimal()
print(p_flux_lx)

# Example 3: Bmag vs. Rmag, colored by Class
# Note: These columns have NAs, so geom_point will automatically drop them.
p_bmag_rmag_class <- ggplot(data, aes(x = Bmag, y = Rmag, color = Class)) +
  geom_point(alpha = 0.6) +
  labs(title = "Bmag vs. Rmag by Class", x = "B-band Magnitude", y = "R-band Magnitude") +
  theme_minimal()
print(p_bmag_rmag_class)

# B. Box Plots (Numerical by Categorical)
cat("\n--- Box Plots (Numerical by Categorical) ---\n")

# Example 1: Log_Lx by Class
p_lx_by_class <- ggplot(data, aes(x = Class, y = Log_Lx, fill = Class)) +
  geom_boxplot() +
  labs(title = "Log_Lx by Class", x = "Class", y = "Log(Lx)") +
  theme_minimal()
print(p_lx_by_class)

# Example 2: Redshift (z) by Class
p_z_by_class <- ggplot(data, aes(x = Class, y = z, fill = Class)) +
  geom_boxplot() +
  labs(title = "Redshift (z) by Class", x = "Class", y = "Redshift (z)") +
  theme_minimal()
print(p_z_by_class)

# C. Grouped Density Plots (Numerical by Categorical)
cat("\n--- Grouped Density Plots (Numerical by Categorical) ---\n")
for (col in numerical_cols) {
  if (col == "RA_deg" || col == "Dec_deg") { # Skip coordinates for density plots by class
    next
  }
  if (sum(!is.na(data[[col]])) < 2 || nlevels(data$Class) < 2) {
    message(paste("Skipping grouped density plot for", col, "due to insufficient data or class levels."))
    next
  }
  p <- ggplot(data, aes_string(x = col, fill = "Class")) +
    geom_density(alpha = 0.6) +
    labs(title = paste("Density of", col, "by Class"), x = col, y = "Density") +
    theme_minimal()
  print(p)
}

# D. Violin Plots (Numerical by Categorical)
cat("\n--- Violin Plots (Numerical by Categorical) ---\n")
for (col in numerical_cols) {
  if (col == "RA_deg" || col == "Dec_deg") { # Skip coordinates for violin plots by class
    next
  }
  if (sum(!is.na(data[[col]])) < 2 || nlevels(data$Class) < 2) {
    message(paste("Skipping violin plot for", col, "due to insufficient data or class levels."))
    next
  }
  p <- ggplot(data, aes_string(x = "Class", y = col, fill = "Class")) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, outlier.shape = NA) + # Add boxplot inside violin
    labs(title = paste("Violin Plot of", col, "by Class"), x = "Class", y = col) +
    theme_minimal()
  print(p)
}

# E. Aggregated Summary Statistics by Class
cat("\n--- Aggregated Summary Statistics by Class ---\n")
# Calculate mean and standard deviation for numerical columns, grouped by Class
summary_by_class <- data %>%
  group_by(Class) %>%
  summarise(
    across(all_of(numerical_cols),
           list(
             mean = ~ mean(.x, na.rm = TRUE),
             sd = ~ sd(.x, na.rm = TRUE),
             median = ~ median(.x, na.rm = TRUE),
             n = ~ sum(!is.na(.x))
           ),
           .names = "{.col}_{.fn}"
    )
  )
print(summary_by_class)


# --- 4. Correlation Analysis ---

cat("\n--- Correlation Analysis ---\n")

# Select only numerical columns for correlation matrix
data_numeric <- data %>% select(all_of(numerical_cols))

# Calculate the correlation matrix (pairwise complete observations to handle NAs)
correlation_matrix <- cor(data_numeric, use = "pairwise.complete.obs")
cat("\n--- Correlation Matrix (cor()) ---\n")
print(correlation_matrix)

# Visualize the correlation matrix using corrplot
cat("\n--- Correlation Plot (corrplot) ---\n")
corrplot(correlation_matrix, method = "circle", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # Add correlation coefficients
         number.cex = 0.7,      # Size of coefficients
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "\nCorrelation Matrix of Numerical Variables")

# --- 5. Hypothesis Testing ---

cat("\n--- Hypothesis Testing Examples ---\n")

# A. Analysis of Variance (ANOVA)
# Test if the mean Log_Lx differs significantly across different 'Class' categories
# ANOVA requires the dependent variable to be numerical and the independent variable categorical.
# It also assumes normality of residuals and homogeneity of variances.
cat("\n--- ANOVA: Log_Lx by Class ---\n")
# Filter out NAs in Log_Lx and Class for ANOVA
data_anova <- data %>% filter(!is.na(Log_Lx) & !is.na(Class))
if (nlevels(data_anova$Class) > 1) {
  anova_model <- aov(Log_Lx ~ Class, data = data_anova)
  print(summary(anova_model))
  # Post-hoc test if ANOVA is significant and more than 2 groups
  if (nlevels(data_anova$Class) > 2 && summary(anova_model)[[1]][["Pr(>F)"]][1] < 0.05) {
    cat("\n--- Tukey HSD Post-Hoc Test for Log_Lx by Class ---\n")
    print(TukeyHSD(anova_model))
  }
} else {
  message("ANOVA for Log_Lx by Class skipped: 'Class' has only one level after NA removal.")
}


# B. Chi-Squared Test of Independence (for categorical variables)
# Since you only have one categorical variable ('Class'), we can only perform a goodness-of-fit test
# to see if the observed frequencies differ from expected frequencies (e.g., equal distribution).
# If you had another categorical variable, you would use it here.
cat("\n--- Chi-Squared Goodness-of-Fit Test for 'Class' (assuming equal proportions) ---\n")
# Filter out NAs in Class
class_counts <- table(data$Class)
if (length(class_counts) > 1) {
  # Assuming null hypothesis of equal proportions for simplicity
  expected_props <- rep(1/length(class_counts), length(class_counts))
  chi_sq_test <- chisq.test(class_counts, p = expected_props)
  print(chi_sq_test)
} else {
  message("Chi-squared test for 'Class' skipped: 'Class' has only one level or no data.")
}


# C. T-test (Example: if you wanted to compare Log_Lx between two specific classes)
# Let's assume you want to compare 'BLAGN' and 'Gal' classes for Log_Lx
cat("\n--- Independent Samples T-test: Log_Lx between BLAGN and Gal (Example) ---\n")
# Subset data for only 'BLAGN' and 'Gal'
data_two_classes <- data %>%
  filter(Class %in% c("BLAGN", "Gal")) %>%
  filter(!is.na(Log_Lx))

if (length(unique(data_two_classes$Class)) == 2) {
  t_test_result <- t.test(Log_Lx ~ Class, data = data_two_classes)
  print(t_test_result)
} else {
  message("T-test for Log_Lx between BLAGN and Gal skipped: Not enough data for two groups.")
}

# D. Shapiro-Wilk Test for Normality (for individual numerical variables)
cat("\n--- Shapiro-Wilk Test for Normality (Examples) ---\n")
for (col in numerical_cols) {
  # Shapiro-Wilk test is sensitive to sample size; typically for n < 5000
  # Also, it requires at least 3 non-NA data points.
  clean_data <- na.omit(data[[col]])
  if (length(clean_data) >= 3 && length(clean_data) < 5000) {
    shapiro_result <- shapiro.test(clean_data)
    cat(paste0("\nShapiro-Wilk Test for ", col, ":\n"))
    print(shapiro_result)
  } else if (length(clean_data) >= 5000) {
    message(paste("Shapiro-Wilk test for", col, "skipped: Sample size too large for reliable interpretation (n =", length(clean_data), "). Consider visual inspection (Q-Q plot) instead."))
  } else {
    message(paste("Shapiro-Wilk test for", col, "skipped: Insufficient non-NA data (n =", length(clean_data), ")."))
  }
}


# E. Levene's Test for Homogeneity of Variances (for ANOVA assumption)
cat("\n--- Levene's Test for Homogeneity of Variances (Log_Lx by Class) ---\n")
# Requires 'car' package
# Filter out NAs in Log_Lx and Class
data_levene <- data %>% filter(!is.na(Log_Lx) & !is.na(Class))
if (nlevels(data_levene$Class) > 1 && nrow(data_levene) > 0) {
  levene_test_result <- leveneTest(Log_Lx ~ Class, data = data_levene)
  print(levene_test_result)
} else {
  message("Levene's test for Log_Lx by Class skipped: 'Class' has only one level or no data after NA removal.")
}


# --- 6. Regression Analysis ---

cat("\n--- Regression Analysis ---\n")

# A. Simple Linear Regression: Log_Lx as a function of redshift (z)
cat("\n--- Simple Linear Regression: Log_Lx ~ z ---\n")
# Remove rows with NA in 'Log_Lx' or 'z' for regression
reg_data_simple <- data %>% filter(!is.na(Log_Lx) & !is.na(z))
simple_lm_model <- lm(Log_Lx ~ z, data = reg_data_simple)
print(summary(simple_lm_model))

# Plotting the simple linear regression
p_simple_lm <- ggplot(reg_data_simple, aes(x = z, y = Log_Lx)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Simple Linear Regression: Log_Lx vs. Redshift (z)",
       x = "Redshift (z)", y = "Log(Lx)") +
  theme_minimal()
print(p_simple_lm)

# B. Multiple Linear Regression: Log_Lx as a function of z, Flux_0p5_4p5, Bmag, Rmag
cat("\n--- Multiple Linear Regression: Log_Lx ~ z + Flux_0p5_4p5 + Bmag + Rmag ---\n")
# Remove rows with NA in any of the model variables
reg_data_multiple <- data %>%
  filter(!is.na(Log_Lx) & !is.na(z) & !is.na(Flux_0p5_4p5) & !is.na(Bmag) & !is.na(Rmag))

# Check if there's enough data after filtering NAs
if(nrow(reg_data_multiple) > length(c("z", "Flux_0p5_4p5", "Bmag", "Rmag")) + 1) {
  multiple_lm_model <- lm(Log_Lx ~ z + Flux_0p5_4p5 + Bmag + Rmag, data = reg_data_multiple)
  print(summary(multiple_lm_model))
  
  # Optional: Plotting residuals vs fitted for multiple regression diagnostics
  cat("\n--- Regression Diagnostics Plots ---\n")
  # Plot 1: Residuals vs Fitted
  plot(multiple_lm_model, which = 1, caption = "Residuals vs Fitted")
  # Plot 2: Normal Q-Q
  plot(multiple_lm_model, which = 2, caption = "Normal Q-Q")
  # Plot 3: Scale-Location (Spread-Location)
  plot(multiple_lm_model, which = 3, caption = "Scale-Location")
  # Plot 4: Residuals vs Leverage
  plot(multiple_lm_model, which = 5, caption = "Residuals vs Leverage")
  
  
} else {
  message("Multiple Linear Regression skipped: Not enough complete observations after removing NAs for all specified variables.")
  message(paste("Number of complete observations:", nrow(reg_data_multiple)))
}

# You can save plots to files if needed (uncomment and modify path)
# ggsave("histogram_z.png", plot = p_z_hist, width = 6, height = 4)
# ggsave("scatter_lx_z_class.png", plot = p_lx_z_class, width = 8, height = 6)
# ggsave("correlation_plot.png", width = 8, height = 8) # For corrplot, save after plotting it
