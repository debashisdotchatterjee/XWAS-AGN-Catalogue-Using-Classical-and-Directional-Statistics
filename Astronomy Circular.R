# --- IMPORTANT: Install and Load Necessary Packages ---
# If you haven't installed these packages, uncomment the 'install.packages()' lines below
# and run them once. Then, ensure the 'library()' calls are run every time you start a new R session.

# install.packages("circular") # Essential for directional statistics
# install.packages("tidyverse") # Essential for data manipulation (dplyr) and plotting (ggplot2)
# install.packages("psych")     # For descriptive statistics (optional, but useful)
# install.packages("car")       # For Levene's test (homogeneity of variances)
# install.packages("plotrix")   # For polar.plot or other circular plots if needed, though 'circular' package handles much

# Load the installed packages
suppressPackageStartupMessages(library(circular))
suppressPackageStartupMessages(library(tidyverse)) # For data manipulation, already in previous Canvas
suppressPackageStartupMessages(library(psych)) # For describe() function
suppressPackageStartupMessages(library(car))   # For Levene's test
# suppressPackageStartupMessages(library(plotrix)) # Only load if polar.plot is specifically used

# --- 1. Data Loading and Preparation ---

# Load the dataset
# Make sure 'xwas_agn_clean.csv' is in your R working directory,
# or provide the full path to the file.
library(readxl)
data <- read_excel("C:/Users/SUBHRAJIT SAHA/Downloads/xwas_agn_clean.xlsx")

# Select the directional columns (RA_deg and Dec_deg) and relevant linear columns
# RA_deg is Right Ascension, typically 0 to 360 degrees
# Dec_deg is Declination, typically -90 to +90 degrees
# Also include 'Class', 'Log_Lx', 'z' for grouped analysis and correlation
directional_data_full <- data %>%
  select(RA_deg, Dec_deg, Class, Log_Lx, z) %>%
  drop_na(RA_deg, Dec_deg) # Remove rows with NA in RA or Dec for directional analysis

# Convert 'Class' to a factor
directional_data_full$Class <- as.factor(directional_data_full$Class)

ra_data <- directional_data_full$RA_deg
dec_data <- directional_data_full$Dec_data

# Convert degrees to radians for circular package functions
ra_radians <- rad(ra_data)
dec_radians <- rad(dec_data)

# Create circular objects for R's 'circular' package
circular_ra <- circular(ra_radians, units = "radians", type = "directions", modulo = "2pi")
circular_dec <- circular(dec_radians, units = "radians", type = "angles", modulo = "2pi")


# --- 2. Univariate Directional Statistics: Tables ---

cat("\n--- Univariate Directional Statistics: Tables ---\n")

# A. Descriptive Circular Statistics for RA_deg
cat("\n--- Circular Statistics for Right Ascension (RA_deg) ---\n")
if (length(circular_ra) > 0) {
  mean_ra <- mean(circular_ra)
  median_ra <- median(circular_ra)
  circular_variance_ra <- var(circular_ra)
  rho_ra <- rho.circular(circular_ra) # Mean resultant length (concentration)
  kappa_ra <- A1inv(rho_ra) # Estimate of concentration parameter kappa (if data is von Mises)
  
  cat(paste0("Mean Direction (radians): ", round(mean_ra, 4), "\n"))
  cat(paste0("Mean Direction (degrees): ", round(deg(mean_ra), 2), "\n"))
  cat(paste0("Median Direction (radians): ", round(median_ra, 4), "\n"))
  cat(paste0("Median Direction (degrees): ", round(deg(median_ra), 2), "\n"))
  cat(paste0("Circular Variance: ", round(circular_variance_ra, 4), "\n"))
  cat(paste0("Mean Resultant Length (rho): ", round(rho_ra, 4), "\n"))
  cat(paste0("Concentration Parameter (kappa, von Mises): ", round(kappa_ra, 4), "\n"))
  cat(paste0("Number of observations: ", length(circular_ra), "\n"))
} else {
  message("No non-NA data for RA_deg to compute circular statistics.")
}


# B. Descriptive Circular Statistics for Dec_deg
cat("\n\n--- Circular Statistics for Declination (Dec_deg) ---\n")
if (length(circular_dec) > 0) {
  mean_dec <- mean(circular_dec)
  median_dec <- median(circular_dec)
  circular_variance_dec <- var(circular_dec)
  rho_dec <- rho.circular(circular_dec)
  kappa_dec <- A1inv(rho_dec)
  
  cat(paste0("Mean Direction (radians): ", round(mean_dec, 4), "\n"))
  cat(paste0("Mean Direction (degrees): ", round(deg(mean_dec), 2), "\n"))
  cat(paste0("Median Direction (radians): ", round(median_dec, 4), "\n"))
  cat(paste0("Median Direction (degrees): ", round(deg(median_dec), 2), "\n"))
  cat(paste0("Circular Variance: ", round(circular_variance_dec, 4), "\n"))
  cat(paste0("Mean Resultant Length (rho): ", round(rho_dec, 4), "\n"))
  cat(paste0("Concentration Parameter (kappa, von Mises): ", round(kappa_dec, 4), "\n"))
  cat(paste0("Number of observations: ", length(circular_dec), "\n"))
} else {
  message("No non-NA data for Dec_deg to compute circular statistics.")
}

# C. Circular Quantiles (Example: 25th, 50th, 75th percentiles)
cat("\n--- Circular Quantiles for RA_deg (degrees) ---\n")
if (length(circular_ra) > 0) {
  ra_quantiles <- quantile(circular_ra, probs = c(0.25, 0.50, 0.75))
  cat(paste0("25th Percentile: ", round(deg(ra_quantiles[1]), 2), " deg\n"))
  cat(paste0("50th Percentile (Median): ", round(deg(ra_quantiles[2]), 2), " deg\n"))
  cat(paste0("75th Percentile: ", round(deg(ra_quantiles[3]), 2), " deg\n"))
} else {
  message("Skipping Circular Quantiles for RA_deg: No data.")
}

cat("\n--- Circular Quantiles for Dec_deg (degrees) ---\n")
if (length(circular_dec) > 0) {
  dec_quantiles <- quantile(circular_dec, probs = c(0.25, 0.50, 0.75))
  cat(paste0("25th Percentile: ", round(deg(dec_quantiles[1]), 2), " deg\n"))
  cat(paste0("50th Percentile (Median): ", round(deg(dec_quantiles[2]), 2), " deg\n"))
  cat(paste0("75th Percentile: ", round(deg(dec_quantiles[3]), 2), " deg\n"))
} else {
  message("Skipping Circular Quantiles for Dec_deg: No data.")
}


# --- 3. Univariate Directional Statistics: Plots ---

cat("\n--- Univariate Directional Statistics: Plots ---\n")

# A. Rose Plot (Circular Histogram) for RA_deg
cat("\n--- Rose Plot for Right Ascension (RA_deg) ---\n")
if (length(circular_ra) > 0) {
  # Plotting with default settings, adjust bins as needed
  plot(circular_ra, stack = TRUE, bins = 36, shrink = 1.5,
       main = "Rose Plot of Right Ascension (RA_deg)",
       col = "skyblue", cex.main = 1.2, cex.lab = 1.1)
  arrows.circular(mean_ra, rho_ra, col = "red", lwd = 2) # Add mean resultant vector
  text(0, 0, "N", pos = 3, offset = 1.5) # North reference
} else {
  message("Skipping Rose Plot for RA_deg: No non-NA data.")
}

# B. Rose Plot (Circular Histogram) for Dec_deg
cat("\n--- Rose Plot for Declination (Dec_deg) ---\n")
if (length(circular_dec) > 0) {
  # Plotting with default settings, adjust bins as needed
  plot(circular_dec, stack = TRUE, bins = 18, shrink = 1.5,
       main = "Rose Plot of Declination (Dec_deg)",
       col = "lightgreen", cex.main = 1.2, cex.lab = 1.1)
  arrows.circular(mean_dec, rho_dec, col = "red", lwd = 2) # Add mean resultant vector
  text(0, 0, "N", pos = 3, offset = 1.5) # North reference
} else {
  message("Skipping Rose Plot for Dec_deg: No non-NA data.")
}

# C. Circular Density Plot for RA_deg
# Load necessary package
library(circular)

# --- Circular Density Plot for Right Ascension (RA_deg) ---
cat("\n--- Circular Density Plot for Right Ascension (RA_deg) ---\n")

if (length(circular_ra) > 0) {
  # Estimate circular density with specified bandwidth
  ra_density <- density.circular(circular_ra, bw = 10)
  
  # Plot the density with better margins and fixed limits
  plot(ra_density,
       main = "Circular Density Plot of RA_deg",
       col = "darkblue",
       lwd = 2,
       cex.main = 1.2,
       cex.lab = 1.1,
       shrink = 1.1,              # expand radius slightly
       xlim = c(-1.5, 1.5),       # ensure full circle visible
       ylim = c(-1.5, 1.5),
       tcl.text = 0.1,            # tick label size
       zero = pi/2,               # rotate so 0 is at the top
       clockwise = TRUE,          # match astronomy convention
       axes = TRUE)
} else {
  message("Skipping Circular Density Plot for RA_deg: No non-NA data.")
}

# D. Circular Density Plot for Dec_deg
cat("\n--- Circular Density Plot for Declination (Dec_deg) ---\n")
if (length(circular_dec) > 0) {
  # Added bw argument to density.circular
  plot(density(circular_dec, bw = 10), main = "Circular Density Plot of Dec_deg",
       col = "darkgreen", lwd = 2, cex.main = 1.2, cex.lab = 1.1)
} else {
  message("Skipping Circular Density Plot for Dec_deg: No non-NA data.")
}

# E. Cartesian Scatter Plot of RA_deg vs Dec_deg (as a proxy for spherical distribution)
cat("\n--- Cartesian Scatter Plot of RA_deg vs Dec_deg ---\n")
if (nrow(directional_data_full) > 0) {
  p_ra_dec_scatter <- ggplot(directional_data_full, aes(x = RA_deg, y = Dec_deg, color = Class)) +
    geom_point(alpha = 0.6) +
    labs(title = "Scatter Plot of Right Ascension vs Declination",
         x = "Right Ascension (degrees)", y = "Declination (degrees)") +
    theme_minimal() +
    coord_fixed(ratio = 1) # Keep aspect ratio for better spatial representation
  print(p_ra_dec_scatter)
} else {
  message("Skipping RA_deg vs Dec_deg Scatter Plot: No non-NA data.")
}

# F. Grouped Rose Plots (RA_deg by Class)
cat("\n--- Grouped Rose Plots for RA_deg by Class ---\n")
if (nrow(directional_data_full) > 0 && nlevels(directional_data_full$Class) > 1) {
  # Create a list of circular objects, one for each class
  class_levels <- levels(directional_data_full$Class)
  circular_ra_by_class <- lapply(class_levels, function(cls) {
    ra_subset <- directional_data_full %>% filter(Class == cls) %>% pull(RA_deg) %>% na.omit()
    if (length(ra_subset) > 0) {
      circular(rad(ra_subset), units = "radians", type = "directions", modulo = "2pi")
    } else {
      NULL
    }
  })
  names(circular_ra_by_class) <- class_levels
  
  # Plot each class's rose plot
  for (i in seq_along(circular_ra_by_class)) {
    cls_name <- names(circular_ra_by_class)[i]
    circ_data <- circular_ra_by_class[[i]]
    if (!is.null(circ_data) && length(circ_data) > 0) {
      plot(circ_data, stack = TRUE, bins = 36, shrink = 1.5,
           main = paste("Rose Plot of RA_deg for Class:", cls_name),
           col = i + 1, # Use different colors for each class
           cex.main = 1.2, cex.lab = 1.1)
      if (length(circ_data) > 0) {
        arrows.circular(mean(circ_data), rho.circular(circ_data), col = "red", lwd = 2)
      }
    } else {
      message(paste("Skipping Grouped Rose Plot for RA_deg (Class:", cls_name, "): No data."))
    }
  }
} else {
  message("Skipping Grouped Rose Plots for RA_deg: Insufficient data or class levels.")
}

# G. Grouped Circular Density Plots (RA_deg by Class)
cat("\n--- Grouped Circular Density Plots for RA_deg by Class ---\n")
if (nrow(directional_data_full) > 0 && nlevels(directional_data_full$Class) > 1) {
  # Convert RA_deg to radians and add to the data frame for ggplot
  directional_data_full_rad <- directional_data_full %>%
    mutate(RA_rad = rad(RA_deg))
  
  p_grouped_density_ra <- ggplot(directional_data_full_rad, aes(x = RA_rad, fill = Class)) +
    geom_density(alpha = 0.6, adjust = 1.5) + # adjust controls smoothness
    coord_polar(start = 0) + # Make it circular
    scale_x_continuous(limits = c(0, 2*pi), breaks = seq(0, 2*pi, pi/4),
                       labels = c("0", "45", "90", "135", "180", "225", "270", "315", "360")) +
    labs(title = "Circular Density Plot of RA_deg by Class",
         x = "Right Ascension (degrees)", y = "Density") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8))
  print(p_grouped_density_ra)
} else {
  message("Skipping Grouped Circular Density Plots for RA_deg: Insufficient data or class levels.")
}

# H. Circular Boxplots (RA_deg by Class)
cat("\n--- Circular Boxplots for RA_deg by Class ---\n")
message("Note: The 'circular' package does not have a direct 'boxplot.circular' function for plotting multiple circular boxplots side-by-side.")
message("The grouped rose plots and grouped circular density plots (above) provide good alternatives for comparing distributions across classes.")
# The previous attempt to use boxplot.circular was incorrect as the function does not exist.
# A direct equivalent for ggplot2 for circular boxplots is also not straightforward.
# Removed the problematic code.


# --- 4. Bivariate Directional Analysis and Circular-Linear Relationships ---

cat("\n--- Bivariate Directional Analysis and Circular-Linear Relationships ---\n")

# A. Circular-Linear Correlation (e.g., RA_deg vs Log_Lx)
# Using Mardia's Circular-Linear Correlation Coefficient as cor.circular was problematic.
cat("\n--- Mardia's Circular-Linear Correlation: RA_deg vs Log_Lx ---\n")
data_cor_ra_lx <- directional_data_full %>% filter(!is.na(RA_deg) & !is.na(Log_Lx))
if (nrow(data_cor_ra_lx) > 1) {
  # Convert RA_deg to radians for trigonometric functions
  ra_radians_for_cor <- rad(data_cor_ra_lx$RA_deg)
  
  # Calculate components for Mardia's coefficient
  r_xc <- cor(data_cor_ra_lx$Log_Lx, cos(ra_radians_for_cor), use = "complete.obs")
  r_xs <- cor(data_cor_ra_lx$Log_Lx, sin(ra_radians_for_cor), use = "complete.obs")
  r_cc <- cor(cos(ra_radians_for_cor), sin(ra_radians_for_cor), use = "complete.obs")
  
  # Calculate Mardia's Circular-Linear Correlation Coefficient Squared
  denominator <- (1 - r_cc^2)
  if (denominator > 1e-9) { # Avoid division by zero or very small numbers
    R_CL_squared <- (r_xc^2 + r_xs^2) / denominator
    R_CL <- sqrt(R_CL_squared)
    cat(paste0("Mardia's Circular-Linear Correlation (RA_deg vs Log_Lx): ", round(R_CL, 4), "\n"))
  } else {
    message("Cannot compute Mardia's Circular-Linear Correlation: Denominator close to zero (perfect correlation between sine and cosine components).")
  }
} else {
  message("Skipping Mardia's Circular-Linear Correlation (RA_deg vs Log_Lx): Insufficient data.")
}

cat("\n--- Mardia's Circular-Linear Correlation: RA_deg vs z ---\n")
data_cor_ra_z <- directional_data_full %>% filter(!is.na(RA_deg) & !is.na(z))
if (nrow(data_cor_ra_z) > 1) {
  # Convert RA_deg to radians for trigonometric functions
  ra_radians_for_cor_z <- rad(data_cor_ra_z$RA_deg)
  
  # Calculate components for Mardia's coefficient
  r_xc_z <- cor(data_cor_ra_z$z, cos(ra_radians_for_cor_z), use = "complete.obs")
  r_xs_z <- cor(data_cor_ra_z$z, sin(ra_radians_for_cor_z), use = "complete.obs")
  r_cc_z <- cor(cos(ra_radians_for_cor_z), sin(ra_radians_for_cor_z), use = "complete.obs")
  
  # Calculate Mardia's Circular-Linear Correlation Coefficient Squared
  denominator_z <- (1 - r_cc_z^2)
  if (denominator_z > 1e-9) { # Avoid division by zero or very small numbers
    R_CL_squared_z <- (r_xc_z^2 + r_xs_z^2) / denominator_z # Corrected: r_xs_z was not squared
    R_CL_z <- sqrt(R_CL_squared_z)
    cat(paste0("Mardia's Circular-Linear Correlation (RA_deg vs z): ", round(R_CL_z, 4), "\n"))
  } else {
    message("Cannot compute Mardia's Circular-Linear Correlation: Denominator close to zero (perfect correlation between sine and cosine components).")
  }
} else {
  message("Skipping Mardia's Circular-Linear Correlation (RA_deg vs z): Insufficient data.")
}

# B. Circular-Linear Plot (RA_deg vs Log_Lx)
cat("\n--- Circular-Linear Plot: RA_deg vs Log_Lx ---\n")
if (nrow(data_cor_ra_lx) > 1) {
  # Convert RA_deg to degrees for plotting on x-axis
  p_ra_lx_linear <- ggplot(data_cor_ra_lx, aes(x = RA_deg, y = Log_Lx, color = Class)) +
    geom_point(alpha = 0.6) +
    labs(title = "RA_deg vs Log_Lx (Linear Scale)",
         x = "Right Ascension (degrees)", y = "Log(Lx)") +
    theme_minimal()
  print(p_ra_lx_linear)
} else {
  message("Skipping Circular-Linear Plot (RA_deg vs Log_Lx): Insufficient data.")
}

# C. Circular-Linear Regression Plot (RA_deg vs Log_Lx) using polar coordinates
cat("\n--- Circular-Linear Regression Plot: RA_deg vs Log_Lx ---\n")
data_reg_ra_lx <- directional_data_full %>% filter(!is.na(RA_deg) & !is.na(Log_Lx))
if (nrow(data_reg_ra_lx) > 1) {
  # Convert RA_deg to radians for polar plot
  data_reg_ra_lx_rad <- data_reg_ra_lx %>%
    mutate(RA_rad = rad(RA_deg))
  
  # Fit a linear model to predict Log_Lx from sin(RA) and cos(RA)
  # This is a common way to model circular-linear relationships in a regression context.
  # Note: This is not a "circular regression" in the sense of a circular response variable,
  # but rather a linear response variable predicted by circular components.
  circular_linear_model <- lm(Log_Lx ~ sin(RA_rad) + cos(RA_rad), data = data_reg_ra_lx_rad)
  data_reg_ra_lx_rad$predicted_Log_Lx <- predict(circular_linear_model, newdata = data_reg_ra_lx_rad)
  
  p_circular_linear_reg <- ggplot(data_reg_ra_lx_rad, aes(x = RA_rad, y = Log_Lx)) +
    geom_point(aes(color = Class), alpha = 0.6) +
    geom_line(aes(y = predicted_Log_Lx), color = "blue", size = 1) + # Add the regression line
    coord_polar(start = 0) +
    scale_x_continuous(limits = c(0, 2*pi), breaks = seq(0, 2*pi, pi/4),
                       labels = c("0", "45", "90", "135", "180", "225", "270", "315", "360")) +
    labs(title = "Circular-Linear Regression: RA_deg vs Log_Lx",
         x = "Right Ascension (degrees)", y = "Log(Lx)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8))
  print(p_circular_linear_reg)
  cat("\nSummary of Circular-Linear Regression Model (Log_Lx ~ sin(RA_rad) + cos(RA_rad)):\n")
  print(summary(circular_linear_model))
} else {
  message("Skipping Circular-Linear Regression Plot: Insufficient data for RA_deg and Log_Lx.")
}


# D. Circular Q-Q Plot for RA_deg (against von Mises distribution)
cat("\n--- Circular Q-Q Plot for RA_deg (against von Mises) ---\n")
if (length(circular_ra) > 1) { # Need at least 2 points
  mu_est <- mean(circular_ra)
  rho_val <- rho.circular(circular_ra) # Use rho_val to avoid confusion with global rho_ra
  kappa_est <- A1inv(rho_val) # Re-estimate kappa for robustness
  
  # Check if kappa_est is finite and non-negative
  if (is.finite(kappa_est) && kappa_est >= 0) {
    sorted_ra_numeric <- sort(as.numeric(circular_ra))
    n_ra_qq <- length(sorted_ra_numeric)
    
    # --- Debugging: Print n_ra_qq ---
    cat(paste0("DEBUG: n_ra_qq = ", n_ra_qq, "\n"))
    # --- End Debugging ---
    
    if (n_ra_qq > 0) {
      # Empirical CDF
      ecdf_ra <- ecdf(sorted_ra_numeric)
      empirical_probs <- ecdf_ra(sorted_ra_numeric)
      
      # Theoretical CDF of von Mises (pvonmises)
      theoretical_probs <- pvonmises(sorted_ra_numeric, mu = mu_est, kappa = kappa_est)
      
      # Plotting empirical vs theoretical probabilities
      plot(theoretical_probs, empirical_probs,
           main = "Circular ECDF Plot (RA_deg vs von Mises)",
           xlab = "Theoretical Cumulative Probability (von Mises)",
           ylab = "Empirical Cumulative Probability (RA_deg)",
           pch = 16, col = "blue")
      abline(0, 1, col = "red", lty = 2) # Add a 45-degree line for reference
    } else {
      message("Skipping Circular Q-Q Plot: No data points after sorting for ECDF calculation.")
    }
  } else {
    message("Skipping Circular Q-Q Plot: Kappa estimate is not finite or is negative (rho too low or too high).")
  }
} else {
  message("Skipping Circular Q-Q Plot for RA_deg: Insufficient data.")
}


# E. Circular-Linear Plot (RA_deg vs Log_Lx) using ggplot2 with polar coordinates
cat("\n--- Circular-Linear Plot (RA_deg vs Log_Lx) with Polar Coordinates ---\n")
data_plot_ra_lx_polar <- directional_data_full %>% filter(!is.na(RA_deg) & !is.na(Log_Lx))
if (nrow(data_plot_ra_lx_polar) > 0) {
  p_ra_lx_polar <- ggplot(data_plot_ra_lx_polar, aes(x = rad(RA_deg), y = Log_Lx, color = Class)) +
    geom_point(alpha = 0.6) +
    coord_polar(start = 0) +
    scale_x_continuous(limits = c(0, 2*pi), breaks = seq(0, 2*pi, pi/4),
                       labels = c("0", "45", "90", "135", "180", "225", "270", "315", "360")) +
    labs(title = "RA_deg vs Log_Lx (Polar Coordinates)",
         x = "Right Ascension (degrees)", y = "Log(Lx)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8))
  print(p_ra_lx_polar)
} else {
  message("Skipping Circular-Linear Plot (RA_deg vs Log_Lx) with Polar Coordinates: Insufficient data.")
}

# F. Circular-Linear Plot (RA_deg vs z) with Polar Coordinates
cat("\n--- Circular-Linear Plot (RA_deg vs z) with Polar Coordinates ---\n")
data_plot_ra_z_polar <- directional_data_full %>% filter(!is.na(RA_deg) & !is.na(z))
if (nrow(data_plot_ra_z_polar) > 0) {
  p_ra_z_polar <- ggplot(data_plot_ra_z_polar, aes(x = rad(RA_deg), y = z, color = Class)) +
    geom_point(alpha = 0.6) +
    coord_polar(start = 0) +
    scale_x_continuous(limits = c(0, 2*pi), breaks = seq(0, 2*pi, pi/4),
                       labels = c("0", "45", "90", "135", "180", "225", "270", "315", "360")) +
    labs(title = "RA_deg vs Redshift (z) (Polar Coordinates)",
         x = "Right Ascension (degrees)", y = "Redshift (z)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8))
  print(p_ra_z_polar)
} else {
  message("Skipping Circular-Linear Plot (RA_deg vs z) with Polar Coordinates: Insufficient data.")
}

# G. Bivariate Circular Plot (RA_deg vs Dec_deg) using circular package's plot function
cat("\n--- Bivariate Circular Plot: RA_deg vs Dec_deg (Spherical-like) ---\n")
# This is a bit more complex as Dec is not a full 360 circular variable.
# We can represent it as a 2D plot where RA is the angle and Dec is mapped to radius or color.
# The 'circular' package doesn't have a direct 2D circular-circular plot for spherical data.
# However, we can use the original Cartesian scatter plot (E) which is more appropriate
# for celestial coordinates.
# For a true spherical plot, specialized astronomical plotting libraries would be needed.
message("Note: For spherical data (RA, Dec), the Cartesian Scatter Plot (E) is generally more interpretable.")
message("A true 'bivariate circular plot' would imply both variables are 0-2pi circular, which Dec_deg is not.")


# --- 5. Hypothesis Testing for Directional Data (Examples) ---

cat("\n--- Hypothesis Testing for Directional Data (Examples) ---\n")

# A. Rayleigh Test for Uniformity (for RA_deg)
# Tests if the population mean direction is uniformly distributed (i.e., no preferred direction)
cat("\n--- Rayleigh Test for Uniformity (RA_deg) ---\n")
if (length(circular_ra) >= 10) { # Rayleigh test typically requires n >= 10
  rayleigh_ra <- rayleigh.test(circular_ra)
  print(rayleigh_ra)
} else {
  message("Skipping Rayleigh Test for RA_deg: Insufficient data (n < 10).")
}

# B. Rayleigh Test for Uniformity (for Dec_deg)
cat("\n--- Rayleigh Test for Uniformity (Dec_deg) ---\n")
if (length(circular_dec) >= 10) {
  rayleigh_dec <- rayleigh.test(circular_dec)
  print(rayleigh_dec)
} else {
  message("Skipping Rayleigh Test for Dec_deg: Insufficient data (n < 10).")
}

# C. Watson's U2 Test for Uniformity (alternative to Rayleigh, more sensitive to bimodal data)
cat("\n--- Watson's U2 Test for Uniformity (RA_deg) ---\n")
if (length(circular_ra) >= 1) { # Watson's U2 test can be used with smaller n
  watson_u2_ra <- watson.test(circular_ra, alpha = 0.05)
  print(watson_u2_ra)
} else {
  message("Skipping Watson's U2 Test for RA_deg: No data.")
}

# D. Watson's U2 Test for Uniformity (Dec_deg)
cat("\n--- Watson's U2 Test for Uniformity (Dec_deg) ---\n")
if (length(circular_dec) >= 1) {
  watson_u2_dec <- watson.test(circular_dec, alpha = 0.05)
  print(watson_u2_dec)
} else {
  message("Skipping Watson's U2 Test for Dec_deg: No data.")
}


# E. Confidence Interval for Mean Direction (RA_deg)
cat("\n--- Confidence Interval for Mean Direction (RA_deg) ---\n")
if (length(circular_ra) > 1) {
  # Calculate necessary components
  n_ra <- length(circular_ra)
  mean_ra_rad <- mean(circular_ra) # Mean direction in radians
  rho_ra_val <- rho.circular(circular_ra) # Mean resultant length
  
  # Define alpha for 95% CI
  alpha_ci <- 0.05
  z_alpha_half <- qnorm(1 - alpha_ci / 2) # Z-score for the confidence level (e.g., 1.96 for 95%)
  
  # Calculate the half-width (delta) of the confidence interval
  # Formula: delta = asin(z_alpha/2 / (rho * sqrt(n)))
  # Check for conditions where the argument to asin might be > 1 or rho is too small
  # This approximation is valid for large samples and assumes von Mises distribution.
  if (rho_ra_val > 0 && (z_alpha_half / (rho_ra_val * sqrt(n_ra))) <= 1) {
    delta_rad <- asin(z_alpha_half / (rho_ra_val * sqrt(n_ra)))
    
    ci_lower_rad <- mean_ra_rad - delta_rad
    ci_upper_rad <- mean_ra_rad + delta_rad
    
    # Convert to circular objects to handle wrapping and then to degrees for display
    ci_lower_deg <- deg(circular(ci_lower_rad, units="radians", type="directions", modulo="2pi"))
    ci_upper_deg <- deg(circular(ci_upper_rad, units="radians", type="directions", modulo="2pi"))
    
    cat(paste0("Approximate 95% Confidence Interval for Mean RA (degrees): [",
               round(ci_lower_deg, 2), ", ", round(ci_upper_deg, 2), "]\n"))
    cat(paste0("Note: This CI is approximate, based on a von Mises assumption for large samples.\n"))
    
  } else {
    message("Cannot compute approximate CI: Mean resultant length (rho) is too small, or sample size is insufficient for this approximation.")
  }
  
} else {
  message("Skipping Confidence Interval for Mean RA: Insufficient data (n <= 1).")
}

# F. Two-Sample Test for Equal Mean Directions (RA_deg by BLAGN vs Gal)
cat("\n--- Two-Sample Test for Equal Mean Directions (RA_deg by BLAGN vs Gal) ---\n")
# Filter data for specific classes and clean NAs
# Combine circular data and create a grouping factor
filtered_data_for_test <- directional_data_full %>%
  filter(Class %in% c("BLAGN", "Gal")) %>%
  drop_na(RA_deg, Class)

if (nrow(filtered_data_for_test) > 0 && nlevels(factor(filtered_data_for_test$Class)) == 2) {
  # Create a single circular object
  combined_ra_circular <- circular(rad(filtered_data_for_test$RA_deg),
                                   units = "radians", type = "directions", modulo = "2pi")
  # Create a grouping factor
  grouping_factor <- factor(filtered_data_for_test$Class)
  
  # Check if there are at least 5 observations in each group
  if (min(table(grouping_factor)) >= 5) {
    # Watson-Williams test (parametric, assumes von Mises and equal kappas)
    ww_test_result <- watson.williams.test(combined_ra_circular, grouping_factor)
    print(ww_test_result)
  } else {
    message("Skipping Two-Sample Test for RA_deg: Insufficient data (less than 5 observations in at least one group).")
  }
} else {
  message("Skipping Two-Sample Test for RA_deg: Not enough data or less than two distinct classes (BLAGN/Gal) after NA removal.")
}


# G. Goodness-of-Fit Test to von Mises Distribution (for RA_deg)
cat("\n--- Goodness-of-Fit Test to von Mises Distribution (RA_deg) ---\n")
if (length(circular_ra) >= 10) { # Requires sufficient data
  von_mises_fit_ra <- rvonmises(length(circular_ra), mu = mean_ra, kappa = kappa_ra)
  # A formal goodness-of-fit test is not directly in 'circular' package.
  # One common approach is to compare empirical CDF with theoretical CDF, or use a bootstrap.
  # As a proxy, we can visually compare density or use a custom test.
  # For now, we'll just indicate the parameters of the fitted von Mises.
  cat(paste0("Fitted von Mises parameters for RA_deg: mu = ", round(deg(mean_ra), 2), " deg, kappa = ", round(kappa_ra, 4), "\n"))
  # If a formal test is needed, consider implementing a bootstrap-based test or using other packages.
} else {
  message("Skipping Goodness-of-Fit Test for RA_deg: Insufficient data.")
}

# H. Plotting Mean Direction and 95% CI on a Circular Plot (RA_deg)
cat("\n--- Mean Direction and 95% CI Plot for RA_deg ---\n")
if (length(circular_ra) > 1 && rho_ra_val > 0 && (z_alpha_half / (rho_ra_val * sqrt(n_ra))) <= 1) {
  # Re-calculate CI bounds for plotting
  mean_ra_rad <- mean(circular_ra)
  rho_ra_val <- rho.circular(circular_ra)
  n_ra <- length(circular_ra)
  alpha_ci <- 0.05
  z_alpha_half <- qnorm(1 - alpha_ci / 2)
  delta_rad <- asin(z_alpha_half / (rho_ra_val * sqrt(n_ra)))
  
  ci_lower_rad <- mean_ra_rad - delta_rad
  ci_upper_rad <- mean_ra_rad + delta_rad
  
  # Plot the circular data
  plot(circular_ra, stack = TRUE, bins = 36, shrink = 1.5,
       main = "Mean Direction and 95% CI for RA_deg",
       col = "skyblue", cex.main = 1.2, cex.lab = 1.1)
  
  # Add the mean resultant vector
  arrows.circular(mean_ra_rad, rho_ra_val, col = "red", lwd = 2)
  
  # Generate points for the arc in radians
  arc_angles_rad <- seq(ci_lower_rad, ci_upper_rad, length.out = 100)
  
  # Convert angles to Cartesian coordinates (x, y) on the unit circle
  x_arc <- cos(arc_angles_rad)
  y_arc <- sin(arc_angles_rad)
  
  # Add the confidence interval arc using standard lines()
  lines(x_arc, y_arc, col = "darkgreen", lwd = 3, lty = 2) # Dashed green line for CI
  
  text(0, 0, "N", pos = 3, offset = 1.5) # North reference
} else {
  message("Skipping Mean Direction and CI Plot for RA_deg: Insufficient data or CI cannot be computed.")
}

