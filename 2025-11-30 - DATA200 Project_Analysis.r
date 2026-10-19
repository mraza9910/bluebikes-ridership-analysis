############################################################
# BlueBikes Modeling + Visualization Script
# Author: Maida Raza
# DATA 200 — Modeling Phase
############################################################
# Force all ggplots to use white backgrounds

# Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(knitr)
library(gridExtra)

# FORCE ALL PLOTS TO PURE WHITE BACKGROUND (no grey anywhere)
theme_set(
  theme_bw(base_size = 14) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      strip.background  = element_rect(fill = "white", color = "black"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = "white", color = NA),
      panel.grid.major  = element_line(color = "grey85"),
      panel.grid.minor  = element_line(color = "grey90")
    ))

############################################################
# PATHS
############################################################

data_path <- "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/bluebikes_final.csv"

# Folder where all Exhibits PNGs will be written
outdir <- '/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Exhibits_Final'

############################################################
# LOAD CLEANED BLUEBIKES DATA
############################################################

bluebikes_final <- read.csv(
  data_path,
  header = TRUE,
  stringsAsFactors = FALSE
)

cat("Rows in bluebikes_final:", nrow(bluebikes_final), "\n")

############################################################
#  BUILD MODELING DATASET (analysis_df)
############################################################

analysis_df <- bluebikes_final %>%
  mutate(
    # Parse timestamps
    start_dt = parse_date_time(starttime, orders = c("ymd HMS", "Ymd HMS"),
                               tz = "America/New_York"),
    stop_dt  = parse_date_time(stoptime,  orders = c("ymd HMS", "Ymd HMS"),
                               tz = "America/New_York"),
    # Duration in minutes
    duration_min = as.numeric(stop_dt - start_dt, units = "secs") / 60,
    # Hour of day
    start_hour = hour(start_dt),
    # Rider type factor
    rider_type = factor(usertype, levels = c("Subscriber", "Customer")),
    # City factor using cleaned city name
    city = factor(PHYSICAL_CITY)
  ) %>%
  # Analysis restrictions
  filter(
    !is.na(start_dt),
    !is.na(stop_dt),
    !is.na(duration_min),
    duration_min > 0,
    duration_min <= 120,                              # cap at 2 hours
    !is.na(rider_type),
    !is.na(city),
    city %in% c("BROOKLINE", "CAMBRIDGE", "SOMERVILLE",
                "BOSTON", "SOUTH BOSTON", "CHARLESTOWN")
  )

cat("Rows in analysis_df:", nrow(analysis_df), "\n")
str(analysis_df)
View(analysis_df)
# =========================================================
# CHECK WHICH ZIP CODES HAVE MISSING PHYSICAL_CITY
# =========================================================

# 1. Filter rows where PHYSICAL_CITY is missing
missing_city_df <- analysis_df %>%
  filter(is.na(PHYSICAL_CITY) | PHYSICAL_CITY == "")

# 2. Count how many rows total
cat("Total rows with missing PHYSICAL_CITY:", nrow(missing_city_df), "\n")

# 3. Count missing by ZIP code
zip_missing_city_counts <- missing_city_df %>%
  group_by(postal.code_cleaned) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

zip_missing_city_counts

# 4. Show unique ZIPs missing city
unique_zips_missing_city <- unique(missing_city_df$postal.code_cleaned)
cat("Unique ZIP codes with missing PHYSICAL_CITY:\n")
print(unique_zips_missing_city)

############################################################
# SUMMARY TABLE (CITY × RIDER TYPE)
############################################################

summary_table <- analysis_df %>%
  group_by(city, rider_type) %>%
  summarise(
    n = n(),
    mean_duration   = round(mean(duration_min), 2),
    median_duration = round(median(duration_min), 2),
    sd_duration     = round(sd(duration_min), 2),
    .groups = "drop"
  )

print(summary_table)

# (If you ever want LaTeX for this table:)
kable(
  summary_table,
  caption = "Descriptive Statistics of Trip Duration by City and Rider Type",
  align   = "lccc"
)

############################################################
# FIT DEGREE-3 POLYNOMIAL REGRESSION MODEL
############################################################

# Make factors use sum-to-zero contrasts so all levels appear
options(contrasts = c("contr.sum", "contr.poly"))

analysis_df$city <- factor(analysis_df$city)  # re-factor to apply contrasts

model_poly3 <- lm(
  duration_min ~
    (start_hour + I(start_hour^2) + I(start_hour^3)) * rider_type +
    city,
  data = analysis_df
)
city_levels <- levels(analysis_df$city)

coef_table <- summary(model_poly3)$coefficients

# Find which rows correspond to the city coefficients
city_rows <- grep("^city[0-9]+$", rownames(coef_table))

# Rename them with real names
rownames(coef_table)[city_rows] <- paste0("city", city_levels)

summary(model_poly3)
coef_table



############################################################
# PREDICTION GRID + POLY CURVES PLOT
############################################################

# Extract factor levels from the model (guarantees compatibility)
valid_cities  <- levels(model_poly3$model$city)
valid_riders  <- levels(model_poly3$model$rider_type)

# Build prediction grid for 0–23 hours, both rider types, all cities
hour_grid <- expand.grid(
  start_hour = seq(0, 23, length.out = 200),
  rider_type = valid_riders,
  city       = valid_cities
)

# Predicted duration
hour_grid$pred <- predict(model_poly3, newdata = hour_grid)

# Plot model-predicted curves
poly_plot <- ggplot(hour_grid,
                    aes(x = start_hour, y = pred, color = rider_type)) +
  geom_line(size = 1.2) +
  facet_wrap(~ city) +
  labs(
    title  = "Predicted Trip Duration Across the Day by Rider Type and City",
    x      = "Start Hour",
    y      = "Predicted Duration (minutes)",
    color  = "Rider Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 16)
  )

ggsave(
  filename = paste0(outdir, "poly_curves.png"),
  plot     = poly_plot,
  width    = 11,
  height   = 8,
  dpi      = 300
)


############################################################
# FAST RAW DATA CURVES (Binned Means Instead of LOESS)
############################################################

# 1. Bin start_hour to 0–23 integer bins
analysis_df_binned <- analysis_df %>%
  mutate(hour_bin = floor(start_hour)) %>%
  group_by(city, rider_type, hour_bin) %>%
  summarise(
    mean_duration = mean(duration_min),
    .groups = "drop"
  )

# 2. Plot binned curves (fast, clean)
raw_fast_plot <- ggplot(analysis_df_binned,
                        aes(x = hour_bin,
                            y = mean_duration,
                            color = rider_type,
                            group = rider_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5, alpha = 0.7) +   # optional; looks good
  facet_wrap(~ city, ncol = 3) +
  scale_color_manual(values = c("Customer"   = "#F8766D",
                                "Subscriber" = "#00BFC4")) +
  labs(
    title  = "Raw Trip Duration Across the Day (Fast Smoothed Curves)",
    x      = "Start Hour",
    y      = "Mean Duration (minutes)",
    color  = "Rider Type"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    strip.background = element_rect(fill = "white", color = "black")
  )

ggsave(
  filename = paste0(outdir, "raw_polypattern_fast.png"),
  plot     = raw_fast_plot,
  width    = 11,
  height   = 8,
  dpi      = 300
)

############################################################
# FAST TIME-OF-DAY PLOTS 
############################################################

# 1. Time windows
time_windows <- list(
  Morning    = c(6, 10),
  Midday     = c(10, 14),
  Afternoon  = c(14, 18),
  Evening    = c(18, 22),
  LateNight  = c(22, 24)   # also wraps around for 0–6
)

# 2. Use EXACT model levels
valid_cities  <- levels(model_poly3$model$city)
valid_riders  <- levels(model_poly3$model$rider_type)

# 3. Build main prediction grid
base_grid <- expand.grid(
  start_hour = seq(0, 23, length.out = 400),
  rider_type = valid_riders,
  city       = valid_cities
)

base_grid$pred <- predict(model_poly3, newdata = base_grid)

# 4. Generate plots
plots_fast <- list()

for (window_name in names(time_windows)) {
  
  hr_min <- time_windows[[window_name]][1]
  hr_max <- time_windows[[window_name]][2]
  
  # Filter hours inside the window
  grid_win <- base_grid %>%
    filter(start_hour >= (hr_min %% 24),
           start_hour <  (hr_max %% 24))
  
  # SKIP if no rows (prevents "faceting variable" errors)
  if (nrow(grid_win) == 0) next
  
  # Build plot
  p <- ggplot(grid_win,
              aes(x = start_hour, y = pred, color = rider_type)) +
    geom_line(size = 1.2) +
    facet_wrap(~ city, ncol = 3) +
    labs(
      title = paste("Predicted Trip Duration —", window_name),
      x = "Start Hour",
      y = "Predicted Duration (minutes)",
      color = "Rider Type"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      legend.position = "bottom",
      strip.background = element_rect(fill = "white", color = "black")
    )
  
  plots_fast[[window_name]] <- p
  
  ggsave(
    filename = paste0(outdir, "poly_window_fast_", window_name, ".png"),
    plot     = p,
    width    = 11,
    height   = 8,
    dpi      = 300
  )
}

# Optional: show all generated plots
for (nm in names(plots_fast)) print(plots_fast[[nm]])

############################################################
# COMBINE ALL TIME-WINDOW PLOTS INTO ONE PDF
############################################################

library(gridExtra)

# Select plots in a preferred order (skip missing ones automatically)
ordered_names <- c("Morning", "Midday", "Afternoon", "Evening", "LateNight")
plots_to_combine <- plots_fast[ordered_names]

# Remove NULL entries (in case any window had no data)
plots_to_combine <- plots_to_combine[!sapply(plots_to_combine, is.null)]

# Save as single PDF
pdf(file = paste0(outdir, "poly_windows_all.pdf"),
    width = 14, height = 12)

grid.arrange(grobs = plots_to_combine, ncol = 2)

dev.off()


############################################################
# ADDITIONAL VISUALS (EXHIBITS)
############################################################

# 6.1 Duration distribution by rider type
p_density <- ggplot(analysis_df,
                    aes(x = duration_min, fill = rider_type)) +
  geom_density(alpha = 0.4) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(
    title = "Distribution of Trip Duration by Rider Type",
    x     = "Trip Duration (minutes)",
    y     = "Density",
    fill  = "Rider Type"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = paste0(outdir, "Exhibitsduration_density.png"),
  plot     = p_density,
  width    = 11,
  height   = 8,
  dpi      = 300
)

# 6.2 Empirical mean duration by hour (raw averages)
raw_hourly <- analysis_df %>%
  group_by(start_hour, rider_type) %>%
  summarise(mean_duration = mean(duration_min), .groups = "drop")

p_raw_means <- ggplot(raw_hourly,
                      aes(x = start_hour, y = mean_duration,
                          color = rider_type)) +
  geom_line(size = 1.2) +
  labs(
    title = "Empirical Mean Trip Duration by Hour of Day",
    x     = "Start Hour",
    y     = "Mean Duration (minutes)",
    color = "Rider Type"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = paste0(outdir, "Exhibitsraw_hourly_means.png"),
  plot     = p_raw_means,
  width    = 11,
  height   = 8,
  dpi      = 300
)

# 6.3 Boxplots by city and rider type
p_box <- ggplot(analysis_df,
                aes(x = city, y = duration_min, fill = rider_type)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(
    title = "Trip Duration Distribution by City and Rider Type",
    x     = "City",
    y     = "Trip Duration (minutes)",
    fill  = "Rider Type"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = paste0(outdir, "Exhibitscity_boxplots.png"),
  plot     = p_box,
  width    = 11,
  height   = 8,
  dpi      = 300
)

# 6.4 Overall city summary table -> saved as PNG
city_table <- analysis_df %>%
  group_by(city) %>%
  summarise(
    n              = n(),
    mean_duration  = round(mean(duration_min), 2),
    median_duration= round(median(duration_min), 2),
    sd_duration    = round(sd(duration_min), 2),
    .groups        = "drop"
  )

png(filename = paste0(outdir, "Exhibitscity_summary_table.png"),
    width = 1000, height = 400)
grid.table(city_table)
dev.off()

# 6.5 Hour × city heatmap of trip counts
heat_df <- analysis_df %>%
  group_by(city, start_hour) %>%
  summarise(count = n(), .groups = "drop")

p_heat <- ggplot(heat_df,
                 aes(x = start_hour, y = city, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Trip Count by City and Hour of Day",
    x     = "Start Hour",
    y     = "City",
    fill  = "Trip Count"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = paste0(outdir, "Exhibitshour_city_heatmap.png"),
  plot     = p_heat,
  width    = 11,
  height   = 8,
  dpi      = 300
)

############################################################
# MODEL DIAGNOSTICS (RESIDUALS + QQ PLOT)
############################################################

# Residuals vs fitted
png(filename = paste0(outdir, "Exhibitsmodel_residuals_vs_fitted.png"),
    width = 800, height = 600)
plot(model_poly3, which = 1)  # residuals vs fitted
dev.off()

# Q-Q plot
png(filename = paste0(outdir, "Exhibitsmodel_qqplot.png"),
    width = 800, height = 600)
plot(model_poly3, which = 2)  # QQ-plot
dev.off()

cat("All analysis and Exhibits generated.\n")
