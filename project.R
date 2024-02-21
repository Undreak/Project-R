library(tidyverse)

d1b <- read.table("data/AD Ni75Co25.dat")
d2b <- read.table("data/AR cO50nI50.dat")
d3b <- read.table("data/JGD Co75Ni25.dat")
d4b <- read.table("data/AD Co.dat")

# No background
d1 <- read.table("data/AD Ni75Co25_nb.dat")
d2 <- read.table("data/AR cO50nI50_nb.dat")
d3 <- read.table("data/JGD Co75Ni25_nb.dat")
d4 <- read.table("data/AD Co_nb.dat")

# TODO: ADD Ni REF DATA AND SUBSAMPLE IT TO FIT

datab <- bind_cols(d1b, d2b$V2, d3b$V2, d4b$V2)

colnames(datab) <- c("2_theta", "Ni75Co25", "Ni50Co50", "Ni25Co75", "Co")

ggplot(datab, aes(x = `2_theta`)) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni25Co75`, fill = "Ni25Co75"),
    alpha = 0.1, color = "green"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni50Co50`, fill = "Ni50Co50"),
    alpha = 0.1, color = "red"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni75Co25`, fill = "Ni75Co25"),
    alpha = 0.1, color = "blue"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Co`, fill = "Co"),
    alpha = 0.1, color = "yellow"
  ) +
  labs(
    title = "Powder X-Ray Diffraction of Ni_(1-x)Co_x perovskites",
    x = "2 theta",
    y = "Intensity",
    fill = "Composition"
  ) +
  scale_fill_manual(values = c(
    "Ni75Co25" = "blue", "Ni50Co50" = "red",
    "Ni25Co75" = "green", "Co" = "yellow"
  )) +
  theme_minimal()

ggsave("plot/graph_nt.png", width = 10, height = 5.625)

ggplot(datab[240:340, ], aes(x = `2_theta`)) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni25Co75`, fill = "Ni25Co75"),
    alpha = 0.1, color = "green"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni50Co50`, fill = "Ni50Co50"),
    alpha = 0.1, color = "red"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni75Co25`, fill = "Ni75Co25"),
    alpha = 0.1, color = "blue"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Co`, fill = "Co"),
    alpha = 0.1, color = "yellow"
  ) +
  labs(
    title = "Powder X-Ray Diffraction of Ni_(1-x)Co_x perovskites",
    x = "2 theta",
    y = "Intensity",
    fill = "Composition"
  ) +
  scale_fill_manual(values = c(
    "Ni75Co25" = "blue", "Ni50Co50" = "red",
    "Ni25Co75" = "green", "Co" = "yellow"
  )) +
  theme_minimal()

ggsave("plot/graph_nt_zoom.png", width = 10, height = 5.625)

data <- bind_cols(d1, d2$V2, d3$V2, d4$V2)

colnames(data) <- c("2_theta", "Ni75Co25", "Ni50Co50", "Ni25Co75", "Co")

# Threshold for peaks
threshold <- 40
# PERF: FIND BEST THRESHOLD VALUE

ggplot(datab, aes(x = `2_theta`)) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni25Co75`, fill = "Ni25Co75"),
    alpha = 0.1, color = "green"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni50Co50`, fill = "Ni50Co50"),
    alpha = 0.1, color = "red"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni75Co25`, fill = "Ni75Co25"),
    alpha = 0.1, color = "blue"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Co`, fill = "Co"),
    alpha = 0.1, color = "yellow"
  ) +
  geom_hline(
    yintercept = threshold,
    linetype = "dashed", color = "black"
  ) +
  labs(
    title = "Powder X-Ray Diffraction of Ni_(1-x)Co_x perovskites",
    x = "2 theta",
    y = "Intensity",
    fill = "Composition"
  ) +
  scale_fill_manual(values = c(
    "Ni75Co25" = "blue", "Ni50Co50" = "red",
    "Ni25Co75" = "green", "Co" = "yellow"
  )) +
  theme_minimal()

ggsave("plot/graph.png", width = 10, height = 5.625)

find_peaks <- function(data) {
  if (length(data) < 2) {
    return(NULL) # No peak in lists with 0 or 1 element
  }
  peaks <- rep(0, length(data))
  # Create a vector to store the value and index of the peaks
  for (i in 2:(length(data) - 1)) {
    if (data[i] > data[i - 1] && data[i] > data[i + 1]) { # Looking for a peak
      peaks[i] <- data[i]
    }
  }
  # Checking if the first and last value is a peak or not
  if (data[1] > data[2]) {
    peaks[1] <- data[1]
  }
  if (tail(data, 1) > tail(data, 2)[1]) {
    peaks[length(peaks)] <- data[length(data)]
  }
  return(peaks)
}

data_peaks <- data.frame(
  x = datab$"2_theta",
  y1 = find_peaks(datab$Ni75Co25), # Curve data
  y2 = find_peaks(datab$Ni50Co50), # Point data 1
  y3 = find_peaks(datab$Ni25Co75),
  y4 = find_peaks(datab$Co)
)

# Filter all the data points with a certain threshold
data_peaks[, 2:5] <- data_peaks[, 2:5] * (data_peaks[, 2:5] > threshold)

colnames(data_peaks) <- c("2_theta", "Ni75Co25", "Ni50Co50", "Ni25Co75", "Co")

# Create a long-format data frame for ggplot
data_long <- pivot_longer(data_peaks,
  cols = -`2_theta`,
  names_to = "Composition", values_to = "Intensity"
)
# FIXME: IMPROVE THE CLUSTERING PROCESS

# Melt the data for ANOVA
data_long <- data_peaks %>%
  pivot_longer(
    cols = -c(`2_theta`),
    names_to = "Composition", values_to = "Intensity"
  )

# Plot using ggplot2 with geom_tile
ggplot(data_long, aes(x = `2_theta`, y = Composition, fill = Intensity)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Peaks Position and Intensity",
    x = "2 theta",
    y = "Composition",
    fill = "Intensity"
  ) +
  theme_minimal()

ggsave("plot/peaks.png", width = 10, height = 5.625)

# Filter out zero values
non_zero_data_long <- data_long[data_long$Intensity != 0, ]

# Perform clustering (replace k with your desired number of clusters)

# Assuming data_long has columns "2_theta" and "Intensity"
data_for_clustering <- non_zero_data_long[, "2_theta",
  drop = FALSE
] # Drop = FALSE ensures it remains a data frame

# Specify the number of clusters (k)
k <- 22
# You can adjust this based on your requirements

# Perform k-means clustering based only on 2_theta
cluster_assignments <- kmeans(data_for_clustering,
  centers = k,
  nstart = 4
)$cluster

# Add cluster assignments to the non-zero data
non_zero_data_long$cluster <- factor(cluster_assignments)

# Convert cluster to a factor
non_zero_data_long$cluster <- factor(non_zero_data_long$cluster)

# Plot using ggplot2 with geom_tile and discrete fill scale
ggplot(non_zero_data_long, aes(
  x = `2_theta`,
  y = Composition, fill = cluster
)) +
  geom_tile(color = "white") +
  scale_fill_discrete() + # Use discrete fill scale
  labs(
    title = "Heatmap of Non-Zero Intensitys",
    x = "2_theta",
    y = "Composition",
    fill = "Peak"
  ) +
  theme_minimal()

ggsave("plot/peak_clusters.png", width = 10, height = 5.625)

# HACK: NOT FINAL, NEED A LOT OF IMPROVEMENT
#       CLUSTERING IS TOO RANDOM

# Perform ANOVA
anova_result <- aov(`2_theta` ~ Composition * cluster,
  data = non_zero_data_long
)

# Print ANOVA summary
summary(anova_result)


# Get the number of unique clusters
num_clusters <- length(unique(non_zero_data_long$cluster))

# Create a boxplot with Composition and cluster information
ggplot(non_zero_data_long, aes(
  x = `2_theta`, y = factor(cluster),
  fill = factor(cluster)
)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of 2_theta Across Composition Levels and Clusters",
    x = "2 theta",
    y = "Cluster"
  ) +
  scale_fill_manual(values = rainbow(num_clusters)) +
  # Use rainbow color palette
  theme_minimal()

ggsave("plot/theta.png", width = 10, height = 5.625)

ggplot(non_zero_data_long[19:40, ], aes(
  x = `2_theta`, y = factor(cluster),
  fill = factor(cluster)
)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of 2_theta Across Composition Levels and Clusters",
    x = "2 theta",
    y = "Cluster"
  ) +
  scale_fill_manual(values = rainbow(num_clusters)) +
  # Use rainbow color palette
  theme_minimal()

ggsave("plot/theta_zoom.png", width = 10, height = 5.625)

# WARNING: WEIRD RESULTS

# Assuming cluster_assignments is your vector of cluster assignments
for (i in unique(cluster_assignments)) {
  cluster_data <- non_zero_data_long[cluster_assignments == i, ]

  # Perform ANOVA within each cluster
  anova_within_cluster <- aov(`2_theta` ~ Composition, data = cluster_data)

  # Print summary for each cluster
  cat(paste("Cluster", i, "\n"))
  print(summary(anova_within_cluster))
}

# Assuming anova_result is the result from your previous ANOVA
residuals_within_clusters <- residuals(anova_result)
print(residuals_within_clusters)

# INTENSITY ANALYSIS

ggplot(non_zero_data_long, aes(
  x = "2_theta",
  y = Intensity, fill = factor(cluster)
)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of the Intensity of the peaks for each cluster",
    x = "2 theta",
    y = "Your Variable"
  ) +
  theme_minimal()

# Calculate mean and standard deviation for each Composition
summary_stats <- non_zero_data_long %>%
  group_by(Composition) %>%
  summarize(
    mean_value = mean(Intensity),
    sd_value = sd(Intensity)
  )

# Identify conditions with lowest and highest mean and variance
lowest_mean_condition <- summary_stats$Composition[
  which.min(summary_stats$mean_value)
]
lowest_sd_condition <- summary_stats$Composition[
  which.min(summary_stats$sd_value)
]
highest_mean_condition <- summary_stats$Composition[
  which.max(summary_stats$mean_value)
]
highest_sd_condition <- summary_stats$Composition[
  which.max(summary_stats$sd_value)
]

# Boxplot code
boxplot_plot <- ggplot(non_zero_data_long, aes(
  x = "2_theta",
  y = Intensity, fill = factor(cluster)
)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of the Intensity of the peaks for each cluster",
    x = "2 theta",
    y = "Intensity"
  ) +
  theme_minimal()

# Add text annotations
boxplot_plot +
  annotate("text",
    x = 4, y = max(non_zero_data_long$Intensity),
    label = paste(
      "Lowest Mean (", lowest_mean_condition, "):",
      round(summary_stats$mean_value[
        summary_stats$Composition == lowest_mean_condition
      ], 2)
    ),
    vjust = 1.5, hjust = 0.5, size = 3, color = "red"
  ) +
  annotate("text",
    x =  10,
    y = max(non_zero_data_long$Intensity),
    label = paste(
      "Highest Mean (", highest_mean_condition, "):",
      round(summary_stats$mean_value[
        summary_stats$Composition == highest_mean_condition
      ], 2)
    ),
    vjust = 1.5, hjust = 0.5, size = 3, color = "blue"
  )

ggsave("plot/intensity.png", width = 10, height = 5.625)

composition_stats <- non_zero_data_long %>%
  group_by(Composition) %>%
  summarize(
    mean_value = mean(Intensity),
    variance_intensity = var(Intensity)
  )

# Print the result
print(composition_stats)

# Perform ANOVA
anova_result <- aov(Intensity ~ Composition * cluster,
  data = non_zero_data_long
)

# Print ANOVA summary
summary(anova_result)
