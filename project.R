library(tidyverse)
# Utilizing tidyverse (ggplot2, tidyr, dplyr) for streamlined data manipulation
# and visualization.
# Packages Used:
# - ggplot2: Data visualization with a focus on grammar of graphics.
# - tidyr: Tools for cleaning and reshaping data.
# - dplyr: A grammar of data manipulation for data wrangling.

# NOTE: DATA ARE STORED IN THE DATA FOLDER
d1 <- read.table("data/AD Ni75Co25.dat")
d2 <- read.table("data/AR cO50nI50.dat")
d3 <- read.table("data/JGD Co75Ni25.dat")
d4 <- read.table("data/AD Co.dat")

# TODO: ADD Ni REF DATA AND SUBSAMPLE IT TO FIT

data <- bind_cols(d1, d2$V2, d3$V2, d4$V2)

colnames(data) <- c("2_theta", "Ni75Co25", "Ni50Co50", "Ni25Co75", "Co")

ggplot(data, aes(x = `2_theta`)) +
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

# NOTE: PLOT ARE STORED IN THE PLOT FOLDER
ggsave("plot/graph_nt.png", width = 10, height = 5.625)

ggplot(data[240:340, ], aes(x = `2_theta`)) +
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

ggplot(data, aes(x = `2_theta`)) +
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
  x = data$"2_theta",
  y1 = find_peaks(data$Ni75Co25),
  y2 = find_peaks(data$Ni50Co50),
  y3 = find_peaks(data$Ni25Co75),
  y4 = find_peaks(data$Co)
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

# Perform clustering
data_for_clustering <- non_zero_data_long[, "2_theta",
  drop = FALSE
] # Drop = FALSE ensures it remains a data frame

# Specify the number of clusters (k)
k <- 22

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

# FIXME: RESULTS CAN VARY DEPENDING ON THE CLUSTER PROCESS

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
#Results can vary depending on the clusters process

# Group by cluster and composition, calculate mean 2_theta
cluster_composition_means <- non_zero_data_long %>%
  group_by(cluster, Composition) %>%
  summarize(mean_2_theta = mean(`2_theta`))

# Find the composition with the lowest and highest mean 2_theta in each cluster
lowest_2_theta <- cluster_composition_means %>%
  slice(which.min(mean_2_theta))

highest_2_theta <- cluster_composition_means %>%
  slice(which.max(mean_2_theta))

# Print the results
cat("Composition with lowest mean 2_theta in each cluster:\n")
print(lowest_2_theta)

cat("\nComposition with highest mean 2_theta in each cluster:\n")
print(highest_2_theta)

# Summary table with mean 2_theta for each composition
composition_summary <- non_zero_data_long %>%
  group_by(Composition) %>%
  summarise(mean_2_theta = mean(`2_theta`))

# Identify composition with lowest and highest mean 2_theta
lowest_composition <- composition_summary[
  which.min(composition_summary$mean_2_theta),
]

# Bar plot for mean 2_theta for each composition
bar_plot <- ggplot(composition_summary, aes(
  x = mean_2_theta,
  y = Composition, fill = Composition
)) +
  geom_bar(stat = "identity", color = "white", width = 0.7, alpha = 0.8) +
  labs(
    title = "Mean 2_theta for Each Composition",
    x = "Mean 2_theta",
    y = "Composition"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Co" = "skyblue",
    "Ni25Co75" = "orange", "Ni50Co50" = "purple", "Ni75Co25" = "red"
  ))

# Print the bar plot
print(bar_plot)
ggsave("plot/mean_theta.png", width = 10, height = 5.625)
