library(tidyverse)

d1 <- read.table("AD Ni75Co25.dat")
d2 <- read.table("AR cO50nI50.dat")
d3 <- read.table("JGD Co75Ni25.dat")
d4 <- read.table("AD Co.dat")

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
    title = "Ni_(1-x)Co_X",
    x = "2_theta",
    y = "Intensity",
    fill = "Composition"
  ) +
  scale_fill_manual(values = c(
    "Ni75Co25" = "blue", "Ni50Co50" = "red",
    "Ni25Co75" = "green", "Co" = "yellow"
  )) +
  theme_minimal()

# Threshold for peaks
threshold <- 40
# PERF: FIND BEST THRESHOLD VALUE

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
  y1 = find_peaks(data$Ni75Co25), # Curve data
  y2 = find_peaks(data$Ni50Co50), # Point data 1
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

# Plot using ggplot2 with geom_tile
ggplot(data_long, aes(x = `2_theta`, y = Composition, fill = Intensity)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Non-Zero Values",
    x = "2_theta",
    y = "Composition",
    fill = "Intensity"
  ) +
  theme_minimal()

# TODO: FIND CLUSTER OF PEAKS TO ANALYSE THEM
