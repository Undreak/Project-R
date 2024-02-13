library(tidyverse)

d1 <- read.table("AD Ni75Co25.dat")
d2 <- read.table("AR cO50nI50.dat")
d3 <- read.table("JGD Co75Ni25.dat")

data <- bind_cols(d1, d2$V2, d3$V2)

colnames(data) <- c("2_theta", "Ni75Co25", "Ni50Co50", "Ni25Co75")

ggplot(data, aes(x = `2_theta`)) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni25Co75`, fill = "Ni25Co75"),
    alpha = 0.5, color = "green"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni50Co50`, fill = "Ni50Co50"),
    alpha = 0.5, color = "red"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = `Ni75Co25`, fill = "Ni75Co25"),
    alpha = 0.5, color = "blue"
  ) +
  labs(
    title = "Ni_(1-x)Co_X",
    x = "2_theta",
    y = "Intensity",
    fill = "Composition"
  ) +
  scale_fill_manual(values = c(
    "Ni75Co25" = "blue", "Ni50Co50" = "red",
    "Ni25Co75" = "green"
  )) +
  theme_minimal()

# Threshold for peaks
threshold <- 50

# Filter data for peaks above the threshold
peaks_data <- data %>%
  filter(Ni75Co25 > threshold)

# Plotting peaks as bars
ggplot(peaks_data, aes(x = `2_theta`, y = Ni75Co25)) +
  geom_bar(
    stat = "identity", position = "dodge",
    color = "black", alpha = 0.7,
    width = 0.01
  ) +
  ggtitle("Peaks Above Threshold in Ni75Co25") +
  xlab("2_theta") +
  ylab("Intensity")

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

peaks_values <- find_peaks(peaks_data$Ni75Co25)
print(peaks_values)

data_peaks <- data.frame(
  x = peaks_data$"2_theta",
  y1 = peaks_data$Ni75Co25,  # Curve data
  y2 = peaks_values  # Point data 1
)

ggplot(data_peaks, aes(x = x)) +
  geom_line(aes(y = y1), color = "blue") +  # Curve
  geom_point(aes(y = y2), color = "red") +  # Point series 1
  ggtitle("Peaks Ni75Co25") +
  xlab("2_theta") +
  ylab("Intensity")

# TODO : DO THIS BUT FOR EVERY CURVE DATA AUTOMATICALLY WITH `sapply` 
