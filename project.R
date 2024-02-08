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

# Filter data for X values between 20 and 25
filtered_data <- data %>%
  filter(`2_theta` >= 20 & `2_theta` <= 25)

# Find the maximum values and corresponding X values for each Y column
max_values <- filtered_data %>%
  summarise(
    max_Ni75Co25 = max(Ni75Co25),
    max_X_Ni75Co25 = `2_theta`[which.max(Ni75Co25)],
    max_Ni50Co50 = max(Ni50Co50),
    max_X_Ni50Co50 = `2_theta`[which.max(Ni50Co50)],
    max_Ni25Co75 = max(Ni25Co75),
    max_X_Ni25Co75 = `2_theta`[which.max(Ni25Co75)]
  )

# Print the results
print(max_values)
