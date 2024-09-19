# Load necessary libraries
library(ggplot2)
library(dplyr)

# the code simulates a relattionship between diversity and temperature.
# then it analyse it using temperature, precipitaion and aridity.
# what model will be better, only with temperature, or with the three variables

# Set seed for reproducibility
set.seed(123)

# Simulate data
n <- 100 # Number of data points
temperature <- seq(-10, 30, length.out = n) # Temperature gradient
precipitation <- runif(n, 50, 150) # Random precipitation values
aridity <- runif(n, 0.1, 0.9) # Random aridity index values

# Simulate diversity as a function of temperature, precipitation, and aridity
# We assume a hypothetical relationship
diversity <- 20 + 0.8 * temperature + rnorm(n, 0, 2) #- 0.05 * precipitation - 10 * aridity

# Create a data frame
data <- data.frame(temperature, precipitation, aridity, diversity)

# Fit a linear model using temperature, precipitation, and aridity as covariates
model_full <- lm(diversity ~ temperature + precipitation + aridity, data = data)

# Fit a linear model using only temperature as a covariate
model_temp <- lm(diversity ~ temperature, data = data)

# Create predictions for plotting
data <- data %>%
  mutate(pred_full = predict(model_full),
         pred_temp = predict(model_temp))

# Plot diversity vs temperature, color-coded by model
p1 <- ggplot(data, aes(x = temperature, y = diversity)) +
  geom_point(color = "black") +
  geom_line(aes(y = pred_full, color = "Model with Temp, Precip, Aridity"), size = 1) +
  geom_line(aes(y = pred_temp, color = "Model with Temp only"), size = 1, linetype = "dashed") +
  labs(title = "Diversity vs Temperature", x = "Temperature (°C)", y = "Diversity") +
  scale_color_manual(values = c("Model with Temp, Precip, Aridity" = "blue", "Model with Temp only" = "red")) +
  theme_minimal()

# Print the plot
print(p1)

# Compare models using summary statistics
summary(model_full)
summary(model_temp)
