library(tidyverse)
library(modelr)
library(easystats)

mushroom_data <- read.csv("./Data/mushroom_growth.csv")

# Plot Nitrogen vs Growth Rate
ggplot(mushroom_data, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Growth Rate vs Nitrogen")

# Plot Light by Species vs Growth Rate
ggplot(mushroom_data, aes(x = Light, y = GrowthRate, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Light by Species vs Growth Rate")

# 4 Models
mod1 <- lm(GrowthRate ~ Nitrogen, data = mushroom_data)
mod2 <- lm(GrowthRate ~ Nitrogen + Light, data = mushroom_data)
mod3 <- lm(GrowthRate ~ Nitrogen + Light + Humidity, data = mushroom_data) # Winner
mod4 <- lm(GrowthRate ~ Nitrogen * Light, data = mushroom_data)

# Calculate MSE and select best model
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)

# Compare Models
compare_performance(mod1, mod2, mod3, mod4)

# Predictions with Hypothetical Data
hypothetical_data <- data.frame(
  Nitrogen = c(10, 20, 50, 100),
  Light = c(0, 10, 20, 30),
  Humidity = c("Low", "Low", "High", "High") 
)

# Predictions with mod3
hyp_preds <- hypothetical_data %>% 
  add_predictions(mod3)

print(hyp_preds)

# 7. Final plot- compare real data vs predictions
ggplot(mushroom_data, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point(alpha = 0.5) + 
  # Red diamonds for predictions
  geom_point(data = hyp_preds, aes(y = pred), color = "red", size = 4, shape = 18) + 
  theme_minimal() +
  labs(title = "Mushroom Growth: Real Data vs. Model 3 Predictions",
       subtitle = "Red Diamonds = Predictions",
       y = "Growth Rate")