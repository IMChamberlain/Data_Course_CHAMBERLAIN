# Isaac Chamberlain
# 10797985
# Exam 2

library(tidyverse)
library(modelr)

# 1----------------------------------------------------------------------------------------------- 
df <- read_csv("unicef-u5mr.csv")


# 2-----------------------------------------------------------------------------------------------  
df_tidy <- df %>% 
  pivot_longer(cols = starts_with("U5MR"), 
               names_to = "Year", 
               values_to = "U5MR",
               names_prefix = "U5MR.") %>% 
  mutate(Year = as.numeric(Year)) # Converting text/ string to actual number

head(df_tidy) # Making sure it looks right in the console


# 3----------------------------------------------------------------------------------------------- 
plot1_countries_by_continent <- ggplot(df_tidy, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line() +
  facet_wrap(~Continent, scales = "fixed", strip.position = "top", axes = "all") +
  theme_minimal() +
  labs(title = "U5MR Over Time by Country",
       x = "Year",
       y = "U5MR")

plot1_countries_by_continent


# 4-----------------------------------------------------------------------------------------------  
ggsave("CHAMBERLAIN_Plot_1.png", plot = plot1_countries_by_continent, width = 10, height = 8)


# 5----------------------------------------------------------------------------------------------- 
df_means <- df_tidy %>% 
  group_by(Continent, Year) %>% 
  summarize(Mean_U5MR = mean(U5MR, na.rm = TRUE))
# ^^ Avg Number/ Continent/ Year

plot2_avg_per_continent <- ggplot(df_means, aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(linewidth = 2) + 
  theme_minimal() +
  labs(title = "Mean U5MR by Continent Over Time",
       x = "Year",
       y = "Mean U5MR")

plot2_avg_per_continent


# 6-----------------------------------------------------------------------------------------------  
ggsave("CHAMBERLAIN_Plot_2.png", plot = plot2_avg_per_continent, width = 10, height = 8)


# 7-----------------------------------------------------------------------------------------------  
mod1 <- lm(U5MR ~ Year, data = df_tidy) # U5MR ~ year only
mod2 <- lm(U5MR ~ Year + Continent, data = df_tidy) # U5MR ~ year and continent
mod3 <- lm(U5MR ~ Year * Continent, data = df_tidy) # U5MR ~ year, continent, and interaction between them


# 8-----------------------------------------------------------------------------------------------
# Experimenting with r.squared instead of MSE. It sounds like MSE is only relative to the other models being compared, but r.squared returns a value that is more universally comparable.
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared
summary(mod3)$adj.r.squared # This model returns the highest/ "best" r.squared value


# 9-----------------------------------------------------------------------------------------------
df_preds <- df_tidy %>% 
  add_predictions(mod1, var = "mod1") %>% 
  add_predictions(mod2, var = "mod2") %>% 
  add_predictions(mod3, var = "mod3")
# ^^ Add predictions to original tidy data

df_preds_long <- df_preds %>% 
  pivot_longer(cols = c("mod1", "mod2", "mod3"), 
               names_to = "Model", 
               values_to = "Predicted_U5MR")
head(df_preds_long)
# ^^ Model and prediction columns

ggplot(df_preds_long, aes(x = Year)) +
  # geom_line(aes(y = U5MR, group = CountryName), alpha = 0.05) + # Shows real data in background if uncommented. Commented out to make it look like the plot in the instructions. 
  geom_line(aes(y = Predicted_U5MR, color = Continent), linewidth = 1) + 
  facet_wrap(~Model) +
  theme_minimal() +
  labs(title = "Model Predictions",
       y = "Predicted U5MR",
       x = "Year")

# 10-----------------------------------------------------------------------------------------------
ecuador_2020 <- data.frame(CountryName = "Ecuador", 
                           Continent = "Americas", 
                           Year = 2020)

pred_mod3 <- predict(mod3, newdata = ecuador_2020)
pred_mod3 #returns a negative value (invalid)

mod4 <- lm(log10(U5MR) ~ Year * Continent, data = df_tidy) # log/ curved relationship instead of linear/ straight

pred_log <- predict(mod4, newdata = ecuador_2020)
pred_mod4 <- 10^pred_log

diff <- pred_mod4 - 13

data.frame(Model = "mod4", 
           Prediction = pred_mod4, 
           Reality = 13, 
           Difference = diff)

# The model predicted 11.99908 - The difference between the model and reality (13) is -1.000915.