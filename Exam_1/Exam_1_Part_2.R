# install.packages("tidyverse")
library(tidyverse)

covid_data <- read_csv("cleaned_covid_data.csv")

A_states <- covid_data %>% 
  filter(grepl("^A", Province_State))

ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  theme_minimal()

state_max_fatality_rate <- covid_data %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

state_max_fatality_rate <- state_max_fatality_rate %>%
  mutate(Province_State = factor
    (Province_State, levels = Province_State))

ggplot(state_max_fatality_rate,
  aes(x = Province_State,y = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

us_deaths <- covid_data %>%
  group_by(Last_Update) %>%
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE))

ggplot(us_deaths, aes(x = Last_Update, y = Total_Deaths, group = 1)) +
  geom_line() +
  theme_minimal()