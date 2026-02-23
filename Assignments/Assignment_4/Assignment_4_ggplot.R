library(ggplot2)
library(dplyr)

patients <- read.csv("Fake Data For Final Project - Fake Patient Info.csv")
appointments <- read.csv("Fake Data For Final Project - Fake Appointment Info.csv")

getwd()
list.files()

colnames(patients)
colnames(appointments)

appointments$Appt.Date <- as.Date(appointments$Appt.Date)

patient_summary <- appointments %>%
  group_by(Patient.Name) %>%
  summarise(
    FirstVisit = min(Appt.Date),
    TotalVisits = n()
  )

patient_summary$LengthWithUs <- as.numeric(
  difftime(Sys.Date(), patient_summary$FirstVisit, units = "days")
) / 365.25

ggplot(patient_summary, aes(x = LengthWithUs, y = TotalVisits)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Patients With Longer Relationships Have More Total Visits",
    x = "Years as Patient",
    y = "Total Number of Visits"
  ) +
  theme_minimal()