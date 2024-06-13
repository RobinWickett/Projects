install.packages("haven")
library(haven)
New <- read_sav("/Users/robwickett/Desktop/Recoded items error/Baseline_July 24, 2023_15.04.sav")
Merged <- read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Merged/Study3.sav")
old <- read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Cleaned_withrawitems/Baseline_withitems_15.04.sav")
unique_values_in_new_data <- setdiff(New$Q1, old$Q1)
print(unique_values_in_new_data)

new_data <- subset(New, !(Q1 %in% unique_values_in_new_data))
write_sav(new_data, "/Users/robwickett/Desktop/PhD/Study 3/Data/Cleaned_withrawitems/Baseline_withitems_27.10.sav")
write_sav(merged_data, "/Users/robwickett/Desktop/Recoded items error/Recoded.sav")

Merged <- Merged %>%
  select(-Openess, -EnergyLevel, -Compassion, -Respectfulness, -Trust,
         -Organisation, -Productiveness, -Responsibility, -Anxiety, -Depression,
         -EmotionalVolatility, -IntellectualCuriosity, -AestheticSensitivty,
         -CreativeImagination, -Conscientiouness, -Neuroticism, -Sociability,
         -Assertivness, -Agreeablness, -Extrave)

merged_data <- merged_data %>%
  select(-Openess_c, -EnergyLevel_c, -Compassion_c, -Respectfulness_c, -Trust_c,
         -Organisation_c, -Productiveness_c, -Responsibility_c, -Anxiety_c, -Depression_c,
         -EmotionalVolatility_c, -IntellectualCuriosity_c, -AestheticSensitivty_c,
         -CreativeImagination_c, -Conscientiousness_c, -Neuroticism_c, -Sociability_c,
         -Assertiveness_c, -Agreeablness_c, -Extraversion_c)

# Remove the specified variables
new_data <- new_data %>%
  select(-Q32, -Q3)

merged_data <- left_join(Merged, select(new_data, Q1, Openess:Extrave), by = "Q1")

library("EMAtools")
merged_data$Extraversion_centered<-gcenter(merged_data$Extrave)
merged_data$Neuroticism_centered<-gcenter(merged_data$Neuroticism)
merged_data$Openess_centered<-gcenter(merged_data$Openess)
merged_data$Conscientiousness_centered<-gcenter(merged_data$Conscientiouness)
merged_data$Agreeablness_centered<-gcenter(merged_data$Agreeablness)

merged_data$Assertiveness_centered<-gcenter(merged_data$Assertivness)
merged_data$Sociability_centered<-gcenter(merged_data$Sociability)
merged_data$EnergyLevel_centered<-gcenter(merged_data$EnergyLevel)

merged_data$Trust_centered<-gcenter(merged_data$Trust)
merged_data$Compassion_centered<-gcenter(merged_data$Compassion)
merged_data$Respectfulness_centered<-gcenter(merged_data$Respectfulness)

merged_data$AestheticSensitivty_centered<-gcenter(merged_data$AestheticSensitivty)
merged_data$IntellectualCuriosity_centered<-gcenter(merged_data$IntellectualCuriosity)
merged_data$CreativeImagination_centered<-gcenter(merged_data$CreativeImagination)

merged_data$Organisation_centered<-gcenter(merged_data$Organisation)
merged_data$Productiveness_centered<-gcenter(merged_data$Productiveness)
merged_data$Responsibility_centered<-gcenter(merged_data$Responsibility)

merged_data$EmotionalVolatility_centered<-gcenter(merged_data$EmotionalVolatility)
merged_data$Anxiety_centered<-gcenter(merged_data$Anxiety)
merged_data$Depression_centered<-gcenter(merged_data$Depression)