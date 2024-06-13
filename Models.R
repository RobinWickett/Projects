data <- read.csv("/Users/robwickett/Desktop/R_Study3/CenteredLevel2.csv")
library(tidyverse)
library(haven)
data <- read_sav("/Users/robwickett/Desktop/Recoded items error/Recoded.sav")

# Assuming your data is named my_data, Q1 represents participant IDs, and Q2 represents the responses
# Filter the data to include only rows where Q2 is equal to 1
filtered_data <- data %>% filter(Q2 == 1)

# Calculate frequency counts using the table() function
frequency_counts <- table(filtered_data$Q1)

# Calculate the mean frequency
mean_frequency <- mean(frequency_counts)

print(mean_frequency)

sd_frequency <- sd(frequency_counts)
range_frequency <- range(frequency_counts)

print(sd_frequency)
print(range_frequency)


# Assuming your data is named my_data, Q1 represents participant IDs, and Q2 represents the responses
# Filter the data to include only rows where Q2 is equal to 1 or 2
filtered_data <- data %>% filter(Q2 %in% c(1, 2))

# Calculate frequency counts using the table() function
frequency_counts <- table(filtered_data$Q1)

print(frequency_counts)

# Calculate the mean, standard deviation (SD), and range
mean_frequency <- mean(frequency_counts)
sd_frequency <- sd(frequency_counts)
range_frequency <- range(frequency_counts)

print(mean_frequency)
print(sd_frequency)
print(range_frequency)

data <- read_sav("/Users/robwickett/Desktop/R_Study3/joined_data.sav")







# Fit the mixed-effects model
model <- lmer(Q6 ~ Extraversion_centered + Neuroticism_centered + (1 | Q1), data = my_data)
model <- lmer(Q7 ~ Extraversion_centered + Neuroticism_centered + (1 | Q1), data = my_data)

# Print model summary
summary(model)

library(lme4)

# Assuming you have a data frame named 'data_no_duplicates'


library(lme4)

library(lme4)
library(mediation)

#Level 1 Models
model_SitMod_Q6 <- lmer(Daily_SitMod ~ Age_centered + Sex + Q6_centered + (1 | ID), data=data)
model_CogApp_Q6 <- lmer(Daily_CogApp ~ Age_centered + Sex + Q6_centered + (1 | ID), data=data)
model_Supp_Q6 <- lmer(Daily_Supp ~ Age_centered + Sex + Q6_centered + (1 | ID), data=data)
model_AttMod_Q6 <- lmer(Daily_AttMod ~ Age_centered + Sex + Q6_centered + (1 | ID), data=data)
model_SocAff_Q6 <- lmer(Daily_SocAff ~ Age_centered + Sex + Q6_centered + (1 | ID), data=data)

model_SitMod_Q7 <- lmer(Daily_SitMod ~ Age_centered + Sex + Q7_centered + (1 | ID), data=data)
model_CogApp_Q7 <- lmer(Daily_CogApp ~ Age_centered + Sex + Q7_centered + (1 | ID), data=data)
model_Supp_Q7 <- lmer(Daily_Supp ~ Age_centered + Sex + Q7_centered + (1 | ID), data=data)
model_AttMod_Q7 <- lmer(Daily_AttMod ~ Age_centered + Sex + Q7_centered + (1 | ID), data=data)
model_SocAff_Q7 <- lmer(Daily_SocAff ~ Age_centered + Sex + Q7_centered + (1 | ID), data=data)

summary(model_SitMod_Q6)
summary(model_CogApp_Q6)
summary(model_AttMod_Q6)
summary(model_Supp_Q6)
summary(model_SocAff_Q6)

summary(model_SitMod_Q7)
summary(model_CogApp_Q7)
summary(model_AttMod_Q7)
summary(model_Supp_Q7)
summary(model_SocAff_Q7)






# Level2 Models
model_Neuroticism_Q6 <- lmer(Q6 ~ Neuroticism_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Extraversion_Q6 <- lmer(Q6 ~ Extraversion_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
summary(model_Extraversion_Q6)
model_Neuroticism_Q7 <- lmer(Q7 ~ Neuroticism_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Extraversion_Q7 <- lmer(Q7 ~ Extraversion_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
summary(model_Neuroticism_Q6)
model_Agreeablness_Q6 <- lmer(Q6 ~ Agreeablness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_Q6 <- lmer(Q6 ~ Openess_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_Q7 <- lmer(Q7 ~ Openess_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Agreeablness_Q7 <- lmer(Q7 ~ Agreeablness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
summary(model_Neuroticism_Q7)


model_Conscientiousness_Q7 <- lmer(Q7 ~ Conscientiousness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_Q6 <- lmer(Q6 ~ Conscientiousness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)

summary(model_Openess_Q6)
summary(model_Extraversion_Neuroticism_Q7)
      
model_Extraversion_facets_Q6 <- lmer(Q6 ~ Sociability_centered + EnergyLevel_centered + Assertiveness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_facets_Q6 <- lmer(Q6 ~ EmotionalVolatility_centered + Depression_centered + Anxiety_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Extraversion_facets_Q7 <- lmer(Q7 ~ Sociability_centered + EnergyLevel_centered + Assertiveness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_facets_Q7 <- lmer(Q7 ~ EmotionalVolatility_centered + Depression_centered + Anxiety_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
summary(model_Neuroticism_facets_Q7)

model_Extraversion_SitMod <- lmer(Daily_SitMod ~ Extraversion_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_SitMod <- lmer(Daily_SitMod ~ Neuroticism_centered + Age_centered + Sex + (1 | Q1), data = model_Neuroticism_facets_Q7)
model_Neuroticism_Extraversion_SitMod <- lmer(Daily_SitMod ~ Neuroticism_centered + Extraversion_centered + Age_centered + Sex + (1 | Q1), data = model_Neuroticism_facets_Q7)
summary(model_Extraversion_SocAff)


model_Extraversion_AttMod <- lmer(Daily_AttMod ~ Extraversion_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_AttMod <- lmer(Daily_AttMod ~ Neuroticism_centered + Age_centered + Sex + (1 | Q1), data = merged_data)

model_Neuroticism_SocAff <- lmer(Daily_SocAff ~ Neuroticism_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Extraversion_SocAff <- lmer(Daily_SocAff ~ Extraversion_centered + Age_centered + Sex + (1 | Q1), data = merged_data)


model_Extraversion_facets_SitMod <- lmer(Daily_SitMod ~ Sociability_centered + EnergyLevel_centered + Assertiveness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_facets_SitMod <- lmer(Daily_SitMod ~ EmotionalVolatility_centered + Depression_centered + Anxiety_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Extraversion_facets_CogApp <- lmer(Daily_CogApp ~ Sociability_centered + EnergyLevel_centered + Assertiveness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_facets_CogApp <- lmer(Daily_CogApp ~ EmotionalVolatility_centered + Depression_centered + Anxiety_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Extraversion_facets_Supp <- lmer(Daily_Supp ~ Sociability_centered + EnergyLevel_centered + Assertiveness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_facets_Supp <- lmer(Daily_Supp ~ EmotionalVolatility_centered + Depression_centered + Anxiety_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Extraversion_facets_SocAff <- lmer(Daily_SocAff ~ Sociability_centered + EnergyLevel_centered + Assertiveness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_facets_SocAff <- lmer(Daily_SocAff ~ EmotionalVolatility_centered + Depression_centered + Anxiety_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Extraversion_facets_AttMod <- lmer(Daily_AttMod ~ Sociability_centered + EnergyLevel_centered + Assertiveness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Neuroticism_facets_AttMod <- lmer(Daily_AttMod ~ EmotionalVolatility_centered + Depression_centered + Anxiety_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
summary(model_Neuroticism_facets_Supp)





model_Openess_SitMod <- lmer(Daily_SitMod ~ Openess_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Agreeablness_SitMod <- lmer(Daily_SitMod ~ Agreeablness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousnessn_SitMod <- lmer(Daily_SitMod ~ Conscientiousness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_CogApp <- lmer(Daily_CogApp ~ Openess_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Agreeablness_CogApp <- lmer(Daily_CogApp ~ Agreeablness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_CogApp <- lmer(Daily_CogApp ~ Conscientiousness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_Supp <- lmer(Daily_Supp ~ Openess_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Agreeablness_Supp <- lmer(Daily_Supp ~ Agreeablness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_Supp <- lmer(Daily_Supp ~ Conscientiousness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_SocAff <- lmer(Daily_SocAff ~ Openess_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Agreeablness_SocAff <- lmer(Daily_SocAff ~ Agreeablness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_SocAff <- lmer(Daily_SocAff ~ Conscientiousness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_AttMod <- lmer(Daily_AttMod ~ Openess_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Agreeablness_AttMod <- lmer(Daily_AttMod ~ Agreeablness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_AttMod <- lmer(Daily_AttMod ~ Conscientiousness_centered + Age_centered + Sex + (1 | Q1), data = merged_data)

model_Agreeablness_facets_Q6 <- lmer(Q6 ~ Compassion_centered + Respectfulness_centered + Trust_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_facets_Q6 <- lmer(Q6 ~ Organisation_centered + Productiveness_centered + Responsibility_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_facets_Q6 <- lmer(Q6 ~ AsetheticSensitivty_centered + IntellectualCuriosity_centered + CreativeImagination_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Agreeablness_facets_Q7 <- lmer(Q7 ~ Compassion_centered + Respectfulness_centered + Trust_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_facets_Q7 <- lmer(Q7 ~ Organisation_centered + Productiveness_centered + Responsibility_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_facets_Q7 <- lmer(Q7 ~ AestheticSensitivty_centered + IntellectualCuriosity_centered + CreativeImagination_centered + Age_centered + Sex + (1 | Q1), data = merged_data)




model_Agreeablness_facets_SitMod <- lmer(Daily_SitMod ~ Compassion_centered + Respectfulness_centered + Trust_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_facets_SitMod <- lmer(Daily_SitMod ~ Organisation_centered + Productiveness_centered + Responsibility_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_facets_SitMod <- lmer(Daily_SitMod ~ AsetheticSensitivty_centered + IntellectualCuriosity_centered + CreativeImagination_centered + Age_centered + Sex + (1 | Q1), data = merged_data)

model_Agreeablness_facets_CogApp <- lmer(Daily_CogApp ~ Compassion_centered + Respectfulness_centered + Trust_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_facets_CogApp <- lmer(Daily_CogApp ~ Organisation_centered + Productiveness_centered + Responsibility_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_facets_CogApp <- lmer(Daily_CogApp ~ AestheticSensitivty_centered + IntellectualCuriosity_centered + CreativeImagination_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Agreeablness_facets_AttMod <- lmer(Daily_AttMod ~ Compassion_centered + Respectfulness_centered + Trust_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_facets_AttMod <- lmer(Daily_AttMod ~ Organisation_centered + Productiveness_centered + Responsibility_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_facets_AttMod <- lmer(Daily_AttMod ~ AsetheticSensitivty_centered + IntellectualCuriosity_centered + CreativeImagination_centered + Age_centered + Sex + (1 | Q1), data = merged_data)

model_Agreeablness_facets_Daily_SocAff <- lmer(Daily_SocAff ~ Compassion_centered + Respectfulness_centered + Trust_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_facets_Daily_SocAff <- lmer(Daily_SocAff ~ Organisation_centered + Productiveness_centered + Responsibility_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_facets_Daily_SocAff <- lmer(Daily_SocAff ~ AsetheticSensitivty_centered + IntellectualCuriosity_centered + CreativeImagination_centered + Age_centered + Sex + (1 | Q1), data = merged_data)

model_Openess_facets_CogApp <- lmer(Daily_Supp ~ Compassion_centered + Respectfulness_centered + Trust_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Conscientiousness_facets_Supp <- lmer(Daily_Supp ~ Organisation_centered + Productiveness_centered + Responsibility_centered + Age_centered + Sex + (1 | Q1), data = merged_data)
model_Openess_facets_Supp <- lmer(Daily_Supp ~ AsetheticSensitivty_centered + IntellectualCuriosity_centered + CreativeImagination_centered + Age_centered + Sex + (1 | Q1), data = merged_data)





#Facet-Level 2 Models
summary(model_Neuroticism_facets_Supp)
summary(model_Neuroticism_Supp)
summary(model_Neuroticism_facets_SocAff)

#Trait-Level 2 Models


print(model_Neuroticism_facets_Q7)
print(model_Extraversion_Neuroticism_Q7)

setwd("/Users/robwickett/Desktop/R_Study3")
