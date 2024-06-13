# Calculate likelihood ratio test statistic
LR_test_statistic <- -2 * (logLik(null_model_sitmod) - logLik(model_SitMod_Q6))

# Calculate degrees of freedom difference
df_difference <- attr(logLik(null_model_sitmod), "df") - attr(logLik(model_SitMod_Q6), "df")

# Calculate pseudo R-squared using likelihood ratio test statistic
pseudo_r2_LR <- 1 - (LR_test_statistic / df_difference)

# Print pseudo R-squared
print(pseudo_r2_LR)

install.packages("MuMIn")
library(MuMIn)

install.packages("r2mlm")
library("r2mlm")

# Calculate pseudo R-squared using r.squaredGLMM()
pseudo_r2_SitMod_Q6 <- r.squaredGLMM(model_SitMod_Q6)
pseudo_r2_AttMod_Q6 <- r.squaredGLMM(model_AttMod_Q6)
pseudo_r2_CogApp_Q6 <- r.squaredGLMM(model_CogApp_Q6)
pseudo_r2_Supp_Q6 <- r.squaredGLMM(model_Supp_Q6)
pseudo_r2_SocAff_Q6 <- r.squaredGLMM(model_SocAff_Q6)
pseudo_r2_FIXED_SITMOD <- r.squaredGLMM(Fixedeffects_model_sitmod)
print(pseudo_r2_FIXED_SITMOD)
pseudo_r2_SitMod_Q7 <- r.squaredGLMM(model_SitMod_Q6)
pseudo_r2_AttMod_Q7 <- r.squaredGLMM(model_AttMod_Q6)
pseudo_r2_CogApp_Q7 <- r.squaredGLMM(model_CogApp_Q6)
pseudo_r2_Supp_Q7 <- r.squaredGLMM(model_Supp_Q6)
pseudo_r2_SocAff_Q7 <- r.squaredGLMM(model_SocAff_Q6)
print(pseudo_r2_CogApp_Q6)
pseudo_r2_Neuroticism_Q6 <- r.squaredGLMM(model_Neuroticism_Q6)
pseudo_r2_Extraversion_Q6 <- r.squaredGLMM(model_Extraversion_Q6)
pseudo_r2_Extraversion_SitMod <- r.squaredGLMM(model_Extraversion_SitMod)


filtered_data <- data %>%
  group_by(ID) %>%
  mutate(count_Q2_1 = sum(Q2 == 1)) %>%
  filter(count_Q2_1 >= 7) %>%
  select(-count_Q2_1)


pseudo_model_Extraversion_SitMod <- r.squaredGLMM(model_Extraversion_SitMod)
print(pseudo_model_Extraversion_SitMod)

model <- lmer(Daily_CogApp ~ 1 + Q6_centered + (1 | ID), data = merged_data, na.action = na.exclude, REML = FALSE)
model <- lmer(Daily_CogApp ~ 1 + (1 | ID), data = merged_data, na.action = na.exclude, REML = FALSE)

r2mlm(model)

r2mlm(fullmodel)
performance(model)
model <- lmer(Daily_AttMod ~ 1 + (1 | ID), data = data)
fullmodel <- lmer(Daily_SocAff ~ 1 + Extraversion_centered + Agreeablness_centered + Conscientiousness_centered + Q6_centered + (1 | ID), data = data, na.action = na.exclude)
summary(fullmodel)


summary(fullmodel)

Fixedeffects_model_sitmod <- lmer(Daily_SitMod ~ 1 + Q6_centered + Q7_centered + Extraversion_centered + (1 | ID), data = filtered_data, REML = TRUE)
model_Extraversion_SitMod <- lmer(Daily_SitMod ~ 1 + Extraversion_centered + Q6_centered + (1 | ID), data = filtered_data, na.action = na.exclude, REML = FALSE)
model_Agreeablness_SocAff <- lmer(Daily_SocAff ~ Agreeablness_centered + (1 | ID), data = filtered_data)
model_Extraversion_AttMod <- lmer(Daily_AttMod ~ Extraversion_centered + Age_centered + Sex + (1 | Q1), data = data)
summary(model)
summary(model_SitMod_Q6)


summary(Level1_level2)

r2mlm(Level1_level2)


Level1 <- lmer(Daily_SitMod ~ 1 + Q7_centered + (1 | ID), data = filtered_data, na.action = na.exclude, REML = FALSE)
Level1_level2 <- lmer(Daily_SitMod ~ 1 + Q6_centered + Extraversion_centered + (1 | ID), data = selected_variables, na.action = na.exclude, REML = FALSE)



selected_variables <- data[c("Extraversion_centered", "Q6", "Q6_centered", "Q7", "Daily_SitMod", "ID")]

# Display the extracted variables

install.packages("sjPlot")
library(sjPlot)
# Replace 'model' with the name of your lmer model
table_output1 <- tab_model(Level1)
table_output2 <- tab_model(Level1_level2)
table_output1
table_output2
library(lme4)
library(lmerTest)
r2mlm(Level1)
data <- read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Merged/Study3.sav")


# Test_Level1 Models
SitMod <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
AttMod <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
CogApp <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered +  (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Supp <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
SocAff <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q6 <- lmer(Q6 ~ 1 + Age_centered + Sex + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q7 <- lmer(Q7 ~ 1 + Age_centered + Sex + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)


SitMod_Table <- tab_model(SitMod)
AttMod_Table <- tab_model(AttMod)
CogApp_Table <- tab_model(CogApp)
Supp_Table <- tab_model(Supp)
SocAff_Table <- tab_model(SocAff)
Q6_Table <- tab_model(Q6)
Q7_Table <- tab_model(Q7)

SitMod_Table
AttMod_Table
CogApp_Table
Supp_Table
SocAff_Table
Q6_Table
Q7_Table

summary(SitMod_Table)


# Test_Level1 Models
SitMod <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
AttMod <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
CogApp <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Supp <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
SocAff <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q6 <- lmer(Q6 ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q7 <- lmer(Q7 ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)


SitMod_Table <- tab_model(SitMod)
AttMod_Table <- tab_model(AttMod)
CogApp_Table <- tab_model(CogApp)
Supp_Table <- tab_model(Supp)
SocAff_Table <- tab_model(SocAff)
Q6_Table <- tab_model(Q6)
Q7_Table <- tab_model(Q7)

SitMod_Table
AttMod_Table
CogApp_Table
Supp_Table
SocAff_Table
Q6_Table
Q7_Table



#Extraversion Facets
SitMod_Extraversion_facet <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + EnergyLevel_centered + Assertiveness_centered + Sociability_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
AttMod_Extraversion_facet <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + EnergyLevel_centered + Assertiveness_centered + Sociability_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
CogApp_Extraversion_facet <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + EnergyLevel_centered + Assertiveness_centered + Sociability_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Supp_Extraversion_facet <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + EnergyLevel_centered + Assertiveness_centered + Sociability_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
SocAff_Extraversion_facet <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + EnergyLevel_centered + Assertiveness_centered + Sociability_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q6_facet_Extraversion <- lmer(Q6 ~ 1 + Age_centered + Sex + EnergyLevel_centered + Assertiveness_centered + Sociability_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q7_facet_Extraversion <- lmer(Q7 ~ 1 + Age_centered + Sex + EnergyLevel_centered + Assertiveness_centered + Sociability_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)

SitMod_facet_Table <- tab_model(SitMod_Extraversion_facet)
AttMod_facet_Table <- tab_model(AttMod_Extraversion_facet )
CogApp_facet_Table <- tab_model(CogApp_Extraversion_facet)
Supp_facet_Table <- tab_model(Supp_Extraversion_facet)
SocAff_facet_Table <- tab_model(SocAff_Extraversion_facet)
Q6_facet_Table <- tab_model(Q6_facet_Extraversion)
Q7_facet_Table <- tab_model(Q7_facet_Extraversion)

SitMod_facet_Table
AttMod_facet_Table
CogApp_facet_Table
Supp_facet_Table
SocAff_facet_Table
Q6_facet_Table
Q7_facet_Table



#Neuroticism facets
SitMod_Neuroticism_facet <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + EmotionalVolatility_centered + Depression_centered + Anxiety_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
AttMod_Neuroticism_facet <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + EmotionalVolatility_centered + Depression_centered + Anxiety_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
CogApp_Neuroticism_facet <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + EmotionalVolatility_centered + Depression_centered + Anxiety_centered + + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Supp_Neuroticism_facet <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + EmotionalVolatility_centered + Depression_centered + Anxiety_centered + + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
SocAff_Neuroticism_facet <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + EmotionalVolatility_centered + Depression_centered + Anxiety_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q6_facet_Neuroticism <- lmer(Q6 ~ 1 + Age_centered + Sex + EmotionalVolatility_centered + Depression_centered + Anxiety_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q7_facet_Neuroticism <- lmer(Q7 ~ 1 + Age_centered + Sex + EmotionalVolatility_centered + Depression_centered + Anxiety_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)

SitMod_facet_Table_Neuroticism <- tab_model(SitMod_Neuroticism_facet)
AttMod_facet_Table_Neuroticism <- tab_model(AttMod_Neuroticism_facet )
CogApp_facet_Table_Neuroticism <- tab_model(CogApp_Neuroticism_facet)
Supp_facet_Table_Neuroticism <- tab_model(Supp_Neuroticism_facet)
SocAff_facet_Table_Neuroticism <- tab_model(SocAff_Neuroticism_facet)
Q6_facet_Table_Neuroticism <- tab_model(Q6_facet_Neuroticism)
Q7_facet_Table_Neuroticism <- tab_model(Q7_facet_Neuroticism)

SitMod_facet_Table_Neuroticism 
AttMod_facet_Table_Neuroticism 
CogApp_facet_Table_Neuroticism 
Supp_facet_Table_Neuroticism 
SocAff_facet_Table_Neuroticism 
Q6_facet_Table_Neuroticism 
Q7_facet_Table_Neuroticism 



# Openess Facets
SitMod_Openess_facet <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + AestheticSensitivty_centered + CreativeImagination_centered + IntellectualCuriosity_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
AttMod_Openess_facet <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + AestheticSensitivty_centered + CreativeImagination_centered + IntellectualCuriosity_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
CogApp_Openess_facet <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + AestheticSensitivty_centered + CreativeImagination_centered + IntellectualCuriosity_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Supp_Openess_facet <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + AestheticSensitivty_centered + CreativeImagination_centered + IntellectualCuriosity_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
SocAff_Openess_facet <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + AestheticSensitivty_centered + CreativeImagination_centered + IntellectualCuriosity_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q6_facet_Openess <- lmer(Q6 ~ 1 + Age_centered + Sex + AestheticSensitivty_centered + CreativeImagination_centered + IntellectualCuriosity_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q7_facet_Openess <- lmer(Q7 ~ 1 + Age_centered + Sex + AestheticSensitivty_centered + CreativeImagination_centered + IntellectualCuriosity_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)

SitMod_facet_Table_Openess <- tab_model(SitMod_Openess_facet)
AttMod_facet_Table_Openess <- tab_model(AttMod_Openess_facet)
CogApp_facet_Table_Openess <- tab_model(CogApp_Openess_facet)
Supp_facet_Table_Openess <- tab_model(Supp_Openess_facet)
SocAff_facet_Table_Openess <- tab_model(SocAff_Openess_facet)
Q6_facet_Table_Openess <- tab_model(Q6_facet_Openess)
Q7_facet_Table_Openess <- tab_model(Q7_facet_Openess)

SitMod_facet_Table_Openess
AttMod_facet_Table_Openess 
CogApp_facet_Table_Openess
Supp_facet_Table_Openess
SocAff_facet_Table_Openess
Q6_facet_Table_Openess
Q7_facet_Table_Openess 

# Agreeableness Facets
SitMod_Agreeablness_facet <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex +  Compassion_centered + Trust_centered + Respectfulness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
AttMod_Agreeablness_facet <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Compassion_centered + Trust_centered + Respectfulness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
CogApp_Agreeablness_facet <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + Compassion_centered + Trust_centered + Respectfulness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Supp_Agreeablness_facet <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + Compassion_centered + Trust_centered + Respectfulness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
SocAff_Agreeablness_facet <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + Compassion_centered + Trust_centered + Respectfulness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q6_facet_Agreeablness <- lmer(Q6 ~ 1 + Age_centered + Sex + Compassion_centered + Trust_centered + Respectfulness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q7_facet_Agreeablness <- lmer(Q7 ~ 1 + Age_centered + Sex + Compassion_centered + Trust_centered + Respectfulness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)

SitMod_facet_Table_Agreeablness <- tab_model(SitMod_Agreeablness_facet)
AttMod_facet_Table_Agreeablness <- tab_model(AttMod_Agreeablness_facet )
CogApp_facet_Table_Agreeablness <- tab_model(CogApp_Agreeablness_facet)
Supp_facet_Table_Agreeablness <- tab_model(Supp_Agreeablness_facet)
SocAff_facet_Table_Agreeablness <- tab_model(SocAff_Agreeablness_facet)
Q6_facet_Table_Agreeablness <- tab_model(Q6_facet_Agreeablness)
Q7_facet_Table_Agreeablness <- tab_model(Q7_facet_Agreeablness)

SitMod_facet_Table_Agreeablness 
AttMod_facet_Table_Agreeablness 
CogApp_facet_Table_Agreeablness 
Supp_facet_Table_Agreeablness 
SocAff_facet_Table_Agreeablness 
Q6_facet_Table_Agreeablness 
Q7_facet_Table_Agreeablness 

# Conscientiousness Facets
SitMod_Conscientiousness_facet <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Organisation_centered + Responsibility_centered + Productiveness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
AttMod_Conscientiousness_facet <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Organisation_centered + Responsibility_centered + Productiveness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
CogApp_Conscientiousness_facet <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex +  Organisation_centered + Responsibility_centered + Productiveness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Supp_Conscientiousness_facet <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + Organisation_centered + Responsibility_centered + Productiveness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
SocAff_Conscientiousness_facet <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + Organisation_centered + Responsibility_centered + Productiveness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q6_facet_Conscientiousness <- lmer(Q6 ~ 1 + Age_centered + Sex + Organisation_centered + Responsibility_centered + Productiveness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q7_facet_Conscientiousness <- lmer(Q7 ~ 1 + Age_centered + Sex + Organisation_centered + Responsibility_centered + Productiveness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)

SitMod_facet_Table_Conscientiousness <- tab_model(SitMod_Conscientiousness_facet)
AttMod_facet_Table_Conscientiousness <- tab_model(AttMod_Conscientiousness_facet )
CogApp_facet_Table_Conscientiousness <- tab_model(CogApp_Conscientiousness_facet)
Supp_facet_Table_Conscientiousness <- tab_model(Supp_Conscientiousness_facet)
SocAff_facet_Table_Conscientiousness <- tab_model(SocAff_Conscientiousness_facet)
Q6_facet_Table_Conscientiousness <- tab_model(Q6_facet_Conscientiousness)
Q7_facet_Table_Conscientiousness <- tab_model(Q7_facet_Conscientiousness)

SitMod_facet_Table_Conscientiousness
AttMod_facet_Table_Conscientiousness
CogApp_facet_Table_Conscientiousness 
Supp_facet_Table_Conscientiousness
SocAff_facet_Table_Conscientiousness
Q6_facet_Table_Conscientiousness 
Q7_facet_Table_Conscientiousness 


# Test_Level1 Models exploratory
SitMod_exploratory <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Openess_centered + Agreeablness_centered + Conscientiousness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
AttMod_exploratory <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Openess_centered + Agreeablness_centered + Conscientiousness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
CogApp_exploratory <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + Openess_centered + Agreeablness_centered + Conscientiousness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Supp_exploratory <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + Openess_centered + Agreeablness_centered + Conscientiousness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
SocAff_exploratory <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + Openess_centered + Agreeablness_centered + Conscientiousness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q6_exploratory <- lmer(Q6 ~ 1 + Age_centered + Sex + Openess_centered + Agreeablness_centered + Conscientiousness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
Q7_exploratory <- lmer(Q7 ~ 1 + Age_centered + Sex + Openess_centered + Agreeablness_centered + Conscientiousness_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)


SitMod_Table_exploratory <- tab_model(SitMod_exploratory)
AttMod_Table_exploratory <- tab_model(AttMod_exploratory)
CogApp_Table_exploratory <- tab_model(CogApp_exploratory)
Supp_Table_exploratory <- tab_model(Supp_exploratory)
SocAff_Table_exploratory <- tab_model(SocAff_exploratory)
Q6_Table_exploratory <- tab_model(Q6_exploratory)
Q7_Table_exploratory <- tab_model(Q7_exploratory)

SitMod_Table_exploratory
AttMod_Table_exploratory
CogApp_Table_exploratory
Supp_Table_exploratory
SocAff_Table_exploratory
Q6_Table_exploratory
Q7_Table_exploratory