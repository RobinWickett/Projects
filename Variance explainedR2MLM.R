
setwd("/Users/robwickett/Desktop/PhD/Study 3/Data/Merged")
install.packages("haven")
library(haven)
data <- read_sav("Study3.sav")

library(r2mlm)
SitMod <- lmer(Daily_SitMod ~ 1 + Conscietiousness_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
r2mlm(SitMod)
summary(SitMod)
SitMod_withExtraversion <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + Extraversion_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
r2mlm(SitMod_withExtraversion)

CogApp <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + Extraversion_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
r2mlm(CogApp)

CogApp_withExtraversion <- lmer(Daily_SitMod ~ 1 + Q6_centered + Q7_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
r2mlm(CogApp_withExtraversion)
AttMod <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
r2mlm(AttMod)

AttMod_withExtraversion <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + Extraversion_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
r2mlm(AttMod_withExtraversion)


# With Neuroticism as well
SocAff_withNeuroticism <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Q6_centered + Q7_centered + Agreeablness_centered + Extraversion_centered + Neuroticism_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
r2mlm(SocAff_withNeuroticism)

CogApp_withNeuroticism <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + Q7_centered + Q6_centered + Neuroticism_centered + Extraversion_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)
r2mlm(CogApp_withNeuroticism)


Null <- lmer(Daily_SitMod ~ 1 + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)

SitMod <- lmer(Daily_SitMod ~ 1 + Age_centered + Sex + Neuroticism_centered + (1 | ID), data = merged_data, na.action = na.exclude, REML = TRUE)
AttMod <- lmer(Daily_AttMod ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = merged_data, na.action = na.exclude, REML = TRUE)
CogApp <- lmer(Daily_CogApp ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = merged_data, na.action = na.exclude, REML = TRUE)
Supp <- lmer(Daily_Supp ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = merged_data, na.action = na.exclude, REML = TRUE)
SocAff <- lmer(Daily_SocAff ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), merged_data = data, na.action = na.exclude, REML = TRUE)
Q6 <- lmer(Q6 ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = merged_data, na.action = na.exclude, REML = TRUE)
Q7 <- lmer(Q7 ~ 1 + Age_centered + Sex + Extraversion_centered + Neuroticism_centered + (1 | ID), data = merged_data, na.action = na.exclude, REML = TRUE)

Fixedeffects_model_sitmod <- lmer(Daily_SitMod ~ 1 + Q6_centered + Q7_centered + (1 | ID), data = data, REML = TRUE)
Supp <- lmer(Daily_CogApp ~ 1 + Q6 + Sex + Q7_centered + Q6_centered + Neuroticism_centered + Extraversion_centered + (1 | ID), data = data, na.action = na.exclude, REML = TRUE)

Model <- lmer(Daily_SitMod ~ Q6_centered + Extraversion_centered +  (1 + | ID), data = merged_data, na.action = na.exclude, REML = TRUE)

summary(Model)
r2mlm(Model)
r2mlm(SitMod)
r2mlm(AttMod)
r2mlm(CogApp)
r2mlm(Supp)
r2mlm(SocAff)
r2mlm(Q6)
r2mlm(Q7)
