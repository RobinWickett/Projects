>  library(performance) # for ICC
> library(lmerTest) # for p-values
> 
  
null_model_sitmod <- lmer(Daily_SitMod ~ 1 + (1 | ID), data = data)
null_model_attmod <- lmer(Daily_AttMod ~ 1 + (1 | ID), data = data)
null_model_Supp <- lmer(Daily_Supp ~ 1 + (1 | ID), data = data)
null_model_SocAff <- lmer(Daily_SocAff ~ 1 + (1 | ID), data = data)
null_model_CogApp <- lmer(Daily_CogApp ~ 1 + (1 | ID), data = data)
null_model_Q6 <- lmer(Q6 ~ 1 + (1 | ID), data = data)
null_model_Q7 <- lmer(Q7 ~ 1 + (1 | ID), data = data)
  
  
  performance::icc(null_model_sitmod)
  performance::icc(null_model_cogapp)
  
  table_sitmod <- tab_model(null_model_sitmod)
  table_attmod <- tab_model(null_model_attmod)
  table_Supp <- tab_model(null_model_Supp)
  table_cogapp <- tab_model(null_model_CogApp)
  table_Q6 <- tab_model(null_model_Q6)
  table_Q7 <- tab_model(null_model_Q7)
  table_socaff <- tab_model(null_model_SocAff)
  
  
  table_socaff
  table_Supp
  table_Q7
  table_Q6
  table_cogapp
  table_attmod
  table_sitmod
  print(model_Extraversion_Neuroticism_Q6)
  print(model_Extraversion_Neuroticism_Q7)

  
  
  

 
  