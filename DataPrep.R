install.packages("haven")  # Run this line only once to install the package
library(haven)
# Replace "file_path/file_name.sav" with the actual file path and file name of your SPSS data file
data <- read_sav("/Users/robwickett/Desktop/R_Study3/Removedduplicates/removed_variables.sav")

# Replace "file_path/file_name.sav" with the actual file path and file name of your SPSS data file
Baseline <- read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Clean/Baseline_July 24, 2023_15.04.sav")

# Get the variable names in the data frame
variable_names <- names(Diary)

# Print the variable names
print(variable_names)


# Count the number of unique participants identified by Q1
unique_participants <- RemovedVariables %>%
  distinct(Q1) %>%
  nrow()

# Print the result
print(unique_participants)

datafile <- read.csv("/Users/robwickett/Desktop/R_Study3/centered.csv")




data <- read.csv("your_data_file.csv")

# Load the necessary package
library(dplyr)

# Assuming your data frame is named 'data'
# Replace 'data' with the actual name of your data frame

# Calculate the frequency of "1" responses in Q2 for each participant (Q1)
response_counts <- filtered_data %>%
  group_by(Q1) %>%
  summarise(count_1 = sum(Q2 == 1, na.rm = TRUE))

# Filter out participant IDs with count_1 greater than or equal to 2
selected_participant_ids <- response_counts %>%
  filter(count_1 >= 2) %>%
  pull(Q1)

# Filter the original data frame based on selected participant IDs
filtered_data <- Diary %>%
  filter(Q1 %in% selected_participant_ids)

# Print the filtered data frame
print(filtered_data)

# Assuming you have two data frames named 'dataset1' and 'dataset2'
# Replace 'dataset1' and 'dataset2' with the actual names of your data frames

# Merge the data frames based on the 'Q1' column
merged_data <- merge(filtered_data, Baseline, by = "Q1")

# Print the merged data frame
print(merged_data)


# Export the filtered data as an SPSS file
write_sav(data, "/Users/robwickett/Desktop/R_Study3/joined_data.sav")


# Count the number of individuals who answered 1 to Q2 less than two times
individuals_less_than_two_times <- sum(merged_data$Q1 == 1 & sum(merged_data$Q2 == 1) < 2)

# Summary statistics for Sex
summary_sex <- table(merged_data$Sex)

# Summary statistics for Age
summary_age <- summary(merged_data$Age)

# Print the results
print("Summary statistics for Sex:")
print(summary_sex)

print("Summary statistics for Age:")
print(summary_age)

# Remove duplicates to keep only one unique ID for each participant (identified by Q1)
unique_data <- merged_data %>%
  distinct(Q1, .keep_all = TRUE)

# Print the results
print(unique_data)

# Install and load the dplyr package for data manipulation
install.packages("dplyr")
library(dplyr)

# Summary statistics for Sex
summary_sex <- unique_data %>%
  count(Sex)

# Summary statistics for Country
summary_country <- unique_data %>%
  count(Country)

# Summary statistics for Age
summary_age <- unique_data %>%
  summarise(Mean_Age = mean(Age),
            Min_Age = min(Age),
            Max_Age = max(Age))

# Summary statistics for Occupation
summary_occupation <- unique_data %>%
  count(Occupation)

# Summary statistics for Employment
summary_employment <- unique_data %>%
  count(Employment)

# Summary statistics for Managerial
summary_managerial <- unique_data %>%
  count(Managerial)

# Summary statistics for Years_Work_E
summary_years_work_ex <- unique_data %>%
  summarise(Mean_Years_Work_Ex = mean(Years_Work_Ex),
            Min_Years_Work_Ex = min(Years_Work_Ex),
            Max_Years_Work_Ex = max(Years_Work_Ex))

# Print the results
print("Summary statistics for Sex:")
print(summary_sex)

print("Summary statistics for Country:")
print(summary_country)

print("Summary statistics for Age:")
print(summary_age)

print("Summary statistics for Occupation:")
print(summary_occupation)

print("Summary statistics for Employment:")
print(summary_employment)

print("Summary statistics for Managerial:")
print(summary_managerial)

print("Summary statistics for Years_Work_Ex:")
print(summary_years_work_ex)
The above code calculates the count of each unique value for "Sex," "Country," "Occupation," and "Employment" using the count() function and calculates the mean, minimum, and maximum for "Age" and "Years_Work_E" using the summarise() function.

Replace "my_data" with the name of your actual data frame to apply this operation to your data. Running this code will provide you with the summary statistics for the mentioned variables in your data.


# Calculate the standard deviation for Age
age_sd <- sd(unique_data$Age)

# Print the result
print(age_sd)

# Calculate the standard deviation for Age
Workxp_sd <- sd(unique_data$Years_Work_Ex)

# Print the result
print(Workxp_sd)


# Calculate the percentage of each occupation in relation to the total count
occupation_percentages <- unique_data %>%
  group_by(Occupation) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Print the result
print(occupation_percentages)

# Calculate the percentage of each category in relation to the total count
managerial_percentages <- unique_data %>%
  group_by(Managerial) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Print the result
print(managerial_percentages)


write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)
write.csv(my_data, file = "withID_data.csv", row.names = FALSE)

my_data <- replace(my_data, my_data == -999, NA)

# Print the updated data frame
print(my_data)

# Create a unique numeric ID for each unique value in Q1
unique_ids <- data.frame(Q1 = unique(merged_data$Q1), ID = 1:length(unique(merged_data$Q1)))

# Merge the unique IDs back to the original data
my_data <- merge(merged_data, unique_ids, by = "Q1", all.x = TRUE)

# Print the result
print(my_data)

# Count the number of unique values for ID
unique_id_count <- my_data %>%
  summarise(Unique_IDs = n_distinct(ID))

# Print the result
print(unique_id_count)

# Use the aggregate function to calculate the sum of each daily variable for each participant
aggregated_data <- aggregate(cbind(Daily_SocAff, Daily_CogApp, Daily_Supp, Daily_AttMod, Daily_SitMod) ~ Q1, data = my_data, sum)

# Print the aggregated data
print(aggregated_data)

# Assuming you have an aggregated data frame named "aggregated_data" with columns "Q1", "Daily_SocAff", "Daily_CogApp", "Daily_Supp", "Daily_AttMod", and "Daily_SitMod"

# Rename the columns in the aggregated data frame
colnames(aggregated_data) <- c("Q1", "Total_Daily_SocAff", "Total_Daily_CogApp", "Total_Daily_Supp", "Total_Daily_AttMod", "Total_Daily_SitMod")

# Print the updated aggregated data
print(aggregated_data)

# Assuming you have a data frame named "diary_data" with columns "Q1", "Q6", and "Q7"

# Use the aggregate function to calculate the sum of "Q6" and "Q7" for each participant
aggregated_data2 <- aggregate(cbind(Q6, Q7) ~ Q1, data = my_data, sum)

# Print the aggregated data
print(aggregated_data)

# Rename the columns in the aggregated data frame
colnames(aggregated_data2)[2:3] <- c("Total_Q6", "Total_Q7")

# Print the updated aggregated data
print(aggregated_data)

joined_data <- merge(aggregated_data, aggregated_data2, by = "Q1")

merged_data2 <- merge(my_data, joined_data, by = "Q1", all.x = TRUE)

write.csv(merged_data_with_means, "merged_data2.csv", row.names = FALSE)

# Assuming you have a data frame named "joined_data"











# Load the haven package
library(haven)

# Export the data frame as an SPSS file
write_sav(means_data, "joined_data.sav")

# Calculate the means for each participant
means_data <- data %>%
  group_by(Q1) %>%
  summarize(
    Mean_Daily_SocAff = mean(Daily_SocAff, na.rm = TRUE),
    Mean_Daily_CogApp = mean(Daily_CogApp, na.rm = TRUE),
    Mean_Daily_Supp = mean(Daily_Supp, na.rm = TRUE),
    Mean_Daily_AttMod = mean(Daily_AttMod, na.rm = TRUE),
    Mean_Daily_SitMod = mean(Daily_SitMod, na.rm = TRUE),
    Mean_Q6 = mean(Q6, na.rm = TRUE),
    Mean_Q7 = mean(Q7, na.rm = TRUE)
  )

# Assuming you have two data frames: "merged_data2" and "means_data" with columns "Q1" and the mean variables.

# Perform a merge based on the "Q1" column
data1 <- merge(data, means_data, by = "Q1", all.x = TRUE)

means_data <- means_data %>%
  select(-Mean_Daily_SocAff, -Mean_Daily_CogApp, -Mean_Q6, -Mean_Daily_AttMod, -Mean_Daily_SitMod, -Mean_Daily_Supp)

# Print the updated merged data
print(merged_data_with_means)

# Load the dplyr package
library(dplyr)

# Remove the specified variables
merged_data_with_means <- merged_data_with_means %>%
  select(-Total_Daily_SocAff, -Total_Daily_CogApp, -Total_Daily_Supp, 
         -Total_Daily_AttMod, -Total_Daily_SitMod, -Total_Q6, -Total_Q7)

# Print the updated merged data
print(merged_data_with_means)

write.csv(data, "merged_data2.csv", row.names = FALSE)

# Load the lme4 package
install.packages("lme4")  # Run this line if you haven't installed the package
library(lme4)




# Calculate the grand mean of level 2 variables
grand_mean_neuroticism <- mean(RemovedVariables$Neuroticism)
grand_mean_extraversion <- mean(RemovedVariables$Extrave)
grand_mean_openess <- mean(RemovedVariables$Openess)
grand_mean_agreeableness <- mean(RemovedVariables$Agreeablness)
grand_mean_conscientiousness <- mean(RemovedVariables$Conscientiouness)

grand_mean_energylevel <- mean(RemovedVariables$EnergyLevel)
grand_mean_assertiveness <- mean(RemovedVariables$Assertivness)
grand_mean_sociability <- mean(RemovedVariables$Sociability)

grand_mean_compassion <- mean(RemovedVariables$Compassion)
grand_mean_respectfulness <- mean(RemovedVariables$Respectfulness)
grand_mean_trust <- mean(RemovedVariables$Trust)

grand_mean_organisation <- mean(RemovedVariables$Organisation)
grand_mean_productiveness <- mean(RemovedVariables$Productiveness)
grand_mean_responsibility <- mean(RemovedVariables$Responsibility)

grand_mean_anxiety <- mean(RemovedVariables$Anxiety)
grand_mean_depression <- mean(RemovedVariables$Depression)
grand_mean_emotionalvolatility <- mean(RemovedVariables$EmotionalVolatility)

grand_mean_intellectualcuriosity <- mean(RemovedVariables$IntellectualCuriosity)
grand_mean_aestheticsensitivity <- mean(RemovedVariables$AestheticSensitivty)
grand_mean_creativeimagination <- mean(RemovedVariables$CreativeImagination)

grand_mean_Age <- mean(data$Age)

# Grand mean center the variables
RemovedVariables$Neuroticism_centered <- RemovedVariables$Neuroticism - grand_mean_neuroticism
RemovedVariables$Extraversion_centered <- RemovedVariables$Extrave - grand_mean_extraversion
RemovedVariables$Openess_centered <- RemovedVariables$Openess - grand_mean_openess
RemovedVariables$Agreeablness_centered <- RemovedVariables$Agreeablness - grand_mean_agreeableness
RemovedVariables$Conscientiousness_centered <- RemovedVariables$Conscientiouness - grand_mean_conscientiousness

RemovedVariables$EnergyLevel_centered <- RemovedVariables$EnergyLevel - grand_mean_energylevel
RemovedVariables$Assertiveness_centered <- RemovedVariables$Assertivness - grand_mean_assertiveness
RemovedVariables$Sociability_centered <- RemovedVariables$Sociability - grand_mean_sociability

RemovedVariables$Compassion_centered <- RemovedVariables$Compassion - grand_mean_compassion
RemovedVariables$Respectfulness_centered <- RemovedVariables$Respectfulness - grand_mean_respectfulness
RemovedVariables$Trust_centered <- RemovedVariables$Trust - grand_mean_trust

RemovedVariables$Organisation_centered <- RemovedVariables$Organisation - grand_mean_organisation
RemovedVariables$Productiveness_centered <- RemovedVariables$Productiveness - grand_mean_productiveness
RemovedVariables$Responsibility_centered <- RemovedVariables$Responsibility - grand_mean_responsibility

RemovedVariables$Anxiety_centered <- RemovedVariables$Anxiety - grand_mean_anxiety
RemovedVariables$Depression_centered <- RemovedVariables$Depression - grand_mean_depression
RemovedVariables$EmotionalVolatility_centered <- RemovedVariables$EmotionalVolatility - grand_mean_emotionalvolatility

RemovedVariables$IntellectualCuriosity_centered <- RemovedVariables$IntellectualCuriosity - grand_mean_intellectualcuriosity
RemovedVariables$AsetheticSensitivty_centered <- RemovedVariables$AestheticSensitivty - grand_mean_aestheticsensitivity
RemovedVariables$CreativeImagination_centered <- RemovedVariables$CreativeImagination - grand_mean_creativeimagination

data$Age_centered <- data$Age - grand_mean_Age

# Check the updated dataset with centered variables
head(data)

data <- read.csv("/Users/robwickett/Desktop/R_Study3/merged_data2.csv")

# Install and load the lme4 package
install.packages("lme4")
library(lme4)

# Fit the mixed effects model
model <- lmer(Daily_CogApp ~ Extraversion_centered + Conscientiousness_centered + (1 | ID), data = data)

# View the model summary
summary(model)

# Person Level Centering
RemovedVariables <- RemovedVariables %>%
  group_by(ID) %>%
  mutate(Q6_centered = Q6 - mean(Q6, na.rm = TRUE))

RemovedVariables <- RemovedVariables %>%
  group_by(ID) %>%
  mutate(Q7_centered = Q7 - mean(Q7, na.rm = TRUE))

RemovedVariables <- RemovedVariables %>%
  group_by(ID) %>%
  mutate(DailyCogApp_centered = Daily_CogApp - mean(Daily_CogApp, na.rm = TRUE))

RemovedVariables <- RemovedVariables %>%
  group_by(ID) %>%
  mutate(DailySupp_centered = Daily_Supp - mean(Daily_Supp, na.rm = TRUE))

RemovedVariables <- RemovedVariables %>%
  group_by(ID) %>%
  mutate(DailySocAff_centered = Daily_SocAff - mean(Daily_SocAff, na.rm = TRUE))

RemovedVariables <- RemovedVariables %>%
  group_by(ID) %>%
  mutate(DailySitMod_centered = Daily_SitMod - mean(Daily_SitMod, na.rm = TRUE))

RemovedVariables <- RemovedVariables %>%
  group_by(ID) %>%
  mutate(DailyAttMod_centered = Daily_AttMod - mean(Daily_AttMod, na.rm = TRUE))

write.csv(data, file = "centered.csv", row.names = FALSE)

# Get the current working directory
current_directory <- getwd()

# Print the current working directory
print(current_directory)

# Set the working directory
setwd("/Users/robwickett/Desktop/R_Study3")

library(lme4)

unique_individuals <- length(unique(data$Q1))
print(unique_individuals)

# Fit the level 1 model
level1_model <- lmer(Daily_SitMod ~ Q6_centered + (1 | ID), data = my_data)

# Print the summary of the model
summary(level1_model)

# Calculate the frequency of "1" responses in Q2 for each participant (Q1)
response_counts <- data %>%
  group_by(Q1) %>%
  summarise(count_1 = sum(Q2 == 1, na.rm = TRUE))


library(dplyr)

# Assuming your dataset is named 'your_data' and the columns are named 'ID' for participant ID and 'Q4' for Quarter 4 responses


q4_frequencies <- table(data$Q4)

# Display the frequencies
print(q4_frequencies)

q4_frequencies <- table(data$Q4)
total_obs <- length(data$Q4)

q4_percentages <- (q4_frequencies / 3028) * 100

# Display the percentages
print(q4_percentages)


q5_frequencies <- table(data$Q5)

# Display the frequencies
print(q5_frequencies)




q5_percentages <- (q5_frequencies / 3028) * 100

# Display the percentages
print(q5_percentages)
q5_mean <- mean(data$Q5, na.rm = TRUE)

# Display the mean
print(q5_mean)







# Assuming your data is named spss_data
variables_to_remove <- c("Mean_daily_SocAff", "Mean_daily_CogApp", "Mean_daily_Supp", "Mean_daily_AttMod",
                         "Mean_daily_SitMod", "Mean_Q6", "Mean_Q7", "Neuroticism_centered", "Extraversion_centered",
                         "Openess_centered", "Agreeablness_centered", "Conscientiousness_centered", "EnergyLevel_centered",
                         "Assertiveness_centered", "Sociability_centered", "Compassion_centered", "Respectfulness_centered",
                         "Trust_centered", "Organisation_centered", "Productiveness_centered", "Responsibility_centered",
                         "Anxiety_centered", "Depression_centered", "EmotionalVolatility_centered", "IntellectualCuriosity_centered",
                         "AsetheticSensitivity_centered", "CreativeImagination_centered", "Q6_centered", "Q7_centered",
                         "DailyCogApp_centered", "DailySupp_centered", "DailySocAff_centered", "DailySitMod_centered",
                         "DailyAttMod_centered")

RemovedVariables <- data[, !(names(data) %in% variables_to_remove)]

# List of variables to be removed
variables_to_remove <- c("Mean_Daily_SocAff", "Mean_Daily_CogApp", "Mean_Daily_Supp",
                         "Mean_Daily_AttMod", "Mean_Daily_SitMod", "AsetheticSensitivty_centered")

# Remove the specified variables from the dataset
RemovedVariables <- RemovedVariables[, !(names(RemovedVariables) %in% variables_to_remove)]



additional_variables <- setdiff(names(data), names(RemovedVariables))

print(additional_variables)

# Assuming your data frames are named RemovedVariables and means_data
merged_data <- merge(RemovedVariables, means_data[, c("Q1", "Mean_Q6", "Mean_Q7")], by = "Q1", all.x = TRUE)

# Rename the columns to match the original variable names
colnames(merged_data)[colnames(merged_data) == "Mean_Q6.x"] <- "Mean_Q6"
colnames(merged_data)[colnames(merged_data) == "Mean_Q7.x"] <- "Mean_Q7"

# Remove duplicate columns (if any)
merged_data <- merged_data[, !duplicated(names(merged_data))]

# Assign the merged data back to RemovedVariables
RemovedVariables <- merged_data


# Specify the file path where you want to save the SPSS file
output_file_path <- "/Users/robwickett/Desktop/R_Study3/Removedduplicates/removed_variables.sav"

# Save the data frame as an SPSS file
write_sav(RemovedVariables, output_file_path)

# Assuming your data frame is named datafile and you want to save it as "output_file.csv"
output_file <- "output_file.csv"

# Save the data frame as a CSV file
write.csv(datafile, file = output_file, row.names = FALSE)


library("EMAtools")


data$DailySitMod_C<-pcenter(data$ID,data$Daily_SitMod)
data$DailyAttMod_C<-pcenter(data$ID,data$Daily_AttMod)
data$DailyCogApp_C<-pcenter(data$ID,data$Daily_CogApp)
data$DailySupp_C<-pcenter(data$ID,data$Daily_Supp)
data$DailySocAff_C<-pcenter(data$ID,data$Daily_SocAff)
data$DailyQ6_C<-pcenter(data$ID,data$Q6)
data$DailyQ7_C<-pcenter(data$ID,data$Q7)

data$Extraversion_c<-gcenter(data$Extrave)
data$Neuroticism_c<-gcenter(data$Neuroticism)
data$Openess_c<-gcenter(data$Openess)
data$Conscientiousness_c<-gcenter(data$Conscientiouness)
data$Agreeablness_c<-gcenter(data$Agreeablness)

data$Assertiveness_c<-gcenter(data$Assertivness)
data$Sociability_c<-gcenter(data$Sociability)
data$EnergyLevel_c<-gcenter(data$EnergyLevel)

data$Trust_c<-gcenter(data$Trust)
data$Compassion_c<-gcenter(data$Compassion)
data$Respectfulness_c<-gcenter(data$Respectfulness)

data$AestheticSensitivty_c<-gcenter(data$AestheticSensitivty)
data$IntellectualCuriosity_c<-gcenter(data$IntellectualCuriosity)
data$CreativeImagination_c<-gcenter(data$CreativeImagination)

data$Organisation_c<-gcenter(data$Organisation)
data$Productiveness_c<-gcenter(data$Productiveness)
data$Responsibility_c<-gcenter(data$Responsibility)

data$EmotionalVolatility_c<-gcenter(data$EmotionalVolatility)
data$Anxiety_c<-gcenter(data$Anxiety)
data$Depression_c<-gcenter(data$Depression)

data$Age_c<-gcenter(data$Age)

# Load the foreign package
library(haven)

# Replace 'your_data_frame' with the actual name of your dataframe
write_sav(data, "/Users/robwickett/Desktop/PhD/Study 3/Data/Merged/Study3.sav")






