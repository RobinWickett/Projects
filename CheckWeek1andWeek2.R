library(dplyr)

library(dplyr)

mean_responsesSitModWeek1 <- data %>%
  filter(Time %in% 1:10) %>%
  group_by(ID) %>%
  summarise(mean_responsesSitModWeek1 = mean(Daily_SitMod, na.rm = TRUE))

mean_responsesCogAppWeek1 <- data %>%
  filter(Time %in% 1:10) %>%
  group_by(ID) %>%
  summarise(mean_responsesCogAppWeek1 = mean(Daily_CogApp, na.rm = TRUE))

mean_responsesSuppWeek1 <- data %>%
  filter(Time %in% 1:10) %>%
  group_by(ID) %>%
  summarise(mean_responsesSuppWeek1 = mean(Daily_Supp, na.rm = TRUE))

mean_responsesSocAffWeek1 <- data %>%
  filter(Time %in% 1:10) %>%
  group_by(ID) %>%
  summarise(mean_responsesSocAffWeek1 = mean(Daily_SocAff, na.rm = TRUE))

mean_responsesAttModWeek1 <- data %>%
  filter(Time %in% 1:10) %>%
  group_by(ID) %>%
  summarise(mean_responsesAttModWeek1 = mean(Daily_AttMod, na.rm = TRUE))


mean_responsesSitModWeek2 <- data %>%
  filter(Time %in% 11:20) %>%
  group_by(ID) %>%
  summarise(mean_responsesSitModWeek2 = mean(Daily_SitMod, na.rm = TRUE))

mean_responsesCogAppWeek2 <- data %>%
  filter(Time %in% 11:20) %>%
  group_by(ID) %>%
  summarise(mean_responsesCogAppWeek2 = mean(Daily_CogApp, na.rm = TRUE))

mean_responsesSuppWeek2 <- data %>%
  filter(Time %in% 11:20) %>%
  group_by(ID) %>%
  summarise(mean_responsesSuppWeek2 = mean(Daily_Supp, na.rm = TRUE))

mean_responsesSocAffWeek2 <- data %>%
  filter(Time %in% 11:20) %>%
  group_by(ID) %>%
  summarise(mean_responsesSocAffWeek2 = mean(Daily_SocAff, na.rm = TRUE))

mean_responsesAttModWeek2 <- data %>%
  filter(Time %in% 11:20) %>%
  group_by(ID) %>%
  summarise(mean_responsesAttModWeek2 = mean(Daily_AttMod, na.rm = TRUE))

# Joining the mean response variables based on Q1
joined_data <- mean_responsesAttModWeek2 %>%
  inner_join(mean_responsesSocAffWeek2, by = "ID") %>%
  inner_join(mean_responsesSuppWeek2, by = "ID") %>%
  inner_join(mean_responsesCogAppWeek2, by = "ID") %>%
  inner_join(mean_responsesSitModWeek2, by = "ID") %>%
  inner_join(mean_responsesAttModWeek1, by = "ID") %>%
  inner_join(mean_responsesSocAffWeek1, by = "ID") %>%
  inner_join(mean_responsesSuppWeek1, by = "ID") %>%
  inner_join(mean_responsesCogAppWeek1, by = "ID") %>%
  inner_join(mean_responsesSitModWeek1, by = "ID")

result_ttest_sup <- t.test(mean_responsesSuppWeek1, mean_responsesSuppWeek2)
result_ttest_attmod <- t.test(mean_responsesAttModWeek1, mean_responsesAttModWeek2)
result_ttest_sitmod <- t.test(mean_responsesSitModWeek1, mean_responsesSitModWeek2)
result_ttest_cogapp <- t.test(mean_responsesCogAppWeek1, mean_responsesCogAppWeek2)
result_ttest_socaff <- t.test(mean_responsesSocAffWeek1, mean_responsesSocAffWeek2)

# Print the result
print(result_ttest_sup)
print(result_ttest_attmod)
print(result_ttest_sitmod)
print(result_ttest_cogapp)
print(result_ttest_socaff)

unique_count <- data %>%
  summarise(unique_Q1 = n_distinct(ID))

# Print the unique count
print(unique_count)

# Group data by Q1 and count unique ID values within each group
inconsistent_q1 <- data %>%
  group_by(Q1) %>%
  summarise(unique_id_count = n_distinct(ID))

# Filter out the groups where unique_id_count is greater than 1
inconsistent_q1 <- inconsistent_q1 %>%
  filter(unique_id_count > 1)

# Print the inconsistent_q1 data frame
print(inconsistent_q1)


customerfacing <- haven::read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Raw/Baseline_July 24, 2023_15.04.sav")
baseline <- haven::read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Merged/Baseline.sav")

selected_columns <- baseline[, c("Q1")]

# Update the 'spss_data' dataframe with only the selected columns
baseline <- selected_columns

merged_data <- merge(baseline, customerfacing, by = "Q1", all.x = TRUE)
duplicates <- merged_data[duplicated(merged_data$Q1), ]

merged_data <- merged_data[-73, ]

# Use table() to count the occurrences of responses in Q9
response_counts <- table(merged_data$Q9)

# Extract the counts for "1" and "2" (assuming these are the response categories)
response_1_count <- response_counts["1"]
response_2_count <- response_counts["2"]

# Calculate the total number of participants
total_participants <- nrow(merged_data)

# Calculate the percentages
percentage_response_1 <- (response_1_count / total_participants) * 100
percentage_response_2 <- (response_2_count / total_participants) * 100

# Print the percentages
cat("Percentage of participants who responded '1' for Q9:", percentage_response_1, "%\n")
cat("Percentage of participants who responded '2' for Q9:", percentage_response_2, "%\n")
