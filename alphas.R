install.packages("haven")
library(haven)

updated_baseline <- read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Cleaned_withrawitems/Baseline_withitems_15.04.sav")

# Load the dplyr package
library(dplyr)

# Merging data
merged_data <- inner_join(data, spss_data, by = "Q1")

# Load the dplyr package
library(dplyr)

# Keep only the specified variables
data_filtered <- merged_data %>%
  select(Q1, Q13_1, Q13_2, Q13_3, Q13_4, Q13_5, Q13_6, Q13_7, Q13_8, Q13_9, Q13_10, Q13_11, Q13_12, Q13_13, Q13_14, Q13_15, Q13_16, Q13_17, Q13_18, Q13_19, Q13_20, 
         Q14_1, Q14_2, Q14_3, Q14_4, Q14_5, Q14_6, Q14_7, Q14_8, Q14_9, Q14_10, Q14_11, Q14_12, Q14_13, Q14_14, Q14_15, Q14_16, Q14_17, Q14_18, Q14_19, Q14_20, 
         Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6, Q15_7, Q15_8, Q15_9, Q15_10, Q15_11, Q15_12, Q15_13, Q15_14, Q15_15, Q15_16, Q15_17, Q15_18, Q15_19, Q15_20)

# Load the dplyr package
library(dplyr)

# Group by participant (Q1) and summarize the data
data_aggregated <- data %>%
  group_by(Q1) %>%
  summarize(
    Q13_1 = first(Q13_1),
    Q13_2 = first(Q13_2),
    Q13_3 = first(Q13_3),
    Q13_4 = first(Q13_4),
    Q13_5 = first(Q13_5),
    # Load the dplyr package
library(dplyr)

# Group by participant (Q1) and summarize the data
data_aggregated <- data_filtered %>%
  group_by(Q1) %>%
  summarize(
    Q13_1 = first(Q13_1),
    Q13_2 = first(Q13_2),
    Q13_3 = first(Q13_3),
    Q13_4 = first(Q13_4),
    Q13_5 = first(Q13_5),
    Q13_6 = first(Q13_6),
    Q13_7 = first(Q13_7),
    Q13_8 = first(Q13_8),
    Q13_9 = first(Q13_9),
    Q13_10 = first(Q13_10),
    Q13_11 = first(Q13_11),
    Q13_12 = first(Q13_12),
    Q13_13 = first(Q13_13),
    Q13_14 = first(Q13_14),
    Q13_15 = first(Q13_15),
    Q13_16 = first(Q13_16),
    Q13_17 = first(Q13_17),
    Q13_18 = first(Q13_18),
    Q13_19 = first(Q13_19),
    Q13_20 = first(Q13_20),
   
    Q14_1 = first(Q14_1),
    Q14_2 = first(Q14_2),
    Q14_3 = first(Q14_3),
    Q14_4 = first(Q14_4),
    Q14_5 = first(Q14_5),
    Q14_6 = first(Q14_6),
    Q14_7 = first(Q14_7),
    Q14_8 = first(Q14_8),
    Q14_9 = first(Q14_9),
    Q14_10 = first(Q14_10),
    Q14_11 = first(Q14_11),
    Q14_12 = first(Q14_12),
    Q14_13 = first(Q14_13),
    Q14_14 = first(Q14_14),
    Q14_15 = first(Q14_15),
    Q14_16 = first(Q14_16),
    Q14_17 = first(Q14_17),
    Q14_18 = first(Q14_18),
    Q14_19 = first(Q14_19),
    Q14_20 = first(Q14_20),
    
    Q15_1 = first(Q15_1),
    Q15_2 = first(Q15_2),
    Q15_3 = first(Q15_3),
    Q15_4 = first(Q15_4),
    Q15_5 = first(Q15_5),
    Q15_6 = first(Q15_6),
    Q15_7 = first(Q15_7),
    Q15_8 = first(Q15_8),
    Q15_9 = first(Q15_9),
    Q15_10 = first(Q15_10),
    Q15_11 = first(Q15_11),
    Q15_12 = first(Q15_12),
    Q15_13 = first(Q15_13),
    Q15_14 = first(Q15_14),
    Q15_15 = first(Q15_15),
    Q15_16 = first(Q15_16),
    Q15_17 = first(Q15_17),
    Q15_18 = first(Q15_18),
    Q15_19 = first(Q15_19),
    Q15_20 = first(Q15_20)
  )



install.packages("psych")
library(psych)

# Assuming you have items Q13_1 to Q13_20 in your scale
extraversion_items <- new_data[, c("Q13_1", "Q13_6", 
                  "Q13_11","Q13_16", "Q14_1", "Q14_6", "Q14_11", "Q14_16", "Q15_1", "Q15_6", "Q15_11", "Q15_16")]

agreeablness_items <- updated_baseline[, c("Q13_2", "Q13_7", 
                                          "Q13_12","Q13_17", "Q14_2", "Q14_7", "Q14_12", "Q14_17", "Q15_2", "Q15_7", "Q15_12", "Q15_17")]

openess_items <- updated_baseline[, c("Q13_5", "Q13_10", 
                                          "Q13_15","Q13_20", "Q14_5", "Q14_10", "Q14_15", "Q14_20", "Q15_5", "Q15_10", "Q15_15", "Q15_20")]

neuroticism_items <- updated_baseline[, c("Q13_4", "Q13_9", 
                                          "Q13_14","Q13_19", "Q14_4", "Q14_9", "Q14_14", "Q14_19", "Q15_4", "Q15_9", "Q15_14", "Q15_19")]

conscientiousness_items <- updated_baseline[, c("Q13_3", "Q13_8", 
                                          "Q13_13","Q13_18", "Q14_3", "Q14_8", "Q14_13", "Q14_18", "Q15_3", "Q15_8", "Q15_13", "Q15_18")]

Sociability_items <- new_data[, c("Q13_1", "Q13_16", 
                                               "Q14_11","Q15_6")]

Assertiveness_items <- new_data[, c("Q13_6", "Q14_1", 
                                         "Q14_16","Q15_11")]

EnergyLevel_items <- new_data[, c("Q13_11", "Q14_6", 
                                           "Q15_1","Q15_16")]
Compassion_items <- new_data[, c("Q13_2", "Q13_17", 
                                         "Q14_12","Q15_7")]

Respectfulness_items <- new_data[, c("Q13_7", "Q14_2", 
                                        "Q14_17","Q15_12")]

Trust_items <- new_data[, c("Q13_12", "Q14_7", 
                                            "Q15_2","Q15_17")]

Organisation_items <- new_data[, c("Q13_3", "Q13_18", 
                                   "Q14_13","Q15_8")]

Productiveness_items <- new_data[, c("Q13_8", "Q14_3", 
                                          "Q14_18","Q15_13")]

Responsibility_items <- new_data[, c("Q13_13", "Q14_8", 
                                            "Q15_3","Q15_18")]

Anxiety_items <- new_data[, c("Q13_4", "Q13_19", 
                                            "Q14_14","Q15_19")]

Depression_items <- new_data[, c("Q13_9", "Q14_4", 
                                     "Q14_19","Q15_14")]

EmotionalVotility_items <- new_data[, c("Q13_14", "Q14_9", 
                                        "Q15_4","Q15_19")]

IntellectualCuriosity_items <- new_data[, c("Q13_10", "Q14_5", 
                                               "Q14_20","Q15_15")]

AestheticCuriosity_items <- new_data[, c("Q13_5", "Q13_20", 
                                                   "Q14_15","Q15_10")]

CreativeImagination_items <- new_data[, c("Q13_15", "Q14_10", 
                                                "Q15_5","Q15_20")]


# Calculate Cronbach's alpha
alpha_extraversion <- alpha(extraversion_items)
alpha_agreeableness <- alpha(agreeablness_items)
alpha_neuroticism <- alpha(neuroticism_items)
alpha_conscientiousness <- alpha(conscientiousness_items)
alpha_openess <- alpha(openess_items)
alpha_Sociability <- alpha(Sociability_items)
alpha_Assertiveness <- alpha(Assertiveness_items)
alpha_EnergyLevel <- alpha(EnergyLevel_items)
alpha_Compassion <- alpha(Compassion_items)
alpha_Respectfulness <- alpha(Respectfulness_items)
alpha_Trust <- alpha(Trust_items)
alpha_Organisation <- alpha(Organisation_items)
alpha_Productiveness <- alpha(Productiveness_items)
alpha_Responsibility <- alpha(Responsibility_items)
alpha_Anxiety <- alpha(Anxiety_items)
alpha_Depression <- alpha(Depression_items)
alpha_EmotionalVotility <- alpha(EmotionalVotility_items)
alpha_IntellectualCuriosity <- alpha(IntellectualCuriosity_items)
alpha_CreativeImagination <- alpha(CreativeImagination_items)

print(alpha_extraversion)
print(alpha_agreeableness)
print(alpha_neuroticism)
print(alpha_conscientiousness)
print(alpha_openess)
print(alpha_Sociability)
print(alpha_Assertiveness)
print(alpha_EnergyLevel)
print(alpha_Compassion)
print(alpha_Respectfulness)
print(alpha_Trust)
print(alpha_Organisation)
print(alpha_Productiveness)
print(alpha_Responsibility)
print(alpha_Anxiety)
print(alpha_Depression)
print(alpha_EmotionalVotility)
print(alpha_IntellectualCuriosity)
print(alpha_CreativeImagination)

spss_data <- read_sav("/Users/robwickett/Desktop/traits.sav")

merged_data <- merge(data_aggregated, spss_data, by = "Q1", all = TRUE)
cleaned_dataset <- na.omit(merged_data)

# Assuming you have items Q13_1 to Q13_20 in your scale
CognitiveChange_items <- cleaned_dataset[, c("Q16_11", "Q16_12", 
                                          "Q16_13","Q16_14", "Q16_15")]

AttentionMod_items <- cleaned_dataset[, c("Q16_6", "Q16_7", 
                                          "Q16_8","Q16_9", "Q16_10")]

ResponseMod_items <- cleaned_dataset[, c("Q16_16", "Q16_17", 
                                          "Q16_18","Q16_19", "Q16_20")]

SitMod_items <- cleaned_dataset[, c("Q16_1", "Q16_2", 
                                         "Q16_3","Q16_4", "Q16_5")]

SocAff_items <- cleaned_dataset[, c("Q16_21", "Q16_22", 
                                               "Q16_23","Q16_24")]

alpha_CogChange <- alpha(CognitiveChange_items)
alpha_AttMod <- alpha(AttentionMod_items)
alpha_ResponseMod <- alpha(ResponseMod_items)
alpha_SitMod <- alpha(SitMod_items)
alpha_SocAff <- alpha(SocAff_items)

print(alpha_CogChange)
print(alpha_AttMod)
print(alpha_ResponseMod)
print(alpha_SitMod)
print(alpha_SocAff)

spss_data <- read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Cleaned_withrawitems/DiariesMerged_withitems.sav")


# Calculate the count of '1' responses in Q2 for each participant
q2_counts <- spss_data %>%
  group_by(Q1) %>%
  summarize(count_1 = sum(Q2 == 1))

# Filter the dataset to exclude participants who answered '1' fewer than 2 times in Q2
data_filtered <- spss_data %>%
  left_join(q2_counts, by = "Q1") %>%
  filter(is.na(count_1) | count_1 >= 2) %>%
  select(-count_1)

# View the filtered dataset
print(data_filtered)

# Count the number of unique values for Q1
unique_values_count <- my_data %>%
  distinct(Q1) %>%
  nrow()

# Print the result
print(unique_values_count)

# Find unique values in dataset2$Q1 that are not in dataset1$Q1
unique_values_in_dataset2 <- setdiff(data_filtered$Q1, data$Q1)

# Print the unique values
print(unique_values_in_dataset2)


# Participants to add
participants_to_add <- c("583c646f6bbe2c00017a1584", "5ceb011d2e4f1700162a8bca", 
                         "5ea86e6c5098912b21d86d3c", "63d4019f44b9ae716c33f519")

# Filter the participants to add from filtered_data
participants_data <- data_filtered[data_filtered$Q1 %in% participants_to_add, ]

# Add the participants to data using rbind
data_with_added_participants <- rbind(data, participants_data)

my_data <- data_with_added_participants[, !colnames(data_with_added_participants) %in% c("Q8_1", "Q8_2", "Q8_3", "Q8_4", "Q8_5", "Q8_6", "Q8_7", "Q8_8", "Q8_9", "Q8_10", "Q8_11", "Q8_12", "Q8_13", "Q8_14", "Q8_15")]

# Assuming your dataset is named "my_data"
participants_to_remove <- c("583c646f6bbe2c00017a1584", "5ceb011d2e4f1700162a8bca", "5ea86e6c5098912b21d86d3c", "63d4019f44b9ae716c33f519")

my_data <- data_filtered[!data_filtered$Q1 %in% participants_to_remove, ]
write_sav(my_data, "/Users/robwickett/Desktop/PhD/Study 3/Data/Cleaned_withrawitems/Compeltemerge.sav.sav")

# Find unique values in dataset2$Q1 that are not in dataset1$Q1
unique_values_in_baseline <- setdiff(spss_data$Q1, my_data$Q1)

# Print the unique values
print(unique_values_in_baseline)

# Assuming your dataset is named "my_data"
participants_to_remove <- c("568708c6369319000c268f8d", "568aebfc39a70e000b57acf9", "5835e18fa708570001a8b0f4",
                            "583c646f6bbe2c00017a1584", "5847e60f73170700013697c6", "58ab1b3c32c2cc0001cd2bed",
                            "5986ce0eb78fa90001adf2f2", "599a88aeb5566b0001aa8535", "5af578683aba4900015ec37a",
                            "5b8fe9af73103d0001077f5d", "5bb7a2f927fb5000013cb0e6", "5c3e3905c061930001b5e89d",
                            "5c7b2a3df48c6d0016f6fb70", "5d31c201c88f0e001a4c3cb3", "5d79093ed66dd90019e49425",
                            "5e33295b0d4362300d5e7e70", "5e78c6725088ab44b24e2545", "5e933bbce6812e6768a81eb9",
                            "5edf878332e230257937edd1", "5f4a9715ad5ee6637c2a1f13", "5f626c607d0e5f2ce48863aa",
                            "5f68df44e847c607809962c8", "5f778d553167a12eae0e1c00", "5fb3e1ecadba8802c0d12fe6",
                            "5fcca7bf6a7182b003b32a36", "6039f584e2105057e060fd1a", "606451b5aab2ec4afaae10f4",
                            "60b0fca5730e04dcd7cd0b2d", "60e76a1d107aa8d3ccd3e37b", "610ab30529234c54788108fd",
                            "6148bc5aece0ddc6c1cf1d66", "6149e00ccaf44920bd2a00fa", "629e42a3800b66722644beda",
                            "62ebd8c191cf8e7fe54af5cf", "63468b3961ea9fad9573cef3", "63468b4dc2726a47dac86511",
                            "634afa5b289f73af3f29058e", "63d3fb4fd3c313dca0920312", "63d4019f44b9ae716c33f519",
                            "63d7a26f594b17eba6a83328", "63ea8ec521931832f4b6b7cb", "6406f1ede4fc99ed3f2ce498",
                            "55b765befdf99b0416ce5382", "5630caf8733ea0000a162d0b", "56f699e876348f000c883bba",
                            "57b53ad8a18afd00013e0565", "57c957626736cf0001b31f39", "587d4ed4e4404c0001c9d1b9",
                            "5894b2033e061b0001dff3d3", "595c19a78c7f1a00013bd40f", "599ab130b5566b0001aa85bb",
                            "5a68cddb31b87a0001c75534", "5b1f717b88ee7400012b4fb1", "5b2838e8b1cd750001498ba9",
                            "5bc33a0bdf712a000177b62b", "5c48b35ca775fb00011f2314", "5c5c7a487b0a16000116e23a",
                            "5c7575d6f4e2ff0001d0f6f8", "5ced5c2112495000171579ee", "5d230886d7533000017a4409",
                            "5d5988df44609300192e3521", "5da3865a7f1f7d001005f581", "5dcc114dca10638a72ddc7fd",
                            "5dfa87ae0562057caa68989c", "5e370bc58390cf6992ba7b1f", "5ea9a852575f8c0d9cd814d2",
                            "5ebaa1ff940078000b52ebf4", "5ec267e5d4cb3e02f37e3ea0", "5ee93e0226f9f0167b877dbe",
                            "5ef37e2ca184db0e6896d19b", "5f1201414cd2ab11393360f6", "5f205b5758182b3243f8ce62",
                            "5f4538b6ffe9f50c3cbe3ae1", "5fddf3f28ac1824a6fb56e29", "5fecf25f2c2ee6a373d61c65",
                            "6002e90144049f32edaf9ccf", "60153f6faf1cb75da820c070", "604b4cefcccde3ca319a38b9",
                            "6081927660b125bdbfdf4fc9", "60a6a76aca6c98b970de90f2", "60bf0f6824fa105c814baabc",
                            "60c5165878a82f763c29536a", "60eb09a302eb539c76dc73d2", "60ecb3f500857abf7ae07653",
                            "613a66c29072d1c3fe23175f", "613c81cee7cb25192a07ddea", "614ce29d78ae25eced10ede1",
                            "615d8a94a76a15626da937c5", "6266b0409582f553a3e10d9a", "62aa591a476ebabe28612a92",
                            "62fbe4c86d484357b6adbc36", "631b882fa0099ad85d8bae6b", "6321d74ca80fe8632af0b5ae",
                            "6350291dda5623a771dab2be", "63a1d7270be2b46d12eebe83", "63a21141da58c1ee1deebe34",
                            "63a595f6647550ecffa24f9e", "63b9792032364178f397d92b", "63d13d863c5fab8267ee12e1",
                            "63d4029514fd2433484c50a4", "63d4244ef7a44d268de65863", "641038529809a815fcb82569",
                            "64137a849e2c3d9bed198069", "559c3e04fdf99b32b55f2d8a", "55cf6a7b34e9060005e56ca2",
                            "56ca0400b30699000bd9d94f", "56fa7c2e51c6bf000ccb2954", "56fad9e4997828000c802706",
                            "56fea1c604254b000f0065a9", "5766788c3d2d3a000138b0ef", "58d3c463bfaa440001d1f3a8",
                            "59d866c4e2762800015eb6e3", "59e32f80d838ae000185083c", "5a513719acc75b000179bff1",
                            "5a533f6de0cf3d000125fbec", "5a8d9bd3eea3d300016eb7f9", "5a9ecc0e6219a30001f54dff",
                            "5ace533135eccf0001a11a10", "5af6e439672ea800010d4e92", "5b168549f6cef00001428386",
                            "5b7ecfe47a63030001019c35", "5bdeda9109e1b40001df5140", "5be2012f7227be0001e965ac",
                            "5c253dc4191c0c0001e7e063", "5c352f5629a7ee0001c19558", "5d403b7d4e509a0019f664f0",
                            "5d41c4d8198538001a375d4b", "5d549ee1b4851f0018177b2e", "5d6a924a9e02d2001514311a",
                            "5d8baf023435540015cd4860", "5de3bb4674c6793bb8b1fdcc", "5df7f73b6d8cc7000a390c2e",
                            "5e713c4f195e8e056046e3c5", "5e85a5bc9722560009b2440f", "5e8f1eb6bb578701f190229b",
                            "5ea49cfe67217a46167ac6c0", "5eaa956e7df877012afbff38", "5eb50e5315a3c137f40cbad3",
                            "5ec7c214ac7e0423adb51bc4", "5ec957b88b62d208f6997476", "5f0c4ca68b49e50566954573",
                            "5f181092bc89f00dc2e577a2", "5f28431d82be890d2f58a32d", "5f2ad259fcefd53b7e09cebf",
                            "5f2b1b85781f285032ce292a", "5f4e0155cf03293db269f873", "5f52a12f0e44f31687ccdff1",
                            "5f69cc6f69949e00098d8295", "5f953584d654c638ae82c4b2", "5fb551ccab996500099ab1aa",
                            "5fb835372b02f05267811822", "6028daf2a770da7b4c6bc0c9", "6035582ef5c002ac22650d7a",
                            "603b8781c6dd2384cd463ed3", "603be4b0babbb59215931fe0", "6054f19cd4e63ca120881eb6",
                            "605da37765cbd262e6778323", "60659fa1c158d333d9ec89b8", "60709b14f0d49133f30096c5",
                            "607b0819e6e87bbcf4932da0", "607ebda28cf7e38fa0160295", "60d9dd8da844c89512174302",
                            "60da18693053b2c1d420085f", "60eb2754cb0dd8c971cb2aeb", "60edd339db322f08d72ecd41",
                            "60fd69a948f66ff8abdd5d0b", "6103082117238ae6d0c3e805", "610a48c97411f54f854be3d8",
                            "613cc5de78e7317f29171d0c", "6143217abd754c7495d8d3f7", "6148ee05f04331f8b7785c29",
                            "614f9a7ff2cecd7d2e2de61a", "615a2169834d0c6f877af1e9", "615dc9a9d61ae634ede50ebb",
                            "6166c2849af8d8e1858e3b14", "61fa9112d42cb19beec492b5","62795033eec9c814d8caf988",
                            "627a86f8b9a98a5c46bf74ec", "6287818abba6a55b1d5db5c7", "628f93509647b89df3644ad1",
                            "62989d142aedce04d304cabf", "629e10bc2c72cfafd3b0aa30", "62a0f0a0e2819d6bfd91f3cf",
                            "62cc3d756e2d1d89abf3c001", "62da689228c4a8004c8d789d", "62dabdf31bca274cb6734149",
                            "62db2644ab0a3a353c0dcb54", "62e02596b92f410946e852c9", "62e2e4f64b127040b1db771e",
                            "62ebcfeb8e3a52c0591fd10c", "62fb7968eceb11fe4ed5ccab", "62fbcf5e491faae4a5f24905",
                            "6310c063bd0cce86b0289d20", "63337737be6252ad76e5f983", "6346887083e9dda2b38f6c56",
                            "6346a9acc2a832b58c8e7229", "6346cd7b7d85c596d543e0ed", "634e7bbcec6f28ed59e5bd59",
                            "63c4291c52ec538e38e82f53", "63cd84b96c0d2867bf761f49", "63cede0e9ea4aeb932e3bb9c",
                            "63d2b22da2bc79f659e4a4a7", "63d7c4b261d53fcf4f1dcd64", "63dd1dab484083017c1b8184",
                            "63ea9c738b20fcda2fb74d17", "63ebd39a63bb79b50d438ab7", "63effcaddd063d35ebc365d8",
                            "63f79eb498a9401ce52a9fe1", "63f8762e8d31b94e82923364", "6414277fc2b4fc3180479b02",
                            "6426cfe964863ee2ff26642f", "642d521dfa462780d6ef7fc5", "644d11521e3adb743d91eb4f",
                            "55f0147fb9401300137f472d", "571cd566e1d2ec0013a68b64", "572b912c3ab9df000dba9eef",
                            "5803cb9a3073510001231d82", "59621bf61602240001030f03", "59646f5198cf77000106feb1",
                            "596dd49a0fe2ff00016a7d50", "59d65ad8e2762800015eb51b", "5a05b488120fb30001948623",
                            "5a5a7cce76d1c60001ab2dc9", "5a9984e8b5e2110001c6bebc", "5ad8dbe2e34a050001b83eac",
                            "5b490bd59afec90001009679", "5b61d7e48de04c000171cfb9", "5b70252c99982e000145ccf7",
                            "5bc36079c9dbcd0001a842ed", "5bc4bfb7056f850001100e03", "5bc7b12e14dd840001c4e448",
                            "5beecf340431f3000193c926", "5c0070626e6cd80001bf0907", "5c277df7015a5b00018527c1",
                            "5c84ed0cbda42300152f5952", "5c86f9ced810a700169ddfe7", "5c8cfc3772503f0001e9c965",
                            "5ca0853c8e8c01001253db27", "5cae3563d80d36001eb0d7b1", "5cd05f58395afc00178f3918",
                            "5d25ee154b5c4f0018fdaaa7", "5d519a9df20feb00180b62bc", "5d83fc04e3cd6a00010bb09b",
                            "5dfaafeec0b81f7f32f10709", "5e5012ec47808406305d6a0d", "5e529aaf2ca8272b83162fa0",
                            "5e716ca3bea4de092aebb04e", "5e96185ae3d45508abe19a36", "5e9c8fed2d191903edc5d16b",
                            "5eb9697b651aca0436adfe9d", "5ed1566237d27008e6239275", "5ee75e7943d024027677c8de",
                            "5ef7783026861249d8eb58ef", "5f0f6811a0ed4a11dbc57b79", "5f1c0a9e47a90a000ad55e2c",
                            "5f211bfd5bfa2a472d7886b7", "5f32de1ebd944907faf937f4", "5f3d429e599d9708bd5ffb2c",
                            "5f6cc4a84b851e0bb2028bb9", "5f74b3a0d780450b219f8616", "5fb249cbf1bb8b7db9bb8d13",
                            "5febcb7129db95d12bec9e2b", "5ffc9c848f292611f524c2dd", "6018079aed3941ac270f54e0",
                            "6035550d95c926d2e7f5f86b", "6051570d959c53ccacb61130", "6066d08253019ea549b1dd37",
                            "607ca741fd54ce0bd3846328", "60c9ab402f1c532d26288def", "60de3c34940c209d729e32b8",
                            "60fc3a1e0eb578aa02590e27", "60fe68fa593d2cfead3f8bba", "611578fe8cbea39bf4928b6d",
                            "6149be0fccc3a80b23287530", "6234815e8b6a224a43abe745", "62d7c7cb331a9ac08b538f64",
                            "62e0289b12d76830eadddf64", "62fcc19888516179d41affb5", "6310c453d91e6e0eddbe1df8",
                            "6346e117e4e283094ff51b84", "639f294882a3797f77a3dd81", "63c300d13681d9127fe5e1c9",
                            "63d1412396ee4fa133b52643", "63d3fd5916541b5031bd4a3f", "63d44d432dba83234be6588c",
                            "63d83aa50ef2d9e7099eff86", "63ea42504d477a5fc21236d4", "64135b6311850e6f84843e48",
                            "6442a22876eacc2e85ba2673")

updated_baseline <- spss_data[!spss_data$Q1 %in% participants_to_remove, ]




# Replace -999 with NA in the entire dataframe
my_data[my_data == -999] <- NA


# Assuming your data is named my_data and you want to calculate alpha for Q8_1 to Q8_5
cronbach_alpha_SitMod <- alpha(my_data[, c("Q8_1", "Q8_2", "Q8_3")])
cronbach_alpha_CogApp <- alpha(my_data[, c("Q8_4", "Q8_5", "Q8_6")])
cronbach_alpha_ResponseMod <- alpha(my_data[, c("Q8_7", "Q8_8", "Q8_9")])
cronbach_alpha_AttMod <- alpha(my_data[, c("Q8_10", "Q8_11", "Q8_12")])
cronbach_alpha_SocAff <- alpha(my_data[, c("Q8_13", "Q8_14", "Q8_15")])


print(cronbach_alpha_SitMod)
print(cronbach_alpha_CogApp)
print(cronbach_alpha_ResponseMod)
print(cronbach_alpha_AttMod)
print(cronbach_alpha_SocAff)

write_sav(updated_baseline, "/Users/robwickett/Desktop/PhD/Study 3/Data/Cleaned_withrawitems/Baseline_July 24, 2023_15.04.sav")
write_sav(my_data, "/Users/robwickett/Desktop/PhD/Study 3/Data/Cleaned_withrawitems/DiariesMerged_withitems.sav")

library(haven)
updated_baseline <- read_sav("/Users/robwickett/Desktop/PhD/Study 3/Data/Cleaned_withrawitems/Baseline_July 24, 2023_15.04.sav")

# Assuming your data data frame is named data
unique_q1 <- unique(my_data$Q1)
num_unique_q1 <- length(unique_q1)
print(num_unique_q1)

q2_frequency <- table(my_data$Q1[my_data$Q2 == 1])
mean_frequency <- mean(q2_frequency)
sd_frequency <- sd(q2_frequency)
range_frequency <- range(q2_frequency)
print(sd_frequency)
