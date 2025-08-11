library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(VIM)
library(cluster)
library(lme4)
library(broom)
library(pheatmap)
library(car)

#loading the data
rural_area_data <- readxl::read_xlsx("C:\\Users\\stalo\\Downloads\\Rural_area_data.xlsx")


# DATA CLEANING AND PREPARATION

# Clean column names (remove spaces)
rural_area_data <- rural_area_data %>%
  rename(
    household_id = `Household ID`,
    num_sources = `Number of sources of information`,
    source_main = `Source of information`,
    social_media = `Social media`,
    news_paper = `News paper`,
    talking_people = `Talking with people`,
    usage_frequency = `Usage of source of information (per week)`,
    news_type = `Type of news accessed`,
    partially_national = `Partially national`,
    partially_international = `Partially international`,
    education = `Education status`,
    marital_status = `Marital status`
  )

rural_area_data

# Convert character variables to factors
factor_vars <- c("Gender", "source_main", "social_media", 
                 "Television", "Radio", "news_paper", "talking_people",
                 "usage_frequency", "Local", "Agricultural", "partially_national",
                 "partially_international", "National", "International",
                 "education", "marital_status", "Electricity")

rural_area_data[factor_vars] <- lapply(rural_area_data[factor_vars], as.factor)

str(rural_area_data)
glimpse(rural_area_data)
summary(rural_area_data)


#percentage  of males and females in the data set
male_female_percentage <-
  rural_area_data %>%
  summarise(
    Total_people = n(),
    male_count = sum(Gender == "Male" , na.rm = TRUE),
    male_percentage = round((male_count / Total_people * 100) , 1),
    female_count = sum(Gender == "Female" , na.rm = TRUE),
    female_percentage = round((female_count / Total_people * 100) , 1)
  )

male_female_percentage
  

#mean age in the area
rural_area_data %>%
  summarise(mean_age = mean(Age))

# Age distribution
ggplot(rural_area_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

#How many people used each different source of information
source_info_used <- rural_area_data %>%
  count(`Source of information`)
source_info_used

ggplot(source_info_used , aes(source_main , n)) +
  geom_col(fill = "steelblue") +
  labs(x = "source of information" , y = "number of people"  )



#How many people have access to electricity in the area
electricity_count <- rural_area_data %>%
  count(Electricity)

ggplot(electricity_count , aes(x = Electricity , y = n)) +
  geom_col(width =0.5) +
  labs(x = "Access to electricity" , y = "Number of people")

rural_area_data %>%
  select(ID , household_id , Gender , Age) %>%
  group_by(Electricity) %>%
  filter(Electricity == "No")


#Are older individuals more likely to rely on traditional sources of information like radio and talking with people? 
rural_area_data %>%
select(Age , contains("Radio") , contains("Talking with people")) %>%
filter(Age >= 60 )

# Education levels
ggplot(rural_area_data, aes(x = education)) +
  geom_bar(fill = "coral") +
  labs(title = "Education Distribution", x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Social media usage by age group
rural_area_data <- rural_area_data %>%
  mutate(age_group = cut(Age, breaks = c(0, 18, 30, 45, 60, 100),
                         labels = c("Under 18", "18-30", "31-45", "46-60", "Over 60")))

ggplot(rural_area_data, aes(x = age_group, fill = social_media)) +
  geom_bar(position = "fill") +
  labs(title = "Social Media Usage by Age Group",
       x = "Age Group", y = "Proportion", fill = "Social Media Use") +
  theme_minimal()


#Are individuals with higher education levels more likely to receive national and international news?
highly_educated_analysis <- rural_area_data %>%
  filter(education %in% c("University", "College", "Senior high school")) %>%
  group_by(education) %>%
  summarise(
    Total_People = n(),
    National_Yes_Count = sum(National == "Yes", na.rm = TRUE),
    National_Rate = round((National_Yes_Count / Total_People) * 100, 1),
    International_Yes_Count = sum(International == "Yes", na.rm = TRUE),
    International_Rate = round((International_Yes_Count / Total_People) * 100, 1),
    .groups = "drop"
  )

highly_educated_analysis

#analysis of the relationship education level and national and international news consumption
education_news_analysis <- rural_area_data %>%
  # Calculate rates for each education level
  group_by(education) %>%
  summarise(
    Total_People = n(),
    National_Yes = sum(National == "Yes", na.rm = TRUE),
    National_Rate = (National_Yes / Total_People) * 100,
    International_Yes = sum(International == "Yes", na.rm = TRUE),
    International_Rate = (International_Yes / Total_People) * 100,
    .groups = "drop"
  ) %>%
  # Create education ranking for proper ordering
  mutate(
    Education_Rank = case_when(
      education == "University" ~ 4,
      education == "College" ~ 3,
      education == "Senior high school" ~ 2,
      education == "Primary school" ~ 1,
      education == "No education" ~ 0,
      TRUE ~ 1.5  # For any other categories
    )
  ) %>%
  arrange(desc(Education_Rank))

# View the results
print(education_news_analysis)

summary_table <- education_news_analysis %>%
  select(education, Education_Rank, National_Rate, International_Rate) %>%
  arrange(desc(Education_Rank)) %>%
  mutate(
    National_Rate = paste0(round(National_Rate, 1), "%"),
    International_Rate = paste0(round(International_Rate, 1), "%")
  ) %>%
  select(-Education_Rank)

print("NEWS CONSUMPTION BY EDUCATION LEVEL:")
summary_table

#Are there any differences in information consumption patterns between men and women?
cont_table <-  table(rural_area_data$Gender , rural_area_data$source_main)
cont_table

ggplot(rural_area_data , aes( x = Gender , fill = source_main)) +
  geom_bar(position = "dodge")

ggplot(rural_area_data , aes( x = Gender , fill = usage_frequency)) +
  geom_bar(position = "dodge")
  

#Age vs Source of information

age_vs_source_info <- table(rural_area_data$Age , rural_area_data$source_main)
age_vs_source_info

ggplot(rural_area_data , aes(x = Age , fill = source_main )) +
  geom_bar(position = "dodge" , width =1 , fill = "coral") 




# INFORMATION VULNERABILITY ANALYSIS

# Check the individual conditions
condition1 <- rural_area_data$num_sources == "One"
condition2 <- rural_area_data$Electricity == "No" 
condition3 <- rural_area_data$education %in% c("Primary School")
condition4 <- rural_area_data$Age >= 60

print(paste("Condition 1 (single source):", sum(condition1, na.rm = TRUE)))
print(paste("Condition 2 (no electricity):", sum(condition2, na.rm = TRUE)))
print(paste("Condition 3 (primary education):", sum(condition3, na.rm = TRUE)))
print(paste("Condition 4 (elderly):", sum(condition4, na.rm = TRUE)))

# Creating information vulnerability score 
rural_area_data$vulnerability_score <- (condition1 * 2) + (condition2 * 1) + (condition3 * 1) + (condition4 * 1)

print("Vulnerability score distribution:")
print(table(rural_area_data$vulnerability_score, useNA = "always"))

# Creating vulnerability level 
rural_area_data$vulnerability_level <- case_when(
  rural_area_data$vulnerability_score >= 4 ~ "High Vulnerability",
  rural_area_data$vulnerability_score >= 2 ~ "Medium Vulnerability", 
  rural_area_data$vulnerability_score >= 1 ~ "Low Vulnerability",
  TRUE ~ "No Vulnerability"
)

print("Vulnerability level distribution:")
print(table(rural_area_data$vulnerability_level, useNA = "always"))

# vulnerability analysis
vulnerability_analysis <- rural_area_data %>%
  group_by(vulnerability_level) %>%
  reframe(
    n = n(),
    pct = n / nrow(rural_area_data) * 100,
    avg_age = mean(Age, na.rm = TRUE),
    male_pct = mean(Gender == "Male", na.rm = TRUE) * 100,
    female_pct = mean(Gender == "Female" , na.rm = TRUE) * 100 ,
    electricity_pct = mean(Electricity == "Yes", na.rm = TRUE) * 100,
    education_primary_pct = mean(education == "Primary School", na.rm = TRUE) * 100
  )

vulnerability_analysis

# Visualize vulnerability patterns
ggplot(rural_area_data, aes(x = vulnerability_level, fill = Gender)) +
  geom_bar(position = "fill") +
  labs(title = "Information Vulnerability by Gender",
       x = "Vulnerability Level", y = "Proportion", fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create binary indicators for each information dimension
rural_area_data$has_any_media <- ifelse(rural_area_data$social_media == "Yes" | 
                               rural_area_data$Television == "Yes" | 
                               rural_area_data$Radio == "Yes" | 
                               rural_area_data$news_paper == "Yes" |
                               rural_area_data$talking_people == "Yes", 1, 0)

# Information scope access (geographic reach)
rural_area_data$has_local_info <- ifelse(rural_area_data$Local == "Yes", 1, 0)
rural_area_data$has_national_info <- ifelse(rural_area_data$National == "Yes" | rural_area_data$partially_national == "Yes", 1, 0)
rural_area_data$has_international_info <- ifelse(rural_area_data$International == "Yes" | rural_area_data$partially_international == "Yes", 1, 0)
rural_area_data$has_agricultural_info <- ifelse(rural_area_data$Agricultural == "Yes", 1, 0)

# Create comprehensive information access score (0-8 scale)
rural_area_data$total_info_access <- rural_area_data$has_any_media + 
  rural_area_data$has_local_info + 
  rural_area_data$has_national_info + 
  rural_area_data$has_international_info + 
  rural_area_data$has_agricultural_info +
  as.numeric(rural_area_data$num_sources == "Multiple") +
  ifelse(rural_area_data$usage_frequency %in% c("Daily", "Thrice"), 1, 0) +
  (as.numeric(rural_area_data$social_media == "Yes") + 
     as.numeric(rural_area_data$Television == "Yes") + 
     as.numeric(rural_area_data$Radio == "Yes") + 
     as.numeric(rural_area_data$news_paper == "Yes"))


info_summary <- rural_area_data %>%
  group_by(Electricity) %>%
  summarise(
    mean_info_access = mean(total_info_access, na.rm = TRUE),
    has_any_info = mean(has_any_media, na.rm = TRUE),
    has_local = mean(has_local_info, na.rm = TRUE),
    has_national = mean(has_national_info, na.rm = TRUE),
    has_international = mean(has_international_info, na.rm = TRUE),
    multiple_sources = mean(num_sources == "Multiple", na.rm = TRUE),
    .groups = 'drop'
  )

print(info_summary)


# Statistical tests
t.test(rural_area_data$total_info_access ~ rural_area_data$Electricity, rural_area_data = rural_area_data)
chisq.test(rural_area_data$Electricity, rural_area_data$has_any_media)

#INFORMATION CONSUMPTION INTENSITY

# Create intensity scores
rural_area_data <- rural_area_data %>%
  mutate(
    usage_intensity = case_when(
      usage_frequency == "Daily" ~ 7,
      usage_frequency == "Thrice" ~ 3,
      usage_frequency == "Twice" ~ 2,
      usage_frequency == "Once" ~ 1,
      usage_frequency == "Occasionally" ~ 0.5,
      TRUE ~ 0
    ),
    media_count = (social_media == "Yes") + (Television == "Yes") + 
      (Radio == "Yes") + (news_paper == "Yes") + (talking_people == "Yes"),
    consumption_intensity = usage_intensity * media_count,
    intensity_category = case_when(
      consumption_intensity >= 14 ~ "High Intensity",
      consumption_intensity >= 7 ~ "Medium Intensity",
      consumption_intensity >= 2 ~ "Low Intensity",
      TRUE ~ "Minimal"
    )
  )

# Intensity patterns by demographics
intensity_analysis <- rural_area_data %>%
  group_by(age_group, Gender, education) %>%
  summarise(
    n = n(),
    avg_intensity = mean(consumption_intensity, na.rm = TRUE),
    high_intensity_pct = mean(intensity_category == "High Intensity", na.rm = TRUE) * 100,
    .groups = "drop"
  )


print(summary(rural_area_data$consumption_intensity))
rural_area_data$consumption_intensity

# Visualize intensity patterns
ggplot(rural_area_data, aes(x = age_group, y = consumption_intensity, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Information Consumption Intensity by Age and Gender",
       x = "Age Group", y = "Consumption Intensity Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
