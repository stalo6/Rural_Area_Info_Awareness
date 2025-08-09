# Rural Information Consumption Analysis
# Load required libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(VIM)
library(cluster)

# Assuming your data is loaded as 'df'
# df <- read.csv("your_data.csv")

# 1. DATA CLEANING AND PREPARATION
# Clean column names (remove spaces)
df <- df %>%
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

# Convert character variables to factpors
factor_vars <- c("Gender", "source_main", "social_media", 
                 "Television", "Radio", "news_paper", "talking_people",
                 "usage_frequency", "Local", "Agricultural", "partially_national",
                 "partially_international", "National", "International",
                 "education", "marital_status", "Electricity")

df[factor_vars] <- lapply(df[factor_vars], as.factor)

# 2. EXPLORATORY DATA ANALYSIS
# Basic summary statistics
summary(df)

# Age distribution
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

# Education levels
ggplot(df, aes(x = education)) +
  geom_bar(fill = "coral") +
  labs(title = "Education Distribution", x = "Education Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. INFORMATION SOURCE ANALYSIS
# Multiple vs single sources by education
ggplot(df, aes(x = education, fill = num_sources)) +
  geom_bar(position = "fill") +
  labs(title = "Information Source Diversity by Education",
       x = "Education Level", y = "Proportion", fill = "Number of Sources") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Social media usage by age group
df <- df %>%
  mutate(age_group = cut(Age, breaks = c(0, 18, 30, 45, 60, 100),
                         labels = c("Under 18", "18-30", "31-45", "46-60", "Over 60")))

ggplot(df, aes(x = age_group, fill = social_media)) +
  geom_bar(position = "fill") +
  labs(title = "Social Media Usage by Age Group",
       x = "Age Group", y = "Proportion", fill = "Social Media Use") +
  theme_minimal()

# 4. MEDIA CONSUMPTION PATTERNS
# Create a media consumption score
df <- df %>%
  mutate(
    media_score = (social_media == "Yes") + (Television == "Yes") + 
      (Radio == "Yes") + (news_paper == "Yes") + (talking_people == "Yes"),
    digital_reliance = case_when(
      social_media == "Yes" & Television == "No" & Radio == "No" & news_paper == "No" ~ "High",
      social_media == "Yes" & media_score <= 2 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# Digital reliance by education
ggplot(df, aes(x = education, fill = digital_reliance)) +
  geom_bar(position = "fill") +
  labs(title = "Digital Reliance by Education Level",
       x = "Education Level", y = "Proportion", fill = "Digital Reliance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. NEWS SCOPE ANALYSIS
# Local vs national news preference
df <- df %>%
  mutate(
    news_scope = case_when(
      Local == "Yes" & National == "No" & International == "No" ~ "Local Only",
      Local == "Yes" & (National == "Yes" | International == "Yes") ~ "Local + Broader",
      Local == "No" & (National == "Yes" | International == "Yes") ~ "Broader Only",
      TRUE ~ "Mixed"
    )
  )

ggplot(df, aes(x = education, fill = news_scope)) +
  geom_bar(position = "fill") +
  labs(title = "News Scope Preference by Education",
       x = "Education Level", y = "Proportion", fill = "News Scope") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. STATISTICAL MODELING
# Logistic regression: What predicts multiple information sources?
df$multiple_sources <- ifelse(df$num_sources == "Multiple", 1, 0)

model1 <- glm(multiple_sources ~ Age + Gender + education + social_media + 
                Television + Radio + news_paper, 
              data = df, family = "binomial")

summary(model1)

# Logistic regression: What predicts social media usage?
df$social_media_binary <- ifelse(df$social_media == "Yes", 1, 0)

model2 <- glm(social_media_binary ~ Age + Gender + education + 
                Television + Radio + news_paper, 
              data = df, family = "binomial")

summary(model2)

# 7. CROSS-TABULATION ANALYSIS
# Education and information sources
table(df$education, df$num_sources)

# Age groups and main information source
table(df$age_group, df$source_main)

# 8. CORRELATION ANALYSIS (for numeric variables)
# Create dummy variables for correlation analysis
df_numeric <- df %>%
  mutate(
    social_media_num = ifelse(social_media == "Yes", 1, 0),
    tv_num = ifelse(Television == "Yes", 1, 0),
    radio_num = ifelse(Radio == "Yes", 1, 0),
    newspaper_num = ifelse(news_paper == "Yes", 1, 0),
    local_num = ifelse(Local == "Yes", 1, 0),
    national_num = ifelse(National == "Yes", 1, 0),
    international_num = ifelse(International == "Yes", 1, 0)
  )

cor_matrix <- cor(df_numeric[c("Age", "social_media_num", "tv_num", "radio_num", 
                               "newspaper_num", "local_num", "national_num", 
                               "international_num")], use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black")

# 9. FOCUSED ANALYSIS: RESEARCH QUESTIONS

# QUESTION 1: Age vs Source of Information
print("=== QUESTION 1: Age vs Source of Information ===")

# Age distribution by main information source
ggplot(df, aes(x = source_main, y = Age)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Age Distribution by Main Information Source",
       x = "Main Information Source", y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Statistical test: ANOVA for age differences across sources
age_source_anova <- aov(Age ~ source_main, data = df)
summary(age_source_anova)

# Post-hoc test if significant
if(summary(age_source_anova)[[1]][["Pr(>F)"]][1] < 0.05) {
  posthoc_results <- TukeyHSD(age_source_anova)
  print("Post-hoc test results:")
  print(posthoc_results)
}

# Mean age by information source
age_by_source <- df %>%
  group_by(source_main) %>%
  summarise(
    n = n(),
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE)
  ) %>%
  arrange(mean_age)

print("Mean age by information source:")
print(age_by_source)

# QUESTION 2: Electricity and Information Consumption
print("\n=== QUESTION 2: Electricity Access and Information Consumption ===")

# Chi-square tests for electricity and various media
electricity_social_media <- table(df$Electricity, df$social_media)
electricity_tv <- table(df$Electricity, df$Television)
electricity_radio <- table(df$Electricity, df$Radio)
electricity_multiple_sources <- table(df$Electricity, df$num_sources)

print("Electricity vs Social Media:")
print(electricity_social_media)
print(chisq.test(electricity_social_media))

print("\nElectricity vs Television:")
print(electricity_tv)
print(chisq.test(electricity_tv))

print("\nElectricity vs Radio:")
print(electricity_radio)
print(chisq.test(electricity_radio))

print("\nElectricity vs Multiple Sources:")
print(electricity_multiple_sources)
print(chisq.test(electricity_multiple_sources))

# Visual: Media consumption by electricity access
df_media_long <- df %>%
  select(ID, Electricity, social_media, Television, Radio, news_paper) %>%
  pivot_longer(cols = c(social_media, Television, Radio, news_paper),
               names_to = "media_type", values_to = "usage") %>%
  mutate(usage_binary = ifelse(usage == "Yes", 1, 0))

ggplot(df_media_long, aes(x = media_type, y = usage_binary, fill = Electricity)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Media Usage by Electricity Access",
       x = "Media Type", y = "Proportion Using", fill = "Electricity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# QUESTION 3: Gender Differences in Information Consumption
print("\n=== QUESTION 3: Gender Differences in Information Consumption ===")

# Gender and information sources
gender_source_table <- table(df$Gender, df$source_main)
print("Gender vs Main Information Source:")
print(gender_source_table)
print(chisq.test(gender_source_table))

# Gender and media usage
gender_media_summary <- df %>%
  group_by(Gender) %>%
  summarise(
    n = n(),
    social_media_pct = mean(social_media == "Yes", na.rm = TRUE) * 100,
    tv_pct = mean(Television == "Yes", na.rm = TRUE) * 100,
    radio_pct = mean(Radio == "Yes", na.rm = TRUE) * 100,
    newspaper_pct = mean(news_paper == "Yes", na.rm = TRUE) * 100,
    multiple_sources_pct = mean(num_sources == "Multiple", na.rm = TRUE) * 100
  )

print("Media usage by gender:")
print(gender_media_summary)

# Visual: Gender differences in media consumption
ggplot(df_media_long, aes(x = media_type, y = usage_binary, fill = Gender)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Media Usage by Gender",
       x = "Media Type", y = "Proportion Using", fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# QUESTION 4: Age and Traditional vs Digital Media
print("\n=== QUESTION 4: Age and Traditional vs Digital Media Preferences ===")

# Create traditional vs digital categories
df <- df %>%
  mutate(
    traditional_media = ifelse(Television == "Yes" | Radio == "Yes" | news_paper == "Yes", 1, 0),
    digital_media = ifelse(social_media == "Yes", 1, 0),
    media_preference = case_when(
      traditional_media == 1 & digital_media == 0 ~ "Traditional Only",
      traditional_media == 0 & digital_media == 1 ~ "Digital Only",
      traditional_media == 1 & digital_media == 1 ~ "Both",
      TRUE ~ "Neither"
    )
  )

# Age by media preference
ggplot(df, aes(x = media_preference, y = Age)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Age Distribution by Media Preference",
       x = "Media Preference", y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Statistical test
age_media_anova <- aov(Age ~ media_preference, data = df)
summary(age_media_anova)

# Mean age by media preference
age_by_media <- df %>%
  group_by(media_preference) %>%
  summarise(
    n = n(),
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE)
  ) %>%
  arrange(mean_age)

print("Age by media preference:")
print(age_by_media)

# Correlation between age and different media types
age_media_correlations <- df %>%
  summarise(
    age_social_media = cor(Age, as.numeric(social_media == "Yes"), use = "complete.obs"),
    age_television = cor(Age, as.numeric(Television == "Yes"), use = "complete.obs"),
    age_radio = cor(Age, as.numeric(Radio == "Yes"), use = "complete.obs"),
    age_newspaper = cor(Age, as.numeric(news_paper == "Yes"), use = "complete.obs")
  )

print("Age correlations with media types:")
print(age_media_correlations)

# Age groups and media preference
ggplot(df, aes(x = age_group, fill = media_preference)) +
  geom_bar(position = "fill") +
  labs(title = "Media Preference by Age Group",
       x = "Age Group", y = "Proportion", fill = "Media Preference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# COMPREHENSIVE SUMMARY
print("\n=== COMPREHENSIVE SUMMARY ===")
print("Key Research Questions Analyzed:")
print("1. Age vs Source of Information - Check ANOVA results above")
print("2. Electricity and Information Consumption - Check chi-square tests")
print("3. Gender Differences - Check summary statistics and tests")
print("4. Age and Traditional vs Digital Media - Check correlations and ANOVA")

# Final summary table
final_summary <- df %>%
  group_by(age_group, Gender) %>%
  summarise(
    n = n(),
    social_media_pct = mean(social_media == "Yes", na.rm = TRUE) * 100,
    traditional_media_pct = mean(traditional_media == 1, na.rm = TRUE) * 100,
    multiple_sources_pct = mean(num_sources == "Multiple", na.rm = TRUE) * 100,
    electricity_pct = mean(Electricity == "Yes", na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("Summary by Age Group and Gender:")
print(final_summary)