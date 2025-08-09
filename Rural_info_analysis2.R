# COMPREHENSIVE RURAL INFORMATION CONSUMPTION ANALYSIS
# Load required libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(VIM)
library(cluster)
library(lme4)
library(broom)
library(pheatmap)
library(car)

# Assuming your data is loaded as 'df'
# Clean column names and prepare data
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

# Convert to factors and create age groups
factor_vars <- c("Gender", "num_sources", "source_main", "social_media", 
                 "Television", "Radio", "news_paper", "talking_people",
                 "usage_frequency", "Local", "Agricultural", "partially_national",
                 "partially_international", "National", "International",
                 "education", "marital_status", "Electricity")

df[factor_vars] <- lapply(df[factor_vars], as.factor)

df <- df %>%
  mutate(
    age_group = cut(Age, breaks = c(0, 18, 30, 45, 60, 100),
                    labels = c("Under 18", "18-30", "31-45", "46-60", "Over 60")),
    adult_only = Age >= 18
  )


# ============================================================================
# 2. EDUCATION AND INFORMATION BEHAVIOR
# ============================================================================
print("\n=== 2. EDUCATION AND INFORMATION BEHAVIOR ===")

# Education gradients
education_analysis <- df %>%
  group_by(education) %>%
  summarise(
    n = n(),
    avg_age = mean(Age, na.rm = TRUE),
    social_media_pct = mean(social_media == "Yes", na.rm = TRUE) * 100,
    tv_pct = mean(Television == "Yes", na.rm = TRUE) * 100,
    radio_pct = mean(Radio == "Yes", na.rm = TRUE) * 100,
    newspaper_pct = mean(news_paper == "Yes", na.rm = TRUE) * 100,
    multiple_sources_pct = mean(num_sources == "Multiple", na.rm = TRUE) * 100,
    local_news_pct = mean(Local == "Yes", na.rm = TRUE) * 100,
    national_news_pct = mean(National == "Yes", na.rm = TRUE) * 100,
    international_news_pct = mean(International == "Yes", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    education_order = case_when(
      education == "Primary School" ~ 1,
      education == "Secondary School" ~ 2,
      education == "College" ~ 3,
      education == "University" ~ 4,
      TRUE ~ 5
    )
  ) %>%
  arrange(education_order)

print("Education and Information Behavior:")
print(education_analysis)

# Education-age interactions
education_age_interaction <- df %>%
  filter(adult_only) %>%
  group_by(education, age_group) %>%
  summarise(
    n = n(),
    social_media_pct = mean(social_media == "Yes", na.rm = TRUE) * 100,
    multiple_sources_pct = mean(num_sources == "Multiple", na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Visualize education-age interaction
ggplot(education_age_interaction, aes(x = age_group, y = social_media_pct, 
                                      group = education, color = education)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Social Media Usage by Education and Age",
       x = "Age Group", y = "Social Media Usage %", color = "Education") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================================
# 3. INFORMATION CONSUMPTION INTENSITY
# ============================================================================
print("\n=== 3. INFORMATION CONSUMPTION INTENSITY ===")

# Create intensity scores
df <- df %>%
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
intensity_analysis <- df %>%
  group_by(age_group, Gender, education) %>%
  summarise(
    n = n(),
    avg_intensity = mean(consumption_intensity, na.rm = TRUE),
    high_intensity_pct = mean(intensity_category == "High Intensity", na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("Information Consumption Intensity:")
print(summary(df$consumption_intensity))

# Visualize intensity patterns
ggplot(df, aes(x = age_group, y = consumption_intensity, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Information Consumption Intensity by Age and Gender",
       x = "Age Group", y = "Consumption Intensity Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================================
# 4. GEOGRAPHIC AND SOCIAL CONNECTIVITY
# ============================================================================
print("\n=== 4. GEOGRAPHIC AND SOCIAL CONNECTIVITY ===")

# News scope analysis
df <- df %>%
  mutate(
    news_scope_score = (Local == "Yes") + (partially_national == "Yes") + 
      (National == "Yes") + (partially_international == "Yes") + 
      (International == "Yes"),
    news_focus = case_when(
      Local == "Yes" & news_scope_score == 1 ~ "Local Only",
      Local == "Yes" & news_scope_score > 1 ~ "Local Plus",
      Local == "No" & news_scope_score > 0 ~ "Non-Local Only",
      TRUE ~ "No News"
    ),
    agricultural_interest = Agricultural == "Yes",
    social_information = talking_people == "Yes"
  )

# Geographic connectivity analysis
geographic_analysis <- df %>%
  group_by(education, age_group) %>%
  summarise(
    n = n(),
    local_only_pct = mean(news_focus == "Local Only", na.rm = TRUE) * 100,
    local_plus_pct = mean(news_focus == "Local Plus", na.rm = TRUE) * 100,
    agricultural_pct = mean(agricultural_interest, na.rm = TRUE) * 100,
    social_info_pct = mean(social_information, na.rm = TRUE) * 100,
    avg_news_scope = mean(news_scope_score, na.rm = TRUE),
    .groups = "drop"
  )

print("Geographic and Social Connectivity:")
print(geographic_analysis)

# Visualize news scope patterns
ggplot(df, aes(x = education, fill = news_focus)) +
  geom_bar(position = "fill") +
  labs(title = "News Focus by Education Level",
       x = "Education", y = "Proportion", fill = "News Focus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================================
# 5. DIGITAL DIVIDE DIMENSIONS
# ============================================================================
print("\n=== 5. DIGITAL DIVIDE DIMENSIONS ===")

# Multi-dimensional digital divide
df <- df %>%
  mutate(
    digital_access = Electricity == "Yes",
    digital_usage = social_media == "Yes",
    digital_preference = case_when(
      social_media == "Yes" & Television == "No" & Radio == "No" & news_paper == "No" ~ "Digital Only",
      social_media == "Yes" & (Television == "Yes" | Radio == "Yes" | news_paper == "Yes") ~ "Digital Plus",
      social_media == "No" & (Television == "Yes" | Radio == "Yes" | news_paper == "Yes") ~ "Traditional Only",
      TRUE ~ "Limited"
    ),
    digital_divide_score = (digital_access == FALSE) + (digital_usage == FALSE) + 
      (education %in% c("Primary School", "Secondary School"))
  )

# Digital divide analysis
digital_divide_analysis <- df %>%
  group_by(age_group, Gender) %>%
  summarise(
    n = n(),
    digital_access_pct = mean(digital_access, na.rm = TRUE) * 100,
    digital_usage_pct = mean(digital_usage, na.rm = TRUE) * 100,
    digital_only_pct = mean(digital_preference == "Digital Only", na.rm = TRUE) * 100,
    traditional_only_pct = mean(digital_preference == "Traditional Only", na.rm = TRUE) * 100,
    avg_divide_score = mean(digital_divide_score, na.rm = TRUE),
    .groups = "drop"
  )

print("Digital Divide Analysis:")
print(digital_divide_analysis)

# Visualize digital divide
ggplot(df, aes(x = age_group, fill = digital_preference)) +
  geom_bar(position = "fill") +
  facet_wrap(~Gender) +
  labs(title = "Digital Preference by Age and Gender",
       x = "Age Group", y = "Proportion", fill = "Digital Preference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================================
# 8. NEWS CONTENT PREFERENCES
# ============================================================================
print("\n=== 8. NEWS CONTENT PREFERENCES ===")

# News content analysis
news_content_analysis <- df %>%
  group_by(age_group, education) %>%
  summarise(
    n = n(),
    local_pct = mean(Local == "Yes", na.rm = TRUE) * 100,
    agricultural_pct = mean(Agricultural == "Yes", na.rm = TRUE) * 100,
    national_pct = mean(National == "Yes", na.rm = TRUE) * 100,
    international_pct = mean(International == "Yes", na.rm = TRUE) * 100,
    news_diversity = mean(news_scope_score, na.rm = TRUE),
    .groups = "drop"
  )

print("News Content Preferences:")
print(news_content_analysis)

# Agricultural information seeking
agricultural_predictors <- df %>%
  filter(adult_only) %>%
  select(Agricultural, Age, Gender, education, social_media, Television, Radio, Local) %>%
  mutate(Agricultural_binary = ifelse(Agricultural == "Yes", 1, 0))

# Logistic regression for agricultural information
agri_model <- glm(Agricultural_binary ~ Age + Gender + education + social_media + Local,
                  data = agricultural_predictors, family = "binomial")

print("Agricultural Information Seeking Model:")
print(summary(agri_model))

# ============================================================================
# 9. CROSS-CUTTING ANALYSIS
# ============================================================================
print("\n=== 9. CROSS-CUTTING ANALYSIS ===")

# Intersection effects
intersection_analysis <- df %>%
  filter(adult_only) %>%
  group_by(age_group, Gender, education) %>%
  summarise(
    n = n(),
    social_media_pct = mean(social_media == "Yes", na.rm = TRUE) * 100,
    multiple_sources_pct = mean(num_sources == "Multiple", na.rm = TRUE) * 100,
    vulnerability_high_pct = mean(vulnerability_level == "High Vulnerability", na.rm = TRUE) * 100,
    avg_intensity = mean(consumption_intensity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 3)  # Only groups with 3+ observations

print("Intersection Analysis (Age x Gender x Education):")
print(intersection_analysis)

# Information privilege analysis
df <- df %>%
  mutate(
    privilege_score = 
      (num_sources == "Multiple") * 2 +
      (education %in% c("University", "College")) * 2 +
      (Electricity == "Yes") * 1 +
      (news_scope_score >= 3) * 1,
    privilege_level = case_when(
      privilege_score >= 5 ~ "High Privilege",
      privilege_score >= 3 ~ "Medium Privilege",
      privilege_score >= 1 ~ "Low Privilege",
      TRUE ~ "No Privilege"
    )
  )

# Privilege analysis
privilege_analysis <- df %>%
  group_by(privilege_level) %>%
  summarise(
    n = n(),
    pct = n() / nrow(df) * 100,
    avg_age = mean(Age, na.rm = TRUE),
    male_pct = mean(Gender == "Male", na.rm = TRUE) * 100,
    university_pct = mean(education == "University", na.rm = TRUE) * 100,
    avg_intensity = mean(consumption_intensity, na.rm = TRUE),
    .groups = "drop"
  )

print("Information Privilege Analysis:")
print(privilege_analysis)

# ============================================================================
# 10. TEMPORAL AND USAGE PATTERNS
# ============================================================================
print("\n=== 10. TEMPORAL AND USAGE PATTERNS ===")

# Usage frequency analysis
frequency_analysis <- df %>%
  group_by(usage_frequency, source_main) %>%
  summarise(
    n = n(),
    avg_age = mean(Age, na.rm = TRUE),
    avg_media_count = mean(media_count, na.rm = TRUE),
    electricity_pct = mean(Electricity == "Yes", na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("Usage Frequency Patterns:")
print(frequency_analysis)

# Information diet quality
df <- df %>%
  mutate(
    diet_quality = case_when(
      num_sources == "Multiple" & news_scope_score >= 3 & usage_intensity >= 3 ~ "High Quality",
      num_sources == "Multiple" & news_scope_score >= 2 ~ "Medium Quality",
      num_sources == "One" & usage_intensity >= 3 ~ "Low Quality",
      TRUE ~ "Poor Quality"
    )
  )

# Diet quality analysis
diet_analysis <- df %>%
  group_by(diet_quality) %>%
  summarise(
    n = n(),
    pct = n() / nrow(df) * 100,
    avg_age = mean(Age, na.rm = TRUE),
    education_high_pct = mean(education %in% c("University", "College"), na.rm = TRUE) * 100,
    electricity_pct = mean(Electricity == "Yes", na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("Information Diet Quality:")
print(diet_analysis)

# ============================================================================
# FINAL COMPREHENSIVE SUMMARY
# ============================================================================
print("\n=== COMPREHENSIVE SUMMARY ===")

# Create final summary matrix
final_summary <- df %>%
  group_by(age_group, Gender, education) %>%
  summarise(
    n = n(),
    social_media_pct = mean(social_media == "Yes", na.rm = TRUE) * 100,
    multiple_sources_pct = mean(num_sources == "Multiple", na.rm = TRUE) * 100,
    high_vulnerability_pct = mean(vulnerability_level == "High Vulnerability", na.rm = TRUE) * 100,
    high_privilege_pct = mean(privilege_level == "High Privilege", na.rm = TRUE) * 100,
    high_quality_diet_pct = mean(diet_quality == "High Quality", na.rm = TRUE) * 100,
    avg_intensity = mean(consumption_intensity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 2)

print("Final Comprehensive Summary:")
print(final_summary)

# Key insights summary
print("\n=== KEY INSIGHTS SUMMARY ===")
print("1. HOUSEHOLD PATTERNS - Check intergenerational differences")
print("2. EDUCATION GRADIENTS - Higher education = more diverse sources")
print("3. CONSUMPTION INTENSITY - Varies by age, gender, education")
print("4. GEOGRAPHIC CONNECTIVITY - Local vs. broader news focus")
print("5. DIGITAL DIVIDE - Multi-dimensional access and usage gaps")
print("6. MARITAL STATUS - Effects on information behavior")
print("7. VULNERABILITY - High-risk groups for information isolation")
print("8. NEWS PREFERENCES - Content type preferences by demographics")
print("9. INTERSECTIONS - Combined effects of multiple factors")
print("10. INFORMATION QUALITY - Diet quality varies significantly")

print("\nAnalysis complete! Review each section for detailed insights.")