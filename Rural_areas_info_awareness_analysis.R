library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

#loading the data set
rural_area_data <- readxl::read_xlsx("C:\\Users\\stalo\\Downloads\\Rural_area_data.xlsx")
rural_area_data
str(rural_area_data)
glimpse(rural_area_data)

#Number of males and females in the data 
gender_count <- rural_area_data %>%
  count(Gender)
gender_count

ggplot(gender_count , aes(x = Gender , y = n)) +
  geom_col(width = 0.5)

#percentage of males and females in the data
rural_area_data %>%
  select(Gender) %>%
  summarise(Total_people = n() ,
            male_count = sum(Gender == "Male" , na.rm = TRUE) ,
            percentage_males = round((male_count/Total_people) *100 , 1) ,
            female_count = sum(Gender == "Female" , na.rm = TRUE),
            percentage_females = round((female_count/Total_people) *100 , 1)
  )


#mean age in the rural area
rural_area_data %>%
  summarise(mean_age = mean(Age))

#How many people used each different source of information in the area
source_info_used <- rural_area_data %>%
  count(`Source of information`)
source_info_used

ggplot(source_info_used , aes(`Source of information` , n)) +
  geom_col() +
  labs(x = "source of information" , y = "number of people"  )



#How many people have access to electricity in the area
electricity_count <- rural_area_data %>%
  count(Electricity)

ggplot(electricity_count , aes(x = Electricity , y = n)) +
  geom_col(width =0.5) +
  labs(x = "Access to electricity" , y = "Number of people")

rural_area_data %>%
  select(ID , `Household ID` , Gender , Age , `Source of information` , `Type of news accessed`) %>%
  filter(rural_area_data$Electricity == "No")


#Are older individuals more likely to rely on traditional sources of information like radio and talking with people? 
rural_area_data %>%
select(Age , contains("Radio") , contains("Talking with people")) %>%
filter(Age >= 60 ) 

#Are individuals with higher education levels more likely to receive national and international news?

highly_educated_national_international <- rural_area_data %>%
  select(`Education status`, National, International) %>%
  filter(`Education status` %in% c("University", "College", "Senior high school"))
highly_educated_national_international

highly_educated_national_international %>%
  pivot_longer(cols = c(National, International), 
               names_to = "Type", values_to = "Count") %>%
  ggplot(aes(x = `Education status`, y = Count, fill = Type)) +
  geom_col(position = "dodge") +
  labs(title = "Education Levels: National vs International",
       x = "Education Level", y = "Count") +
  theme_minimal()

  #ggplot(highly_educated_national_international , aes(x = `Education status`, y = Count, fill = Type)) +
  #geom_col(position = "dodge") +
  #labs(title = "Education Levels: National vs International",
      # x = "Education Level", y = "Count") +
  #theme_minimal()



#ggplot(highly_educated_national_international , aes())


#Are there any differences in information consumption patterns between men and women?

#Contingency table showing information consumption among males and females
#through different sources of information
cont_table <-  table(rural_area_data$Gender , rural_area_data$`Source of information`)
cont_table

# The proportions
prop_table <- prop.table(cont_table, margin = 1) * 100  # Row percentages
round(prop_table, 1)




ggplot(rural_area_data , aes( x = Gender , fill = `Source of information`)) +
  geom_bar(position = "dodge")

ggplot(rural_area_data , aes( x = Gender , fill = `Usage of source of information (per week)`)) +
  geom_bar(position = "dodge")
  

#Age vs Source of information

age_vs_source_info <- table(rural_area_data$Age , rural_area_data$`Source of information`)
age_vs_source_info

ggplot(rural_area_data , aes(x = Age , fill = `Source of information` )) +
  geom_bar(position = "dodge" , width =1) 

  
