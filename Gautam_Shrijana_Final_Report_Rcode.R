data <- read.csv("C:/Users/shrij/OneDrive - University of Connecticut/Desktop/Fall 2024 courses/Categorical Data Analysis/Project Work/Mental Health/MentalHealthSurvey new.csv")

Sport_engagement<- ifelse(data$sports_engagement =="No Sports", 0, 1)
Residential_status<- ifelse(data$residential_status == "On-Campus" , 1, 0)
Gender<- ifelse(data$gender=="Female", 1, 0)

#install.packages("dplyr")
library(dplyr)
data$cgpa_hat[data$cgpa < 3.0 ] <- 1
data$cgpa_hat[data$cgpa > 3.0 & data$cgpa <3.5] <- 2
data$cgpa_hat[data$cgpa == "3.5-4.0"] <- 3

data$age_hat[data$age<20]<-1
data$age_hat[data$age==20] <-2
data$age_hat[data$age>20]<-3
table(data$age_hat)


data$average_sleep_hat[data$average_sleep< 4] <-1
data$average_sleep_hat[data$average_sleep> 4 & data$average_sleep <6] <-2
data$average_sleep_hat[data$average_sleep>6 & data$average_sleep<8] <-3
table(data$average_sleep_hat)


data$academic_year_hat[data$academic_year=="1st year"] <- 1
data$academic_year_hat[data$academic_year=="2nd year"] <- 2
data$academic_year_hat[data$academic_year=="3rd year"] <- 3
data$academic_year_hat[data$academic_year=="4th year"] <- 4

data$degree_major_binary<- ifelse(data$degree_major=="Data Science", 1,0)


data$campus_discrimination_binary<- ifelse(data$campus_discrimination == "Yes", 1, 0)
data$study_satisfaction_binary<- ifelse(data$study_satisfaction >= 4, 1,0)
data$academic_workload_binary <- ifelse(data$academic_workload >= 4, 1, 0)
data$academic_pressure_binary <- ifelse (data$academic_pressure >= 4, 1, 0)
data$financial_concerns_binary <- ifelse (data$financial_concerns >= 4,1,0)
data$future_insecurity_binary <- ifelse(data$future_insecurity >= 4, 1, 0)

data$depression_binary <- ifelse(data$depression >= 4, 1, 0)
data$anxiety_binary <- ifelse(data$anxiety >= 4, 1, 0)
data$isolation_binary <- ifelse(data$isolation >= 4, 1, 0)
data$social_relationships_binary<- ifelse(data$social_relationships>=4, 1, 0)


data$social_relationships_hat[data$social_relationships <= 2 ]<-1
data$social_relationships_hat[data$social_relationships > 2 & data$social_relationships<=3 ]<-2
data$social_relationships_hat[data$social_relationships > 3 ]<-3

# Depression as a dependent variable

model1<-glm(data$depression_binary~ data$degree_major_binary+ data$campus_discrimination_binary+
              data$sports_engagement_binary+data$residential_status_binary+
              data$cgpa_hat+data$average_sleep_hat+data$gender_binary+
              data$age_hat+data$study_satisfaction_binary+data$academic_workload_binary+
              data$academic_pressure_binary+data$financial_concerns_binary+
              data$academic_year_hat+ data$social_relationships_hat, 
            data=data, family= "binomial")
summary(model1)
library(MASS)
stepwise_model <- stepAIC(model1, direction = "both")
summary(stepwise_model)
model_1 <- stepwise_model
library(pscl)
pR2(model_1)


M<-glm(data$depression_binary~as.factor(data$academic_year), family= "binomial")
summary(M)



model_trial <- glm(data$depression_binary~as.factor(data$cgpa), family = "binomial")
summary(model_trial)
# Combine categories
data$cgpa <- data$cgpa %>% 
  as.character() %>% # Convert to character for modification
  dplyr::recode(
    `0.0-0.0` = "0-3",
    `1.5-2.0` = "0-3",
    `2.0-2.5` = "0-3",
    `2.5-3.0` = "0-3"
  )

# Convert back to factor to retain categorical properties
data$cgpa <- factor(data$cgpa, levels = c("0-3", "3.0-3.5", "3.5-4.0"))

# View the updated table
table(data$cgpa)
model_trial <- glm(data$depression_binary~as.factor(data$cgpa), family = "binomial")
summary(model_trial)

model_trail1<- glm(data$depression_binary~as.factor(data$social_relationships_hat), family="binomial")
summary(model_trail1)

model_trial2<- glm(data$depression_binary~ as.factor(data$social_relationships_hat), family="binomial")
summary(model_trail1)

model_trial3<- glm(data$depression_binary~ as.factor(data$social_relationships_hat), family="binomial")
summary(model_trial3)



aa<- glm(data$depression_binary~as.factor(data$academic_year),family="binomial")
summary(aa)
pR2(aa)


library(dplyr)





summary(model_1)
pscl::pR2(model_1)["McFadden"]
caret::varImp(model_1)
#calculate VIF values for each predictor variable in our model
car::vif(model_1)


# for Anxiety as a dependent variable
model11<-glm(data$anxiety_binary~ data$degree_major_binary+ data$campus_discrimination_binary+
              data$sports_engagement_binary+data$residential_status_binary+
              data$cgpa_hat+data$average_sleep_hat+data$gender_binary+
              data$age_hat+data$study_satisfaction_binary+data$academic_workload_binary+
              data$academic_pressure_binary+data$financial_concerns_binary+
              data$academic_year_hat+ data$social_relationships_hat, 
            data=data, family= "binomial")
summary(model11)
library(MASS)
stepwise_model <- stepAIC(model11, direction = "both")
summary(stepwise_model)
model_11 <- stepwise_model


summary(model_11)
pscl::pR2(model_11)["McFadden"]
caret::varImp(model_11)
#calculate VIF values for each predictor variable in our model
car::vif(model_11)

# For isolation as a dependent variable

model111<-glm(data$isolation_binary~ data$degree_major_binary+ data$campus_discrimination_binary+
               data$sports_engagement_binary+data$residential_status_binary+
               data$cgpa_hat+data$average_sleep_hat+data$gender_binary+
               data$age_hat+data$study_satisfaction_binary+data$academic_workload_binary+
               data$academic_pressure_binary+data$financial_concerns_binary+
               data$academic_year_hat+ data$social_relationships_hat, 
             data=data, family= "binomial")
summary(model111)
library(MASS)
stepwise_model <- stepAIC(model111, direction = "both")
summary(stepwise_model)
model_111 <- stepwise_model

model_trial <- glm()


summary(model_111)
pscl::pR2(model_111)["McFadden"]
caret::varImp(model_111)


#depression among male student
male_data<- data%>% filter(data$gender=="Male")
female_data <- data%>% filter(data$gender=="Female")
model_male <- glm(data$depression_binary~ data$degree_major_binary+ data$campus_discrimination_binary+
                    data$sports_engagement_binary+data$residential_status_binary+
                    data$cgpa_hat+data$average_sleep_hat+
                    data$age_hat+data$study_satisfaction_binary+data$academic_workload_binary+
                    data$academic_pressure_binary+data$financial_concerns_binary+
                    data$academic_year_hat+ data$social_relationships_hat, 
                  data=male_data, family= "binomial")
summary(model_male)
library(MASS)
stepwise_model <- stepAIC(model_male, direction = "both")
summary(stepwise_model)
model_male1 <- stepwise_model
pscl::pR2(model_male1)["McFadden"]
caret::varImp(model_male1)




#Depression among female student
model_female <- glm(data$depression_binary~ data$degree_major_binary+ data$campus_discrimination_binary+
                    data$sports_engagement_binary+data$residential_status_binary+
                    data$cgpa_hat+data$average_sleep_hat+
                    data$age_hat+data$study_satisfaction_binary+data$academic_workload_binary+
                    data$academic_pressure_binary+data$financial_concerns_binary+
                    data$academic_year_hat+ data$social_relationships_hat, 
                  data=female_data, family= "binomial")
summary(model_female)
library(MASS)
stepwise_model <- stepAIC(model_female, direction = "both")
summary(stepwise_model)
model_female1 <- stepwise_model
pscl::pR2(model_female1)["McFadden"]
caret::varImp(model_female1)






# Objective 2 To evaluate the prevalence of depression among students and examine
#its relationship with anxiety, social isolation, and social engagement.

model_2<-glm(data$depression_binary~ data$anxiety_binary+data$isolation_binary+
               data$future_insecurity_binary, data=data, family=binomial)
summary(model_2)
pscl::pR2(model_2)["McFadden"]
caret::varImp(model_2)



# Depression among male student
model_22<-glm(data$depression_binary~ data$anxiety_binary+data$isolation_binary+
               data$future_insecurity_binary, data=male_data, family=binomial)

summary(model_22)
pscl::pR2(model_22)["McFadden"]
caret::varImp(model_22)

# Depression among female student
model_222<-glm(data$depression_binary~ data$anxiety_binary+data$isolation_binary+
                data$future_insecurity_binary, data=female_data, family=binomial)

summary(model_222)
pscl::pR2(model_222)["McFadden"]
caret::varImp(model_222)







#calculate VIF values for each predictor variable in our model
car::vif(model_22)




cor.test(data$depression_binary,data$isolation_binary, method= "kendall")
cor.test(data$depression_binary,data$anxiety_binary, method= "kendall")
cor.test(data$depression_binary,data$future_insecurity_binary, method= "kendall")

anova_result <- aov(data$depression_binary ~ data$cgpa_hat, data = data)
summary(anova_result)

library(tidyverse)  # For data manipulation and visualization
library(car)
post_hoc <- TukeyHSD(anova_result)
library(datasets)
table(data$gender_binary, data$depression_binary==1)
table(data$gender_binary, data$depression_binary)



