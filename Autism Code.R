setwd("~/TRENDI/Elder Interview")

data <- read.csv("Autism-Adult-Data (2).csv", header = T, stringsAsFactors = F)

library(stats)
#install.packages("dplyr")
library("dplyr", lib.loc="~/R/win-library/3.5")
#install.packages("DataExplorer")
library(DataExplorer)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("stringr")
library(stringr)
#install.packages("MASS")
library(caret)
#install.packages("tree")
library(tree)
#install.packages("partykit")
library(partykit)

############################
### Exploratory Analysis ###
############################

# create a vector of the data which can be used to easily decipher its structure
data_list <- list(data)
plot_str(data_list)

introduce(data)

plot_intro(data)

plot_missing(data)

plot_bar(data)

plot_histogram(data)

plot_correlation(data)

#############################
### Initial Data Cleaning ###
#############################

# Question marks in original data were used in place of NA
data[data == "?"] <- NA 

# Remove outlier age, replace it with the median then replace NA values with mean
x <- as.numeric(data$Age)
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(data$Age, na.rm = T)
x[x > (qnt[2] + H)] <- median(data$Age)
data$Age <- (x)

#Impute the mean age
data$Age <- as.numeric(data$Age)
data$Age[is.na(data$Age)] = mean(data$Age, na.rm = TRUE)

#Impute most frequent factor into relation
data$Relation[is.na(data$Relation)] <- "Self"

#Remove special characterers from country of residence, ethnicity, and relation
special.characters <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" 
data$Country_of_res <- str_replace_all(data$Country_of_res, "[[:punct:]]", "")
data$Ethnicity <- str_replace_all(data$Ethnicity, "[[:punct:]]", "")
data$Relation <- str_replace_all(data$Relation, "[[:punct:]]", "")
data$Age_desc <- str_replace_all(data$Age_desc, "[[:punct:]]", "")
data$Country_of_res <- trimws(data$Country_of_res)
data$Ethnicity <- trimws(data$Ethnicity)
data$Relation <- trimws(data$Relation)
data$Age_desc <- trimws(data$Age_desc)

# Impute unknown ethnicities with most frequent ethnicity from country of residence
Breed = c(
  "WhiteEuromean",
  "Latino",
  "Others",
  "Black",
  "Pasifika",
  "MiddleEastern",
  "SouthAsian"
  "Turkish"
)
df=data.frame(Breed)

for (i in unique(df$breed)){
  df[,paste0(i)]=ifelse(df$Breed==i,1,0)
}

#############
### Plots ###
#############
ggplot(data, aes(x = Gender, fill = ASD)) +
  geom_bar() + 
  theme_economist() +
  scale_fill_economist()

ggplot(data, aes(x = Jundice, fill = ASD)) +
  geom_bar() + 
  theme_economist() +
  scale_fill_economist()

ggplot(data, aes(x = Relation, fill = ASD)) +
  geom_bar() + 
  theme_economist() +
  scale_fill_economist()

ggplot(data, aes(x = Relation, y = Age, fill = ASD)) +
  geom_boxplot() + 
  theme_economist() +
  scale_fill_economist()

data %>%
  mutate(Ethnicity = factor(Ethnicity, levels = unique(Ethnicity))) %>%
  ggplot(aes(x = Ethnicity, fill = ASD)) +
  geom_bar() + 
  theme_economist() +
  scale_fill_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(x = App_result, y = ASD)) + 
  geom_point() + 
  theme_economist() +
  scale_fill_economist()

ggplot(data, aes(x = ASD, y = Age)) + 
  geom_point() + 
  theme_economist() +
  scale_fill_economist()

ggplot(data, aes(x = ASD, y = Age)) + 
  geom_point() + 
  theme_economist() +
  scale_fill_economist()

#########################
### Clean Data ###
#########################
data.clean <- data
data.clean <- drop_columns(data.clean, "Age_desc")
data.clean$Age <- as.integer(data.clean$Age)
data.clean$Gender <- ifelse(data.clean$Gender == "m", 0, 1)
data.clean$Ethnicity <- as.factor(data.clean$Ethnicity)
data.clean$Jundice <- ifelse(data.clean$Jundice == "yes", 1, 0)
data.clean$Family <- ifelse(data.clean$Family == "yes", 1, 0)
data.clean$Country_of_res <- as.factor(data.clean$Country_of_res)
data.clean$App_result <- as.integer(data.clean$App_result)
data.clean$ASD <- ifelse(data.clean$ASD == "YES", 1, 0)

######################################
### Split data into test and train ###
######################################
set.seed(100)
train <- sample(nrow(data.clean), 0.8*nrow(data.clean), replace = FALSE)
TrainSet <- data.clean[train,]
ValidSet <- data.clean[-train,]

##################################### 
### Step-wise Logistic Regression ###
#####################################
library(MASS)

data.answers <- drop_columns(data.clean, c("Ethnicity", "Country_of_res","Relation", "Age", "Gender", "Jundice", "Family", "Used_app_before", "App_result", "Age_desc"))
# Fit the model
answers.model <- glm(ASD ~ .^2, data = data.answers, family = binomial(link = logit),  control = list(maxit = 50)) %>%
  stepAIC(trace = FALSE, direction = c("forward"))
# Summarize the final selected model
summary(answers.model)

data.demo <- drop_columns(data.clean, c("A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score", "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score", "Used_app_before", "App_result", "Age_desc", "Relation", "Ethnicity", "Country_of_res"))
# Fit the model
demo.model <- glm(ASD ~ ., data = data.demo, family = binomial(link = logit),  control = list(maxit = 50))
# Summarize the final selected model
summary(demo.model)
lm.pred <- predict(demo.model, ValidSet)
table(lm.pred, ValidSet$ASD) 

###########
### SVM ###
###########
library(e1071)

svm.model <- svm(ASD ~ ., data = TrainSet)
svm.pred <- predict(svm.model, TrainSet)
svm.model

svm.error <- svm.model$residuals
lm_error <- sqrt(mean(svm.error^2))
lm_error

#For svm, we have to manually calculate the difference between actual values (train$y) with our predictions (pred)
svm.error_2 <- TrainSet$ASD - svm.pred
svm_error <- sqrt(mean(svm.error_2^2))
svm_error

#####################
### Random Forest ###
#####################
install.packages("randomForest")
library(randomForest)

data.Forest <- data
data.Forest$ASD <- as.factor(data.Forest$ASD)
data.Forest <- drop_columns(data.Forest, "Age_desc")
data.Forest$Age <- as.factor(data.Forest$Age)
data.Forest$Gender <- as.factor(data.Forest$Gender)
data.Forest$Ethnicity <- as.factor(data.Forest$Ethnicity)
data.Forest$Jundice <- as.factor(data.Forest$Jundice)
data.Forest$Family <- as.factor(data.Forest$Family)
data.Forest$Country_of_res <- as.factor(data.Forest$Country_of_res)
data.Forest$App_result <- as.factor(data.Forest$App_result)
data.Forest$Used_app_before <- as.factor(data.Forest$Used_app_before)
data.Forest$Relation <- as.factor(data.Forest$Relation)

train.forest <- sample(nrow(data.Forest), 0.8*nrow(data.Forest), replace = FALSE)
TrainSet.Forest <- data.Forest[train,]
ValidSet.Forest <- data.Forest[-train,]

data_list <- list(TrainSet.Forest)
plot_str(data_list)

forest.model <- randomForest(ASD ~ . - Country_of_res, data = TrainSet.Forest, importance = TRUE)
forest.model

# Predicting on train set
predTrain <- predict(forest.model, TrainSet.Forest, type = "class")
# Checking classification accuracy
predValid <- predict(forest.model, ValidSet.Forest, type = "class")
table(predTrain, TrainSet$ASD)  
table(predValid,ValidSet$ASD)
mean(predValid == ValidSet$ASD) 
