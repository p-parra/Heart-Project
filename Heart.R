# title: Heart Disease
# author: Pablo Parra
# date: 20/02/2022

# Install packages (if it is required)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm")
if(!require(gridExtra)) install.packages("gridExtra")

# Load the packages
library(tidyverse)
library(caret)
library(data.table)
library(rpart)
library(Rborist)
library(gbm)
library(gridExtra)

# Selected colors (it will be used instead of the default ones)
coloursOfTheProject1 <- c("steelblue3")
coloursOfTheProject2 <- c("cadetblue3", "steelblue3")
coloursOfTheProject3 <- c("cadetblue3", "steelblue3", "steelblue")
coloursOfTheProject4 <- c("cadetblue3", "steelblue3", "steelblue", "steelblue4")

# Loading data - Different options
# If the user only need to read the data online;
data <- read_csv("https://raw.githubusercontent.com/p-parra/Heart-Project/main/heart.csv")

# If the user prefer to download it in your PC:
# url <- "https://raw.githubusercontent.com/p-parra/Heart-Project/main/heart.csv"
# download.file(url, destfile = "heart.csv")
# data <- read_csv("heart.csv")

# If data it is already downloaded (in the active directory):
# data <- read_csv("heart.csv")

# First rows of data
head(data)

# Data structure
str(data)

# Data cleaning
is.na(data) %>% sum() # There are not any NA

# Searching '0' values:
data %>% ggplot(aes(Cholesterol)) + geom_bar() + geom_bar(fill=coloursOfTheProject1)
values_with_0 <- data %>% filter(Cholesterol == 0) %>% nrow()
percentage_0 <- round(values_with_0 / nrow(data) * 100, 2)

# Convert features in factors
data <- data %>% mutate(Sex = as.factor(Sex)) %>%
  mutate(ChestPainType = as.factor(ChestPainType)) %>%
  mutate(RestingECG = as.factor(RestingECG)) %>%
  mutate(ExerciseAngina = as.factor(ExerciseAngina)) %>%
  mutate(ST_Slope = as.factor(ST_Slope)) %>%
  mutate(HeartDisease = as.factor(HeartDisease))

## DATA EXPLOTARION AND VISUALIZATION

# Heart Disease Feature
data %>% ggplot(aes(HeartDisease)) + 
  geom_bar(fill=coloursOfTheProject2)

# Features distribution
# Discrete features
d1 <- data %>% ggplot(aes(Sex)) + geom_bar(fill=coloursOfTheProject2)
d2 <- data %>% ggplot(aes(ChestPainType)) + geom_bar(fill = coloursOfTheProject4) 
d3 <- data %>% ggplot(aes(FastingBS)) + geom_bar(fill=coloursOfTheProject2)
d4 <- data %>% ggplot(aes(RestingECG)) + geom_bar(fill=coloursOfTheProject3) 
d5 <- data %>% ggplot(aes(ExerciseAngina)) + geom_bar(fill=coloursOfTheProject2) 
d6 <- data %>% ggplot(aes(ST_Slope)) + geom_bar(fill=coloursOfTheProject3) 
grid.arrange(d1, d2, d3, d4, d5, d6, ncol=2)

# Continuous features
c1 <- data %>% ggplot(aes(Age)) + geom_bar(fill=coloursOfTheProject1)
c2 <- data %>% ggplot(aes(RestingBP)) + geom_bar(fill=coloursOfTheProject1)
c3 <- data %>% filter(Cholesterol>0) %>% 
  ggplot(aes(Cholesterol)) + geom_bar(fill=coloursOfTheProject1) 
c4 <- data %>% ggplot(aes(MaxHR)) + geom_bar(fill=coloursOfTheProject1) 
c5 <- data %>% ggplot(aes(Oldpeak)) + geom_bar(fill=coloursOfTheProject1)
grid.arrange(c1, c2, c3, c4, c5, ncol=2)

# For each of the features, a table and a column chart of its distribution will be presented. 
# These graphs will also distinguish if the different categories of the feature has the disease or not.
# 1. Sex
data %>% group_by(Sex) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% ggplot(aes(Sex, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2) 

# 2. Age
data %>% mutate(AgeRounded = round(Age, -1)) %>%
  group_by(AgeRounded) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% mutate(AgeRounded = round(Age, -1)) %>% 
  ggplot(aes(AgeRounded, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2) 

# 3. ChestPainType [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
data %>% group_by(ChestPainType) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% ggplot(aes(ChestPainType, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)

# 4. RestingBP: resting blood pressure [mm Hg]
data %>% mutate(RoundedRestingBP = round(RestingBP, -1)) %>%
  group_by(RoundedRestingBP) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% mutate(RoundedRestingBP = round(RestingBP, -1)) %>%
  filter(RoundedRestingBP>0) %>%
  ggplot(aes(RoundedRestingBP, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)

# 5. Cholesterol: serum cholesterol [mm/dl]
data %>% mutate(RoundedCholesterol = round(Cholesterol, -2)) %>%
  group_by(RoundedCholesterol) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% mutate(RoundedCholesterol = round(Cholesterol, -2)) %>%
  ggplot(aes(RoundedCholesterol, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)
  
# 6. FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
data %>% group_by(FastingBS) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% ggplot(aes(FastingBS, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)

# 7. RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality
data %>% group_by(RestingECG) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% ggplot(aes(RestingECG, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)

# 8. MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]
data %>% mutate(RoundedMaxHR = round(MaxHR, -1)) %>%
  group_by(RoundedMaxHR) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% mutate(RoundedMaxHR = round(MaxHR, -1)) %>%
  ggplot(aes(RoundedMaxHR, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)

# 9. ExerciseAngina: yes or no
data %>% group_by(ExerciseAngina) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% ggplot(aes(ExerciseAngina, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)

# 10. Oldpeak: oldpeak = ST [Numeric value measured in depression]
data %>% mutate(RoundedOldpeak = round(Oldpeak)) %>%
  group_by(RoundedOldpeak) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% mutate(RoundedOldpeak = round(Oldpeak)) %>%
  ggplot(aes(RoundedOldpeak, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)

# 11. ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping]
data %>% group_by(ST_Slope) %>% 
  summarize(HeartDisease = mean(HeartDisease == 1), n=n()) %>% knitr::kable()
data %>% ggplot(aes(ST_Slope, fill=HeartDisease)) + geom_bar() +
  scale_fill_manual(values=coloursOfTheProject2)

## MODELING APPROACH

# Creating data partition:
set.seed(2022, sample.kind = "Rounding")
index <- createDataPartition(data$HeartDisease, 1, 0.5, FALSE)
train <- data %>% slice(-index)
test <- data %>% slice(index)

# GLM
fit_glm <- train(HeartDisease ~ ., data = train, method = "glm")
pred_glm <- predict(fit_glm, test)
acc_glm <- mean(pred_glm == test$HeartDisease)
acc_glm

# kNN
fit_knn <- train(HeartDisease ~ ., data = train, method = "knn")
pred_knn <- predict(fit_knn, test)
acc_knn <- mean(pred_knn == test$HeartDisease)
acc_knn

# LDA
fit_lda <- train(HeartDisease ~ ., data = train, method = "lda")
pred_lda <- predict(fit_lda, test)
acc_lda <- mean(pred_lda == test$HeartDisease)
acc_lda

# rpart
fit_rpart <- train(HeartDisease ~ ., data = train, method = "rpart")
pred_rpart <- predict(fit_rpart, test)
acc_rpart <- mean(pred_rpart == test$HeartDisease)
acc_rpart

# Rborist
fit_Rborist <- train(HeartDisease ~ ., data = train, method = "Rborist")
pred_Rborist <- predict(fit_Rborist, test)
acc_Rborist <- mean(pred_Rborist == test$HeartDisease)
acc_Rborist

# GBM
fit_gbm <- train(HeartDisease ~ ., data = train, method = "gbm")
pred_gbm <- predict(fit_gbm, test)
acc_gbm <- mean(pred_gbm == test$HeartDisease)
acc_gbm

# Ensemble
ensemble <- tibble(glm = pred_glm, Rborist = pred_Rborist, gbm = pred_gbm)
ensemble <- ensemble %>% 
  mutate(final = as.numeric(glm) + as.numeric(Rborist) + as.numeric(gbm) - 3) %>%
  mutate(final = ifelse(final>1, 1, 0)) %>%
  mutate(final = as.factor(final))
acc_ensemble <- mean(ensemble$final == test$HeartDisease)

# RESULTS table
results <- tibble(method = "GLM", accuracy = acc_glm) %>% 
  bind_rows(tibble(method="kNN", accuracy = acc_knn)) %>%
  bind_rows(tibble(method="LDA", accuracy = acc_lda)) %>%
  bind_rows(tibble(method="rpart", accuracy = acc_rpart)) %>%
  bind_rows(tibble(method="Rborist", accuracy = acc_Rborist)) %>%
  bind_rows(tibble(method="GBM", accuracy = acc_gbm)) %>% 
  bind_rows(tibble(method="Ensemble", accuracy = acc_ensemble))
results %>% knitr::kable()

# Best method is Rborist
results %>% top_n(1) %>% knitr::kable()

# Importance of the features in Rborist method (best method)
varImp(fit_Rborist)$importance %>% arrange(desc(Overall)) %>% knitr::kable()

