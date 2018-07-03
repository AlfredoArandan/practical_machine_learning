# Project Practical Machine Learning

# The goal of your project is to predict the manner in which they did the exercise
setwd("C:/Users/alfre/Google Drive/cursos/Data Science Specialization/8-Practical Machine Learning/4-week_4/3-Course project")

# Reading the data
library(readr)
pml_training <- read_csv("pml-training.csv")
pml_testing <- read_csv("pml-testing.csv")

# Knowing the data
nrow(pml_training)
nrow(pml_testing)

# Significance statistic test and knowing the relevant predictors
# Anova analysis
# summary(pml_training)

# What kind of classe do we have?
table(pml_training$classe)

# To do an ANOVA statistiv test, we must change the "classe" variable with number 1,2,3,4 and 5
for(i in 1:nrow(pml_training)){
  if(pml_training$classe[i] == "A"){
    pml_training$new_classe[i] <- 1
  }
  if(pml_training$classe[i] == "B"){
    pml_training$new_classe[i] <- 2
  }
  if(pml_training$classe[i] == "C"){
    pml_training$new_classe[i] <- 3
  }
  if(pml_training$classe[i] == "D"){
    pml_training$new_classe[i] <- 4
  }
  if(pml_training$classe[i] == "E"){
    pml_training$new_classe[i] <- 5
  }
}


# Exploratory analysis
table(pml_training$new_classe)
hist(pml_training$new_classe, xlab = "Classe Variable", main = "Histogram of Classe variable")

# Selecting just the numeric columns
library("dplyr")
pml_training_numeric <- select_if(pml_training, is.numeric)

# Removing columns with NA
not_any_na <- function(x) all(!is.na(x))
pml_training_numeric_WNA <- pml_training_numeric %>% select_if(not_any_na)
pml_training_numeric_WNA$new_classe_factor <- as.factor(pml_training_numeric_WNA$new_classe)
pml_training_numeric_WNA_filter <- pml_training_numeric_WNA[-1]
pml_training_numeric_WNA_filter <- pml_training_numeric_WNA_filter[-54]

# ANOVA test
res.aov <- aov(new_classe ~ ., data = pml_training_numeric_WNA_filter)
summary(res.aov)


# If we analyze the p value results, we can set our significance statistic variables (rejecting the null hypothesis, P_value <= 0.001): 


# Models to predict the "Classe" variable
library(caret)
set.seed(30334)
trControl <- trainControl(method = "cv", number = 3)
pml_training_numeric_WNA_filter$new_classe_factor <- as.character(pml_training_numeric_WNA_filter$new_classe)
pml_training_numeric_WNA_filter <- pml_training_numeric_WNA_filter[-53]

# Random Forest
set.seed(30334)

rf <- train(new_classe_factor ~ ., data = pml_training_numeric_WNA_filter, 
            method = "rf", prox = TRUE, trControl = trControl)
print(rf)
print(rf$results)

# Accuracy cross validations respect to randomly selected predictors
plot(rf, pch=19,lty=6, lwd=2)

# Final model errors
plot(rf$finalModel, main = "Final Model")

# Importance of variables, top 10
plot(varImp(rf), top = 10)

# resample Histogram
resampleHist((rf))

# We observe that it can exist a overtfit in the model, however, this one was trained with the statical significant variables analyzed in the ANOVa test and a cross validation test was used. 

# Predictions
pred <- predict(rf, newdata = pml_testing)
print(pred)
pred_train <- predict(rf, newdata = pml_training)
pml_training$new_classe_factor <- as.factor(pml_training$new_classe)
confusionMatrix(pred_train, pml_training$new_classe_factor)

