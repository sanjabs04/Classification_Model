#The code provides insights into the process of training, evaluating, and tuning SVM models 
#for classification using R programming language.
#libraries
library(kernlab)
library(caret)
 
## Step 2: Exploring and preparing the data ----
# read in data and examine structure
letters <- read.csv("letterdata.csv")
str(letters)
table(letters$letter)


letters$letter <- as.factor(letters$letter)


# divide into training and test data
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

#lets set a seed to reproduce results
set.seed(123)


# begin by training a simple linear SVM

letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot", scaled=TRUE)

# look at basic information about the model
letter_classifier

## Step 4: Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)

table(letter_predictions, letters_test$letter)

# look only at agreement vs. non-agreement
# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

#use caret confusion matrix
confusionMatrix(letter_predictions,letters_test$letter) #83.9% AVG Accuracy

## Step 5: Improving model performance ----
set.seed(123)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_classifier_rbf
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

#performance of radial basis function kernel
confusionMatrix(letter_predictions_rbf,letters_test$letter) 
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

#the greater C the more flexible or higher variance
set.seed(123)
letter_classifier_rbf_c25 <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot",C=.25)
letter_predictions_rbf_c25 <- predict(letter_classifier_rbf_c25, letters_test)
confusionMatrix(letter_predictions_rbf_c25,letters_test$letter) 

