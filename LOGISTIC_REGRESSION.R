#The purpose of this code is to perform logistic regression on a credit card transaction dataset stored in a CSV file named "creditcard.csv" using R programming language

Credit_details <- read.csv("creditcard.csv")

head(Credit_details)
str(Credit_details)


# Checking the Summary of the data.
summary(Credit_details)

install.packages("caTools")
library(caTools)

# checking before building a model 

set.seed(101)
# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(Credit_details$Amount, SplitRatio = 0.70) 

# Training Data
train = subset(Credit_details, sample == TRUE)

# Testing Data
test = subset(Credit_details, sample == FALSE)

summary(train)

##Logistic model
Log_Mod <- glm(class ~ Amount, family = binomial(logit), data = train)
summary(Log_Mod)


# Predict the binary outcome for the test set
predicted <- predict(Log_Mod, newdata = test, type = "response")

# Convert predicted probabilities to binary outcomes (0 or 1) using a threshold of 0.5
predicted_binary <- ifelse(predicted > 0.5, 1, 0)

accuracy <- sum(predicted_binary == test$class) / nrow(test)


cat("Accuracy:", round(accuracy, 3), "\n")

##The code provides insights into the process of performing logistic regression for binary classification in R 
##specifically for predicting fraudulent credit card transactions using transaction amount as a predictor variable.

