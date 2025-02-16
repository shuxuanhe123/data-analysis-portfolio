rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data_raw<- read.csv("shielder@caitnguyen.csv")
data <- data_raw[complete.cases(data_raw),]
library(rpart)
library(MASS)
library(rpart.plot)
library(leaps)
library(ggplot2)
library(lattice)
library(Matrix)
library(caret)
library(glmnet)

# 2. Partition the dataset
set.seed(123)
target_variable <- 'TARGET'
index <- createDataPartition(data$TARGET, p = 0.8, list = FALSE)

# Create training and test sets
train_data <- data[index, ]
test_data <- data[-index, ]

# 3. Train the decision tree model
tree_model <- rpart(TARGET ~ ., data = train_data, method = "class")

# 4. Visualize the decision tree
rpart.plot(tree_model)

# Make predictions
predictions <- predict(tree_model, test_data, type = "class")

# Convert to factor ensuring that both have the same levels
predictions <- as.factor(predictions)
test_data[[target_variable]] <- as.factor(test_data[[target_variable]])

# Make sure that both factors have the same levels
levels(predictions) <- levels(test_data[[target_variable]])

# Now, use the confusionMatrix function
confusion_matrix <- confusionMatrix(predictions, test_data$TARGET)
accuracy <- confusion_matrix$overall['Accuracy']
print(accuracy)
print(confusion_matrix$table)

#accuracy and the confusion matrix
#The accuracy of the decision tree model is approximately 63.94%.
#The confusion matrix shows that the model predicted 3467 true negatives and 935 true positives.
#The high rate of false positives suggests that non-defaults may be mistakenly classified as defaults by the model. This could result in pointless inquiries or the refusal of loans to prospective borrowers who might not truly be at danger.
#Even though there are fewer false negatives than false positives, the amount is nevertheless noteworthy. This implies that some defaults may go unnoticed by the model, which could lead to monetary losses.

#Primary factors influencing loan default
#Debt-to-Income Ratio: A high debt-to-income ratio might be a significant predictor of default because it indicates that a borrower has a substantial amount of debt compared to their income.
#Credit History: Borrowers with a history of late payments or defaults could be more likely to default again.
#Loan Characteristics: The terms of the loan could influence defaults.
#Economic Factors: Macro-economic indicators like unemployment rates could also be predictive since they affect borrowers' ability to repay.
#Borrower’s Income: Lower or unstable income might predict higher default rates.

# PART 2
# Regression task for predicting 'AMT_INCOME_TOTAL'
# Prepare the dataset for regression (without the 'TARGET' column)
train_data_regression <- train_data[, !(names(train_data) %in% c('TARGET'))]
test_data_regression <- test_data[, !(names(test_data) %in% c('TARGET'))]

# 1. Scale the training set for regression
preprocess_params <- preProcess(train_data_regression[, !(names(train_data_regression) %in% c('AMT_INCOME_TOTAL'))], method = c("center", "scale"))
train_data_scaled <- predict(preprocess_params, train_data_regression)
test_data_scaled <- predict(preprocess_params, test_data_regression)

# Ensure 'AMT_INCOME_TOTAL' is numeric for regression
train_data_scaled$AMT_INCOME_TOTAL <- as.numeric(train_data_regression$AMT_INCOME_TOTAL)
test_data_scaled$AMT_INCOME_TOTAL <- as.numeric(test_data_regression$AMT_INCOME_TOTAL)

# Set up cross-validation for model tuning
control <- trainControl(method = "cv", number = 10)

# Train the lasso regression model (alpha = 1)
lasso_model <- train(
  AMT_INCOME_TOTAL ~ ., 
  data = train_data_scaled, 
  method = "glmnet", 
  trControl = control,
  tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-3, -1, length = 10))
)

# Train the ridge regression model (alpha = 0)
ridge_model <- train(
  AMT_INCOME_TOTAL ~ ., 
  data = train_data_scaled, 
  method = "glmnet", 
  trControl = control,
  tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3, -1, length = 10))
)

# Make predictions with both models
lasso_predictions <- predict(lasso_model, test_data_scaled)
ridge_predictions <- predict(ridge_model, test_data_scaled)

# Calculate RMSE for both models
lasso_rmse <- RMSE(lasso_predictions, test_data_scaled$AMT_INCOME_TOTAL)
ridge_rmse <- RMSE(ridge_predictions, test_data_scaled$AMT_INCOME_TOTAL)

print(lasso_rmse)
print(ridge_rmse)

#significant predictors
#When using lasso regression, certain coefficients may be reduced to zero, emphasizing the least significant factors.
#Larger absolute values of coefficients indicate significant predictors, which have a greater impact on the prediction.

#Potential Implications:
#To reduce the RMSE, the regression models might be adjusted further or different modeling strategies could be applied.
#Regression model feature importance can help determine which variables are important to pay attention to while developing marketing plans or risk assessments.





