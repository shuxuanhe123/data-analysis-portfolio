# Clear the environment and set working directory ----
rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load necessary libraries ----
library(randomForest)
library(randomForestExplainer)
library(readr)
library(caret)
library(glmnet)
library(MASS)
library(leaps)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(Matrix)
library(caret)

# Read in the data ----
data <- read.csv("Streamline@caitnguyen.csv")
data2 <- read.csv("Streamline_testing.csv")

# Removing unnecessary columns ----
data <- data[, !(names(data) %in% c("title", "status", "runtime", "adult", "original_language",
                                    "overview", "genres", "production_companies", 
                                    "production_countries", "spoken_languages", 
                                    "Director", "Writer", "Actors", "Awards",
                                    "RottenTomatoesRating", "vote_average", "vote_count",
                                    "Metascore", "imdbRating", "imdbVotes", "BoxOffice",
                                    "popularity"))]

data2 <- data2[, !(names(data2) %in% c("title", "status", "runtime", "adult", "original_language",
                                       "overview", "genres", "production_companies", 
                                       "production_countries", "spoken_languages", 
                                       "Director", "Writer", "Actors", "Awards", 
                                       "RottenTomatoesRating"))]

# Preparing for tree and forest ----
set.seed(123)
trainIndex <- createDataPartition(data$revenue, p = .8, list = FALSE, times = 1)
dataTrain <- data[trainIndex,]
dataValid <- data2

# Address missing values ----
dataTrain <- na.omit(dataTrain)

# For numerical columns, fill NA with the median of the column
numeric_columns <- sapply(dataTrain, is.numeric)
dataTrain[numeric_columns] <- lapply(dataTrain[numeric_columns], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# For categorical columns, fill NA with the mode or a placeholder
categorical_columns <- sapply(dataTrain, is.factor)
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
dataTrain[categorical_columns] <- lapply(dataTrain[categorical_columns], function(x) ifelse(is.na(x), get_mode(x), x))

# Decision Tree Model ----
# Train the decision tree model for regression
tree_model <- rpart(revenue ~ ., data = dataTrain, method = "anova", 
                    control = rpart.control(cp = 0.001))

# Find all levels present in both the training and validation sets
all_levels <- union(levels(dataTrain$Rated), levels(dataValid$Rated))

# Set the levels of the 'Rated' factor in both datasets to be the same
dataTrain$Rated <- factor(dataTrain$Rated, levels = all_levels)
dataValid$Rated <- factor(dataValid$Rated, levels = all_levels)

# Prune the tree based on cross-validation error
optimal_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree_model, cp = optimal_cp)

# Predict on validation set
pruned_pred <- predict(pruned_tree, dataValid)
pruned_pred <- as.numeric(pruned_pred)
# Ensure 'revenue' is numeric
dataValid$revenue <- as.numeric(dataValid$revenue)

# Check if 'pruned_pred' and 'dataValid$revenue' are of the same length
if(length(pruned_pred) != nrow(dataValid)) {
  stop("The length of predictions and actual values does not match.")
}

# Check for NAs in predictions or actual values
if(any(is.na(pruned_pred)) || any(is.na(dataValid$revenue))) {
  stop("There are missing values in predictions or actual revenue data.")
}
# Calculate RMSE
RMSE_tree <- sqrt(mean((pruned_pred - dataValid$revenue)^2))

#pruned tree
rpart.plot(pruned_tree, main = "Pruned Regression Tree for Movie Revenue", under = TRUE)

# Display predictions and RMSE
predictions_df_tree <- data.frame(
  Actual = dataValid$revenue,
  Tree_Predicted = pruned_pred)
print(head(predictions_df_tree))
cat("Root Mean Squared Error (RMSE) for Decision Tree: ", RMSE_tree, "\n")

#Random Forest ----

forest1 <- randomForest(revenue ~ ., data = dataTrain)
forest2 <- randomForest(revenue ~ ., data = dataTrain, ntree = 600)
forest3 <- randomForest(revenue ~ ., data = dataTrain, mtry = 3)
pred_forest1 <- predict(forest1, dataValid)
pred_forest2 <- predict(forest2, dataValid)
pred_forest3 <- predict(forest3, dataValid)
RMSE_forest1 <- sqrt(mean((pred_forest1 - dataValid$revenue)^2))
RMSE_forest2 <- sqrt(mean((pred_forest2 - dataValid$revenue)^2))
RMSE_forest3 <- sqrt(mean((pred_forest3 - dataValid$revenue)^2))
print(head(data.frame(Actual = dataValid$revenue, Forest1_Predicted = pred_forest1, Forest2_Predicted = pred_forest2, Forest3_Predicted = pred_forest3)))
cat("RMSE for Random Forest 1: ", RMSE_forest1, "\n")
cat("RMSE for Random Forest 2: ", RMSE_forest2, "\n")
cat("RMSE for Random Forest 3: ", RMSE_forest3, "\n")

#new dataset ----
data2$revenue <- predictions_df_forest$forest1_Predicted
data2 <- data2[, !(names(data2) %in% c("Rated"))]
write.csv(data2, file = "/Users/caitlin/Desktop/Streamline@caitnguyen/Streamline_testing.csv")
#RandomForestPeer ----
# Set seed for reproducibility
set.seed(123)
# Train the Random Forest model
rf_model <- randomForest(revenue ~ ., data = dataTrain, ntree = 1000, nodesize = 5)
# Predict on validation set and calculate RMSE
predictions <- predict(rf_model, dataValid)
RMSE <- sqrt(mean((predictions - dataValid$revenue)^2))
print(paste("Root Mean Squared Error (RMSE):", RMSE))
# Visualize significant features
forest_explainer <- create_rf_explainer(rf_model)
plot_min_depth_distribution(forest_explainer)


#IMDB Rating ----
data <- read.csv("steamline@caitnguyen.csv")
data <- data[, !(names(data)%in% c("title","status","runtime", "adult","original_language",
                                    "overview", "genres", "production_companies", 
                                    "production_countries", "spoken_languages", 
                                    "Director", "Writer", "Actors", "Awards",
                                    "vote_average","vote_count","Metascore", 
                                   "imdbVotes","BoxOffice","popularity"))]
# Creating a binary classification for revenue prediction based on IMDb rating
data$revenue_prediction <- data$imdbRating >= 7 
data$revenue_prediction <- as.factor(data$revenue_prediction)
set.seed(123)
trainIndex <- createDataPartition(data$revenue_prediction, p = 0.8, list = FALSE, times = 1)
trainingData <- data[trainIndex, ]
validationData <- data2
randomForestModel <- randomForest(revenue_prediction ~ ., data = trainingData, method = "class")
validationData$imdbRating <- 0
forestPredictions <- predict(randomForestModel, validationData)
comparisonDataFrame <- data.frame(
  ActualRevenue = validationData$revenue,
  PredictedRevenue_RF = forestPredictions
)
comparisonDataFrame

