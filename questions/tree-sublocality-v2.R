install.packages('performanceEstimation')
install.packages("psych")
install.packages("ggRandomForests")
library(psych) # for general functions
library(ggplot2) # for data visualization
library(caret) # for training and cross validation (also calls other model libraries)
library(rpart) # for trees
library(rpart.plot) # Enhanced tree plots
library(RColorBrewer) # Color selection for fancy tree plot
library(party) # Alternative decision tree algorithm
library(partykit) # Convert rpart object to BinaryTree
library(pROC) # for ROC curves
library(readr) # for reading in data
library(dplyr) # for data manipulation
library(corrplot) # for correlation plots
install.packages("kernlab")
library(kernlab) # for SVM
library(performanceEstimation) # for SMOTE
library(ggRandomForests) # for random forest plots

# Set the seed for reproducibility
set.seed(123)

## Get data ##
# Read the dataset from CSV file
df <- read_csv("dataset/NY-House-Dataset 2.csv")
df <- df[, -1]

# Data structure
head(df, 10)

# Sample descriptives
describe(df)

# Check for missing data
colSums(is.na(df))

# Visualization of the distribution of numerical variables
numeric_vars <- c("PRICE", "BEDS", "BATH", "PROPERTYSQFT")
par(mfrow = c(2, 2))
for (var in numeric_vars) {
  hist(df[[var]], main = var, xlab = var)
}
par(mfrow = c(1, 1))

# Correlation between numerical variables
correlation_matrix <- cor(df[numeric_vars])
corrplot(correlation_matrix, method = 'number', title = 'Correlation Matrix')

# Feature engineering
# Creation of interaction variables
interaction_vars <- c("BEDS:BATH", "BATH:PROPERTYSQFT")
df <- df %>%
  mutate(
    BEDS_BATH_interaction = BEDS * BATH,
    BATH_PROPERTYSQFT_interaction = BATH * PROPERTYSQFT
  )

# Count unique values in each column
sapply(df, function(x)
  n_distinct(x))

# Remove unused columns
df <- df[, !(
  names(df) %in% c(
    "ADDRESS",
    "STATE",
    "MAIN_ADDRESS",
    "ADMINISTRATIVE_AREA_LEVEL_2",
    "LOCALITY",
    "STREET_NAME",
    "LONG_NAME",
    "FORMATTED_ADDRESS"
  )
)]
df <- df[, !(names(df) %in% c("LATITUDE", "LONGITUDE", "BROKERTITLE"))]

# Filter the data based on selected variables
vars <- c("PRICE", "BEDS", "BATH", "PROPERTYSQFT")
df_filtered <- df

for (var in vars) {
  Q1 <- quantile(df_filtered[[var]], 0.25)
  Q3 <- quantile(df_filtered[[var]], 0.75)
  
  IQR <- Q3 - Q1
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  df_filtered <- df_filtered[df_filtered[[var]] >= lower_limit &
                               df_filtered[[var]] <= upper_limit, ]
}
df <- df_filtered

# Factorize
df$SUBLOCALITY <- as.factor(df$SUBLOCALITY)
df$TYPE <- as.factor(df$TYPE)

# Examine the distribution of values in the prediction column
value_counts <- table(df$SUBLOCALITY)

# Identify values that appear less than three times
rare_values <- names(value_counts[value_counts < 3])

# Create a new category for rare values and assign them a new value
new_category <- "Others"

# Add the new level to the categorical variable
df$SUBLOCALITY <- factor(df$SUBLOCALITY, levels = c(levels(df$SUBLOCALITY), new_category))

# Update the dataset by replacing rare values with the new value
df$SUBLOCALITY[df$SUBLOCALITY %in% rare_values] <- new_category
df$SUBLOCALITY <- droplevels(df$SUBLOCALITY)

# Visualization of the distribution of numerical variables
par(mfrow = c(2, 2))
for (var in numeric_vars) {
  hist(df[[var]], main = var, xlab = var)
}
par(mfrow = c(1, 1))

# Apply SMOTE to balance the classes
balancedDf <- performanceEstimation::smote(SUBLOCALITY ~ ., df, perc.over = 50, perc.under = 100)

# Remove missing values
balancedDf <- na.omit(balancedDf)

# Check the distribution of the target variable
table(balancedDf$SUBLOCALITY)
ggplot(balancedDf, aes(x = SUBLOCALITY)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Classes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

df <- balancedDf

# Correlation between numerical variables
correlation_matrix <- cor(df[numeric_vars], use = "complete.obs")
corrplot(correlation_matrix, method = 'number')

# Visualization of the distribution of categorical variables
categorical_vars <- c("SUBLOCALITY", "TYPE")
par(mfrow = c(1, 2))
for (var in categorical_vars) {
  barplot(table(df[[var]]), main = var)
}
par(mfrow = c(1, 1))

## Split the data so that we use 70% of it for training
train_index <- createDataPartition(y = df$SUBLOCALITY,
                                   p = 0.7,
                                   list = FALSE)

## Subset the data
training_set <- df[train_index, ]
testing_set <- df[-train_index, ]

## Define repeated cross validation with 5 folds and twenty repeats
repeat_cv <- trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 3,
  allowParallel = TRUE
)

## Define number of trees
ntree <- 500

## Classification tree
classification_tree <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set,
  method = "rpart2",
  trControl = repeat_cv,
  tuneLength = 10
)
classification_tree

# Plot the decision tree
plot(classification_tree, uniform = TRUE, compress = TRUE, main = "Classification Tree")
rpart.plot(classification_tree$finalModel, main = "Classification Tree", type = 4, extra = 0, xcompact = FALSE, ycompress = TRUE, compress = TRUE, under = TRUE)

# Predict the test set
predictions <- predict(classification_tree, newdata = testing_set, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set$SUBLOCALITY)

# Plot confusion matrix
confMatrix <- confusionMatrix(predictions, testing_set$SUBLOCALITY)
dfConfMatrix <- as.data.frame(confMatrix$table)
dfConfMatrix$Reference <- factor(dfConfMatrix$Reference, levels = rev(levels(dfConfMatrix$Reference)))
ggplot(data = dfConfMatrix, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual")


## Random forest ##
random_forest <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set,
  method = "rf",
  trControl = repeat_cv,
  ntree = ntree,
  tuneLength = 10
)
random_forest

# Plot the random forest
plot(random_forest, main = "Random Forest")
plot(caret::varImp(random_forest), main = "Variable Importance")
gg_dta<- gg_error(random_forest$finalModel)
plot(gg_dta)

# Predict the test set
predictions <- predict(random_forest, newdata = testing_set, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set$SUBLOCALITY)

# Plot confusion matrix
confMatrix <- confusionMatrix(predictions, testing_set$SUBLOCALITY)
dfConfMatrix <- as.data.frame(confMatrix$table)
dfConfMatrix$Reference <- factor(dfConfMatrix$Reference, levels = rev(levels(dfConfMatrix$Reference)))
ggplot(data = dfConfMatrix, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual")


## Bagging ##
bagging <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set,
  method = "treebag",
  trControl = repeat_cv,
  ntree = ntree,
  tuneLength = 10
)
bagging

# Plot the bagging
plot(varImp(bagging), main = "Variable Importance")

# Predict the test set
predictions <- predict(bagging, newdata = testing_set, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set$SUBLOCALITY)

# Plot confusion matrix
confMatrix <- confusionMatrix(predictions, testing_set$SUBLOCALITY)
dfConfMatrix <- as.data.frame(confMatrix$table)
dfConfMatrix$Reference <- factor(dfConfMatrix$Reference, levels = rev(levels(dfConfMatrix$Reference)))
ggplot(data = dfConfMatrix, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual")

## Boosting ##
gbmGrid <-  expand.grid(
  interaction.depth = 4,
  n.trees = ntree,
  shrinkage = 0.01,
  n.minobsinnode = 10
)

boosting <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set,
  method = "gbm",
  trControl = repeat_cv,
  verbose = FALSE,
  tuneLength = 10,
  tuneGrid = gbmGrid
)
boosting

# Predict the test set
predictions <- predict(boosting, newdata = testing_set, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set$SUBLOCALITY)

# Plot confusion matrix
confMatrix <- confusionMatrix(predictions, testing_set$SUBLOCALITY)
dfConfMatrix <- as.data.frame(confMatrix$table)
dfConfMatrix$Reference <- factor(dfConfMatrix$Reference, levels = rev(levels(dfConfMatrix$Reference)))
ggplot(data = dfConfMatrix, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual")


## Support Vector Machine ##
# Define the preprocessor
preProc <- preProcess(training_set, method = c("center", "scale", "zv"))

# Apply the preprocessor to the training and testing sets
training_set_preprocess <- predict(preProc, training_set)
testing_set_preprocess <- predict(preProc, testing_set)

svm <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set_preprocess,
  method = "svmRadial",
  trControl = repeat_cv,
  tuneLength = 10
)
svm

# Plot the SVM
plot(svm)

# Predict the test set
predictions <- predict(svm, newdata = testing_set_preprocess, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set_preprocess$SUBLOCALITY)

# Plot confusion matrix
confMatrix <- confusionMatrix(predictions, testing_set_preprocess$SUBLOCALITY)
dfConfMatrix <- as.data.frame(confMatrix$table)
dfConfMatrix$Reference <- factor(dfConfMatrix$Reference, levels = rev(levels(dfConfMatrix$Reference)))
ggplot(data = dfConfMatrix, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual")


## Compare models ##
results <- resamples(
  list(
    ClassificationTree = classification_tree,
    RandomForest = random_forest,
    Bagging = bagging,
    Boosting = boosting,
    SVM = svm
  )
)
summary(results)

# Plot the results
bwplot(results, metric = "Accuracy", main = "Model Comparison")