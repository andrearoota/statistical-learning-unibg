
library(caret)
library(e1071)
library(readr)
library(randomForest)
library(tidyverse)

set.seed(123)
rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot
df <- read_csv("dataset/NY-House-Dataset Price_clean.csv")
df <- df[, -1]

# Visualizzazione istogrammi
dev.new()
par(mfrow = c(2, 2))
hist(df$PRICE, main = "Histogram of PRICE", xlab = "PRICE")
hist(df$BEDS, main = "Histogram of BEDS", xlab = "BEDS")
hist(df$BATH, main = "Histogram of BATH", xlab = "BATH")
hist(df$PROPERTYSQFT, main = "Histogram of PROPERTYSQFT", xlab = "PROPERTYSQFT")




# Dividere il dataset in training e test set
set.seed(123)
train_indices <- sample(dim(df)[1],floor(dim(df)[1]*0.70),replace = FALSE);
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Creare il modello Random Forest
repeat_cv <- trainControl(
  method = 'repeatedcv',
  number = 20,
  repeats = 3,
  allowParallel = TRUE
)
random_forest <- train(
  PRICE ~ .,
  data = train_data,
  method = "rf",
  trControl = repeat_cv,
  ntree = ntree,
  tuneLength = 50
)
random_forest
plot(random_forest)

# Prevedere i valori di PRICE nel test set
predictions <- predict(rf_model, newdata = test_data)

# Calcolare RMSE
actual <- test_data$PRICE
rmse <- sqrt(mean((predictions - actual)^2))
print(paste("RMSE:", rmse))

# Importanza delle variabili
importance(rf_model)
varImpPlot(rf_model)
