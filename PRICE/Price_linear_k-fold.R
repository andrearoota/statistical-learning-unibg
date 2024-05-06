install.packages('caret')
library(readr)
library(boot)
library(dplyr)
library(caret)


# Set the seed for reproducibility
set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2 - clean.csv")
df <- df[, -1]

lm_fit <- lm(PRICE ~ ., data = df)
control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
cv_results <- train(PRICE ~ ., data = df, method = "lm", trControl = control)
print(cv_results$pred$pred)