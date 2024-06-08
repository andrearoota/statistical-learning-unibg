
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
df <- df[, !(names(df) %in% c("BROKERTITLE_Brokered by 5 Boro Realty Corp"))]



cor_matrix <- cor(df)

# Estrai la colonna di correlazioni con 'PRICE'
cor_with_price <- cor_matrix[, "PRICE"]

# Identifica le variabili con correlazione assoluta con 'PRICE' inferiore a 0.1
low_cor_vars <- names(cor_with_price[abs(cor_with_price) < 0.2])

# Rimuovi le variabili identificate dal dataset
df <- df[, !colnames(df) %in% low_cor_vars]

# Verifica il risultato
print(colnames(df))


library(randomForest)




set.seed(123) 
train_indices <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]



common_cols <- intersect(colnames(train_data), colnames(test_data))
train_data <- train_data[, common_cols]
test_data <- test_data[, common_cols]
print(train_data)
print(test_data)

#Random Forest
rf_model <- randomForest(log(PRICE) ~ BATH+BEDS+PROPERTYSQFT+(BEDS*BATH), data = train_data)

# predictions
predictions <- predict(rf_model, newdata = test_data)
actual <- log(test_data$PRICE)

#RMSE
correlation <- cor(predictions,actual)
rmse <- sqrt(mean((predictions - actual)^2))
print(rmse)
