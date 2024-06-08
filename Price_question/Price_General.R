library(tidyverse)
library(corrplot)
library(caret)
library(e1071)
library(readr)
library(boot)
library(dplyr)
library(caret)
library(foreach)
library(gam)
library(akima)
library ( ISLR2 )
library( glmnet )
library(fastDummies)
library(knitr)
library(kableExtra)
library(dplyr)
library(geosphere)
library(ggplot2)

library(car)


# load dataset and set seed

set.seed(123)
rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot
df <- read_csv("dataset/NY-House-Dataset Price_clean.csv")
df <- df[, -1]


#///////////////////////////////////////////

# Calcola la matrice di correlazione
cor_matrix <- cor(df)

# Estrai la colonna di correlazioni con 'PRICE'
cor_with_price <- cor_matrix[, "PRICE"]

# Identifica le variabili con correlazione assoluta con 'PRICE' inferiore a 0.1
low_cor_vars <- names(cor_with_price[abs(cor_with_price) < 0.1])

# Rimuovi le variabili identificate dal dataset
df <- df[, !colnames(df) %in% low_cor_vars]

# Verifica il risultato
print(colnames(df))


#///////////////////////////////////////////


lm_fit<- lm(log(PRICE) ~ ., data = df)
vif(lm_fit)
a<-alias(lm_fit)
a$Complete
df <- df[, !(names(df) %in% c("SUBLOCALITY_Others","TYPE_Others", "BROKERTITLE_Others"))]



#///////////////////////////////////////////////

#dataset histogram
dev.new()
par(mfrow = c(2, 2))
hist(df$PRICE)
hist(df$BEDS)
hist(df$BATH)
hist(df$PROPERTYSQFT)


#dataset summary
summary(df$PRICE)
summary(df$BATH)
summary(df$BEDS)
summary(df$PROPERTYSQFT)


#dataset correlation


ggplot(df, aes(x = BEDS, y = PRICE)) +
  geom_point(aes(color = PRICE, size = PRICE)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Houses Price vs number of beds ",
       x = "Number of beds",
       y = "Houses Price (USD)") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(df, aes(x = BATH, y = PRICE)) +
  geom_point(aes(color = PRICE, size = PRICE)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Houses price vs number of baths",
       x = "Number of baths",
       y = "Houses price(USD)") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(df, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point(aes(color = PRICE, size = PRICE)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Houses price vs propertysqft",
       x = "Propertysqft",
       y = "Houses price (USD)") +
  theme_minimal() +
  theme(legend.position = "right")


#//////////////////////////////////////////////////////////////////////////////
# train 70% 30% test(-train)
train <- sample(dim(df)[1], floor(dim(df)[1] * 0.70), replace = FALSE)

#linear regression 
lm_fit<- lm(log(PRICE) ~ ., data = df[train,])

summary(lm_fit)
dev.new()
par(mfrow = c(2, 2))
plot(lm_fit)


log_predictions <- predict(lm_fit, newdata = df[-train, ])
log_true_values <- log(df$PRICE[-train])
correlation_log <- cor(log_predictions, log_true_values)
rmse_log <- sqrt(mean((log_true_values - log_predictions)^2))
predicted_values <- exp(log_predictions)
true_values <- df$PRICE[-train]
correlation_real <- cor(predicted_values, true_values)
rmse_real <- sqrt(mean((true_values - predicted_values)^2))

dev.new()
plot(log_predictions, log_true_values, xlab = "Predictions linear", ylab = "Actual Values",
     main = "Comparison between Predictions and Actual Values (Linear Regression)")
abline(a = 0, b = 1, col = "red")


#SHRINKAGE METHODS

x <- model.matrix(PRICE ~ ., df)
y <- log(df$PRICE)  # Applicare la trasformazione logaritmica

# Ridge regression
cv_model <- cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10)
dev.new()
plot(cv_model)
opt_lambda <- cv_model$lambda.min

#ridge regression correlation and fitted values
model <- glmnet(x[train,], y[train], alpha = 0, lambda = opt_lambda, standardize = TRUE)

fitt_value <- predict(model, s=opt_lambda, newx = x[-train,])
true_values <- y[-train]
rmse_ridge <- sqrt(mean((true_values - fitt_value)^2))
correlation_ridge <- cor(fitt_value, true_values)

fitt_value_real <- exp(fitt_value)
true_values_real <- exp(true_values)
rmse_ridge_real <- sqrt(mean((true_values_real - fitt_value_real)^2))
correlation_ridge_real <- cor(fitt_value_real, true_values_real)

dev.new()
plot(log_predictions, log_true_values, xlab = "Predictions Ridge", ylab = "Actual Values",
     main = "Comparison between Predictions and Actual Values (Ridge Regression)")
abline(a = 0, b = 1, col = "red")

#Lasso regression
cv_lasso <- cv.glmnet(x[train,],y[train],alpha=1,nfolds = 10);
dev.new()
plot(cv_lasso)
opt_lambda <- cv_lasso$lambda.min;

#lasso regression correlation and fitted values
model <- glmnet(x[train,],y[train],alpha = 1,lambda = opt_lambda, standardize = TRUE)


fitt_value <- predict(model,s=opt_lambda, newx=x[-train,])
true_values <- y[-train]
rmse_lasso = sqrt(mean((true_values - fitt_value)^2))
correlation_lasso <- cor(fitt_value, true_values)

fitt_value_real <- exp(fitt_value)
true_values_real <- exp(true_values)
rmse_lasso_real <- sqrt(mean((true_values_real - fitt_value_real)^2))
correlation_lasso_real <- cor(fitt_value_real, true_values_real)


dev.new()
plot(log_predictions, log_true_values, xlab = "Predictions Lasso", ylab = "Actual Values",
     main = "Comparison between Predictions and Actual Values (Lasso Regression)")
abline(a = 0, b = 1, col = "red")

#GAMs


gam_model <- gam(log(PRICE) ~ s(BEDS,4) + s(BATH,4)+poly(PROPERTYSQFT,4)+., data = df[train,]);


#gams correlation and fitted values
fitt_value <- predict(gam_model,newdata = df[-train,])
true_values <- log(df$PRICE[-train])
err = (true_values - fitt_value)^2
rmse_GAMs = sqrt(mean(err[-train]))
correlation_GAMs <- cor(fitt_value, true_values)


fitt_value_real <- exp(fitt_value)
true_values_real <- exp(true_values)
rmse_GAMs_real <- sqrt(mean((true_values_real - fitt_value_real)^2))
correlation_GAMs_real <- cor(fitt_value_real, true_values_real)

dev.new()
plot(log_predictions, log_true_values, xlab = "Predictions GAMs", ylab = "Actual Values",
     main = "Comparison between Predictions and Actual Values (GAMs)")
abline(a = 0, b = 1, col = "red")


#SVR

standardize_dataset <- function(df) {
  # Separazione della variabile target
  target <- df$PRICE
  predictors <- df[, colnames(df) != "PRICE"]
  
  # Centrare e scalare le variabili predittive
  scaler <- preProcess(predictors, method = c("center", "scale"))
  predictors_scaled <- predict(scaler, predictors)
  
  # Combinazione delle variabili scalate con la variabile target
  df_scaled <- cbind(predictors_scaled, PRICE = target)
  
  return(list(scaled_data = df_scaled, scaler = scaler))
}

# Applicazione della standardizzazione all'intero dataset
standardized <- standardize_dataset(df)
df <- standardized$scaled_data

train_data <- df[train, ]
test_data <- df[-train, ]
Y_train <- log(train_data$PRICE)
X_train <- train_data[, -which(names(train_data) == "PRICE")]
Y_test <- log(test_data$PRICE)
X_test <- test_data[, -which(names(test_data) == "PRICE")]
svr_model <- svm(x = X_train, y = Y_train, kernel = "radial", gamma = 0.1, cost = 1)
predictions <- predict(svr_model, X_test)
rmse <- sqrt(mean((predictions - Y_test)^2))
correlation_SVR <- cor(predictions, Y_test)




#valutazione

   
data <- data.frame(
  Method = c("Linear", "Lasso", "Ridge", "GAMs"),
  Correlation_Log = round(c(correlation_log, correlation_lasso, correlation_ridge, correlation_GAMs), 3),
  Correlation = round(c(correlation_real, correlation_lasso_real, correlation_ridge_real, correlation_GAMs_real), 3),
  RMSE_Test = round(c(rmse_real, rmse_lasso_real, rmse_ridge_real, rmse_GAMs_real))
)

# Ensure RMSE_Test is displayed as integer
data$RMSE_Test <- as.integer(data$RMSE_Test)

table <- kable(data, format = "html", caption = "Correlation and RMSE Test") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:4, width = "4cm") %>%
  add_header_above(c(" " = 1, "Metrics" = 3))

# Display the table
table