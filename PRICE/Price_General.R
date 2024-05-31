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
#reference_point <- c(-74.0060, 40.7128)

# Calcolo della distanza dal punto di riferimento

#df <- df %>%
#  mutate(DISTANZA = distHaversine(cbind(LONGITUDE, LATITUDE), reference_point))
#df <- df %>%
#  mutate(DISTANZA = DISTANZA/1000)

#cor(df$PRICE,df$LONGITUDE)
#cor(df$PRICE,df$LATITUDE)
#cor(df$PRICE,df$DISTANZA)

#dataset histogram
dev.new()
par(mfrow = c(2, 2))
hist(df$PRICE)
hist(df$BEDS)
hist(df$BATH)
hist(df$PROPERTYSQFT)
hist(df$DISTANZA)

#dataset summary
summary(df$PRICE)
summary(df$BATH)
summary(df$BEDS)
summary(df$PROPERTYSQFT)
summary(df$DISTANZA)

#dataset correlation
cor_matrix <- cor(df)
cor_matrix_subset <- cor_matrix
cor_matrix_subset[abs(cor_matrix) <= 0.05 ] <- NA

#dataset grafici

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



# train 70% 30% test(-train)
train <- sample(dim(df)[1],floor(dim(df)[1]*0.70),replace = FALSE);

#linear regression
lm_fit <- lm(log(PRICE) ~ ., data = df[train,])

#linear regression results
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
plot(log_predictions, log_true_values, xlab = "Previsioni linear", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri (linear)")
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
plot(fitt_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri (ridge)")
abline(a = 0, b = 1, col = "red")

#lasso regression
cv_lasso <- cv.glmnet(x[train,],y[train],alpha=1,nfolds = 10);
dev.new()
plot(cv_lasso)
opt_lambda <- cv_lasso$lambda.min;

#lasso regression correlation and fitted values
model <- glmnet(x[train,],y[train],alpha = 1,lambda = opt_lambda)


fitt_value <- predict(model,s=opt_lambda, newx=x[-train,])
true_values <- y[-train]
rmse_lasso = sqrt(mean((true_values - fitt_value)^2))
correlation_lasso <- cor(fitt_value, true_values)

fitt_value_real <- exp(fitt_value)
true_values_real <- exp(true_values)
rmse_lasso_real <- sqrt(mean((true_values_real - fitt_value_real)^2))
correlation_lasso_real <- cor(fitt_value_real, true_values_real)


dev.new()
plot(fitt_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri (lasso)")
abline(a = 0, b = 1, col = "red")


#GAMs

gam_model <- gam(log(PRICE) ~ s(BEDS,4) + s(BATH,4)+., data = df[train,]);

#gams correlation and fitted values
fitt_value <- predict(gam_model,newdata = df[-train,])
true_values <- log(df$PRICE[-train])
err = (log(df$PRICE) - predict(gam_model, df))^2
rmse_GAMs = sqrt(mean(err[-train]))
correlation_GAMs <- cor(fitt_value, true_values)


fitt_value_real <- exp(fitt_value)
true_values_real <- exp(true_values)
rmse_GAMs_real <- sqrt(mean((true_values_real - fitt_value_real)^2))
correlation_GAMs_real <- cor(fitt_value_real, true_values_real)

dev.new()
plot(fitt_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri (GAMs)")
abline(a = 0, b = 1, col = "red")


#SVR

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
    Correlation = round(c(correlation_linear,correlation_lasso,correlation_ridge,correlation_GAMs),3),
    RMSE_Test = round(c(rmse_linear,rmse_lasso,rmse_ridge,rmse_GAMs),3)
  )
  
  data$RMSE_Test <- sprintf("%.3f  $", data$RMSE_Test)
  
  table <- kable(data, format = "html", caption = "Correlation and RMSEtest") %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:3, width = "4cm") %>%
    add_header_above(c(" " = 1, Metrics="2"))
  
  # Visualizzazione della tabella
  table