# Load required libraries
library(readr)
library(boot)
library(dplyr)
library(fastDummies)

# Set the seed for reproducibility


set.seed(123)
rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

df <- read_csv("dataset/NY-House-Dataset 2.csv")
df <- df[, -1]
df <- df[, -1]
df <- df[, !(names(df) %in% c("ADDRESS", "STATE", "MAIN_ADDRESS", "ADMINISTRATIVE_AREA_LEVEL_2", "LOCALITY", "STREET_NAME", "LONG_NAME", "FORMATTED_ADDRESS"))]

vars <- c("PRICE","BEDS","BATH","PROPERTYSQFT")
df_filtered <- df

for(var in vars){
  Q1 <- quantile(df_filtered[[var]], 0.25)
  Q3 <- quantile(df_filtered[[var]], 0.75)
  
  IQR <- Q3-Q1
  lower_limit <- Q1-2*IQR
  upper_limit <- Q3+2*IQR
  
  df_filtered <- df_filtered[df_filtered[[var]] >= lower_limit & df_filtered[[var]] <= upper_limit, ]
  
}


df <- dummy_cols(df_filtered)
df <- df[, !(names(df) %in% c("TYPE", "SUBLOCALITY"))]


correlation_matrix <- cor(df)[, "PRICE"]
low_corr_vars <- names(correlation_matrix[abs(correlation_matrix) < 0.05])
df <- df[, !(names(df) %in% low_corr_vars)]


## TEST 30 - TRAIN 70

train <- sample(dim(df)[1], round(dim(df)[1]*0.7))
lm_fit <- lm(PRICE ~ ., data = df[train,])
summary(lm_fit)
dev.new()
par(mfrow = c(2, 2))
plot(lm_fit)

#MSE TEST E TRAIN

err = (df$PRICE - predict(lm_fit, df))^2
training_err = mean(err[train])
test_err = mean(err[-train])


#PREVISIONI SUL SET DI TEST CONTRO I VALORI REALI

test_predictions <- predict(lm_fit, newdata = df[-train, ])
true_values <- df$PRICE[-train]
correlation <- cor(test_predictions, true_values) #correlazione calcolata tra le previsioni fatte dal modello sul set di test e i valori reali del set.
print(correlation)
dev.new()
plot(test_predictions, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri")
abline(a = 0, b = 1, col = "red")


# Confidence intervals on coefficients

confint(lm_fit, level = 0.95) # ATTENZIONE!! ALCUNI COEFFICIENTI HANNO ZERO DENTRO INTERVALLO DI CONFIDENZA!!
dev.new()
plot(predict(lm_fit)) # SONO PRESENTI PATTERN -> non linearità nei dati
dev.new()
plot(predict(lm_fit), rstudent(lm_fit))




