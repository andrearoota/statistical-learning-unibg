# Load required libraries
library(readr)

# Set the seed for reproducibility
set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2 - clean.csv")
df <- df[, -1]

## Test 30 - Train 70
train <- sample(dim(df)[1], round(dim(df)[1]*0.7))

##### LINEAR MULTIPLE REGRESSION ##### 
lm_fit <- lm(PRICE ~ ., data = df[train,])
summary(lm_fit)

# Confidence intervals on coefficients
confint(lm_fit, level = 0.95) # ATTENZIONE!! ALCUNI COEFFICIENTI HANNO ZERO DENTRO INTERVALLO DI CONFIDENZA!!
dev.new()
plot(predict(lm_fit), residuals(lm_fit)) # SONO PRESENTI PATTERN -> non linearità nei dati
dev.new()
plot(predict(lm_fit), rstudent(lm_fit))

#MSE