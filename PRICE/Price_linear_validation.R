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
par(mfrow = c(2, 2))
plot(lm_fit)

# Confidence intervals on coefficients

confint(lm_fit, level = 0.95) # ATTENZIONE!! ALCUNI COEFFICIENTI HANNO ZERO DENTRO INTERVALLO DI CONFIDENZA!!
dev.new()
plot(predict(lm_fit)) # SONO PRESENTI PATTERN -> non linearità nei dati
dev.new()
plot(predict(lm_fit), rstudent(lm_fit))

#MSE

err = (df$PRICE - predict(lm_fit, df))^2
training_err = mean(err[train])
test_err = mean(err[-train])

#correlazione calcolata tra le previsioni fatte dal modello sul set di test e i valori reali del set.

test_predictions <- predict(lm_fit, newdata = df[-train, ])
true_values <- df$PRICE[-train]
correlation <- cor(test_predictions, true_values)
print(correlation)



