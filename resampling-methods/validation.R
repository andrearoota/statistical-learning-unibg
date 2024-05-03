install.packages("fastDummies")

library(readr)
library(fastDummies)

set.seed(123) # Imposta il seed per la riproducibilità

df <- read_csv("dataset/NY-House-Dataset 2.csv")

## Missing data
missing_values <- colSums(is.na(df))
print(missing_values)

## Remove unused column
df <- df[, !(names(df) %in% c("ADDRESS", "STATE", "MAIN_ADDRESS", "ADMINISTRATIVE_AREA_LEVEL_2", "LOCALITY", "STREET_NAME", "LONG_NAME", "FORMATTED_ADDRESS"))]

## Filter data

vars <- c("PRICE","BEDS","BATH","PROPERTYSQFT")
df_filtered <- df

for (var in vars){
  
  Q1 <- quantile(df_filtered[[var]], 0.25)
  Q3 <- quantile(df_filtered[[var]], 0.75)
  
  IQR <- Q3-Q1
  lower_limit <- Q1-1.5*IQR
  upper_limit <- Q3+1.5*IQR
  
  df_filtered <- df_filtered[df_filtered[[var]] >= lower_limit & df_filtered[[var]] <= upper_limit, ]
  
}

## Dummies
# Trasformazione delle variabili categoriche in dummy variables
df <- dummy_cols(df_filtered)

# Rimozione delle colonne originali delle variabili categoriche
df <- df[, !(names(df) %in% c("BROKERTITLE", "TYPE", "SUBLOCALITY"))]

## Test 30 - Train 70
train <- sample(dim(df)[1], round(dim(df)[1]*0.7))

##### LINEAR MULTIPLE REGRESSION ##### 
lm_fit <- lm(BEDS ~ ., data = df[train,])
summary(lm_fit)

# Confidence intervals on coefficients
confint(lm_fit, level = 0.95) # ATTENZIONE!! ALCUNI COEFFICIENTI HANNO ZERO DENTRO INTERVALLO DI CONFIDENZA!!

plot(predict(lm_fit), residuals(lm_fit)) # SONO PRESENTI PATTERN -> non linearità nei dati 
plot(predict(lm_fit), rstudent(lm_fit))