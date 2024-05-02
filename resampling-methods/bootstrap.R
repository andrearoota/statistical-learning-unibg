install.packages("fastDummies")

library(readr)
library(fastDummies)
library( boot )


set.seed(123) # Imposta il seed per la riproducibilità

df <- read_csv("dataset/NY-House-Dataset 2.csv")

## Missing data
missing_values <- colSums(is.na(df))
print(missing_values)

## Remove unused column
df <- df[, !(names(df) %in% c("ADDRESS", "STATE", "MAIN_ADDRESS", "ADMINISTRATIVE_AREA_LEVEL_2", "LOCALITY", "STREET_NAME", "LONG_NAME", "FORMATTED_ADDRESS"))]

## Filter data

vars <- c("PRICE","BEDS","BATH","PROPERTYSQFT")

for (var in vars){
  
  Q1 <- quantile(df[[var]], 0.25)
  Q3 <- quantile(df[[var]], 0.75)
  
  IQR <- Q3-Q1
  lower_limit <- Q1-1.5*IQR
  upper_limit <- Q3+1.0*IQR
  
  df_filtered <- df[df[[var]] >= lower_limit & df[[var]] <= upper_limit, ]
  
}

## Dummies
# Trasformazione delle variabili categoriche in dummy variables
df <- dummy_cols(df_filtered)

# Rimozione delle colonne originali delle variabili categoriche
df <- df[, !(names(df) %in% c("BROKERTITLE", "TYPE", "SUBLOCALITY"))]

##### LINEAR MULTIPLE REGRESSION ##### 
lm_fit <- lm(BEDS ~ ., data = df[train,])
summary(lm_fit)

# perform bootstrap simulation
fun_boot <- function(data,index){
  lm_fit <- lm(BEDS ~ ., data = data, subset = index)
  return (lm_fit$coefficients)
}

# test function rm
fun_boot(df,1:dim(df)[1])
fun_boot(df, sample(dim(df)[1], floor(dim(df)[1]*0.7), replace = TRUE));

boot(df,fun_boot,R = 1000);
