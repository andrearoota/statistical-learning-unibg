# Load required libraries
library(readr)
library(boot)
library(dplyr)

# Set the seed for reproducibility
set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2 - clean.csv")
df <- df[, -1]

##### LINEAR MULTIPLE REGRESSION ##### 
# perform bootstrap simulation
fun_boot <- function(data,index){
  lm_fit <- lm(PRICE ~ ., data = data, subset = index)
  return (lm_fit$coefficients)
}

# test function rm
fun_boot(df,1:dim(df)[1])
fun_boot(df, sample(dim(df)[1], floor(dim(df)[1]*0.7), replace = TRUE));

bootstrap_result <- boot(df,fun_boot,R = 3);
View(bootstrap_result)
dev.new()
plot(bootstrap_result)