install.packages('caret')
library(readr)
library(boot)
library(dplyr)
library(caret)


# Set the seed for reproducibility
set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2 - clean.csv")
df <- df[, -1]



cv.error.10=rep(0 ,10)
for (i in 1:10){
glm.fit=lm_fit <- lm(PRICE ~ ., data = df)
cv.error.10[i]=cv.glm(df ,lm_fit ,K=10) $delta [1]
 }
cv.error.10


