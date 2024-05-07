library(readr)
library(boot)
library(dplyr)
library(caret)
library(foreach)
library(gam)
library(akima)
library ( ISLR2 )
library( glmnet )

# load dataset and set seed

set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2 - clean.csv")
df <- df[, -1]

# train 70% 30% test(-train)

train <- sample(dim(df)[1],floor(dim(df)[1]*0.70),replace = FALSE);

#linear regression

lm_fit <- lm(PRICE ~ ., data = df[train,])
fitt_value <- predict(lm_fit, newdata = df[-train, ])
true_values <- df$PRICE[-train]
correlation_linear <- cor(fitt_value, true_values)
mse_linear <- mean((df$PRICE[-train] - predict(lm_fit, newdata = df[-train, ]))^2)



#SHRINKAGE

x <- model.matrix ( PRICE ~ . , df )
y <- df$PRICE

#ridge

cv_model <- cv.glmnet(x[train, ],y[train], alpha = 0, nfolds = 10);
opt_lambda <- cv_model$lambda.min;
model <- glmnet(x[train,],y[train],alpha = 0,lambda = opt_lambda,standardize=TRUE)
fitt_value <- predict(model,newx = x[-train,])
true_values <- df$PRICE[-train]
mse_ridge = mean((y[-train] - fitt_value)^2)
correlation_ridge <- cor(fitt_value, true_values)

#lasso

cv_lasso <- cv.glmnet(x[train,],y[train],alpha=1,nfolds = 10);
opt_lambda <- cv_lasso$lambda.min;
model <- glmnet(x[train,],y[train],alpha = 1,lambda = opt_lambda)
fitt_value <- predict(model,s=opt_lambda, newx=x[-train,])
true_values <- df$PRICE[-train]
mse_lasso = mean((y[-train] - fitt_value)^2)
correlation_lasso <- cor(fitt_value, true_values)


#GAMs

gam_model <- gam(PRICE ~ s(BEDS,4) + s(PROPERTYSQFT,3)+ lo(LONGITUDE, span=0.7)+., data = df[train,]); 
fitt_value <- predict(gam_model,newdata = df[-train,])
true_values <- df$PRICE[-train]
err = (df$PRICE - predict(gam_model, df))^2
mse_GAMs = mean(err[-train])
correlation_GAMs <- cor(fitt_value, true_values)


#valutazione

vet1 <- c(correlation_linear,correlation_lasso,correlation_ridge,correlation_GAMs)
vet2 <- c(mse_linear,mse_lasso,mse_ridge,mse_GAMs)

  
  print(max(vet1))
  print(min(vet2))

