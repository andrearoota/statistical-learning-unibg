install.packages("glmnet")
library(readr)
library ( ISLR2 )
library( boot )
library( glmnet )

set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2 - clean.csv")
df <- df[, -1]

x <- model.matrix ( PRICE ~ . , df )
y <- df$PRICE

#RIDGE



#train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);
train <- sample(nrow(x),floor(nrow(x)*0.5),replace = FALSE);

# lambda = seq() parameters is optional 
cv_model <- cv.glmnet(x[train, ],y[train], alpha = 0, nfolds = 10);
dev.new()
plot(cv_model)
opt_lambda <- cv_model$lambda.min; # cv_model$lambda.1se

# predict on test dataset
model <- glmnet(x[train,],y[train],alpha = 0,lambda = opt_lambda,standardize=TRUE)
fitt_value <- predict(model,newx = x[-train,])
true_values <- df$PRICE[-train]
dev.new()
plot(fitt_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri")
abline(a = 0, b = 1, col = "red")

test_MSE_ridge = mean((y[-train] - fitt_value)^2)



#LASSO


cv_lasso <- cv.glmnet(x[train,],y[train],alpha=1,nfolds = 10);
plot(cv_lasso)
opt_lambda <- cv_lasso$lambda.min;

# use full datasets
model <- glmnet(x[train,],y[train],alpha = 1,lambda = opt_lambda)
fitt_value <- predict(model,s=opt_lambda, newx=x[-train,])
true_values <- df$PRICE[-train]
dev.new()
plot(fitt_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri")
abline(a = 0, b = 1, col = "red")
test_MSE_lasso = mean((y[-train] - fitt_value)^2)
