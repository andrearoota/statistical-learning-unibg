install.packages("glmnet")
library(readr)
library ( ISLR2 )
library( boot )
library( glmnet )
library(fastDummies)
set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2.csv")
df <- df[, -1]
df <- df[, -1]
df <- df[, !(names(df) %in% c("ADDRESS", "STATE", "MAIN_ADDRESS", "ADMINISTRATIVE_AREA_LEVEL_2", "LOCALITY", "STREET_NAME", "LONG_NAME", "FORMATTED_ADDRESS"))]

vars <- c("PRICE","BEDS","BATH","PROPERTYSQFT")
df_filtered <- df

for (var in vars){
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



x <- model.matrix ( PRICE ~ . , df )
y <- df$PRICE

#RIDGE



train <- sample(dim(x)[1],floor(dim(x)[1]*0.7),replace = FALSE);


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
correlation <- cor(fitt_value, true_values)


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
correlation <- cor(fitt_value, true_values)


#boostrap


fun_boot <- function(data,index){
  cv_lasso <- cv.glmnet(x[index,],y[index],alpha=1,nfolds = 10);
  opt_lambda <- cv_lasso$lambda.min;
  model <- glmnet(x[index,],y[index],alpha = 1,lambda = opt_lambda)
  fitt_value <- predict(model,s=opt_lambda, newx=x[-index,])
  true_values <- df$PRICE[-index]
  test = mean((y[-index] - fitt_value)^2)
  corr <- cor(fitt_value, true_values)
  return (test)
}

fun_boot(df, sample(dim(x)[1],floor(dim(x)[1]*0.7),replace = FALSE));

bootstrap_result <- boot(df,fun_boot,R = 3)
bootstrap_result$t
