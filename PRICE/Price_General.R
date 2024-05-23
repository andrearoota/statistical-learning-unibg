
library(corrplot)
library(caret)
library(e1071)
library(readr)
library(boot)
library(dplyr)
library(caret)
library(foreach)
library(gam)
library(akima)
library ( ISLR2 )
library( glmnet )
library(fastDummies)
library(knitr)
library(kableExtra)
library(dplyr)

# load dataset and set seed

set.seed(123)
rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot
df <- read_csv("dataset/NY-House-Dataset 2.csv")
df <- df[, -1]
df <- df[, !(names(df) %in% c("ADDRESS", "STATE", "MAIN_ADDRESS", "ADMINISTRATIVE_AREA_LEVEL_2", "LOCALITY", "STREET_NAME", "LONG_NAME", "FORMATTED_ADDRESS"))]
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


df <- dummy_cols(df_filtered)
df <- df[, !(names(df) %in% c( "BROKERTITLE", "TYPE", "SUBLOCALITY"))]

correlation_matrix <- cor(df)
relevant_variables <- names(which(abs(correlation_matrix["PRICE", ]) >= 0.01))
relevant_variables <- relevant_variables[relevant_variables != "PRICE"]
df <- df[, c("PRICE", relevant_variables)]



# train 70% 30% test(-train)
train <- sample(dim(df)[1],floor(dim(df)[1]*0.70),replace = FALSE);

#linear regression
lm_fit <- lm(log(PRICE) ~ ., data = df[train,])
summary(lm_fit)
par(mfrow = c(2, 2)
plot(lm_fit)

fitt_value <- predict(lm_fit, newdata = df[-train, ])
true_values <- log(df$PRICE[-train])
correlation_linear <- cor(fitt_value, true_values)
mse_linear <- mean((true_values - fitt_value)^2)
dev.new()
plot(fitt_value, true_values, xlab = "Previsioni linear", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri (linear)")
abline(a = 0, b = 1, col = "red")




#SHRINKAGE

x <- model.matrix(PRICE ~ ., df)
y <- log(df$PRICE)  # Applicare la trasformazione logaritmica

# Ridge regression
cv_model <- cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10)
opt_lambda <- cv_model$lambda.min
model <- glmnet(x[train,], y[train], alpha = 0, lambda = opt_lambda, standardize = TRUE)
fitt_value <- predict(model, s=opt_lambda, newx = x[-train,])
true_values <- y[-train]
mse_ridge <- mean((true_values - fitt_value)^2)
correlation_ridge <- cor(fitt_value, true_values)


dev.new()
plot(fitt_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri (ridge)")
abline(a = 0, b = 1, col = "red")

#lasso

cv_lasso <- cv.glmnet(x[train,],y[train],alpha=1,nfolds = 10);
opt_lambda <- cv_lasso$lambda.min;
model <- glmnet(x[train,],y[train],alpha = 1,lambda = opt_lambda)
fitt_value <- predict(model,s=opt_lambda, newx=x[-train,])
true_values <- y[-train]
mse_lasso = mean((true_values - fitt_value)^2)
correlation_lasso <- cor(fitt_value, true_values)

dev.new()
plot(fitt_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri (lasso)")
abline(a = 0, b = 1, col = "red")


#GAMs

gam_model <- gam(log(PRICE) ~ s(BEDS,4) + s(BATH,4)+., data = df[train,]); 
fitt_value <- predict(gam_model,newdata = df[-train,])
true_values <- log(df$PRICE[-train])
err = (log(df$PRICE) - predict(gam_model, df))^2
mse_GAMs = mean(err[-train])
correlation_GAMs <- cor(fitt_value, true_values)
dev.new()
plot(fitt_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
     main = "Confronto tra Previsioni e Valori Veri (GAMs)")
abline(a = 0, b = 1, col = "red")



#valutazione

   
  data <- data.frame(
    Method = c("Linear", "Lasso", "Ridge", "GAMs"),
    Correlation = round(c(correlation_linear,correlation_lasso,correlation_ridge,correlation_GAMs),3),
    MSE_Test = round(c(mse_linear,mse_lasso,mse_ridge,mse_GAMs),3)
  )
  
  data$MSE_Test <- sprintf("%.3f  $^2", data$MSE_Test)
  
  table <- kable(data, format = "html", caption = "Correlation and MSEtest") %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:3, width = "4cm") %>%
    add_header_above(c(" " = 1, Metrics="2"))
  
  # Visualizzazione della tabella
  table