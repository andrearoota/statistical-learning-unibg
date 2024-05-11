install.packages('gam')
install.packages('akima')
library(foreach)
library(gam)
library(akima)
library(readr)
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



#GAM


train <- sample(dim(df)[1], round(dim(df)[1]*0.7))
gam_model <- gam(PRICE ~ s(BEDS,5) + s(BATH,4)+., data = df[train,]); 
pred_value <- predict(gam_model,newdata = df[-train,])
true_values <- df$PRICE[-train]
dev.new()
plot(pred_value, true_values, xlab = "Previsioni", ylab = "Valori Veri",
    main = "Confronto tra Previsioni e Valori Veri")
abline(a = 0, b = 1, col = "red")

err = (df$PRICE - predict(gam_model, df))^2
training_err = mean(err[train])
test_err_GAMs = mean(err[-train])

correlation <- cor(pred_value, true_values)
print(correlation)



