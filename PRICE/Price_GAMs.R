install.packages('gam')
install.packages('akima')
library(foreach)
library(gam)
library(akima)
library(readr)


set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2 - clean.csv")
df <- df[, -1]

#GAM


train <- sample(dim(df)[1], round(dim(df)[1]*0.7))
gam_model <- gam(PRICE ~ s(BEDS,4) + lo(PROPERTYSQFT, span=0.7) + lo(LONGITUDE, span=0.7)+., data = df[train,]); 
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




