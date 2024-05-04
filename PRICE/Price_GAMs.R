install.packages('gam')
install.packages('akima')
library(foreach)
library(gam)
library(akima)


set.seed(123)

df <- read_csv("dataset/NY-House-Dataset 2 - clean.csv")
df <- df[, -1]

#GAM


train <- sample(dim(df)[1], round(dim(df)[1]*0.7))
gam_model <- gam(PRICE ~ s(BEDS,4) + PROPERTYSQFT + BATH, data = df[train,]); 
dev.new()
plot(gam_model,se=TRUE)

pred_value <- predict(gam_model,newdata = df[-train,])
plot(df$PRICE[-train],pred_value)


# add local regression with span = 0.7
gam_model2 <- gam(PRICE ~ s(BEDS,4) + lo(PROPERTYSQFT, span = 0.7) + BATH,data = df[train,])
plot(gam_model2,se=TRUE)

pred_value2 <- predict(gam_model2,newdata = df[-train,])
plot(df$PRICE[-train],pred_value2)