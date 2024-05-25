# Installa e carica le librerie necessarie
install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)

# Carica il dataset
df <- read_csv("dataset/NY-House-Dataset 2.csv")
df <- df[, -1]

X <- subset(df, select = -c(BROKERTITLE))
Y <- as.factor(df$BROKERTITLE)

# Addestra un modello di Random Forest
set.seed(123) # Per riproducibilità
rf_model <- train(
  x = X,
  y = Y,
  method = "rf", # Utilizza Random Forest
  trControl = trainControl(method = "cv", number = 5), # Cross-validation con 5 fold
  tuneGrid = expand.grid(mtry = c(2, 3, 4)) # Testa diverse combinazioni di mtry
)


plot(rf_model)


varImp(rf_model$finalModel)

# Ora puoi utilizzare il modello addestrato per predire la categoria "brokertitle" per le osservazioni con categorie rare
