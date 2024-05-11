library(psych) #for general functions
library(ggplot2) #for data visualization
library(caret) #for training and cross validation (also calls other model libaries)
library(rpart) #for trees
library(rpart.plot) # Enhanced tree plots
library(RColorBrewer) # Color selection for fancy tree plot
library(party) # Alternative decision tree algorithm
library(partykit) #Convert rpart object to BinaryTree
library(pROC) #for ROC curves
library(readr) #for reading in data
library(dplyr) #for data manipulation
library(corrplot) #for correlation plots

# Set the seed for reproducibility
set.seed(123)

## Get data ##
# Read the dataset from CSV file
df <- read_csv("../dataset/NY-House-Dataset 2.csv")
df <- df[, -1]

# data structure
head(df, 10)

# sample descriptives
describe(df)

# Check for missing data
colSums(is.na(df))

# Visualizzazione della distribuzione delle variabili numeriche
numeric_vars <- c("PRICE", "BEDS", "BATH", "PROPERTYSQFT")
par(mfrow = c(2, 2))
for (var in numeric_vars) {
  hist(df[[var]], main = var, xlab = var)
}

# Correlazione tra variabili numeriche
correlation_matrix <- cor(df[numeric_vars])
corrplot(correlation_matrix, method = 'number')

# Ingegneria delle caratteristiche
# Creazione di variabili di interazione
interaction_vars <- c("BEDS:BATH", "BATH:PROPERTYSQFT")
df <- df %>% 
  mutate(BEDS_BATH_interaction = BEDS * BATH,
         BATH_PROPERTYSQFT_interaction = BATH * PROPERTYSQFT)


# Count unique values in each column
sapply(df, function(x)
  n_distinct(x))

# Remove unused columns
df <- df[, !(
  names(df) %in% c(
    "ADDRESS",
    "STATE",
    "MAIN_ADDRESS",
    "ADMINISTRATIVE_AREA_LEVEL_2",
    "LOCALITY",
    "STREET_NAME",
    "LONG_NAME",
    "FORMATTED_ADDRESS"
  )
)]
df <- df[, !(names(df) %in% c("LATITUDE", "LONGITUDE", "BROKERTITLE"))]

# Filter the data based on selected variables
vars <- c("PRICE", "BEDS", "BATH", "PROPERTYSQFT")
df_filtered <- df

for (var in vars) {
  Q1 <- quantile(df_filtered[[var]], 0.25)
  Q3 <- quantile(df_filtered[[var]], 0.75)
  
  IQR <- Q3 - Q1
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  df_filtered <- df_filtered[df_filtered[[var]] >= lower_limit &
                               df_filtered[[var]] <= upper_limit, ]
}
df <- df_filtered

# Factor
df$SUBLOCALITY <- as.factor(df$SUBLOCALITY)
df$TYPE <- as.factor(df$TYPE)

# Esamina la distribuzione dei valori nella colonna di predizione
value_counts <- table(df$SUBLOCALITY)

# Identifica i valori che compaiono meno di tre volte
rare_values <- names(value_counts[value_counts < 3])

# Crea una nuova categoria per i valori rari e assegna loro un nuovo valore
new_category <- "Others"

# Aggiungi il nuovo livello alla variabile categorica
df$SUBLOCALITY <- factor(df$SUBLOCALITY, levels = c(levels(df$SUBLOCALITY), new_category))

# Aggiorna il dataset sostituendo i valori rari con il nuovo valore
df$SUBLOCALITY[df$SUBLOCALITY %in% rare_values] <- new_category
df$SUBLOCALITY <- droplevels(df$SUBLOCALITY)

## Split the data so that we use 70% of it for training
train_index <- createDataPartition(y = df$SUBLOCALITY,
                                   p = 0.7,
                                   list = FALSE)

## Subset the data
training_set <- df[train_index, ]
testing_set <- df[-train_index, ]

## Define repeated cross validation with 5 folds and three repeats
repeat_cv <- trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 3,
  allowParallel = TRUE
)

## Define number of trees
ntree <- 500

## Decision tree
decision_tree <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set,
  method = "ctree",
  trControl = repeat_cv,
  tuneLength = 10
)
decision_tree

# Plot the decision tree
plot(decision_tree)
plot(decision_tree$finalModel,
     uniform = TRUE,
     main = "Decision Tree")

# Predict the test set
predictions <- predict(decision_tree, newdata = testing_set, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set$SUBLOCALITY)

## Random forest ##
random_forest <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set,
  method = "rf",
  trControl = repeat_cv,
  ntree = ntree,
  tuneLength = 10
)
random_forest

# Plot the random forest
plot(random_forest)
plot(random_forest$finalModel, main = "Random Forest")

# Predict the test set
predictions <- predict(random_forest, newdata = testing_set, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set$SUBLOCALITY)

## Bagging ##
bagging <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set,
  method = "treebag",
  trControl = repeat_cv,
  ntree = ntree,
  tuneLength = 10
)
bagging

# Plot the bagging
plot(varImp(bagging))

# Predict the test set
predictions <- predict(bagging, newdata = testing_set, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set$SUBLOCALITY)

## Boosting ##
gbmGrid <-  expand.grid(
  interaction.depth = 4,
  n.trees = ntree,
  shrinkage = 0.01,
  n.minobsinnode = 10
)

boosting <- train(
  as.factor(SUBLOCALITY) ~ .,
  data = training_set,
  method = "gbm",
  trControl = repeat_cv,
  verbose = FALSE,
  tuneLength = 10,
  tuneGrid = gbmGrid
)
boosting

# Predict the test set
predictions <- predict(boosting, newdata = testing_set, type = "raw")
head(predictions)
confusionMatrix(predictions, testing_set$SUBLOCALITY)

## Compare models ##
results <- resamples(
  list(
    DecisionTree = decision_tree,
    RandomForest = random_forest,
    Bagging = bagging,
    Boosting = boosting
  )
)
summary(results)

# Plot the results
bwplot(results)