## (1) Define the packages that will be needed
packages <- c('dplyr', 'ggplot2', 'caret', 'visNetwork')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## (3) Load the packages into R session
invisible(lapply(packages, library, character.only = TRUE))
library(readr)
library(randomForest)

# Set the seed for reproducibility
set.seed(123)

## Get data
# Read the dataset from CSV file
df <- read_csv("dataset/NY-House-Dataset 2.csv")
df <- df[, -1]

# Check for missing data
colSums(is.na(df))

# Count unique values in each column
sapply(df, function(x) n_distinct(x))

# Remove unused columns
df <- df[, !(names(df) %in% c("ADDRESS", "STATE", "MAIN_ADDRESS", "ADMINISTRATIVE_AREA_LEVEL_2", "LOCALITY", "STREET_NAME", "LONG_NAME", "FORMATTED_ADDRESS"))]
df <- df[, !(names(df) %in% c("LATITUDE", "LONGITUDE", "BROKERTITLE"))]

# Filter the data based on selected variables
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

## Define repeated cross validation with 5 folds and three repeats
repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3)

## Split the data so that we use 70% of it for training
train_index <- createDataPartition(y=df$SUBLOCALITY, p=0.7, list=FALSE)

## Subset the data
training_set <- df[train_index, ]
testing_set <- df[-train_index, ]

## Train a random forest model
forest <- train(
  
  # Formula. We are using all variables to predict SUBLOCALITY
  SUBLOCALITY~., 
  
  # Source of data; remove the Species variable
  data=training_set, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

## Print out the details about the model
forest$finalModel
plot(forest$finalModel)
plot(margin(forest,test_Data$SUB))

## Get variable importance, and turn into a data frame
var_imp <- varImp(forest, scale=FALSE)$importance
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall)

## Create a plot of variable importance
var_imp %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )

## Generate predictions
y_hats <- predict(
  
  ## Random forest object
  object=forest, 
  
  ## Data to use for predictions; remove the SUBLOCALITY
  newdata=testing_set[, -6])

## Print the accuracy
accuracy <- mean(y_hats == testing_set$SUBLOCALITY)*100
cat('Accuracy on testing data: ', round(accuracy, 2), '%',  sep='')

plot(forest$finalModel)
text(forest,pretty = 0)



## train basic tree
# Valuate performance with train and test data sets
table(df$SUBLOCALITY[training_set])


tree_model <- tree( SUBLOCALITY ~ . , training_set, split = "gini", control=tree.control(1866496, mincut = 250, minsize=500))

# show result 
summary(tree_model)
plot(tree_model)
text(tree_model,pretty = 0)

## Do predict
pred_value <- predict(tree_model, newdata = testing_set,type = "class")
table(pred_value,testing_set$SUBLOCALITY)
summary(pred_value)
plot(pred_value)
text(pred_value,pretty = 0)