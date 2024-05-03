# Install the necessary package
install.packages("fastDummies")

# Load required libraries
library(readr)
library(fastDummies)
library(boot)
library(dplyr)

# Set the seed for reproducibility
set.seed(123)

# Read the dataset from CSV file
df <- read_csv("dataset/NY-House-Dataset 2.csv")

# Check for missing data
colSums(is.na(df))

# Count unique values in each column
sapply(df, function(x) n_distinct(x))

# Remove unused columns
df <- df[, !(names(df) %in% c("ADDRESS", "STATE", "MAIN_ADDRESS", "ADMINISTRATIVE_AREA_LEVEL_2", "LOCALITY", "STREET_NAME", "LONG_NAME", "FORMATTED_ADDRESS"))]

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

# Convert categorical variables into dummy variables
df <- dummy_cols(df_filtered)
# Remove original columns of categorical variables to avoid multicollinearity
df <- df[, !(names(df) %in% c("BROKERTITLE", "TYPE", "SUBLOCALITY"))]

# Save data
write.csv(df, "dataset/NY-House-Dataset 2 - clean.csv")