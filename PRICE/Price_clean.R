library(readr)
library(fastDummies)
library(boot)
library(dplyr)

df <- read_csv("dataset/NY-House-Dataset 2.csv")
df <- df[, -1]
df$BROKERTITLE <- as.factor(df$BROKERTITLE)


# Examine the distribution of values in the prediction column
value_counts <- table(df$BROKERTITLE)

# Identify values that appear less than three times
rare_values <- names(value_counts[value_counts < 10])

# Create a new category for rare values and assign them a new value
new_category <- "Others"

# Add the new level to the categorical variable
df$BROKERTITLE <- factor(df$BROKERTITLE, levels = c(levels(df$BROKERTITLE), new_category))

# Update the dataset by replacing rare values with the new value
df$BROKERTITLE[df$BROKERTITLE %in% rare_values] <- new_category
df$BROKERTITLE <- droplevels(df$BROKERTITLE)

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
df <- df[, !(names(df) %in% c("BROKERTITLE", "TYPE", "SUBLOCALITY"))]



write.csv(df, "dataset/NY-House-Dataset Price_clean.csv")