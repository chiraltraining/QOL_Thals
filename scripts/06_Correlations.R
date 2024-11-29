# load packages 
library(tidyverse)
library(easystats)
library(gtsummary)
library(gt)

# load data 
data <- readxl::read_excel("clean_data/QOL_Clean.xlsx")

# select numeric variables 
num_data <- data |> 
  select(20:30)

# perform correlation between 2 variables 
cor(num_data$Physical_Functioning, num_data$QOL_Score)

# perform correlation test 
corr_test <- cor.test(num_data$Physical_Functioning, num_data$QOL_Score)
report(corr_test)

# correlation matrix 
results <- correlation(num_data) 
results |> 
  summary(redundant = TRUE) |> 
  plot()