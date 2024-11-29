# load packages 
library(tidyverse)
library(easystats)
library(gtsummary)
library(gt)

# load data 
data <- readxl::read_excel("clean_data/QOL_Clean.xlsx")

# recode QOL_Status 
data <- data |> 
  mutate(QOL_Code = case_when(
    QOL_Status == "Poor" ~ 0, 
    QOL_Status == "Good" ~ 1
  ))

# impact of gender and diagnosis on quality of life(QOL_Score)
mv_lm <- lm(QOL_Score ~ Gender + Diagnosis, data = data)
report(mv_lm)

# tabular form 
mv_lm |> 
  tbl_regression() |> 
  bold_p(t = 0.05)

# impact of gender and diagnos on quality of life status (QOL_Status)
mv_logreg <- glm(QOL_Code ~ Gender+Diagnosis, data = data, family = "binomial")
report(mv_logreg)

# tabular 
mv_logreg |> 
  tbl_regression(exponentiate = TRUE)
