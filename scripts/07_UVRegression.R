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

# impact of gender on quality of life(QOL_Score)
uv_lm <- lm(QOL_Score ~ Gender, data = data)
report(uv_lm)

# impact of gender on quality of life status (QOL_Status)
uv_logreg <- glm(QOL_Code ~ Gender, data = data, family = "binomial")
report(uv_logreg)

# Factors associated with quality of life score in the linear regression analysis
data |> 
  select(1:19, QOL_Score) |> 
  tbl_uvregression(
    method = lm, 
    y = QOL_Score
  ) |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/UV_LinReg.docx")



# Factors associated with quality of life score in the univariate logistic regression analysis
data |> 
  select(1:19, QOL_Code) |> 
  tbl_uvregression(
    method = glm, 
    method.args = list(family = binomial), 
    y = QOL_Code, 
    exponentiate = TRUE
  ) |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/UV_LogReg.docx")
