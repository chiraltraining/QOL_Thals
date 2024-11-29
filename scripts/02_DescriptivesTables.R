# load packages 
library(tidyverse)
library(gtsummary)
library(gt)

# load data 
data <- readxl::read_excel("clean_data/QOL_Clean.xlsx")

# Table 1: Demonstrates the demographic characteristics of the patients
data |> 
  select(1:8) |> 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean}±{sd}"
      # all_categorical() ~ "{n} / {N} ({p}%)"
    )) |> 
  as_gt() |> 
  gtsave("tables/Table1.docx")

# Table 2: Clinical characteristics of thalassemia patients
data |> 
  select(9:19) |> 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean}±{sd}"
    ), 
    type = c("Having a severe condition at the time of diagnosis?", "Receiving a blood transfusion during the previous 3 months", "Did you had splenectomy") ~ "categorical"
  ) |> 
  as_gt() |> 
  gtsave("tables/Table2.docx")

