# load packages 
library(tidyverse)
library(gtsummary)
library(gt)

# load data 
data <- readxl::read_excel("clean_data/QOL_Clean.xlsx")

# Table 7: Association 
data |> 
  select(2:9, QOL_Status) |> 
  tbl_summary(
    by = QOL_Status
  ) |> 
  add_p() |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table7.docx")



  
