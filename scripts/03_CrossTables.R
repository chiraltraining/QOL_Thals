# load packages 
library(tidyverse)
library(gtsummary)
library(gt)

# load data 
data <- readxl::read_excel("clean_data/QOL_Clean.xlsx")

# Table 3: Quality of life status by demographics 
data |> 
  select(1:8, QOL_Status) |> 
  tbl_summary(
    by = QOL_Status, 
    statistic = list(
      all_continuous() ~ "{mean}±{sd}"
    )
  ) |> 
  add_overall() |> 
  as_gt() |> 
  gtsave("tables/Table3.docx")


# Table 4: Quality of life status by clinical characteristics 
data |> 
  select(9:19, QOL_Status) |> 
  tbl_summary(
    by = QOL_Status, 
    statistic = list(
      all_continuous() ~ "{mean}±{sd}"
    )
  ) |> 
  add_overall() |> 
  as_gt() |> 
  gtsave("tables/Table4.docx")