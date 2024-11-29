# load packages 
library(tidyverse)
library(easystats)
library(gtsummary)
library(gt)

# load data 
data <- readxl::read_excel("clean_data/QOL_Clean.xlsx")

# perform t-test
diff_test <- t.test(data$QOL_Score ~ data$Gender)

# report 
report(diff_test)


# Table 5: Quality of life domains score difference by gender (2 groups)
data |> 
  select(20:30, Gender) |> 
  tbl_summary(
    by = Gender, 
    type = everything() ~ "continuous", 
    statistic =  all_continuous() ~ "{mean}±{sd}"
  ) |> 
  add_p() |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table5.docx")


# Table 6: Quality of life domains score difference by economic class (more than 2 groups)
data |> 
  select(20:30, `Your economic class`) |> 
  tbl_summary(
    by = `Your economic class`, 
    type = everything() ~ "continuous", 
    statistic =  all_continuous() ~ "{mean}±{sd}"
  ) |> 
  add_p() |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table6.docx")
