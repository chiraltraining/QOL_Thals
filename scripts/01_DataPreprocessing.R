# load packages 
library(tidyverse)
library(readxl)
library(xlsx)

# load data 
data <- read_excel("raw_data/QOL_Raw.xlsx")

# check missing values 
is.na(data)
sum(is.na(data))

# remove missing values 
data <- na.omit(data)
sum(is.na(data))

sf36 <- data |> 
  select(20:55)
colnames(sf36) <- paste0("Q", 1:36)

# 1. physical functioning 
physical_functioning <- sf36 |> 
  select(Q3:Q12) |> 
  mutate(across(Q3:Q12, ~case_when(
    . == "Yes, Limited a Little" ~ 0,
    . == "No, Not Limited at all"~ 50, 
    . == "Yes, Limited a lot" ~ 100, 
    TRUE ~ NA_real_ 
  ))) |> 
  rowwise() |> 
  mutate(Physical_Functioning = mean(c_across(Q3:Q12), na.rm = TRUE))
  
  

# 2. physical role
physical_role <- sf36 |> 
  select(Q13:Q16) |> 
  mutate(across(Q13:Q16, ~case_when(
    . == "Yes" ~ 0,
    . == "No"~ 100, 
    TRUE ~ NA_real_ 
  ))) |> 
  rowwise() |> 
  mutate(Physical_Role = mean(c_across(Q13:Q16), na.rm = TRUE))


# 3. bodily pain 
bodily_pain <- sf36 |> 
  select(Q17:Q19) |> 
  mutate(across(Q17:Q19, ~case_when(
    . == "Yes" ~ 0,
    . == "No"~ 100, 
    TRUE ~ NA_real_ 
  ))) |> 
  rowwise() |> 
  mutate(Bodily_Pain = mean(c_across(Q17:Q19), na.rm = TRUE))

# 4. general health
general_health <- sf36 |> 
  select(Q1, Q33:Q36) |> 
  mutate(Q1 = case_when(
    Q1 == "Excellent" ~ 100, 
    Q1 == "Very Good" ~ 75, 
    Q1 == "Good" ~ 50, 
    Q1 == "Fair" ~ 25, 
    Q1 == "Poor" ~ 0, 
  )) |> 
  mutate(across(Q33:Q35, ~case_when(
    . == "Definitely true" ~ 0,
    . == "Mostly true"~ 25, 
    . == "Don't know"~ 50, 
    . == "Mostly false"~ 75, 
    . == "Definitely false"~ 100 , 
    TRUE ~ NA_real_ 
  ))) |> 
  mutate(Q36 = case_when(
    Q36 == "Definitely true" ~ 100,
    Q36 == "Mostly true"~ 75, 
    Q36 == "Don't know"~ 50, 
    Q36 == "Mostly false"~ 25, 
    Q36 == "Definitely false"~ 0 , 
  )) |> 
  # Calculate row-wise mean directly on the selected columns
  mutate(General_Health = rowMeans(cbind(Q1, Q33, Q34, Q35, Q36), na.rm = TRUE))


# 5. vitality 
vitality <- sf36 |> 
  select(Q23, Q27, Q29, Q31) |> 
  mutate(Q23 = case_when(
    Q23 == "All of the time" ~ 100, 
    Q23 == "Most of the time" ~ 80, 
    Q23 == "A good Bit of the time" ~ 60, 
    Q23 == "Some of the time" ~ 40, 
    Q23 == "A little bit of the time" ~ 20, 
    Q23 ==  "None of the Time" ~ 0
  )) |> 
  mutate(Q27 = case_when(
    Q27 == "All of the time" ~ 100, 
    Q27 == "Most of the time" ~ 80, 
    Q27 == "A good Bit of the time" ~ 60, 
    Q27 == "Some of the time" ~ 40, 
    Q27 == "A little bit of the time" ~ 20, 
    Q27 ==  "None of the Time" ~ 0
  )) |> 
  mutate(Q29 = case_when(
    Q29 == "All of the time" ~ 0, 
    Q29 == "Most of the time" ~ 20, 
    Q29 == "A good Bit of the time" ~ 40, 
    Q29 == "Some of the time" ~ 60, 
    Q29 == "A little bit of the time" ~ 80, 
    Q29 ==  "None of the Time" ~ 100
  )) |> 
  mutate(Q31 = case_when(
    Q31 == "All of the time" ~ 0, 
    Q31 == "Most of the time" ~ 20, 
    Q31 == "A good Bit of the time" ~ 40, 
    Q31 == "Some of the time" ~ 60, 
    Q31 == "A little bit of the time" ~ 80,  
    Q31 ==  "None of the Time" ~ 100
  )) |> 
  mutate(Vitality = rowMeans(cbind(Q23, Q27, Q29, Q31), na.rm = TRUE))

  

# 6. social functioning
social_functioning <- sf36 |> 
  select(Q20, Q32) |> 
  mutate(Q20 = case_when(
    Q20 == "None" ~ 100, 
    Q20 == "Very Mild" ~ 75, 
    Q20 == "Mild" ~ 75, 
    Q20 == "Moderate" ~ 50, 
    Q20 == "Severe" ~ 25, 
    Q20 == "Very Severe" ~ 0, 
  )) |> 
  mutate(Q32 = case_when(
    Q32 == "All of the time" ~ 0, 
    Q32 == "Most of the time" ~ 25, 
    Q32 == "Some of the time" ~ 50,
    Q32 == "A good Bit of the time" ~ 50,
    Q32 == "A little bit of the time" ~ 75, 
    Q32 == "None of the Time" ~ 100, 
  )) |> 
  mutate(Social_Functioning = rowMeans(cbind(Q20, Q32), na.rm = TRUE))


# 7. emotional role 
emotional_role <- sf36 |> 
  select(Q24, Q25, Q26,Q28,Q30) |> 
  mutate(across(Q24:Q25, ~case_when(
    . == "All of the time" ~ 0, 
    . == "Most of the time" ~ 20, 
    . == "A good Bit of the time" ~ 40,
    . == "Some of the time" ~ 60, 
    . == "A little bit of the time" ~ 80, 
    . ==  "None of the Time" ~100,
    TRUE ~ NA_real_ 
  ))) |> 
  mutate(Q28 = case_when(
    Q28 == "All of the time" ~ 0, 
    Q28 == "Most of the time" ~ 20, 
    Q28 == "A good Bit of the time" ~ 40,
    Q28 == "Some of the time" ~ 60, 
    Q28 == "A little bit of the time" ~ 80, 
    Q28 ==  "None of the Time" ~100,
    TRUE ~ NA_real_ 
  )) |> 
  mutate(Q30 = case_when(
    Q30 == "All of the time" ~ 0, 
    Q30 == "Most of the time" ~ 20, 
    Q30 == "A good Bit of the time" ~ 40,
    Q30 == "Some of the time" ~ 60, 
    Q30 == "A little bit of the time" ~ 80, 
    Q30 ==  "None of the Time" ~100,
    TRUE ~ NA_real_ 
  )) |> 
  mutate(Q26 = case_when(
    Q26 == "All of the time" ~ 100, 
    Q26 == "Most of the time" ~ 80, 
    Q26 == "A good Bit of the time" ~ 60,
    Q26 == "Some of the time" ~ 40, 
    Q26 == "A little bit of the time" ~ 20, 
    Q26 ==  "None of the Time" ~0,
    TRUE ~ NA_real_ 
  )) |> 
  mutate(Emotional_Role = rowMeans(cbind(Q24, Q25, Q26,Q28,Q30), na.rm = TRUE))



# 8. pain 
pain <- sf36 |> 
  select(Q21, Q22) |> 
  mutate(Q21 = case_when(
    Q21 == "None" ~ 100, 
    Q21 == "Very Mild" ~ 80, 
    Q21 == "Mild" ~ 60, 
    Q21 == "Moderate" ~ 40, 
    Q21 == "Severe" ~ 20, 
    Q21 == "Very Severe" ~ 0, 
  )) |> 
  mutate(Q22 = case_when(
    Q22 == "Not at all" ~ 100, 
    Q22 == "A little bit" ~ 75, 
    Q22 == "Moderately" ~ 50, 
    Q22 == "Quite a bit" ~ 25, 
    Q22 == "Extremely" ~ 0, 
  )) |>
  mutate(Pain = rowMeans(cbind(Q21, Q22), na.rm = TRUE))

# combined the data 
demographics <- data |> 
  select(1:19)

sf_domains <- cbind(physical_functioning, physical_role, 
                    bodily_pain, general_health, vitality, social_functioning,
                    emotional_role, pain)

sf_domains <- sf_domains |> 
  select(Physical_Functioning, Physical_Role, Bodily_Pain, General_Health, 
         Vitality, Social_Functioning, Emotional_Role, Pain) |> 
  mutate(Physical_Health = rowMeans(cbind(Physical_Functioning, Physical_Role, Bodily_Pain, General_Health, 
                                          Vitality), na.rm = TRUE)) |> 
  mutate(Mental_Health = rowMeans(cbind(Social_Functioning, Emotional_Role, Pain), na.rm = TRUE)) |> 
  mutate(QOL_Score = rowMeans(cbind(Physical_Health, Mental_Health), na.rm = TRUE)) |> 
  mutate(QOL_Status = case_when(
    QOL_Score <= 50 ~ "Poor", 
    QOL_Score > 50 ~ "Good", 
  ))
  

# combined data 
qol_data <- cbind(demographics, sf_domains)

# export the data as CSV
write.csv(qol_data, "clean_data/QOL_Clean.csv", row.names = FALSE)

# export the data as Excel
write.xlsx(qol_data, "clean_data/QOL_Clean.xlsx", row.names = F)

