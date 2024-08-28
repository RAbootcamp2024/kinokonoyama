install.packages("tidyverse")

library(tidyverse)

data <- read_csv("C:/Users/Owner/Desktop/kinokonoyama/dataset.csv")

view(data)

# make table2

data1 <- data |> 
  library(dplyr)

data1 <- data %>%
  mutate(pre_reg_s = case_when(
    state == "California" & year >= 2009 ~ 1,
    state == "Colorado" & year >= 2013 ~ 1,
    state == "Delaware" & year >= 2010 ~ 1,
    state == "Florida" & year >= 2007 ~ 1,
    state == "Louisiana" & year >= 2014 ~ 1,
    state == "Maine" & year >= 2011 ~ 1,
    state == "Maryland" & year >= 2010 ~ 1,
    state == "Massachusetts" & year >= 2014 ~ 1,
    state == "North Carolina" & year >= 2009 & year <= 2013 ~ 1,
    state == "Oregon" & year >= 2007 ~ 1,
    state == "Rhode Island" & year >= 2010 ~ 1,
    state == "Utah" & year >= 2015 ~ 1,
    state == "Hawaii" & year >= 1993 ~ 1,
    state == "New Jersey" & year >= 2015 ~ 1,
    TRUE ~ 0  # Default case if none of the conditions match
  )) |>
  arrange(stid, year) |>
  mutate(pre_reg = 0) |>
  mutate(pre_reg_s_lag = lag(pre_reg_s, default = 0),  # 前の行の 'pre_reg_s' を取得
         pre_reg = if_else(pre_reg_s_lag == 0 & pre_reg_s == 1, 1, pre_reg), .by = stid)


data1 <- data1 |>
  mutate(F0_pre = 0)






# Create F0_pre, F1_pre, ..., F10_pre
data1 <- data1 |>
  mutate(across(starts_with("F"), ~ ifelse(is.na(.), 0, .)))  # まず NA を 0 に置き換え

for (kk in 0:10) {
  var_name <- paste0("F", kk, "_pre")
  data1[[var_name]] <- data1[[paste0("F", kk, ".pre_reg")]]
  data1[[var_name]][is.na(data1[[var_name]])] <- 0
}

# Create L1_pre, L2_pre, ..., L6_pre
data1 <- data1 |>
  mutate(across(starts_with("L"), ~ ifelse(is.na(.), 0, .)))  # まず NA を 0 に置き換え

for (kk in 1:6) {
  var_name <- paste0("L", kk, "_pre")
  data1[[var_name]] <- data1[[paste0("L", kk, ".pre_reg")]]
  data1[[var_name]][is.na(data1[[var_name]])] <- 0
}

  

  #select(state, year, e001, e031, stid, p_income, )

#*************** BASELINE ***************
#  reg  F10_last_pre F9_pre F8_pre F7_pre F6_pre F5_pre F4_pre F3_pre F2_pre uno F0_pre L1_pre L2_pre L3_pre L4_last_pre	
# eststo no_trend

data |>
  filter()