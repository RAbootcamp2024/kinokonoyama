library(tidyverse)
library(haven)
stata_data <- haven::read_dta("C:/Users/Owner/Downloads/112082-V1/Political-Responsiveness--State-Level-/Generate-Dataset/State_Level_Dataset_Modified.dta")
stata_data <- stata_data |>
  mutate(uno = 0) 

# Baseline Regression----
model2 <- feols(lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +
                  F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +
                  L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income | stid + year, 
                cluster = ~ stid, data = stata_data)
# reg lne031 F10_last_pre F9_pre F8_pre F7_pre F6_pre F5_pre F4_pre F3_pre F2_pre uno F0_pre L1_pre L2_pre L3_pre L4_last_pre i.stid i.year lnp_income, cluster(stid)	
# eststo no_trend
summary(model2)

## figure(PanelA)----
coefplot(model2,
         keep = c("F10_last_pre", "F9_pre", "F8_pre", "F7_pre", 
                  "F6_pre", "F5_pre", "F4_pre", "F3_pre", "F2_pre", "uno", "F0_pre", 
                  "L1_pre", "L2_pre", "L3_pre", "L4_last_pre"),
         main = "PanelA. DD model")

# Regression with State Time Trends ----
# stata_data$sts50 |>
#   view()
# sts1からsts50までの変数を文字列として作成
sts_vars <- paste0("sts", 1:50)

# 回帰分析のモデル式を作成
model_formula3 <- as.formula(
  paste(
    "lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +",
    "F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +",
    "L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income +",
    paste(sts_vars, collapse = " + "), # sts1からsts50までの変数を追加
    "| stid + year"
  )
)

# regression 
model3 <- feols(
  fml = model_formula3,
  cluster = ~ stid,
  data = stata_data
)
summary(model3)

#figure(PanelB)----
coefplot(model3,
         keep = c("F10_last_pre", "F9_pre", "F8_pre", "F7_pre", 
         "F6_pre", "F5_pre", "F4_pre", "F3_pre", "F2_pre", "uno", "F0_pre", 
                  "L1_pre", "L2_pre", "L3_pre", "L4_last_pre"),
         main = "PanelB. DD model with state time trend")


library(fixest)

stata_data$stid

##Regression with other voting reforms----

model4 <- feols(lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +
                  F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +
                  L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income + 
                  same + online + nrva, 
                paste0("sts", 1:50) | stid + year, 
                cluster = ~ stid, data = stata_data)

# 回帰分析のモデル式を作成
model_formula4 <- as.formula(
  paste(
    "lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +
                  F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +
                  L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income + 
                  same + online + nrva +",
    paste(sts_vars, collapse = " + "), # sts1からsts50までの変数を追加
    "| stid + year"
  )
)

# regression 
model4 <- feols(
  fml = model_formula4,
  cluster = ~ stid,
  data = stata_data
)
summary(model4)

# global socio_ec med_age sh16_25 lnenrollment blk_pop_sh wht_pop_sh gini lnpopulation unemp
# global politics i.gub_years democ incumbent gov_run_again past_gov_dem rel_margin turnout_rate term_limited
# global fiscal lne001 current_exp lnr05
socio_ec <- c("med_age", "sh16_25", "lnenrollment", "blk_pop_sh", "wht_pop_sh", "gini", "lnpopulation", "unemp")

politics <- c("factor(gub_years)","democ", "incumbent", "gov_run_again", "past_gov_dem", "rel_margin", "turnout_rate", "term_limited")
fiscal <- c("lne001", "current_exp", "lnr05")

socio_ec_str <- paste(socio_ec, collapse = " + ")
politics_str <- paste(politics, collapse = " + ")
fiscal_str <- paste(fiscal, collapse = " + ")

model_formula5 <- as.formula(
  paste(
    "lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +
                  F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +
                  L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income + 
                  same + online + nrva +",
    socio_ec_str,
    politics_str,
    fiscal_str,
    paste(sts_vars, collapse = " + "), # sts1からsts50までの変数を追加
    "| stid + year" 
  )
)

# regression 
model5 <- feols(
  fml = model_formula5,
  cluster = ~ stid,
  data = stata_data
)
summary(model5)

names(stata_data)


# reg lne031 F10_last_pre F9_pre F8_pre F7_pre F6_pre F5_pre F4_pre F3_pre F2_pre
# uno F0_pre L1_pre L2_pre L3_pre L4_last_pre i.stid i.year sts1-sts50 lnp_income, cluster(stid)	
# eststo trend

