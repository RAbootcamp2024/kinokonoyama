library(tidyverse)
library(haven)
stata_data <- haven::read_dta("C:/Users/Owner/Downloads/112082-V1/Political-Responsiveness--State-Level-/Generate-Dataset/State_Level_Dataset_Modified.dta")
stata_data <- stata_data |>
  mutate(uno = 0) 

# Baseline Regression----
model1 <- feols(lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +
                  F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +
                  L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income | stid + year, 
                cluster = ~ stid, data = stata_data)
# reg lne031 F10_last_pre F9_pre F8_pre F7_pre F6_pre F5_pre F4_pre F3_pre F2_pre uno F0_pre L1_pre L2_pre L3_pre L4_last_pre i.stid i.year lnp_income, cluster(stid)	
# eststo no_trend
summary(model1)

## figure(PanelA)----
coefplot(model1,
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
model_formula2 <- as.formula(
  paste(
    "lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +",
    "F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +",
    "L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income +",
    paste(sts_vars, collapse = " + "), # sts1からsts50までの変数を追加
    "| stid + year"
  )
)

# regression 
model2 <- feols(
  fml = model_formula2,
  cluster = ~ stid,
  data = stata_data
)
summary(model2)

#figure(PanelB)----
coefplot(model2,
         keep = c("F10_last_pre", "F9_pre", "F8_pre", "F7_pre", 
         "F6_pre", "F5_pre", "F4_pre", "F3_pre", "F2_pre", "uno", "F0_pre", 
                  "L1_pre", "L2_pre", "L3_pre", "L4_last_pre"),
         main = "PanelB. DD model with state time trend")


library(fixest)

##Regression with other voting reforms----

# model3 <- feols(lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +
#                   F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +
#                   L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income + 
#                   same + online + nrva +, 
#                 paste0("sts", 1:50) | stid + year, 
#                 cluster = ~ stid, data = stata_data)

# 回帰分析のモデル式を作成
model_formula3 <- as.formula(
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
model3 <- feols(
  fml = model_formula3,
  cluster = ~ stid,
  data = stata_data
)
summary(model3)

# global socio_ec med_age sh16_25 lnenrollment blk_pop_sh wht_pop_sh gini lnpopulation unemp
# global politics i.gub_years democ incumbent gov_run_again past_gov_dem rel_margin turnout_rate term_limited
# global fiscal lne001 current_exp lnr05
socio_ec <- c("med_age", "sh16_25", "lnenrollment", "blk_pop_sh", "wht_pop_sh", "gini", "lnpopulation", "unemp")

politics <- c("democ", "incumbent", "gov_run_again", "past_gov_dem", "rel_margin", "turnout_rate", "term_limited")
fiscal <- c("lne001", "current_exp", "lnr05")

socio_ec_str <- paste(socio_ec, collapse = " + ")
politics_str <- paste(politics, collapse = " + ")
fiscal_str <- paste(fiscal, collapse = " + ")

model_formula4 <- as.formula(
  paste(
    "lne031 ~ F10_last_pre + F9_pre + F8_pre + F7_pre + F6_pre +
                  F5_pre + F4_pre + F3_pre + F2_pre + uno + F0_pre +
                  L1_pre + L2_pre + L3_pre + L4_last_pre + lnp_income + 
                  same + online + nrva +",
    socio_ec_str,
    "+",
    politics_str,
    "+",
    fiscal_str,
    "+",
    paste(sts_vars, collapse = " + "), # sts1からsts50までの変数を追加
    "| stid + year + gub_years" 
  )
)

# regression 
model4 <- feols(
  fml = model_formula4,
  cluster = ~ stid,
  data = stata_data
)
summary(model4)

# regression with average effect5----
#   eststo: reg lne031 pre_reg_s i.stid i.year sts1-sts50 lnp_income $reforms $socio_ec $politics $fiscal, cluster(stid)
# eststo reg_control_all

model_formula5 <- as.formula(
  paste(
    "lne031 ~ lnp_income + pre_reg_s + same + online + nrva +",
    socio_ec_str,
    "+",
    politics_str,
    "+",
    fiscal_str,
    "+",
    paste(sts_vars, collapse = " + "), # sts1からsts50までの変数を追加
    "| stid + year + gub_years" 
  )
)

# regression 
model5 <- feols(
  fml = model_formula5,
  cluster = ~ stid,
  data = stata_data
)
summary(model5)


# パッケージの読み込み
library(sjPlot)
# モデル結果をテーブル表示
tab_model(model5,
          terms = c("F0_pre", "L1_pre", "L2_pre",  "L3_pre", "L4_last_pre","pre_reg_s"))


library(modelsummary)


df_summary <- modelsummary::msummary(
  list(
    "model1" = model1,
    "model2" = model2,
    "model3" = model3,
    "model4" = model4,
    "model5" = model5
  ),
  statistic = c("std.error"),
 # coef_map = 
    # c("F0_pre" = "F0_pre"),
    #"model1" = c("F0_pre", "L1_pre", "L2_pre",  "L3_pre", "L4_last_pre"),
    # "model2" = c("F0_pre", "L1_pre", "L2_pre",  "L3_pre", "L4_last_pre"),
    # "model3" = c("F0_pre", "L1_pre", "L2_pre",  "L3_pre", "L4_last_pre"),
    # "model4" = c("F0_pre", "L1_pre", "L2_pre",  "L3_pre", "L4_last_pre"),
    # "model5" = c("pre_reg_s")
  title = "table2",
  output = "data.frame",
  digits = 3
)
