
# 周辺科に比べて頭いいは無理？ ----------------------------------------------------------
# 他の外科と比べてみるか

# 読み替えテーブル持ってくる
df_診療科名 = readxl::read_excel("1_data/HUM04_読み替えテーブル.xlsx")

df_doctor.2 = df_doctor.2 %>% 
  inner_join(df_診療科名, by = c("診療科目_１" = "診療科目コード"))

df_doctor.2 %>% 
  filter(str_detect(診療科目名称, "\\外科")|
           美容外科ダミー == 1) %>% 
  filter(!開勤区分 == 9) %>% 
  filter(!is.na(score_enter)) %>% 
  select(美容外科ダミー, score_enter, 性別, 開勤区分, age, 出身校名称) %>% 
  tbl_summary(by = 美容外科ダミー) %>% 
  add_n() %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "記述統計量_外科のみ.docx")

regression.5 = glm(美容外科ダミー ~
                     score_enter +
                     性別 +
                     開勤区分 +
                     age +
                     I(age^2) +
                     as.factor(出身校名称),
                   data = df_doctor.2, family = binomial(link = "logit"))
summary(regression.5)
step.reg.5 = step(regression.5)
stargazer::stargazer(regression.5, type = "text", single.row = TRUE, ci = TRUE)
# もはや偏差値が有意じゃなくなる
coef = regression.5$coefficients
exp(coef)


# 形成外科だけでやってみるか -----------------------------------------------------------
a = df_doctor.2 %>% 
  mutate(
    # 形成外科ダミーの作成
    形成外科ダミー = ifelse(診療科目_１ == "J01" |
                        診療科目_２ == "J01" |
                        診療科目_３ == "J01" |
                        診療科目_４ == "J01" |
                        診療科目_５ == "JO1", 1, 0),
  ) %>% 
  filter(美容外科ダミー == 1 | 形成外科ダミー == 1) %>% 
  filter(!is.na(score_enter)) %>% 
  select(美容外科ダミー, score_enter, 性別, 開勤区分, age, 出身校名称) %>% 
  tbl_summary(by = 美容外科ダミー) %>% 
  add_n() %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "記述統計量_vs形成外科.docx")

regression.6 = glm(美容外科ダミー ~
                     score_enter +
                     性別 +
                     開勤区分 +
                     age +
                     I(age^2) +
                     as.factor(出身校名称),
                   data = a, family = binomial(link = "logit"))
summary(regression.6)
stargazer::stargazer(regression.6, type = "text", single.row = TRUE, ci = TRUE)

# もはや偏差値が有意じゃなくなる


df_doctor %>% 
  filter(卒業年 - 6 < 1980)

exp(0.024)
