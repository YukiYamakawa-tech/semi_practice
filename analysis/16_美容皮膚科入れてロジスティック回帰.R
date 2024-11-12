
# 美容皮膚科とかも含めてやってみるか -------------------------------------------------------
df_doctor.2 = df_doctor.2 %>% 
  mutate(
    # 美容皮膚科ダミーの作成
    美容皮膚科ダミー = ifelse(診療科目_１ == "J03" |
                       診療科目_２ == "J03" |
                       診療科目_３ == "J03" |
                       診療科目_４ == "J03" |
                       診療科目_５ == "JO3", 1, 0),
    
    # 美容皮膚科ダミーのNAを0にかえる
    美容皮膚科ダミー = coalesce(美容皮膚科ダミー, 0)) %>% 
  relocate(美容皮膚科ダミー, .before = 美容外科ダミー) %>% 
  
  # 美容皮膚科ダミーまたは美容外科ダミーが1のやつに美容ダミー
  mutate(美容ダミー = ifelse(美容皮膚科ダミー == 1 |
                          美容外科ダミー == 1, 1, 0)) %>% 
  relocate(美容ダミー, .before = 美容皮膚科ダミー) %>% 
  
  # 開勤区分が不明のやつ除く
  filter(!開勤区分 == 9)

# 学校別で美容系の医者とそうでない医者に違いがないか見てみよう
df_doctor.2 %>% 
  filter(!is.na(出身校名称), !is.na(score_enter)) %>% 
  ggplot2::ggplot(aes(x = score_enter, fill = as.factor(美容ダミー)))+
  geom_histogram(alpha = 0.6, binwidth = 1, position = "identity")


# 記述統計量
library(gtsummary)
df_doctor.2 %>% 
  filter(!is.na(score_enter)) %>% 
  select(美容ダミー, score_enter, 性別, 開勤区分, age, 出身校名称) %>% 
  tbl_summary(by = 美容ダミー) %>% 
  add_n() %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "記述統計量_美容ダミー.docx")

# ロジスティック回帰

regression.4 = glm(美容ダミー ~
                     score_enter +
                     性別 +
                     開勤区分 +
                     age +
                     I(age^2) +
                     as.factor(出身校名称),
                   data = df_doctor.2, family = binomial(link = "logit"))
summary(regression.4)
step.reg.4 = step(regression.4)
stargazer::stargazer(step.reg.4, type = "text", single.row = TRUE, ci = TRUE)

coef = regression.4$coefficients
exp(coef)