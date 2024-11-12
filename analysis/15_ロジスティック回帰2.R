
# もう一回ロジスティック回帰 -----------------------------------------------------------

regression.3 = glm(美容外科ダミー ~
                     score_enter+
                     as.factor(出身校名称),
                   data = df_linear.3, family = binomial(link = "logit"))
summary(regression.3)

coef = regression.3$coefficients
exp(coef)

# p値0.0007で係数0.97 ->偏差値が1上がるとオッズが0.97倍（ダメじゃん）

# モデルの当てはまり
Pred = (fitted(regression.3) >= 0.5) %>% 
  factor(levels = c(FALSE, TRUE),
         labels = c("美容外科でない予測", "美容外科である予測"))

table(Pred, a$美容外科ダミー) %>% 
  addmargins()

a = df_linear.3 %>% 
  filter(!is.na(score_enter), !is.na(出身校名称))

