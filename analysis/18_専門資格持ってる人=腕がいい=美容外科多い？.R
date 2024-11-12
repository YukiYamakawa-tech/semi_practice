sapply(df_doctor.2, summary)

a =df_doctor.2 %>% 
  filter(美容外科ダミー == 1)
sapply(a, summary)

# 外科専門医13人, 整形外科専門医7人, 皮膚科専門医21人(699人中)

a = df_doctor.2 %>% 
  filter(美容ダミー == 1) %>% 
  mutate(美容ダミー = as.factor(美容ダミー))
sapply(a, summary)
# 外科専門医15人、整形外科専門医7人、皮膚科専門医41人(861人中)

# これ持ってる人は美容系多いも無理そう(医者の腕の良さの指標としては使えなそう)

df_doctor.2 %>% 
  filter(美容外科ダミー == 1) %>% 
  filter(美容皮膚科ダミー == 1)
  
