# 病院データ見てみる
df_hospital = readxl::read_excel("1_data/HUM01_DCF_施設ファイル.xlsx")
sapply(df_hospital, summary)
# $患者数平均_外来
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    30.0    50.0    72.3    80.0  3758.4   49350 

# $患者数平均_在宅
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    0.00    0.70    4.08    3.70   99.60   86227 

# ここら辺は使えそうやなNAも少ないし

# 勤務先データ見てみる
df_勤務先ファイル = readxl::read_excel("1_data/HUM03_DCF_勤務先ファイル.xlsx")
sapply(df_勤務先ファイル, summary)


# 勤務先データから施設コードと個人コードを取得 --------------------------------------------------
df_key = df_勤務先ファイル %>% 
  select(個人コード, DCF施設コード)

# 病院データから使えそうな列を選択して、勤務先データとdcfコードで結合
df_hospital.2 = df_hospital %>% 
  select(DCF施設コード, '正式施設名（漢字）', 都道府県, 経営体, 許可病床数_合計, 
         患者数平均_外来, 患者数平均_在宅) %>% 
  inner_join(df_key, by = "DCF施設コード")

df_doctor.3 = df_doctor %>% 
  inner_join(df_hospital.2, by = "個人コード") %>% 
  # 卒業年 - 生年月日 < 24 はだめよ
  filter(!(卒業年 - 生年月日_年 < 24)) %>%
  
  # 入学年度を作成
  mutate(year_enter = as.character(卒業年 - 6)) %>% 
  
  # 出身校コードを1 -> 001のように変えて大学データの出身校コードと同じようにする
  mutate(出身校コード = sprintf("%03d", 出身校)) %>% 
  
  # 入学年度で医者データと大学データをがっしゃんこ
  inner_join(df_uni_new, by = c("出身校コード", "year_enter" = "year")) %>% 
  
  mutate(
    # 美容外科ダミーの作成
    美容外科ダミー = ifelse(診療科目_１ == "J02" |
                       診療科目_２ == "J02" |
                       診療科目_３ == "J02" |
                       診療科目_４ == "J02" |
                       診療科目_５ == "JO2", 1, 0),
    
    # 美容外科ダミーのNAを0にかえる
    美容外科ダミー = coalesce(美容外科ダミー, 0),
    
    # score列をわかりやすく当時の偏差値列に
    score_enter = as.numeric(score),
    
    # 年齢の追加
    age = 2024 - as.numeric(生年月日_年)
  ) %>% 
  
  # 専門医の資格列をfactor型に 
  mutate_at(vars(性別, 開勤区分, 皮膚科専門医: 美容外科ダミー), .funs = list(~as.factor(.))) %>% 
  
  # 見やすくするために入学時の大学偏差値を現在の大学偏差値の前に
  relocate(score_enter, .before = score_now) %>% 
  
  # 見やすくするために入学年度を卒業年の前に
  relocate(year_enter, .before = 卒業年)


# 兼務のせいで同じ個人コードがいるはず。
df_doctor.3 %>% 
  group_by(個人コード) %>% 
  mutate(count = n()) %>%
  relocate(count, .before = 性別) %>% 
  filter(count > 1) %>% 
  distinct(個人コード, .keep_all = TRUE) %>% 
  group_by(count) %>% 
  summarise(n())

# 重いから使わなそうな変数は消す
df_linear.3 = df_doctor.3 %>% 
  select(2:5, 10, 18, 79:84, 89:92, 94, 96:99)

# 平均患者数見たい ----------------------------------------------------------------
sapply(df_linear.3, summary)

df_linear.3 = df_linear.3 %>% 
  mutate(患者数平均 = as.numeric(患者数平均_外来) + as.numeric(患者数平均_在宅))

# 平均患者数と病床数の関係
df_linear.3 %>% 
  filter(!is.na(許可病床数_合計), !is.na(患者数平均)) %>% 
  ggplot2::ggplot(aes(x = as.numeric(許可病床数_合計), y = 患者数平均))+
  geom_point()

# 美容外科ダミー1の人の平均患者と病床数も観たい
df_linear.3 %>% 
  filter(!is.na(許可病床数_合計), !is.na(患者数平均)) %>% 
  ggplot2::ggplot(aes(x = as.numeric(許可病床数_合計), y = 患者数平均,
                  fill = 美容外科ダミー))+
  geom_point()

df_linear.3 %>% 
  filter(美容外科ダミー == 1) %>% 
  sapply(., summary)
# 患者数平均NAが1115件もあっちゃ無理だあ1276人しかいないんだから

df_doctor.3 %>% 
  filter(美容外科ダミー == 1) %>% 
  group_by(個人コード) %>% 
  mutate(count = n()) %>%
  relocate(count, .before = 性別) %>% 
  filter(count > 1) %>% 
  distinct(個人コード, .keep_all = TRUE) %>% 
  group_by(count) %>% 
  summarise(n())

225+88*2+42*3+7*4+3*5+7
# 1276 - 577 = 599件ある（かぶりなしで）