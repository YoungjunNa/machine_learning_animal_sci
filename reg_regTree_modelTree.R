#library
pacman::p_load("tidyverse","rpart","rpart.plot","RWeka")


#setting dataframe
df <- read.csv("hanwoo2.txt",fileEncoding = "EUC-KR")
df <- filter(df, is.na(windex)==FALSE)
# df1 <- df1[,c("windex","month","weight","등심단면적","근내지방")]

df1 <- df[,c(1:9,14,15,11)]
df1$SexNm <- ifelse(df1$SexNm=="거세",1,0) #dummy coding

str(df1)

table(df1$gradeNm)

# separate dataset
df_train <- df1[1:8000,-11]
df_test <- df1[8001:10920,-11]

df_train_labels <- df1[1:8000,11]
df_test_labels <- df1[8001:10920,11]

# simple reg
lm(육량지수 ~ 도체중 + 근내지방 + month*SexNm, data=df_train) %>% summary()
lm(육량지수 ~ ., data=df_train) %>% summary()
lm(육량지수 ~ 등지방 + 등심단면적 + 도체중 + 근내지방 + month*SexNm, data=df_train) %>% summary()

# reg tree
m.rpart <- rpart(육량지수 ~ ., data=df_train)
m.rpart
rpart.plot(m.rpart, digits=2)

p.rpart <- predict(m.rpart, df_test)
cor(p.rpart,df_test$육량지수)

# model tree
df2 <- df1
colnames(df2) <- c("backfat","rib_area","carcass_weight","windex","marbling","colour","fat_colour","texture","maturity","month","gradeNm","SexNm")

df_train2 <- df2[1:8000,-11]
df_test2 <- df2[8001:10920,-11]

df_train_labels2 <- df2[1:8000,11]
df_test_labels2 <- df2[8001:10920,11]

m.m5p <- M5P(windex ~ ., data=df_train2)
m.m5p

p.m5p <- predict(m.m5p, df_test2)

cor(p.m5p, df_test$windex)
