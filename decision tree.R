#library
pacman::p_load("tidyverse","C50","gmodels")

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

# C5.0 decision tree
df_model <- C5.0(df_train[-17], df_train_labels)
summary(df_model)

df_pred <- predict(df_model, df_test)
CrossTable(df_test_labels, df_pred)
table(df_pred == df_test_labels) %>% prop.table()


