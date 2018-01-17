#library
pacman::p_load("tidyverse","class","gmodels")

#setting dataframe
df <- read.csv("hanwoo2.txt",fileEncoding = "EUC-KR")
df <- filter(df, is.na(windex)==FALSE)
# df1 <- df1[,c("windex","month","weight","등심단면적","근내지방")]

df1 <- df[,c(1:9,14,15,11)]
df1$SexNm <- ifelse(df1$SexNm=="거세",1,0) #dummy coding

str(df1)

table(df1$gradeNm)

#normalize ####
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df_norm <- as.data.frame(lapply(df1[,1:10], normalize))
df_norm$SexNm <- df1$SexNm

summary(df_norm$month)

#set 분리
df_train <- df_norm[1:8000,]
df_test <- df_norm[8001:10920,]

df_train_labels <- df1[1:8000,11]
df_test_labels <- df1[8001:10920,11]

#train
sqrt(10)
df_test_pred <- knn(train = df_train, test = df_test, cl = df_train_labels, k = 3)

#evaluation
CrossTable(x = df_test_labels, y = df_test_pred, prop.chisq = FALSE)
table(df_test_pred == df_test_labels) %>% prop.table()

#z-scale normalize ####
df_z <- as.data.frame(scale(df1[,1:10]))
summary(df_z)

#set 분리
df_train <- df_z[1:8000,]
df_test <- df_z[8001:10920,]

df_train_labels <- df1[1:8000,11]
df_test_labels <- df1[8001:10920,11]

#train
sqrt(10)
df_test_pred <- knn(train = df_train, test = df_test, cl = df_train_labels, k = 3)

#evaluation
CrossTable(x = df_test_labels, y = df_test_pred, prop.chisq = FALSE)
table(df_test_pred == df_test_labels) %>% prop.table()
