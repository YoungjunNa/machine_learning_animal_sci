# library
pacman::p_load("tidyverse","RWeka","neuralnet","kernlab","caret")

# setting dataframe
df <- read.csv("hanwoo2.txt",fileEncoding = "EUC-KR")
df <- filter(df, is.na(windex)==FALSE)
# df1 <- df1[,c("windex","month","weight","등심단면적","근내지방")]

df1 <- df[,c(1:9,14,15,11)]
df1$SexNm <- ifelse(df1$SexNm=="거세",1,0) #dummy coding

colnames(df1) <- c("backfat","rib_area","carcass_weight","windex","marbling","colour","fat_colour","texture","maturity","month","gradeNm","SexNm")

# normalize ####
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df_norm <- as.data.frame(lapply(df1[,1:10], normalize))
df_norm$SexNm <- df1$SexNm

summary(df_norm$month)

# set 분리
df_train <- df_norm[1:8000,]
df_test <- df_norm[8001:10920,]

# Java setting for mac
dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')

# Neuralnet ####
# df_model <- neuralnet(windex ~ backfat + rib_area + carcass_weight + marbling + SexNm, data=df_train)
# plot(df_model)

df_model2 <- neuralnet(windex ~ backfat + rib_area + carcass_weight + marbling + SexNm, data=df_train, hidden = 3)
plot(df_model2)

df_result <- compute(df_model2, df_test[c("backfat","rib_area","carcass_weight","marbling","SexNm")])
predicted_windex <- df_result$net.result
cor(predicted_windex, df_test$windex)

# 단순 다항 회귀식과 비교
reg1 <- lm(windex ~ backfat + rib_area + carcass_weight + marbling + SexNm, data=df_train)
summary(reg1)



# SVM ####
# begin by training a simple linear SVM
grade_classifier <- ksvm(gradeNm ~ windex + backfat + rib_area + carcass_weight + marbling + SexNm, data=df_train, kernel = "vanilladot")

# look at basic information about the model
grade_classifier

## Step 4: Evaluating model performance ----
# predictions on testing dataset
grade_predictions <- predict(grade_classifier, df_test)

head(grade_predictions)

table(grade_predictions, df_test$gradeNm)

# look only at agreement vs. non-agreement
# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- grade_predictions == df_test$gradeNm
table(agreement)
prop.table(table(agreement))

## Step 5: Improving model performance ----
set.seed(12345)
grade_classifier_rbf <- ksvm(gradeNm ~ windex + backfat + rib_area + carcass_weight + marbling + SexNm, data=df_train, kernel = "rbfdot")
grade_predictions_rbf <- predict(grade_classifier_rbf, df_test)

agreement_rbf <- grade_predictions_rbf == df_test$gradeNm
table(agreement_rbf)
prop.table(table(agreement_rbf))


