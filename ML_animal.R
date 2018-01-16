pacman::p_load("tidyverse","RWeka","neuralnet","kernlab","caret")

#setting dataframe
df <- read.csv("hanwoo2.txt",fileEncoding = "EUC-KR")
df1 <- filter(df, is.na(windex)==FALSE)
df1 <- df1[,c("windex","month","weight","등심단면적","근내지방")]


#normalize
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df_norm <- as.data.frame(lapply(df1, normalize))
summary(df_norm$windex)

#set 분리
df_train <- df1[1:8000,]
df_test <- df1[8001:10920,]

# Java setting for mac
dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')

# Neuralnet ####
df_model <- neuralnet(windex ~ month + weight + 등심단면적 + 근내지방, data=df_train)
plot(df_model)

df_model12 <- neuralnet(windex ~ month + weight + 등심단면적 + 근내지방, data=df_train, hidden = 5)
plot(df_model12)

df_result <- compute(df_model12, df_test)
predicted_windex <- df_result$net.result
cor(predicted_windex, df_test$windex)[,1]

# 단순 다항 회귀식과 비교
reg1 <- lm(windex ~ ., data=df_train)
summary(reg1)

# SVM
# begin by training a simple linear SVM

grade_classifier <- ksvm(gradeNm ~ 등지방 + 등심단면적 + 도체중 + 육량지수 + 근내지방 + month, data = df_train,
                          kernel = "vanilladot")

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
grade_classifier_rbf <- ksvm(gradeNm ~ 등지방 + 등심단면적 + 도체중 + 육량지수 + 근내지방 + month, data = df_train,
                             kernel = "rbfdot")
grade_predictions_rbf <- predict(grade_classifier_rbf, df_test)

agreement_rbf <- grade_predictions_rbf == df_test$gradeNm
table(agreement_rbf)
prop.table(table(agreement_rbf))


