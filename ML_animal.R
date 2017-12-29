#Java setting for mac
dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')

library(RWeka)
library(neuralnet)

#normalize
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#setting dataframe
df1 <- read.csv("ML_hanwoo.txt")

df_norm <- as.data.frame(lapply(df1, normalize))
summary(df_norm$windex)

df_train <- df_norm[1:350,]
df_test <- df_norm[351:489,]

df_model <- neuralnet(windex ~ pen_size_animal + month + BW + backfat_thick + eye_rib + carcass_weight + marbling , data=df_train)
plot(df_model)

df_model12 <- neuralnet(windex ~ pen_size_animal + month + BW + backfat_thick + eye_rib + carcass_weight + marbling , data=df_train, hidden = 5)
plot(df_model12)

df_result <- compute(df_model12, df_test[,-7])
predicted_windex <- df_result$net.result
cor(predicted_windex, df_test$windex)[,1]

#단순 다항 회귀식과 비교
reg1 <- lm(windex ~ ., data=df_train)
summary(reg1)
