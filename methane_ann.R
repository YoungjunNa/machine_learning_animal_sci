# dataframe ####
df <- readxl::read_excel("methane.xlsx")

# normalize ####
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df_norm <- as.data.frame(lapply(df[,1:10], normalize))
summary(df_norm$DMI)

# data separation (train vs. test) ####
set.seed(3)
indexTrain <- sample(1:nrow(df),round(nrow(df)*0.7))
df_train <- df_norm[indexTrain,]
df_test <- df_norm[-indexTrain,]

# ANN modeling ####
library(neuralnet)
df_model <- neuralnet(CH4d ~ DMI + OMI + CPI + NDFI + DDMI + DOMI + DCPI + DNDFI, data=df_train, hidden = 5)
df_model2 <- neuralnet(CH4d ~ DMI + OMI + CPI + NDFI, data=df_train, hidden = 5)

plot(df_model)
plot(df_model2)

# reg modeling ####
reg1 <- lm(CH4d ~ DMI + OMI + CPI + NDFI + DDMI + DOMI + DCPI + DNDFI, data=df_train)
reg2 <- lm(CH4d ~ DMI + OMI + CPI + NDFI, data=df_train)

# evaluation ####
df_result <- compute(df_model, df_test[,1:8])
df_result2 <- compute(df_model2, df_test[,1:4])
predicted_CH4_reg_1 <- predict(reg1, df_test[,1:8])
predicted_CH4_reg_2 <- predict(reg2, df_test[,1:4])

library(dplyr)

# model 1
predicted_CH4 <- df_result$net.result
cor(predicted_CH4, df_test$CH4d)^2

compare <- data.frame(predicted=predicted_CH4,actual=df_test$CH4d)
RMSPE <- mutate(compare, diff=(actual-predicted)^2)
sqrt(sum(RMSPE$diff)/nrow(RMSPE))

# model 2
predicted_CH4_2 <- df_result2$net.result
cor(predicted_CH4_2, df_test$CH4d)^2

compare2 <- data.frame(predicted=predicted_CH4_2,actual=df_test$CH4d)
RMSPE2 <- mutate(compare2, diff=(actual-predicted)^2)
sqrt(sum(RMSPE2$diff)/nrow(RMSPE2))

# model 3
cor(predicted_CH4_reg_1, df_test$CH4d)^2
compare3 <- data.frame(predicted=predicted_CH4_reg_1,actual=df_test$CH4d)
RMSPE3 <- mutate(compare3, diff=(actual-predicted)^2)
sqrt(sum(RMSPE3$diff)/nrow(RMSPE3))

# model 4
cor(predicted_CH4_reg_2, df_test$CH4d)^2
compare4 <- data.frame(predicted=predicted_CH4_reg_2,actual=df_test$CH4d)
RMSPE4 <- mutate(compare4, diff=(actual-predicted)^2)
sqrt(sum(RMSPE4$diff)/nrow(RMSPE4))

# visulization ####

library(ggplot2)

# function setting (y=x)
myfun <- function(x){
  x
}

a <- ggplot(compare, aes(predicted, actual)) + geom_point() + xlim(0,1) + ylim(0,1) + stat_function(fun=myfun,geom="line") + theme_classic() + labs(x="Predicted", y="Observed", title="(a) ANN model 1")
b <- ggplot(compare2, aes(predicted, actual)) + geom_point() + xlim(0,1) + ylim(0,1) + stat_function(fun=myfun,geom="line") + theme_classic() + labs(x="Predicted", y="Observed", title="(b) ANN model 2")
c <- ggplot(compare3, aes(predicted, actual)) + geom_point() + xlim(0,1) + ylim(0,1) + stat_function(fun=myfun,geom="line") + theme_classic() + labs(x="Predicted", y="Observed", title="(c) Regression model 3")
d <- ggplot(compare4, aes(predicted, actual)) + geom_point() + xlim(0,1) + ylim(0,1) + stat_function(fun=myfun,geom="line") + theme_classic() + labs(x="Predicted", y="Observed", title="(d) Regression model 4")

library(gridExtra)
grid.arrange(a,b,c,d, nrow=2)

# etc ####
a <- summary(lm(CH4d ~ DMI + OMI + CPI + NDFI + DDMI + DOMI + DCPI + DNDFI, data=df_train))
b <- summary(lm(CH4d ~ DMI + OMI + CPI + NDFI, data=df_train))
rbind(a$coefficients, b$coefficients) %>% write.csv("reg.txt")

