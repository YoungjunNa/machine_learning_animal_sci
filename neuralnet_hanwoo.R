library(dplyr)
library(hanwoo)

df <- readxl::read_excel("nn_data.xlsx")

# date
df[,2] <- lapply(df[,2], FUN = function(x){
  gsub('\\.', '/', x) %>%
    lubridate::ymd()
})

# char into num
df[,-c(1,2,3,9)] <- as.numeric(unlist(df[,-c(1,2,3,9)]))

# bind bull info
bull <- hanwoo_bull(df$아빠[1])
for(i in 2:nrow(df)) {
  bind <- hanwoo_bull(df$아빠[i])
  bull <- rbind(bull,bind)
}

str(bind)

df1 <- cbind(df, bull[,-c(4,13,14,20,21,24,25,27,28)])
df1[,c(20:40)] <- as.numeric(unlist(df1[,c(20:40)]))
df2 <- df1[,-c(1,2,3,9)]
str(df2)

# neuralnet
library(neuralnet)

# normalize ####
normalize <- function(x){
  return((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)))
}
df_norm <- as.data.frame(lapply(df2, normalize))
df_norm <- df_norm[complete.cases(df_norm),]

# data separation (train vs. test) ####
set.seed(4)
indexTrain <- sample(1:nrow(df1),round(nrow(df1)*0.7))
df_train <- df_norm[indexTrain,]
df_test <- df_norm[-indexTrain,]

# ANN modeling ####
frm <- as.formula(paste(
                        paste(names(df_train)[1:5], collapse = "+"), 
                        " ~ ", 
                        paste(names(df_train)[-c(1:5)], collapse= "+"))
                  )

nn <- neuralnet(frm, data=df_train, hidden = 5)
# plot(nn)

df_result <- compute(nn, df_test[,-c(1:5)])
predicted <- df_result$net.result
predicted

cor(predicted, df_test[,c(1:5)])
