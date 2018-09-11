# 평균의 허상
# library
pacman::p_load("tidyverse")

# dataframe
df <- read.csv("hanwoo2.txt",fileEncoding = "EUC-KR")
df <- filter(df, is.na(windex)==FALSE)
df <- filter(df, SexNm=="거세")

df %>% ggvis(~month,~weight) %>% layer_points(size=0.1)

# average
average <- data.frame(등지방=round(mean(df$등지방),0),등심단면적=round(mean(df$등심단면적),0),도체중=round(mean(df$도체중),0),근내지방=round(mean(df$근내지방),0))

variation <- 0.1

#도체중 제외
result <- filter(df, 등지방<=(average$등지방*(1+variation)) & 등지방>(average$등지방*(1-variation)) & 등심단면적<=(average$등심단면적*(1+variation)) & 등심단면적>(average$등심단면적*(1-variation)) & 근내지방<=(average$근내지방*(1+variation)) & 근내지방>(average$근내지방*(1-variation))) %>% nrow()

result/nrow(df)

#도체중 추가
result1 <- filter(df, 등지방<=(average$등지방*(1+variation)) & 등지방>(average$등지방*(1-variation)) & 등심단면적<=(average$등심단면적*(1+variation)) & 등심단면적>(average$등심단면적*(1-variation)) & 근내지방<=(average$근내지방*(1+variation)) & 근내지방>(average$근내지방*(1-variation)) & 도체중<=(average$도체중*(1+variation)) & 도체중>(average$도체중*(1-variation))) %>% nrow()

result1/nrow(df)

mean(df$도체중)
filter(df, 도체중==448) %>% nrow()

# visualization
ggplot(df, aes(x=month,y=도체중)) + geom_point()

library(ggvis)
df %>% ggvis(~month, ~도체중) %>% layer_points()
