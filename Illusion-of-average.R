#평균의 허상

# library
pacman::p_load("tidyverse")

# dataframe
df <- read.csv("hanwoo2.txt",fileEncoding = "EUC-KR")
df <- filter(df, is.na(windex)==FALSE)

# average
average <- data.frame(등지방=round(mean(df$등지방),0),등심단면적=round(mean(df$등심단면적),0),도체중=round(mean(df$도체중),0),근내지방=round(mean(df$근내지방),0))

filter(df, 등지방==13 & 등심단면적==92 & 도체중==437 & 근내지방==5)

limit <- 2

filter(df, 등지방<=(13+limit) & 등지방>(13-limit) & 등심단면적<=(92+limit) & 등심단면적>(92-limit) & 도체중<=(437+limit) & 도체중>(437-limit) & 근내지방<=(5+limit) & 근내지방>(5-limit)) %>% nrow()
