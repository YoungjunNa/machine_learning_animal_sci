df_test <- readxl::read_excel("model1.xlsx")
df_test <- df_test[,2:5]

df_test <- as.data.frame(lapply(df_test, normalize))

df_test <- readxl::read_excel("model1.xlsx")

result <- df_result2$net.result
result <- as.data.frame(result)
result <- cbind(df_test, result)

# back-normalize ####
max <- max(df$CH4d)
min <- min(df$CH4d)

mutate(result, result=min+((max-min)*V1))
