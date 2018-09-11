test <- data.frame(DMI=10,OMI=9,CPI=2,NDFI=5)

base_frame <- function() {
  # attr(base_frame,"unit") <- "g/d"
  assign("base_frame",data.frame(DMI=NA,OMI=NA,CPI=NA,NDFI=NA,unit="g/d"),envir = globalenv())
}

base_frame()
attributes(base_frame)

base_frame[1,1:4] <- c(10,20,30,40)

model2$model.list
result <- compute(model2,base_frame[,1:4])$net.result
attr(result,"unit") <- "L/d"

result
str(result)
