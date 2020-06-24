library(tidyverse)
motor <- read.table("CopyOfmotor_vehicles.txt", header = T)
str(motor)
motor_04 <- motor %>% filter(YEAR>=2004 & YEAR <= 2012)
y <- rep(0,96)
l<-1
for (i in 2:9){
  for (j in 2:13){
    y[l] <- motor_04[i,j]
    l=l+1
  }
}
y1<- y[-96]
y1<- c(motor_04[1,13],y1)
y12<-rep(0,96)
l<-1
for (i in 1:8){
  for (j in 2:13){
    y12[l] <- motor_04[i,j]
    l=l+1
  }
}
logy <- log10(y)

data_for_model <- data.frame(y,y1,y12)

data_for_model_80 <- data_for_model[1:80,]

summary(lm(data = data_for_model_80, y~.))

data_for_model <- data.frame(logy,y1,y12)

data_for_model_80 <- data_for_model[1:80,]
summary(lm(data = data_for_model_80, logy~.))

