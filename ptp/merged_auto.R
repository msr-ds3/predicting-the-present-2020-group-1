library(tidyverse)
library(modelr)
merged <- read_csv("merged.csv")
merged$sales<-log(merged$sales)
# y<-merged$sales[13:91]
# y1 <- merged$sales[12:90]
# y12 <- merged$sales[1:79]
# logy <- log(y)
# 
# model_data <- data.frame(y,y1,y12)
# summary(lm(data = model_data, y~. ))
# model_data <- data.frame(logy,y1,y12)
# summary(lm(data = model_data, logy~. ))

# model.matrix(lm(data = merged, sales~lag(sales, 1)+lag(sales,12)))

model1 <- lm(data = merged, sales~lag(sales, 1)+lag(sales,12))
summary(model1)
model_with_trend <- lm(data = merged, sales~lag(sales, 1)+lag(sales,12) + suvs + insurance)
summary(model_with_trend)


baseline <- merged
# creating baseline 
for (i in 18:91){
  merged_t <- merged[1:i-1,]
  model1 <- lm(data = merged_t, sales~lag(sales, 1)+lag(sales,12))
  baseline$sales[i] <- predict(model1,merged[1:i,])[i]
}
baseline <- baseline[18:91,]


# creating trends 

trends <- merged
for (i in 18:91){
  merged_t <- merged[1:i-1,]
  model1 <- lm(data = merged_t, sales~lag(sales, 1)+lag(sales,12)+ suvs + insurance)
  trends$sales[i] <- predict(model1,merged[1:i,])[i]
}
trends <- trends[18:91,]

# Make the graph

actual <- merged[18:91,]
actual <- actual %>% 
  mutate(label ="actual")
baseline <- baseline %>% 
  mutate(label = "base")
trends <- trends %>% 
  mutate(label ="trends")
plot_data <- rbind(actual, baseline, trends)
ggplot(plot_data, aes(x=Period, y = sales, color = label, linetype = label))+
  geom_line()+
  scale_colour_manual(values=c("black", "red","grey"))+
  scale_linetype_manual(values = c("solid", "dashed", "solid"))+
  ylab('log(mvp)')+
  xlab('Index')

# means absolute error
mean(abs(trends$sales-actual$sales))
mean(abs(baseline$sales-actual$sales))

# data for recession period Dec 2007 to June 2009
recession_trends <- trends %>% 
  filter(Period>="2007-12-01"& Period<="2009-06-01")
recession_base <- baseline %>% 
  filter(Period>="2007-12-01"& Period<="2009-06-01")
recession_actual <- actual %>% 
  filter(Period>="2007-12-01"& Period<="2009-06-01")
mean(abs(recession_trends$sales-recession_actual$sales))
mean(abs(recession_base$sales-recession_actual$sales))


# Overall improvement 
(mean(abs(baseline$sales-actual$sales))-mean(abs(trends$sales-actual$sales)))/mean(abs(baseline$sales-actual$sales))


# recession improvement
(mean(abs(recession_base$sales-recession_actual$sales))-mean(abs(recession_trends$sales-recession_actual$sales)))/mean(abs(recession_base$sales-recession_actual$sales))
