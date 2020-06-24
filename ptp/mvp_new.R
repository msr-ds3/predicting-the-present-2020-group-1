library(readxl)
library(lubridate)
library(zoo)
sales <- read_excel("sales.xls")
ym <- as.yearmon(sales$Period, "%b-%Y")
sales$Period <- as.Date(ym)
sales$Value <-as.numeric(sales$Value)
merged <- read_csv("merged.csv")
sales <- sales %>% left_join(merged, by = "Period") %>% select(-sales)
View(sales)
try_model <- sales[1:91,]
try_model$Value <- log(try_model$Value)
model1 <- lm(data = try_model, Value~lag(Value, 1)+lag(Value,12))
summary(model1)
model_with_trend <- lm(data = try_model, Value~lag(Value, 1)+lag(Value,12) + suvs + insurance)
summary(model_with_trend)



baseline <- try_model
# creating baseline 
for (i in 18:91){
  merged_t <- try_model[1:i-1,]
  model1 <- lm(data = merged_t, Value~lag(Value, 1)+lag(Value,12))
  baseline$Value[i] <- predict(model1,try_model[1:i,])[i]
}
baseline <- baseline[18:91,]


# creating trends 

trends <- try_model
for (i in 18:91){
  merged_t <- try_model[1:i-1,]
  model1 <- lm(data = merged_t, Value~lag(Value, 1)+lag(Value,12) + suvs + insurance)
  trends$Value[i] <- predict(model1,try_model[1:i,])[i]
}
trends <- trends[18:91,]

# Make the graph

# actual <- try_model[18:91,]
# actual <- actual %>% 
#   mutate(label ="actual")
# baseline <- baseline %>% 
#   mutate(label = "base")
# trends <- trends %>% 
#   mutate(label ="trends")
# plot_data <- rbind(actual, baseline, trends)
# ggplot(plot_data, aes(x=Period, y = Value, color = label, linetype = label))+
#   geom_line()+
#   scale_colour_manual(values=c("black", "red","grey"))+
#   scale_linetype_manual(values = c("solid", "dashed", "solid"))+
#   ylab('log(mvp)')+
#   xlab('Index')

actual_un <- try_model[18:91,]
actual_un <- actual_un %>%
  mutate(label ="actual_unadjusted") %>% 
  rename(sales=Value)
baseline_un <- baseline %>%
  mutate(label = "base_unadjusted")%>% 
  rename(sales=Value)
trends_un <- trends%>%
  mutate(label ="trends_unadjusted")%>% 
  rename(sales=Value)



merged <- read_csv("merged.csv")
merged$sales<-log(merged$sales)

# model1 <- lm(data = merged, sales~lag(sales, 1)+lag(sales,12))
# summary(model1)
# model_with_trend <- lm(data = merged, sales~lag(sales, 1)+lag(sales,12) + suvs + insurance)
# summary(model_with_trend)


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

plot_data <- rbind(actual, baseline, trends, actual_un,baseline_un,trends_un)
ggplot(plot_data, aes(x=Period, y = sales, color = label, linetype = label))+
  geom_line()+
  scale_colour_manual(values=c("black","black", "red","red","grey","grey"))+
  scale_linetype_manual(values = c("solid","solid","dashed", "dashed", "solid","solid"))+
  ylab('log(mvp)')+
  xlab('Index')

