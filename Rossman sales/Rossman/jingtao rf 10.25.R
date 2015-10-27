
library(randomForest)
library(magrittr)

df = read.csv('train.csv')

df$Date%<>%as.Date

dat = df$Date %>% unique

idx = df$Date%in% dat[1:(0.7*length(dat))]

df = df[order(df$Date),]

names(df)

cols = c('Store', 'DayOfWeek', 'Sales', 'Customers', 'Open', 'Promo', 'StateHoliday', "SchoolHoliday")

train = df[idx & df$Store %in% c(1:5), cols]

test = df[!idx& df$Store %in% c(1:5), cols]

dim(train)
dim(test)

rf = randomForest(Sales~., train)

rf
summary(rf)
varImpPlot(rf)

pred = predict(rf, test)

pred%>% head

plot(pred - test$Sales)

rf$oob.times%>%head
