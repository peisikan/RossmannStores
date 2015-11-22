
library(randomForest)
library(magrittr)
library(h2o)
library(ggplot2)

h2o.shutdown()
localH2O = h2o.init(nthreads=-1,max_mem_size='6G')

df = read.csv('train.csv') %>% merge(read.csv('store.csv'), by = 'Store')
test = read.csv('test.csv') %>% merge(read.csv('store.csv'), by = 'Store')

summary(df)
summary(test)

df$Store%<>% as.factor
df$Date%<>%as.Date
df$DayOfWeek %<>% as.factor
df$Open%<>% as.factor
df$Promo%<>% as.factor
df$SchoolHoliday %<>% as.factor
df$Promo2 %<>% as.factor
df$Month = as.character(df$Date, '%m') %>% as.factor
df$Quarter = switch(df$Month, )
df[is.na(df$CompetitionDistance), 'CompetitionDistance'] = mean(df$CompetitionDistance, na.rm = T)

test$Store%<>% as.factor
test$Date%<>%as.Date
test$Open%<>% as.factor
test$Promo%<>% as.factor
test$SchoolHoliday %<>% as.factor
test$Promo2 %<>% as.factor
# test$month = as.character(test$Date, '%m')
test[is.na(test$CompetitionDistance), 'CompetitionDistance'] = mean(test$CompetitionDistance, na.rm = T)


summary(df)
summary(test) 

plot(df[df$Store ==2, 'Customers'])

#========= Store 1 ===========#

plot(df[df$Store == 1 & df$Sales !=0, c('Date','Sales')])

df$Date %>% summary
test$Date %>% summary

testNum = test$Date %>% unique %>% length
testStartDate = max(df$Date) - testNum + 1

train = df[df$Date < testStartDate, ]
test = df[df$Date >= testStartDate, ]

t = df[df$Store == 1 & df$Month == '08',]
ggplot(t, aes(x = factor(DayOfWeek), y = Sales)) + geom_boxplot()



#=========== Model ==========#

test$Customers = aggregate(Customers~Store + DayOfWeek + Promo, df, median) %>% 
  merge(test, by = names(.)[names(.)!= 'Customers'])

train = as.h2o(df)

features<-colnames(train)[!(colnames(train) %in% c('Id',"Date","Sales"))]

rf = h2o.randomForest(features, 'Sales', train)

summary(rf)

summary(df)

testHex<-as.h2o(test)

## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(rf,testHex))

pred <- predictions[,1]
summary(pred)
submission <- data.frame(Id=test$Id, Sales=pred)



pred = aggregate(Sales~Store, df, mean) %>% merge(test, by = 'Store', all.y = T)
submission <- data.frame(Id=test$Id, Sales=pred$Sales)

write.csv(submission, "h2o_rf.csv",row.names=F)

