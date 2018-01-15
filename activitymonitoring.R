rm(list=ls())

#Read the data
am<- read.csv(file.choose(), header = TRUE)
View(am)

#Total Missing values 
library(mice)
md.pattern(am) #2304 missing values in steps
colSums(is.na(am))

#Summary and structure of data set
summary(am)
str(am) #Date is in factor format
am$date<-as.Date(am$date)

#Calculate total number of steps each day
dailystepsmissing<-as.data.frame.table(tapply(am$steps, format(am$date, '%Y-%m-%d'), sum))
str(dailystepsmissing) #Convert date to date class
dailystepsmissing$Var1<-as.Date(dailystepsmissing$Var1)

#Histogram of total number of steps each day
library(ggplot2)
ggplot(dailystepsmissing, aes(x=Var1, y=Freq))+geom_histogram(stat="identity", fill="salmon")

# Average/Mean number of steps taken each day
library(dplyr)
mean_steps<- as.data.frame( am %>% group_by(date) %>% summarise(mean(steps)))
head(mean_steps,10)

#Median steps taken each day
median_steps<- as.data.frame( am %>% group_by(date) %>% summarise(median(steps,na.rm = FALSE)))
head(median_steps,10)

#Time Series plot of total number of steps without imputing Missing Values 
library(plotly)
p1 <- plot_ly(x = dailystepsmissing$Var1, y = dailystepsmissing$Freq, mode = 'lines') %>%
  layout(
    title = "Histogram of daily Steps with Missing Values"
  ) 
show(p1)

#5 minute interval that contains maximum number of steps each day
#Use aggregate to create new data frame with the maxima
am_agg <- aggregate(steps ~ date, am, max)
# then simply merge with the original
am_max <- merge(am_agg, am)
head(am_max,10)

#Using mice package to impute missings
#Mice package is a machine learning package which will consider other numeric features to impute 
#the missing package
am1<-am[,c(1,3)]
set.seed(123)
index1<- sample(2, nrow(am1), replace= T, prob = c(0.7,0.3))
library(mice)
init <- mice(am1, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix


set.seed(100)
imputed = mice(am1, method=meth, predictorMatrix=predM, m=5)
imputed <- complete(imputed)
sapply(imputed, function(x) sum(is.na(x)))
am$steps<-imputed$steps
summary(am)

#Calculate total number of steps each day
dailysteps<-as.data.frame.table(tapply(am$steps, format(am$date, '%Y-%m-%d'), sum))
str(dailysteps) #Convert date to date class
dailysteps$Var1<-as.Date(dailysteps$Var1)

#Histogram using ggplot
library(ggplot2)
ggplot(dailysteps, aes(x = dailysteps$Var1, y = dailysteps$Freq)) +
  geom_bar(stat = "identity", fill = "salmon")+ xlab('Date')+ylab('Number of steps')
#Plotting histogram using plotly
library(plotly)
p <- plot_ly(dailysteps,aes(x = dailysteps$Var1, y = dailysteps$Freq), type = "bar")
show(p)

#Panel Plot containing average number of steps taken per 5-minute interval across weekdays and weekends



