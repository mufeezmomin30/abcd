library(readr)
library(datasets)
library(ggplot2)
library(ggExtra)
library(rstudioapi)
library(corrplot)
library(Metrics)
library(dplyr)
library(tidyr)
library(caret)
library(tictoc)
library(lubridate)
#install.packages("ggExtra")
#install.packages("corrplot")
rm(list=ls())
setwd("C:/Users/ayush/Desktop/MBA_IIMU_TERMWISE/Term 5/AMDA/R code")
# load datasets
rawData<- read_csv("hour.csv")
# Pre-processing data -----------------------------------------------------


processedData<-rawData
processedData$weeknum<-week(processedData$dteday)
processedData$dteday<- NULL
processedData<-processedData[,c(1,17,2:16)]

for(i in unique(processedData$season)) {
  processedData[[paste0("season_",i)]] <- ifelse(processedData$season==i,1,0)
}
for(i in unique(processedData$weekday)) {
  processedData[[paste0("weekday_",i)]] <- ifelse(processedData$weekday==i,1,0)
}
for(i in unique(processedData$weathersit)) {
  processedData[[paste0("weathersit_",i)]] <- ifelse(processedData$weathersit==i,1,0)
}
for(i in unique(processedData$mnth)) {
  processedData[[paste0("mnth_",i)]] <- ifelse(processedData$mnth==i,1,0)
}
for(i in unique(processedData$hr)) {
  processedData[[paste0("hour_",i)]] <- ifelse(processedData$hr==i,1,0)
}

for(i in unique(processedData$weeknum)) {
  processedData[[paste0("weeknum_",i)]] <- ifelse(processedData$weeknum==i,1,0)
}
# Checkign ACF
acf(x=processedData$cnt, lag.max=10 , plot=TRUE)

#creating lag values
processedData$lag1<-0
processedData$lag2<-0
processedData$lag3<-0

processedData$lag1[2:17379]=processedData$cnt[1:17378]
processedData$lag2[3:17379]=processedData$cnt[1:17377]
processedData$lag3[4:17379]=processedData$cnt[1:17376]


# Fix cyclic variables
processedData$weeknum<-pmin(abs(3-processedData$weeknum),56-processedData$weeknum);  
processedData$mnth<-pmin(abs(1-processedData$mnth),13-processedData$mnth);  
processedData$hr<-pmin(abs(4-processedData$hr),28-processedData$hr);  


# move count to end
processedData<- processedData[c(1:16, 18:81,17)]
# remove excessive columns
processedData<- processedData[-c(1,3,8,10,15,16)]

processedData<- processedData[3:17379,]

# Exploratory data analysis -----------------------------------------------


# plot  atemp
install.packages("ggExtra")
library(ggplot2)
library(ggExtra)
p<-ggplot(processedData,aes(x=atemp,y=cnt))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Adjusted Temperature', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))


# plot temp
p<-ggplot(rawData,aes(x=temp,y=cnt))+
  geom_point(alpha=0.07,color='orange')+
  labs(x='Temperature', y= 'Hourly Usage cnt')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot humidity
p<-ggplot(processedData,aes(x=hum,y=cnt))+
  geom_point(alpha=0.07, color='violet')+
  labs(x='Humidity', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot weather situation
p<-ggplot(rawData,aes(x=weathersit,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color='green')+
  labs(x='Weather situation', y= 'Hourly Usage Count')+
  geom_smooth(method='loess')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot wind-speed
p<-ggplot(rawData,aes(x=windspeed,y=cnt))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Wind speed', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# correlation plot
install.packages("corrplot")
library(corrplot)
cont_data<-rawData[c(11:17)]
o=corrplot(cor(cont_data),method='number')

# plot 24 hours
ggplot(rawData,aes(x=hr,y=cnt))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'coral')+
  labs(x='Military time', y= 'Usage count')+
  geom_smooth()

# plot hours from 4am using processed Data
ggplot(processedData,aes(x=hr,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Hours from 4am', y= 'Hourly Usage Count')+
  geom_smooth(method='lm', color='red')+
  geom_smooth(method='loess', color ='blue')

# Plot Months
ggplot(rawData,aes(x= mnth,y=cnt))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'green')+
  labs(x='Month number', y= 'Hourly Usage Count')+
  geom_smooth()

# plot months from January
ggplot(processedData,aes(x=mnth,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Months from January', y= 'Usage count')+
  geom_smooth(method='lm', color ='black')+
  geom_smooth(method='loess', color ='blue')

# Boxplots -plot year vs count
rawData$yr<-factor(rawData$yr)
ggplot(data=rawData,aes(x=yr,y=cnt,  fill=yr ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Year', y= 'Hourly Usage Count')


# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
data_norm <- as.data.frame(lapply(processedData, normalize))
head(data_norm)
dim(data_norm)
class(data_norm$weeknum_16)
class(data_norm$hour_12)
ls(data_norm)

summary(data_norm)
data_norm <- as.matrix(data_norm)
dimnames(data_norm) <- NULL

# create training and test data
set.seed(1234)
train.index <- sample(seq(1, dim(processedData)[1], by=1), round(dim(processedData)[1]*0.8), replace=FALSE)
data_train <- data_norm[train.index, ]
data_test <- data_norm[-train.index, ]
head(data_train)
dim(data_train)
head(data_test)

x_train <- data_norm[train.index,1:74]
head(x_train)
y_train <- data_norm[train.index,75]
head(y_train)
dim(y_train)
x_test  <- data_norm[-train.index,1:74]
y_test  <- data_norm[-train.index,75]


install.packages("keras")
install.packages("tensorflow")

install.packages("devtools")
install.packages("ps")
library("devtools")

devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/reticulate")

library("keras")
library("tensorflow")
install_tensorflow()
install_keras()


# initialize our model
rnn_model <- keras_model_sequential()

rnn_model %>%
  layer_dense(input_shape = dim(x_train)[2], units = 128) %>% #this
  layer_simple_rnn(units = 64, dropout = 0.2, recurrent_dropout = 0.2) %>% #this
  layer_dense(units = 1, activation = 'sigmoid')

rnn_model %>% compile(
  loss = 'mse',
  optimizer = 'adam',
  metrics = c('accuracy')
)

batch_size = 128 #this
epochs = 100 #this
validation_split = 0.2

rnn_history <- rnn_model %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  validation_split = validation_split
)


plot(rnn_history)

rnn_model %>% 
  evaluate(x_test, y_test)


