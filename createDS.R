################################
#CREATION DE LA BASE DE DONNEES#
################################

setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
library(useful)
library(dplyr)
library(zoo)
df <- read.csv('data_sujet2.csv',header = T)
dataset <- df[!is.na(df$LOAD),] #on crée une copie en retirant les NA

#la température considérée est la moyenne des températures données
dataset$temp <- apply(dataset[,2:26],FUN=mean,MARGIN = 1)

#variables de température
dataset<-shift.column(data = dataset,columns = c('temp'),newNames = c('laggedtemp'),len=1,up=F)
dataset$tempdiff<-dataset$temp-dataset$laggedtemp
dataset$maxtemp<-rollmax(dataset$temp,k = 7*24,fill = NA,align = 'right')
dataset$mintemp<- -rollmax(-dataset$temp,k = 7*24,fill = NA,align = 'right')
dataset$averagetemp<-rollmean(dataset$temp,k = 7*24,fill = NA,align = 'right')

#variables de consommation
dataset<-shift.column(data = dataset,columns = c('LOAD'),newNames = ('prevload_24'),len = 24,up = F)
dataset<-shift.column(data = dataset,columns = c('LOAD'),newNames = ('prevload_1'),len = 1,up = F)
dataset$maxload<-rollmax(dataset$LOAD,k = 24,fill = NA,align = 'right')
dataset$minload<- -rollmax(-dataset$LOAD,k=24,fill = NA,align = 'right')
dataset$averageload<-rollmean(dataset$LOAD,k=7*24,fill = NA,align = 'right')

#variables calendaires
extractDate<-function(string,my){
  if (my=='month'){return(as.integer(substr(string,6,7)))}
  if (my=='year'){return(as.integer(substr(string,1,4)))}
}
dataset$month<-apply(as.matrix(dataset$date),MARGIN=1,FUN=extractDate,my='month')
dataset$year<-apply(as.matrix(dataset$date),MARGIN=1,FUN=extractDate,my='year')

holidays<-read.csv('usholidays.csv',header=T)
holidays$date<-as.factor(holidays$Date)
dataset$day <- weekdays(as.Date(dataset$date))
dataset$weekend <- as.integer(dataset$day %in% c('Samedi','Dimanche'))
dataset <- left_join(dataset,holidays,by='date')
dataset$farniente <- 1-is.na(dataset$Holiday)
dataset$notworking<-dataset$farniente+dataset$weekend-dataset$farniente*dataset$weekend
dataset$heurepleine<-as.integer(!dataset$hour%in%seq(8,20))

#elimination des variables inutiles
dataset<-select(dataset,-c(X,Date,farniente,weekend,day,
                           w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,
                           w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,w25,
                           Holiday,date))
#elimination des NA et reordonnancement
dataset<-dataset[!is.na(dataset$averageload),]
dataset<-dataset[,c(1,9,10,11,12,3,4,5,6,7,8,2,13,14,15,16,17)]
write.csv(x = dataset,file = 'modified_data8.csv')
