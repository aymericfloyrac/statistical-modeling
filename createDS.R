setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
dataset <- read.csv('data_sujet2.csv',header = T)
#matrice de corrélation
corm<-apply(cor(data[,1:26]),2,rev)
image(corm)
#modification des données 
extractDate<-function(string,my){
  if (my=='month'){return(as.integer(substr(string,6,7)))}
  if (my=='year'){return(as.integer(substr(string,1,4)))}
}

dataset$month<-apply(as.matrix(dataset$date),MARGIN=1,FUN=extractDate,my='month')
unique(dataset$month)
dataset$year<-apply(as.matrix(dataset$date),MARGIN=1,FUN=extractDate,my='year')

# séparation de la base
target = dataset[dataset$year==2011,]
train = dataset[dataset$year!=2011,]

#dependance de load par rapport à la temp moyenne 
dataset$average <- apply(dataset[,2:26],FUN=mean,MARGIN = 1)
pairs(cbind(dataset$LOAD,dataset$average))

#création d'une base cool 
library(dplyr)

holidays<-read.csv('usholidays.csv',header=T)
holidays$date<-as.factor(holidays$Date)

createDs <-function(ds){
  df <- ds[!is.na(s$LOAD),] #on crée une copie en retirant les NA
  
  df$maxload <- integer(length = length(df$LOAD))
  df$minload <- integer(length = length(df$LOAD))
  df$averageload <- integer(length = length(df$LOAD))
  df$laggedtemp <- integer(length = length(df$LOAD))
  df$maxtemp <-integer(length = length(df$LOAD))
  df$mintemp<-integer(length = length(df$LOAD))
  df$averagetemp <-integer(length = length(df$LOAD))
  df$prevload <-integer(length(df$LOAD))
  df$tempdiff <-integer(length(df$LOAD))
  
  for (i in seq(1,length(df$LOAD))){
    back1 <- min(i,24)-1
    back2 <- min(i,7*24)-1
    back3<-min(1,i)-1
    
    df$maxload[i]<-max(df$LOAD[i-back1:i])
    df$minload[i]<-min(df$LOAD[i-back1:i])
    df$averageload[i] <- mean(df$LOAD[i-back2:i])
    df$laggedtemp[i]<-df$average[i-back3]
    df$maxtemp[i]<-max(df$average[i-back2:i])
    df$mintemp[i]<-min(df$average[i-back2:i])
    df$averagetemp[i]<-mean(df$average[i-back2:i])
    if (i<=24){df$prevload[i]<-df$average[i]}
    else{df$prevload[i]<-df$average[i-24]}
    df$tempdiff[i]<-df$average[i]-df$average[i-as.integer(i!=1)]
  }
  df$day <- weekdays(as.Date(df$date))
  df$weekend <- as.integer(df$day %in% c('Samedi','Dimanche'))
  df <- left_join(df,holidays,by='date')
  df$farniente <- 1-is.na(df$Holiday)
  df$notworking<-df$farniente+df$weekend-df$farniente*df$weekend
  df<-select(df,-c(X.y,Date,farniente,weekend,day))
  df$heurepleine<-as.integer(!df$hour%in%seq(8,20))
  
  return (df)
}

#ds <- createDs(ds = dataset)

