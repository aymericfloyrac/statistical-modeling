setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
data <- read.csv('data_sujet2.csv',header = T)
#matrice de corrélation
corm<-apply(cor(data[,1:26]),2,rev)
image(corm)
#modification des données 
extractDate<-function(string,my){
  if (my=='month'){return(as.integer(substr(string,6,7)))}
  if (my=='year'){return(as.integer(substr(string,1,4)))}
}

data$month<-apply(as.matrix(data$date),MARGIN=1,FUN=extractDate,my='month')
unique(data$month)
data$year<-apply(as.matrix(data$date),MARGIN=1,FUN=extractDate,my='year')

# séparation de la base
target = data[data$year==2011,]
train = data[data$year!=2011,]

#