library(stargazer)
library(dplyr)
library(glmnet)
library(plotmo)
library(xtable)
library(mgcv)
library(Metrics)
library(ggplot2)
library(lubridate)

rm(list=ls())
setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques" )
df <-read.csv('modified_data7.csv')



######################
#iterative prediction#
######################
iterative<-function(object,dataset,forward,name){
  #object est le prédicteur, ds la base de données entière(train+test),forward est le nombre de pas dans le futur, 
  #name le nom pour le graphique exporté
  ds<-dataset
  l<-length(ds$LOAD)
  ytrue<-ds$LOAD[(l-forward+1):l]
  ds$LOAD[(l-forward+1):l]<-rep(0,forward) #on remplit la colonne de LOADs inconnue avec des 0 
  for (i in seq(l-forward+1,l)){
    features<-ds[i,]
    features$prevload<-ds$LOAD[(i-24)]
    features$maxload<-max(ds$LOAD[(i-24):(i-1)])
    features$minload<-min(ds$LOAD[(i-24):(i-1)])
    features$averageload<-mean(ds$LOAD[(i-24*7):(i-1)])
    if (name == "reglin"){
      ds$LOAD[i]<-predict(object,features)
    }
    if (name == "ridge"){
      features = select(features, -c(X,LOAD))
      features = as.matrix(features)
      ds$LOAD[i]<-predict(object,features)
    }
    if (name == "lasso"){
      features = select(features, -c(X,LOAD))
      features = as.matrix(features)
      ds$LOAD[i]<-predict(object,features)
    }
  }
  ypred<-ds$LOAD[(l-forward+1):l]
  p1 <- ggplot() + geom_line(aes(y = ypred, x = seq(1,forward), colour = 'prédiction'),alpha=0.4)+
    geom_line(aes(y = ytrue,x=seq(1,forward),colour='réelle'),alpha=0.4)+
    labs(x="Horizon temporel",y = 'Niveau consommation électricité', colour="Variable") +
    theme_bw()
  p1
  ggsave(paste0("pred_",name,".png"),plot = p1)
  result<-list(rmse=rmse(ytrue,ypred),ytrue=ytrue,ypred=ypred)
  return(result)
}



########################
#Prédictions itérative sur l'année#
########################

nbjours <- function(date) {
  #donne le nombre de jours dans le mois correspondant à la date donnée
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}



forecast_next_month <- function(predicteur,dataseti,year=2011,month_ahead=12) {
  #Predicteur est le predicteur fitté sur le train set
  #dataseti est le dataset en entier (y compris 2011)
  #year donne l'année qu'on va prédire (ici la dernière du dataset)
  #month_ahead donne le nombre de mois que l'on veut prédire (max 12 et min 1)
  #renvoie une liste contenant les résultats de prédictions pour chaque mois
  ans <- vector("list", month_ahead)
  for (i in 1:month_ahead) {
    remove_month <- c(seq(1:i))
    df_cop <-dataseti[!(dataseti$year == year & !(dataseti$month %in% c(seq(1:i)))),]
    month=df_cop$month[length(df_cop$month)]
    year=df_cop$year[length(df_cop$year)]
    d <- ymd(paste(as.character(year),paste(paste("-",as.character(month),sep=""),"-11",sep=""),sep=""))
    nb_days = nbjours(d)
    ans[[i]]<-iterative(object=predicteur,df_cop,nb_days*24,month.abb[month])
  }
  return(ans)
}

pred_annuelle=forecast_next_month(g,df,year=2011,month_ahead=12)


plot_rmse_month <- function(li) {
  rmse = c(rep(0,12))
  for (i in 1:12){
    rmse[i] = li[[i]]$rmse
  }
  p1 <- ggplot() + geom_line(aes(y = rmse, x = seq(1:12)),alpha=0.4)+
    labs(x="Mois",y = 'RMSE', colour="Variable") +
    theme_bw()
  p1
}

plot_pred_hour <- function(li) {
  pred_heure = c(li[[1]]$ypred)
  true_heure = c(li[[1]]$ytrue)
  for (i in 2:12){
    pred_heure = c(pred_heure,li[[i]]$ypred)
    true_heure = c(true_heure,li[[i]]$ytrue)
  }
  p1 <- ggplot() + geom_line(aes(y = pred_heure, x = seq(1,length(pred_heure)), colour = 'prediction'),alpha=0.5)+
    geom_line(aes(y = true_heure,x=seq(1,length(pred_heure)),colour='truth'),alpha=0.3)+
    labs(title = "Prédictions au pas horaire sur toute l'année",x="Heure",y = 'Niveau consommation électricité', colour="Variable") +
    theme_bw()
  p1
}

plot_rmse_month(pred_annuelle)
plot_pred_hour(pred_annuelle)


get_errors <- function(li){
  me=c(rep(0,12))
  mp=c(rep(0,12))
  for (i in 1:12){
    me[i]=mae(li[[i]]$ytrue,li[[i]]$ypred)
    mp[i]=mape(li[[i]]$ytrue,li[[i]]$ypred)
  }
  mp=round(mp*100,2)
  me=round(me,2)
  res=data.frame(month=c("Jan","Fev","Mar","Avr","Mai","Juin","Jui","Aout","Sep","Oct","Nov","Dec"),mae=me,mape=mp)
  return(res)
}

error_table<-get_errors(pred_annuelle)

#pour le code latex
xtable(error_table)

########################
#Prédictions naives à l'horizon horaire ou mensuel#
########################
naive_forecast <- function(data, step,year=2011) {
  #Quand step = month, la fonction renvoie la mae et la mape des prédicitons naive
  #détaillé pour chaque mois de l'année "year"
  #pour step = hour, les predicitons naive sont réalisées au pas horaire et la fonction donne 
  #la mae et la mape moyenne sur tout le dataset.
  if (step=="month") {
    nf<-data[c("LOAD","date")]
    tf<-nf[floor_date(ymd(nf$date),'month')!=ymd("2005-01-01"),]
    pf<-nf[floor_date(ymd(nf$date),'month')!=ymd("2011-12-01"),]
    me=c(rep(0,12))
    mp=c(rep(0,12))
    tf<-cbind(tf,pf$LOAD)
    for (i in 1:12) {
      res=data.frame(predicted=tpf[(year(ymd(tpf$date))==year & month(ymd(tpf$date))==i),]$`pf$LOAD`,true=tpf[(year(ymd(tpf$date))==year & month(ymd(tpf$date))==i),]$LOAD)
      me[i]=round(mae(res$predicted,res$true),2)
      mp[i]=round(100*mape(res$predicted,res$true),2)
    }
    res=data.frame(month=c("Jan","Fev","Mar","Avr","Mai","Juin","Jui","Aout","Sep","Oct","Nov","Dec"),mae=me,mape=mp)
    return(res)
  }
  if (step=="hour") {
    nf<-data[c("LOAD","date")]
    tf<- nf[-1,]
    pf<-nf[1:length(nf[[1]])-1,]
    res=data.frame(true=pf$LOAD,predicted=tf$LOAD)
    mae=mean(abs(res$true-res$predicted))
    mape=mean(abs(res$true-res$predicted)/res$true)
    return(list(mae,mape))
  }
}


naive_forecast(df,"hour")
naive_forecast(df,"month")


###############
###Série temp##
###############

plot.ts(df$LOAD, main = "LOAD au cours du temps", xlab = "Temps", ylab = "Valeur de la consommation")
#On a l'impression que c'est stationnaire

#Si on utilise le package forecast pour donner des paramètres p d q du ARIMA
auto.arima(df$LOAD)
#On obtient un modèle ARIMA(4,1,1)

#Arime forecats
#si on test sur les 24 dernières heures
forward<-24
l <- length(df$LOAD)
train<-df[0:-forward,]
ytrue<-df[(l-forward+1):l,]
ytrue <- ytrue$LOAD

arima.411 <- arima(train$LOAD, order = c(4,1,1))
ypred <- predict(arima.411, n.ahead = forward)
ts.plot(ytrue,ypred$pred, col =c("blue","red"), main = "Prédictions de l ARIMA")
        



#Si on le tune nous même

#Autocorrélogramme et autocorrélogramme partiel
acf(df$LOAD,lag.max = 50, main="Autocorrélogramme de la série", xlab="Retard")
#ce n'est clairement pas stationnaire car la décroissance vers 0 est très lente donc d = 1
# le dernier a être significatif est pour q = 15
pacf(df$LOAD, lag.max = 50, main = "Autocorrélogramme partiel", xlab = "Retard")
# le dernier a être significatif est pour p = 28

#Xt - Xt-1 (au pas horaire)
acf(diff(df$LOAD, differences = 1))
pacf(diff(df$LOAD, differences = 1))

#Test de Dickey-Fuller : pour la stationnarité de la série temp
adf.test(df$LOAD, alternative = "stationary", k=0)

#Arime forecats
#Vu que ça ne donne rien avec pet q trop grand, on fait plus faible
arima.28115 <- arima(train$LOAD, order = c(10,1,10))
ypred <- predict(arima.28115, n.ahead = forward)
ts.plot(ytrue,ypred$pred, col =c("blue","red"), main = "Prédictions de l ARIMA")


#####################
#régression linéaire#
#####################
forward<-24
train<-df[0:-forward,]
fit<-lm(LOAD~hour+month+year+temp+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+notworking+heurepleine, data=train )
summary(fit)
score<-iterative(object = fit,dataset = df,forward = forward,name = "reglin")
stargazer(fit, title="Resultats")



########################
#régressions pénalisées#
########################

#On définit les matrices pour les deux reg pénalisées
forward<-24
train<-df[0:-forward,]
x = as.matrix(select(train,-c(X,LOAD )))
y = as.matrix(train$LOAD)

##regression ridge
rcv <- cv.glmnet(x= x,y=y,alpha = 0,family="gaussian",nfold=3)
plot(rcv)
ridge <- glmnet(x = x,y = y,alpha = 0,family='gaussian',lambda = c(rcv$lambda.min))
plot_glmnet(ridge, label=5,main = paste("Régression Ridge", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(rcv$lambda.min),col="red", lty=2)
Beta_ridge = data.frame(as.data.frame(as.matrix(ridge$beta)))
xtable(Beta_ridge)


score<-iterative(object = ridge,dataset = df,forward = forward,name = "ridge")


##LASSO
rcv <- cv.glmnet(x= x,y=y,alpha = 1,family="gaussian",nfold=3) #alpha=1 donne la pénalisation lasso
plot(rcv)
lasso <- glmnet(x = x,y = y,alpha = 1,family='gaussian',lambda = c(rcv$lambda.min))
plot_glmnet(lasso, label = 5,  main = paste("Régression Lasso", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(rcv$lambda.min),col="red", lty=2)
Beta_lasso = data.frame(as.data.frame(as.matrix(best_lasso$beta)))
xtable(Beta_lasso)

score<-iterative(object = lasso,dataset = df,forward = forward,name = "lasso")


##########
##GAM#####
##########

#GLM, à voir si ça sert à quelque chose 
model<-glm(formula = LOAD~hour+month+year+average+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+weekend+notworking+heurepleine, 
           data=df,
           family = 'gaussian' )
summary(model)
plot(model)
stargazer(model, title="Resultats")

#GAM 
#GAM 
g <- gam(formula = LOAD~s(hour,k=10)+s(month,k=10)+notworking+heurepleine+year+s(temp,k=10)+s(maxload,k=10)+s(minload,k=10)+s(averageload,k=10)+s(laggedtemp,k=10)+s(maxtemp,k=10)+s(mintemp,k=10)+s(averagetemp,k=10)+s(prevload,k=10)+s(tempdiff,k=10),
         data = df,
         fit = T,
         family = 'gaussian',
         select = T)

summary(g)
gam.check(g)

#train test
#index<-dim(df)[1]-30*24
#train<-df[1:index,]
#test <-df[index:dim(df)[1],]

train<-df[!(df$year == 2011),]
test <-df[(df$year == 2011),]
g <- gam(formula = LOAD~s(hour,k=10)+s(month,k=10)+notworking+heurepleine+year+s(temp,k=10)+s(maxload,k=10)+s(minload,k=10)+s(averageload,k=10)+s(laggedtemp,k=10)+s(maxtemp,k=10)+s(mintemp,k=10)+s(averagetemp,k=10)+s(prevload,k=10)+s(tempdiff,k=10),
         data = train,
         fit = T,
         family = 'gaussian',
         select = T)

ypred<-predict.gam(object = g,newdata = test[,3:17],type = 'lpmatrix')
ytrue<-test[,2]
plot(ypred,type='l',col="red")
lines(ytrue,col="green")

stargazer(g, title="Resultats")



