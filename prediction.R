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
    ds$LOAD[i]<-predict(object,features)
  }
  ypred<-ds$LOAD[(l-forward+1):l]
  p1 <- ggplot() + geom_line(aes(y = ypred, x = seq(1,forward), colour = 'prediction'),alpha=0.4)+
    geom_line(aes(y = ytrue,x=seq(1,forward),colour='truth'),alpha=0.4)+
    labs(x="Horizon temporel",y = 'Niveau consommation électricité', colour="Variable") +
    theme_bw()
  p1
  ggsave(paste0("pred_",name,".png"))
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


plot_rmse_month(pred_annuelle)


########################
#Prédictions naives à l'horizon horaire ou mensuel#
########################
naive_forecast <- function(data, step) {
  if (step=="month") {
    nf<-data[c("LOAD","date")]
    tf<-nf[floor_date(ymd(nf$date),'month')!=ymd("2005-01-01"),]
    pf<-nf[floor_date(ymd(nf$date),'month')!=ymd("2011-12-01"),]
    res=data.frame(true=pf$LOAD,predicted=tf$LOAD)
    mae=mean(abs(res$true-res$predicted))
    mape=mean(abs(res$true-res$predicted)/res$true)
    return(list(mae,mape))
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
#A préciser

plot.ts(df$LOAD, main = "LOAD au cours du temps", xlab = "Temps", ylab = "Valeur de la consommation")

#Autocorrélogramme et autocorrélogramme partiel
acf(df$LOAD,lag.max = 15, main="Autocorrélogramme de la série", xlab="Retard")
pacf(df$LOAD, lag.max = 15, main = "Autocorrélogramme partiel", xlab = "Retard")
#Forte corrélationa vec le lagged 1 et lagged 2

#Xt - Xt-1 (au pas horaire)
acf(diff(df$LOAD, differences = 1))
pacf(diff(df$LOAD, differences = 1))

#Test de Dickey-Fuller : pour la stationnarité de la série temp
adf.test(df$LOAD, alternative = "stationary", k=0)

#ARIMA (pas du tout le bon modèle)
arima(df$LOAD, order = c(2,0,0))

#Arime forecats
test.arima.200 <- arima(df$LOAD, order = c(2,0,0))
pred <- predict(test.arima.200, n.ahead = 100)




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

x = as.matrix(select(df,-c(X,LOAD )))

##regression ridge
rcv <- cv.glmnet(x= x,y=df$LOAD,alpha = 0,family="gaussian",nfold=3)
plot(rcv)
ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian',lambda = c(rcv$lambda.min))
plot_glmnet(ridge, label=5,main = paste("Régression Ridge", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(rcv$lambda.min),col="red", lty=2)
Beta_ridge = data.frame(as.data.frame(as.matrix(ridge$beta)))
xtable(Beta_ridge)


score<-iterative(object = ridge,dataset = df,forward = forward,name = "ridge")


##LASSO
rcv <- cv.glmnet(x= x,y=df$LOAD,alpha = 1,family="gaussian",nfold=3) #alpha=1 donne la pénalisation lasso
plot(rcv)
lasso <- glmnet(x = x,y = df$LOAD,alpha = 1,family='gaussian',lambda = c(rcv$lambda.min))
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



