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

df_naif <- read.csv('data_sujet2.csv',header = T) #Dataset inchangé qui servira pour les prédictions naïves.
df_naif<-df_naif[!rowSums(is.na(df_naif[c("LOAD")])), ]

######################
#iterative prediction#
######################

iterative<-function(object,dataset,forward,name,title=''){
  #object est le prÃ©dicteur, ds la base de donnÃ©es entiÃ¨re(train+test),forward est le nombre de pas dans le futur, 
  #name le nom pour le graphique exportÃ©
  if (title==''){title<-name}
  ds<-dataset
  l<-length(ds$LOAD)
  ytrue<-ds$LOAD[(l-forward+1):l]
  ds$LOAD[(l-forward+1):l]<-rep(0,forward) #on remplit la colonne de LOADs inconnue avec des 0 
  for (i in seq(l-forward+1,l)){
    features<-ds[i,]
    features$prevload_24<-ds$LOAD[(i-24)]
    features$prevload_1<-ds$LOAD[(i-1)]
    features$maxload<-max(ds$LOAD[(i-24):(i-1)])
    features$minload<-min(ds$LOAD[(i-24):(i-1)])
    features$averageload<-mean(ds$LOAD[(i-24*7):(i-1)])
    if (name == "reglin" || name=='gam'){
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
    if (name == "arima"){
      yp = predict(object,n.ahead = 1)
      ds$LOAD[i]<- yp$pred[i]
    }
  }
  ypred<-ds$LOAD[(l-forward+1):l]
  p1 <- ggplot() + geom_line(aes(y = ypred, x = seq(1,forward), colour = 'prédiction'),alpha=0.4)+
    geom_line(aes(y = ytrue,x=seq(1,forward),colour='rÃ©elle'),alpha=0.4)+
    labs(x="Horizon temporel",y = 'Niveau consommation électricité', colour="Variable") +
    theme_bw()
  p1
  ggsave(paste0("pred_",title,".png"),plot = p1)
  result<-list(mape=mape(ytrue,ypred),ytrue=ytrue,ypred=ypred)
  return(result)
}



###################################
#Prédictions itérative sur l'année#
###################################

nbjours <- function(date) {
  #donne le nombre de jours dans le mois correspondant à la date donnée
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}


forecast_next_month <- function(predicteur,dataseti,year=2011,month_ahead=12, name) {
  #Predicteur est le predicteur fittÃ© sur le train set
  #dataseti est le dataset en entier (y compris 2011)
  #year donne l'annÃ©e qu'on va prÃ©dire (ici la derniÃ¨re du dataset)
  #month_ahead donne le nombre de mois que l'on veut prÃ©dire (max 12 et min 1)
  #renvoie une liste contenant les rÃ©sultats de prÃ©dictions pour chaque mois
  ans <- vector("list", month_ahead)
  for (i in 1:month_ahead) {
    remove_month <- c(seq(1:i))
    df_cop <-dataseti[!(dataseti$year == year & !(dataseti$month %in% c(seq(1:i)))),]
    month=df_cop$month[length(df_cop$month)]
    year=df_cop$year[length(df_cop$year)]
    d <- ymd(paste(as.character(year),paste(paste("-",as.character(month),sep=""),"-11",sep=""),sep=""))
    nb_days = nbjours(d)
    ans[[i]]<-iterative(object=predicteur,dataset = df_cop,forward = nb_days*24,name,title = month.abb[month])
  }
  return(ans)
}



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

plot_pred_hour <- function(li,name) {
  pred_heure = c(li[[1]]$ypred)
  true_heure = c(li[[1]]$ytrue)
  for (i in 2:12){
    pred_heure = c(pred_heure,li[[i]]$ypred)
    true_heure = c(true_heure,li[[i]]$ytrue)
  }
  p1 <- ggplot() + geom_line(aes(y = pred_heure, x = seq(1,length(pred_heure)), colour = 'prediction'),alpha=0.5)+
    geom_line(aes(y = true_heure,x=seq(1,length(pred_heure)),colour='truth'),alpha=0.3)+
    labs(title = "Prédictions au pas horaire sur l'année 2011",x="Heure",y = 'Niveau consommation électricité', colour="Variable") +
    scale_x_continuous(breaks=c(1,745,1417,2161,2881,3625,4345,5089,5833,6553,7297,8071), labels=c("Janvier","Février", "Mars", "Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Novembre","Décembre"))+
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1))
  p1
  ggsave(paste0('plot_',name,'_heure.png'),p1)
}


#uniquement pour ben car mape ne marche pas sur son pc de tocard
mape <- function(true,pred){
  mp <- mean(abs((true-pred)/true))
  return(mp)
}

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



###################################################
#Prédictions naives à l'horizon horaire ou mensuel#
###################################################


naive_forecast <- function(data, step,year=2011) {
  #Quand step = month, la fonction renvoie la mae et la mape des prÃ©dicitons naive
  #dÃ©taillÃ© pour chaque mois de l'annÃ©e "year"
  #pour step = hour, les predicitons naive sont rÃ©alisÃ©es au pas horaire et la fonction donne 
  #la mae et la mape moyenne sur tout le dataset.
  if (step=="month") {
    nf<-data[c("LOAD","date")]
    tf<-nf[floor_date(ymd(nf$date),'month')!=ymd("2005-01-01"),]
    pf<-nf[floor_date(ymd(nf$date),'month')!=ymd("2011-12-01"),]
    me=c(rep(0,12))
    mp=c(rep(0,12))
    tpf<-cbind(tf,pf$LOAD)
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
    #return(list(mae,mape))
    return(res)
  }
}

df_naif <- read.csv('data_sujet2.csv',header = T) #Dataset inchangé qui servira pour les prédictions naïves.
df_naif<-df_naif[!rowSums(is.na(df_naif[c("LOAD")])), ]

res =naive_forecast(df_naif,"hour")
res_2011 = res[(52584:61343),]

p1 <- ggplot() + geom_line(aes(y = res_2011$predicted, x = seq(1,n), colour = 'prediction'),alpha=0.5)+
  geom_line(aes(y = res_2011$true,x=seq(1, n),colour='truth'),alpha=0.3)+
  labs(title = "Prédictions au pas horaire sur l'année 2011",x="Heure",y = 'Niveau consommation électricité', colour="Variable") +
  scale_x_continuous(breaks=c(1,745,1417,2161,2881,3625,4345,5089,5833,6553,7297,8071), labels=c("Janvier","Février", "Mars", "Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Novembre","Décembre"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))
p1


###############
###Série temp##
###############
#CODE ARIMA

#On vire 2005 aussi car trop de données manquantes dans des jours diff => flingue la saisonnalité
train<-df[!(df$year == 2005) | (df$year == 2011),]
test <-df[(df$year == 2011),]

TS = ts(train$LOAD)
plot(TS)
desaison <- diff(TS,24)
plot(desaison)

#Autocorrélogramme et autocorrélogramme partiel
acf(desaison,lag.max = 200, main="Autocorrélogramme de la série", xlab="Retard")

pacf(desaison, lag.max = 500, main = "Autocorrélogramme partiel", xlab = "Retard")


#Arime forecats
xarima <- arima(TS, order = c(4,1,2))
n = length(test$LOAD)
ytrue <- test$LOAD
ypred <- predict(xarima, n.ahead = n)


#sur 2011
pred_annuelle_arima=forecast_next_month(xarima,df,year=2011,month_ahead=12,name = "arima")
error_table_arima <-get_errors(pred_annuelle_arima)
#pour le code latex
xtable(error_table_arima)
plot_pred_hour(pred_annuelle_arima,'arima')


#####################
#régression linéaire#
#####################

forward<-24
train<-df[0:-forward,]
fit<-lm(LOAD~hour+month+year+temp+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+notworking+heurepleine, data=train )
summary(fit)
score<-iterative(object = fit,dataset = df,forward = forward,name = "reglin")
stargazer(fit, title="Resultats")

#sur 2011
pred_annuelle_reglin=forecast_next_month(fit,df,year=2011,month_ahead=12,name = "reglin")
error_table_reglin <-get_errors(pred_annuelle_reglin)
#pour le code latex
xtable(error_table_reglin)
plot_pred_hour(pred_annuelle_reglin,'reglin')


########################
#régressions pénalisées#
########################

#On définit les matrices pour les deux reg pénalisées
forward<-24
train<-df[0:-forward,]
x = as.matrix(select(train,-c(X,LOAD )))
y = as.matrix(train$LOAD)

##regression ridge##

rcv <- cv.glmnet(x= x,y=y,alpha = 0,family="gaussian",nfold=3)
plot(rcv)
ridge <- glmnet(x = x,y = y,alpha = 0,family='gaussian',lambda = c(rcv$lambda.min))
plot_glmnet(ridge, label=5,main = paste("Régression Ridge", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(rcv$lambda.min),col="red", lty=2)
Beta_ridge = data.frame(as.data.frame(as.matrix(ridge$beta)))
xtable(Beta_ridge)


#sur 2011
pred_annuelle_ridge=forecast_next_month(ridge,df,year=2011,month_ahead=12,name = "ridge")
error_table_ridge<-get_errors(pred_annuelle_ridge)
#pour le code latex
xtable(error_table_ridge)
plot_pred_hour(pred_annuelle_ridge,'ridge')

##régression LASSO##

rcv <- cv.glmnet(x= x,y=y,alpha = 1,family="gaussian",nfold=3) #alpha=1 donne la pénalisation lasso
plot(rcv)
lasso <- glmnet(x = x,y = y,alpha = 1,family='gaussian',lambda = c(rcv$lambda.min))
plot_glmnet(lasso, label = 5,  main = paste("Régression Lasso", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(rcv$lambda.min),col="red", lty=2)
Beta_lasso = data.frame(as.data.frame(as.matrix(best_lasso$beta)))
xtable(Beta_lasso)


#sur 2011
pred_annuelle_lasso=forecast_next_month(lasso,df,year=2011,month_ahead=12,name = "ridge")
error_table_lasso <-get_errors(pred_annuelle_lasso)
#pour le code latex
xtable(error_table_lasso)
plot_pred_hour(pred_annuelle_lasso,'lasso')


#########
##GAM#####
##########

#option cycle : s(..., bs = "cc")

#GLM, à voir si ça sert à quelque chose 
model<-glm(formula = LOAD~hour+month+year+average+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+weekend+notworking+heurepleine, 
           data=df,
           family = 'gaussian' )
summary(model)
plot(model)
stargazer(model, title="Resultats")

#GAM 
train<-df[!(df$year == 2011),]
test <-df[(df$year == 2011),]
g <- gam(formula = LOAD~s(hour,k=20)+month+notworking+heurepleine+year+s(temp,k=20)+s(maxload,k=10)+s(minload,k=10)+s(averageload,k=10)+s(laggedtemp,k=30)+s(maxtemp,k=10)+s(mintemp,k=10)+s(averagetemp,k=20)+s(prevload_1,k=40)+s(prevload_24,k=10)+s(tempdiff,k=20),
         data = train,
         fit = T,
         family = 'gaussian',
         select = T)

summary(g)
plot(g,shade=T,residuals = T)

#res<-iterative(object = g,dataset = df,forward = dim(test)[1],name = 'gam')

#Sur 2011
pred_annuelle_gam=forecast_next_month(g,df,year=2011,month_ahead=12,name = 'gam')
error_table_gam <-get_errors(pred_annuelle_gam)
#pour le code latex
xtable(error_table_gam)
plot_pred_hour(pred_annuelle_gam,'gam')
gam.check(g)
stargazer(g, title="Resultats")




