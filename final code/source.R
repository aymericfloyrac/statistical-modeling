library(plotmo)
library(xtable)
library(mgcv)
library(Metrics)
library(ggplot2)
library(lubridate)

############## FONCTION AUXILIAIRE #####################

nbjours <- function(date) {
  #donne le nombre de jours dans le mois correspondant à la date donnée
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

############## PREDICTION NAIVE #####################

naive_forecast <- function(data, step,year=2011) {
  #Quand step = month, la fonction renvoie la mae et la mape des prÃ©dicitons naive
  #dÃ©taillÃ© pour chaque mois de l'annÃ©e "year"
  #pour step = hour, les predicitons naive sont rÃ©alisÃ©es au pas horaire et la fonction donne 
  #la mae et la mape moyenne sur tout le dataset.
  data<-data[!rowSums(is.na(data[c("LOAD")])), ]
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

############## PREDICTION ITERATIVE #####################


iterative<-function(object,dataset,forward,name,title=''){
  #object est le prÃ©dicteur, ds la base de donnÃ©es entiÃ¨re(train+test),forward est le nombre de pas dans le futur, 
  #name le nom pour le graphique exportÃ©
  if (title==''){title<-name}
  ds<-dataset
  l<-length(ds$LOAD)
  ytrue<-ds$LOAD[(l-forward+1):l]
  ds$LOAD[(l-forward+1):l]<-rep(0,forward) #on remplit la colonne de LOADs inconnue avec des 0 
  TS <- ts(ds$LOAD[0:(l-forward)])
  mod <- arima(TS, order = c(4,1,2))
  yp <-rep(0,l-forward)
  p = predict(mod,n.ahead = forward+1)
  ypred = c(yp,p$pred)
  for (i in seq(l-forward+1,l)){
    features<-ds[i,]
    features$prevload<-ds$LOAD[(i-24)]
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
      ds$LOAD[i]<- ypred[i]
    }
  }
  ypred<-ds$LOAD[(l-forward+1):l]
  p1 <- ggplot() + geom_line(aes(y = ypred, x = seq(1,forward), colour = 'prédiction'),alpha=0.4)+
    geom_line(aes(y = ytrue,x=seq(1,forward),colour='rÃ©elle'),alpha=0.4)+
    labs(x="Horizon temporel",y = 'Niveau consommation électricité', colour="Variable") +
    theme_bw()
  p1
  #ggsave(paste0("pred_",title,".png"),plot = p1)
  result<-list(mape=mape(ytrue,ypred),ytrue=ytrue,ypred=ypred)
  return(result)
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

############## VISUALISATION DES PREDICTIONS #####################

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
  #ggsave(paste0('plot_',name,'_heure.png'),p1)
}

############## EVALUATION DE L'ERREUR #####################

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
