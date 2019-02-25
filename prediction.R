library(stargazer)
library(dplyr)
library(glmnet)
library(plotmo)
library(xtable)
library(mgcv)
library(Metrics)
library(ggplot2)

rm(list=ls())
setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques" )
df <-read.csv('modified_data7.csv')
######################
#iterative prediction#
######################
iterative<-function(object,dataset,forward){
#object est le prédicteur, ds la base de données entière(train+test),forward est le nombre de pas dans le futur
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
  p1 <- ggplot() + geom_line(aes(y = ypred, x = seq(1,forward), colour = 'prediction'))+
    geom_line(aes(y = ytrue,x=seq(1,forward),colour='truth'))
  p1
  result<-list(rmse=rmse(ytrue,ypred),ytrue=ytrue,ypred=ypred)
  return(result)
}



#####################
#régression linéaire#
#####################
forward<-24
train<-df[0:-forward,]
fit<-lm(LOAD~hour+month+year+temp+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+notworking+heurepleine, data=train )
summary(fit)
ret<-iterative(object = fit,dataset = df,forward = forward)
ret$rmse

stargazer(fit, title="Resultats")


########################
#régressions pénalisées#
########################

x = as.matrix(select(df,-c(X,LOAD )))


##regression ridge

ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian' ) #alpha=0 donne la pénalisation ridge
rcv <- cv.glmnet(x= x,y=df$LOAD,alpha = 0,family="gaussian",nfold=3)
plot(rcv)
best_ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian',lambda = c(rcv$lambda.min))
best_ridge$beta
plot_glmnet(ridge, label=5,main = paste("Régression Ridge", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(rcv$lambda.min),col="red", lty=2)
Beta_ridge = data.frame(as.data.frame(as.matrix(best_ridge$beta)))
xtable(Beta_ridge)


##LASSO
lasso <- glmnet(x = x,y = df$LOAD,alpha = 1,family='gaussian' ) #alpha=1 donne la pénalisation lasso
rcv <- cv.glmnet(x= x,y=df$LOAD,alpha = 1,family="gaussian",nfold=3)
plot(rcv)
best_lasso <- glmnet(x = x,y = df$LOAD,alpha = 1,family='gaussian',lambda = c(rcv$lambda.min))
best_lasso$beta
plot_glmnet(lasso, label = 5,  main = paste("Régression Lasso", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(rcv$lambda.min),col="red", lty=2)
Beta_lasso = data.frame(as.data.frame(as.matrix(best_lasso$beta)))
xtable(Beta_lasso)

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
index<-dim(df)[1]-30*24
train<-df[1:index,]
test <-df[index:dim(df)[1],]
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
