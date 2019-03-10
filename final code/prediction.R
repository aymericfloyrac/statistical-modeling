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
df <-read.csv('modified_data8.csv')
source('source.R')


############## PREDICTION NAIVE #####################

naive_forecast(df,"hour")
naive_forecast(df,"month")


############## MODELISATION - SERIES TEMPORELLES #####################


#On vire 2005 aussi car trop de données manquantes dans des jours diff => flingue la saisonnalité
train<-df[!(df$year == 2005) | (df$year == 2011),]
test <-df[(df$year == 2011),]

TS = ts(train$LOAD)
plot(TS)
#On enlevait la composante saisonnière on différentie
desaison <- diff(TS,24)

plot(desaison)
#Autocorrélogramme et autocorrélogramme partiel
acf(desaison,lag.max = 500, main="Autocorrélogramme de la série", xlab="Retard")
# le dernier a être significatif est pour q = 410
pacf(desaison, lag.max = 500, main = "Autocorrélogramme partiel", xlab = "Retard")
# le dernier a être significatif est pour p = 490

#Arima forecast
xarima <- arima(TS, order = c(4,1,2))

#sur 2011
pred_annuelle_arima=forecast_next_month(xarima,df,year=2011,month_ahead=12,name = "arima")
error_table_arima <-get_errors(pred_annuelle_arima)
#pour le code latex
xtable(error_table_arima)
plot_pred_hour(pred_annuelle_arima,'arima')


############## REGRESSION LINEAIRE #####################

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



############## REGRESSIONS PENALISEES #####################

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



############## MODELE GAM #####################

train<-df[!(df$year == 2011),]
test <-df[(df$year == 2011),]
g <- gam(formula = LOAD~s(hour,k=20,bs='cc')+s(month,k=10,bs='cc')+notworking+heurepleine+year+s(temp,k=20)+s(maxload,k=10)+s(minload,k=10)+s(averageload,k=10)+s(laggedtemp,k=30)+s(maxtemp,k=10)+s(mintemp,k=10)+s(averagetemp,k=20)+s(prevload,k=40)+s(tempdiff,k=20),
         data = train,
         fit = T,
         family = 'gaussian',
         select = T)

summary(g)
plot(g,shade=T,residuals = T)


#Sur 2011
pred_annuelle_gam=forecast_next_month(g,df,year=2011,month_ahead=12,name = 'gam')
error_table_gam <-get_errors(pred_annuelle_gam)
#pour le code latex
xtable(error_table_gam)
plot_pred_hour(pred_annuelle_gam,'gam')
gam.check(g)
stargazer(g, title="Resultats")

