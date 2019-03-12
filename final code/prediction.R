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
source('source.R')


############## PREDICTION NAIVE #####################

initial_df<-read.csv('data_sujet2.csv')
naive_forecast(initial_df,"hour")
naive_forecast(initial_df,"month")


############## MODELISATION - SERIES TEMPORELLES #####################

train<-df[!(df$year == 2005) | (df$year == 2011),] #2005 compte trop de NA et affecte la saisonnalité
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

#sur 2011
pred_annuelle_arima=forecast_next_month(xarima,df,year=2011,month_ahead=12,name = "arima")
error_table_arima <-get_errors(pred_annuelle_arima)
error_table_arima
#pour le code latex
xtable(error_table_arima)
plot_pred_hour(pred_annuelle_arima,'arima')


############## REGRESSION LINEAIRE #####################

train<-df[!(df$year == 2011),]
test <-df[(df$year == 2011),]
fit<-lm(LOAD~., data=train )
summary(fit)

#sur 2011
pred_annuelle_reglin=forecast_next_month(fit,df,year=2011,month_ahead=12,name = "reglin")
error_table_reglin <-get_errors(pred_annuelle_reglin)
error_table_reglin
#pour le code latex
xtable(error_table_reglin)
plot_pred_hour(pred_annuelle_reglin,'reglin')



############## REGRESSIONS PENALISEES #####################

#On définit les matrices pour les deux régressions pénalisées
train<-df[!(df$year == 2011),]
test <-df[(df$year == 2011),]
x = as.matrix(select(train,-c(X,LOAD )))
y = as.matrix(train$LOAD)

##regression ridge##

rcv <- cv.glmnet(x= x,y=y,alpha = 0,family="gaussian",nfold=5)
plot(rcv)
best<-rcv$lambda.min

ridge <- glmnet(x = x,y = y,alpha = 0,family='gaussian',intercept = T)
plot_glmnet(ridge, label=5,main = paste("Régression Ridge", "meilleur lambda =",best),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(best),col="red", lty=2)

best_ridge<-glmnet(x=x,y=y,alpha=0,family='gaussian',lambda = c(best))
Beta_ridge = data.frame(as.data.frame(as.matrix(best_ridge$beta)))
xtable(Beta_ridge)


#sur 2011
pred_annuelle_ridge=forecast_next_month(best_ridge,df,year=2011,month_ahead=12,name = "ridge")
error_table_ridge<-get_errors(pred_annuelle_ridge)
error_table_ridge
#pour le code latex
xtable(error_table_ridge)
plot_pred_hour(pred_annuelle_ridge,'ridge')

##régression LASSO##

rcv <- cv.glmnet(x= x,y=y,alpha = 1,family="gaussian",nfold=5) #alpha=1 donne la pénalisation lasso
plot(rcv)

lasso<-glmnet(x = x,y = y,alpha = 1,family = 'gaussian')
plot_glmnet(lasso, label = 5,  main = paste("Régression Lasso", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de log(Lambda)", ylab="Valeur des coefficients")
abline(v = log(rcv$lambda.min),col="red", lty=2)

best_lasso <- glmnet(x = x,y = y,alpha = 1,family='gaussian',lambda = c(rcv$lambda.min))
Beta_lasso = data.frame(as.data.frame(as.matrix(best_lasso$beta)))
xtable(Beta_lasso)


#sur 2011
pred_annuelle_lasso=forecast_next_month(best_lasso,df,year=2011,month_ahead=12,name = "ridge")
error_table_lasso <-get_errors(pred_annuelle_lasso)
error_table_lasso
#pour le code latex
xtable(error_table_lasso)
plot_pred_hour(pred_annuelle_lasso,'lasso')



############## MODELE GAM #####################

train<-df[!(df$year == 2011),]
test <-df[(df$year == 2011),]
g <- gam(formula = LOAD~s(hour,k=20,bs='cc')+s(month,k=10,bs='cc')+notworking+heurepleine+s(year,k=3)+s(temp,k=25)+s(maxload,k=60)+s(minload,k=70)+s(averageload,k=60)+s(laggedtemp,k=40)+s(maxtemp,k=70)+s(mintemp,k=70)+s(averagetemp,k=50)+s(prevload,k=35)+s(tempdiff,k=20),
         data = train,
         fit = T,
         family = 'gaussian',
         select = T)

summary(g)
plot(g,shade=T,residuals = T)

#normalité des résidus
r<-residuals(g)
ks.test(x = r,y=pnorm,mean=0, sd=sd(r),alternative = 'two.sided')

#Sur 2011
pred_annuelle_gam=forecast_next_month(g,df,year=2011,month_ahead=12,name = 'gam')
error_table_gam <-get_errors(pred_annuelle_gam)
error_table_gam
#pour le code latex
xtable(error_table_gam)
plot_pred_hour(pred_annuelle_gam,'gam')
stargazer(g, title="Resultats")


