library(stargazer)
library(dplyr)
library(glmnet)
library(plotmo)
library(xtable)
library(mgcv)

rm(list=ls())
setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques" )
df <-read.csv('modified_data7.csv')

########################
#régression linéaire#
########################
fit<-lm(LOAD~hour+month+year+temp+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+notworking+heurepleine, data=df )
summary(fit )
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
         family = 'gaussian' )

summary(g)

g2 <- gam(LOAD~s(maxload,k=1)+s(maxtemp,k=1)+weekend+notworking+heurepleine,data = df)
summary(g2)


g3 <- gam(LOAD~s(laggedtemp,k=1,bs='tp')+s(maxtemp,k=1)+weekend+notworking+heurepleine,data = df)
summary(g3)
stargazer(g3, title="Resultats")
