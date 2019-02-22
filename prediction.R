library(stargazer)
library(dplyr)
library(glmnet)
library(plotmo)
library(xtable)
library(mgcv)


setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques" )

df<-select(df,-c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,w25 ) )
df<-select(df,-c(farniente,day,Holiday,X.1,X.x,date ) )
#regression linéaire
fit<-lm(LOAD~hour+month+year+average+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+weekend+notworking+heurepleine, data=df )
summary(fit )
stargazer(fit, title="Resultats")


########################
#regressions pénalisées#
########################

x = as.matrix(select(df,-c(X,LOAD )))


##regression ridge

ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian' ) #alpha=0 donne la pénalisation ridge
rcv <- cv.glmnet(x= x,y=df$LOAD,alpha = 0,family="gaussian",nfold=3)
plot(rcv)
best_ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian',lambda = c(rcv$lambda.min))
best_ridge$beta
plot(ridge, label = 5, main = paste("Régression Ridge", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de Lambda", ylab="Valeur des coefficients")
abline(v = rcv$lambda.min,col="red", lty=2)
Beta_ridge = data.frame(as.data.frame(as.matrix(best_ridge$beta)))
xtable(Beta_lasso)


##LASSO
lasso <- glmnet(x = x,y = df$LOAD,alpha = 1,family='gaussian' ) #alpha=1 donne la pénalisation lasso
rcv <- cv.glmnet(x= x,y=df$LOAD,alpha = 1,family="gaussian",nfold=3)
plot(rcv)
best_lasso <- glmnet(x = x,y = df$LOAD,alpha = 1,family='gaussian',lambda = c(rcv$lambda.min))
best_lasso$beta
plot(lasso, label = 5,  main = paste("Régression Lasso", "meilleur lambda =",rcv$lambda.min),xlab="Valeurs de Lambda", ylab="Valeur des coefficients")
abline(v = rcv$lambda.min,col="red", lty=2)
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
g <- gam(formula = LOAD~s(hour,k=6)+s(month,k=6)+s(average,k=6)+s(maxload,k=6)+s(minload,k=6)+s(averageload,k=6)+s(laggedtemp,k=6)+s(maxtemp,k=6)+s(mintemp,k=6)+s(averagetemp,k=6)+s(prevload,k=6)+s(tempdiff,k=6),
    data = df,
    fit = T,
    family = 'gaussian' )

g2 <- gam(LOAD~s(maxload,k=1)+s(maxtemp,k=1)+weekend+notworking+heurepleine,data = df)
summary(g2)


g3 <- gam(LOAD~s(laggedtemp,k=1,bs='tp')+s(maxtemp,k=1)+weekend+notworking+heurepleine,data = df)
summary(g3)
stargazer(g3, title="Resultats")


