setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
df <- read.csv('modified_data4.csv',header = T)
df<-select(df,-c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,w25))
df<-select(df,-c(farniente,day,Holiday,X.1,X.x,date))
#regression linéaire
fit<-lm(LOAD~hour+month+year+average+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+weekend+notworking+heurepleine, data=df)
summary(fit)

#regression LASSO
library(lars)
library(dplyr)
x = as.matrix(select(df,-c(X,LOAD)))
lasso<-lars(x=x,y=df$LOAD)
plot(lasso)

#regression ridge
library(glmnet)
ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian') #alpha=1 donne la pénalisation lasso
best<-ridge$lambda.min
ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian',lambda = c(best))
ridge$beta
plot(ridge,label=T)