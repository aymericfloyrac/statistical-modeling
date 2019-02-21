setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
df <- read.csv('modified_data4.csv',header = T)

#regression linéaire
fit<-lm(LOAD~hour+date+month+year+average+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+weekend+notworking+heurepleine, data=df)
summary(fit)

#regression LASSO
library(lars)
library(dplyr)
x = select(df,-c(X,LOAD))
lasso<-lars(x=,y=df$LOAD)
plot(lasso)

#regression ridge
library(glmnet)
ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian') #alpha=1 donne la pénalisation lasso
best<-ridge$lambda.min
ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian',lambda = c(best))
ridge$beta
plot(ridge,label=T)