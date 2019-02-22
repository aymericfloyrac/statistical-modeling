setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques" )
df <- read.csv('modified_data4.csv',header = T )
df<-select(df,-c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,w25 ) )
df<-select(df,-c(farniente,day,Holiday,X.1,X.x,date ) )
#regression linéaire
fit<-lm(LOAD~hour+month+year+average+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+weekend+notworking+heurepleine, data=df )
summary(fit )

#regressions pénalisées
library(dplyr )
x = as.matrix(select(df,-c(X,LOAD ) ) )


##regression ridge
library(glmnet )
library(plotmo )
ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian' ) #alpha=0 donne la pénalisation ridge
best<-ridge$lambda.min
ridge <- glmnet(x = x,y = df$LOAD,alpha = 0,family='gaussian',lambda = c(best ) )
ridge$beta
plot_glmnet(ridge )

##LASSO
lasso <- glmnet(x = x,y = df$LOAD,alpha = 1,family='gaussian' ) #alpha=1 donne la pénalisation lasso
best<-lasso$lambda.min
lasso <- glmnet(x = x,y = df$LOAD,alpha = 1,family='gaussian',lambda = c(best ) )
lasso$beta
plot_glmnet(lasso )

#GLM, à voir si ça sert à quelque chose 
model<-glm(formula = LOAD~hour+month+year+average+maxload+minload+averageload+laggedtemp+maxtemp+mintemp+averagetemp+prevload+tempdiff+weekend+notworking+heurepleine, 
           data=df,
           family = 'gaussian' )
summary(model)
plot(model)

#GAM 
library(mgcv )
gam(formula = LOAD~s(hour,k=6)+s(month,k=6)+s(average,k=6)+s(maxload,k=6)+s(minload,k=6)+s(averageload,k=6)+s(laggedtemp,k=6)+s(maxtemp,k=6)+s(mintemp,k=6)+s(averagetemp,k=6)+s(prevload,k=6)+s(tempdiff,k=6),
    data = df,
    fit = T,
    family = 'gaussian' )
