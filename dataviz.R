setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
df <- read.csv('modified_data4.csv',header = T)
library(dplyr)
df<-select(df,-c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,w25))
df<-select(df,-c(farniente,day,Holiday,X.1,X.x))
df2<-select(df,-c(date))
corm<-apply(cor(df2),2,rev)
image(corm)


#Dataviz
#Sur la série entière
plot.ts(df2$LOAD, main = "LOAD au cours du temps", xlab = "Temps", ylab = "Valeur de la consommation")
plot(df2$average,df2$LOAD, main = "Consommation en fonction de la température", xlab = "Température", ylab = "Valeur de la consommation")
plot(df2$hour,df2$LOAD, main = "Consommation en fonction de l'heure", xlab = "Heure de la journée", ylab = "Valeur de la consommation")


#Sur un subset en particulier
ds_janv_2007 = df2[which(df2$year == "2007" & df2$month == "1"),]
plot.ts(ds_janv_2007$LOAD, main = "LOAD au cours du mois de Janvier", xlab = "Temps", ylab = "Valeur de la consommation")
