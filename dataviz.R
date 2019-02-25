setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
rm(list=ls())
library(dplyr)
library(forecast)
library(lattice)
df <- read.csv('modified_data7.csv',header = T) #on vire la première semaine

#matrice de corrélation
corm<-apply(cor(df),2,rev)
levelplot(corm,
          col.regions = heat.colors(100)[length(heat.colors(100)):1],
          main="matrice de corrélation")

#Dataviz
#Sur la série entière
plot.ts(df$LOAD, main = "LOAD au cours du temps", xlab = "Temps", ylab = "Valeur de la consommation")
plot(df$temp,df$LOAD, main = "Consommation en fonction de la température", xlab = "Température", ylab = "Valeur de la consommation")
plot(df$hour,df$LOAD, main = "Consommation en fonction de l'heure", xlab = "Heure de la journée", ylab = "Valeur de la consommation")


#Sur un subset en particulier
ds_janv_2007 = df[which(df$year == "2007" & df$month == "1"),]
plot.ts(ds_janv_2007$LOAD, main = "LOAD au cours du mois de Janvier", xlab = "Temps", ylab = "Valeur de la consommation")

#autocorrelation 
Acf(df$LOAD,lag.max=NULL,type='correlation',plot=T,main='autocorrelation')
