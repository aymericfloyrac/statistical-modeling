setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
rm(list=ls())
library(dplyr)
library(forecast)
library(lattice)
library(corrplot)
df <- read.csv('modified_data7.csv',header = T) #on vire la première semaine

#On charge la base entière inchangée puis on vire les na.
dataset <- read.csv('data_sujet2.csv',header = T)
dataset<-dataset[!rowSums(is.na(dataset[c("LOAD")])), ]
#On visualise ici les correlations de temperatures entres les différentes stations
mat<-cor(dataset[,c(colnames(dataset)[2:26])])
corrplot(mat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
mean(mat) #correlation moyenne
min(mat) #correlation minimum


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
#Janvier 2007
ds_janv_2007 = df[which(df$year == "2007" & df$month == "1"),]
plot.ts(ds_janv_2007$LOAD, main = "Consommation électique au cours du mois de Janvier 2007", xlab = 'Temps', ylab = "Valeur de la consommation", xaxt = "n")
axis(1, at=seq(1,744,by = 100), labels = c(as.character(ds_janv_2007[seq(1,744,by = 100),29])))



#ete
ds_ete_2007 = df[which((df$month == "6" | df$month == "7"  | df$month == "8"  | df$month == "9") & df$year == "2007"),]
plot.ts(ds_ete_2007$LOAD, main = "Consommation électique au cours de l'été 2007", xlab = "Temps", ylab = "Valeur de la consommation", xaxt = "n")
axis(1, at=seq(1,2928,by= 744), labels = c(as.character(ds_ete_2007[seq(1,2928,by = 744),29])))


#ete
ds_hiver_2007 = df[which((df$month == "1"  | df$month == "2"  | df$month == "3") & df$year == "2007"),]
plot.ts(ds_hiver_2007$LOAD, main = "Consommation électique au cours de l'hiver 2007", xlab = "Temps", ylab = "Valeur de la consommation", xaxt = "n")
axis(1, at=seq(1,2928,by= 744), labels = c(as.character(ds_hiver_2007[seq(1,2928,by = 744),29])))


#autocorrelation 
Acf(df$LOAD,lag.max=NULL,type='correlation',plot=T,main='autocorrelation')




###########
t = df  %>%  group_by(month,year) %>% dplyr::summarize(Mean = mean(LOAD, na.rm=TRUE))
t2 = df  %>%  group_by(hour, month) %>% dplyr::summarize(Mean = mean(LOAD, na.rm=TRUE))



ggplot(t, aes(month, Mean , group=year, colour=factor(year))) +
  geom_line() +
  geom_point() +
  labs(title ="Consommation moyenne d'électricité par mois selon l'année",x="Mois",y = 'LOAD en moyenne', colour="Année") +
  scale_x_continuous(breaks=c(2,4,6,8,10), labels=c("Février", "Avril", "Juin","Août","Octobre"))+
  theme_bw()


ggplot(t2, aes(hour, Mean , group=month, colour=factor(month))) +
  geom_line() +
  geom_point() +
  labs(title ="Consommation moyenne d'électricité par heure selon le mois",x="Heure",y = 'LOAD en moyenne', colour="Mois") +
  theme_bw()


# Year plot
t$label <- as.factor(paste("Mean", t$year, sep = ""))
t$b <-month.abb[t$month]
# Superposition des années en radar
ggplot(data = t, aes(x = month, y = Mean, color = label)) + 
  geom_line() +
  geom_point() + 
  labs(title ="Consommation moyenne d'électricité par heure selon le mois",x="Heure",y = 'LOAD en moyenne', colour="Mois") +
  coord_polar() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11), labels=c("Janvier","Février","Mars", "Avril","Mai", "Juin","Juillet","Août","Septembre","Octobre","Novembre")) +
  theme(legend.position = "bottom") +
  theme_bw()




