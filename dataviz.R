library(ggplot2)
library(scales)
library(dplyr)

setwd(dir = "/Users/aymeric/Documents/ENSAE/2A/Semestre 2/Séminaire statistiques")
df <- read.csv('modified_data6.csv',header = T)
df <- df[!is.na(df$averagetemp),]
df_naif <- df
df<-select(df,-c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,w25 ) )
df<-select(df,-c(Holiday,date ) )


#Dataviz
#Sur la série entière
plot.ts(df$LOAD, main = "LOAD au cours du temps", xlab = "Temps", ylab = "Valeur de la consommation")
plot(df$average,df$LOAD, main = "Consommation en fonction de la température", xlab = "Température", ylab = "Valeur de la consommation")
plot(df$hour,df$LOAD, main = "Consommation en fonction de l'heure", xlab = "Heure de la journée", ylab = "Valeur de la consommation")


#Sur un subset en particulier
ds_janv_2007 = df[which(df$year == "2007" & df$month == "1"),]
plot.ts(ds_janv_2007$LOAD, main = "LOAD au cours du mois de Janvier", xlab = "Temps", ylab = "Valeur de la consommation")



t = df  %>%  group_by(month,year) %>% dplyr::summarize(Mean = mean(LOAD, na.rm=TRUE))

ggplot(t, aes(month, Mean , group=year, colour=factor(year))) +
  geom_line() +
  geom_point() +
  labs(x="Mois",y = 'LOAD en moyenne', colour="Year") +
  theme_bw()


# Year plot
t$label <- as.factor(paste("Mean", t$year, sep = ""))
t$b <-month.abb[t$month]
# Superposition des années en radar
ggplot(data = t, aes(x = month, y = Mean, color = label)) + 
  geom_line() +
  geom_point() + 
  coord_polar() +
  scale_x_continuous(breaks = 1:12,labels =  month.name) +
  theme(legend.position = "bottom") +
  theme_bw()





