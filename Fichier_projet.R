#premier fichier pour projet

#Cours
library(plm)
data("Grunfeld", package = "plm")
model <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
summary(model)

#Pour trouver la fichier de datagouv
#import de la base de donnée


#lien internet de la base : 
#https://www.data.gouv.fr/datasets/production-delectricite-par-filiere-et-couts-de-production-au-pas-horaire/

#Portail original de la source des données :
#https://opendata-corse-outremer.edf.fr/explore/dataset/courbe-de-charge-de-la-production-delectricite-par-filiere/information/?disjunctive.territoire

data <- read.csv("courbe-de-charge-de-la-production-delectricite-par-filiere.csv", sep = ";")
View(data)


dim(data)
#On est sur une base de données de 263 040 données avec 14 variables


types_des_variables <- str(data)
types_des_variables


length(unique(data$Date))
#Il y a plus de 52 000 dates différentes -> compliquer pour faire des analyses mais vasy.


#si on regarde seulement au nivrau de la corse:
library(dplyr)
data_corse <- data %>% 
  filter(Territoire == "Corse")



