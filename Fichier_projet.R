#premier fichier pour projet
library(lubridate)
library(dplyr)
library(ggplot2)

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


###Je vais essayer de nettoyer un peu la base parce que les dates sont mal foutue###

#Déja il faut convertir les dates en bon format

new_data <- data


new_data$Date <- ymd_hms(new_data$Date, tz = "UTC")
new_data$Date_semi <- as.Date(data$Date)

#il faut changer les noms des colones.

colnames(new_data) <- c("territoire","statut","date","production_totale","thermique",
                    "bagasse_charbon","hydraulique","micro_hydraulique","solaire_photovoltaique",
                    "eolien","bioenergies","géothermie","importations","cmp_euro_mwh","date_semi")

new_data <- new_data %>% 
  mutate(territoire = as.factor(territoire)) %>% 
  arrange(territoire, date,date_semi)

data_panel_1 <- pdata.frame(new_data, index = c("territoire","date"))
data_panel_2 <- pdata.frame(new_data, index = c("territoire","date_semi"))


View(data_panel)


#Premier graphique sur la production totale en regardant tous les territoires:

graph_corse_1 <- data_panel_1 %>% 
  ggplot(aes(x = date, y=production_totale, color = territoire)) +
  geom_point() +
  theme_minimal()
graph_corse_1




