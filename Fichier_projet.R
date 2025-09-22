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









#############e Modification de la base

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

new_data$cmp_euro_mwh <- as.numeric(new_data$cmp_euro_mwh)

new_data[is.nan(as.matrix(new_data))] <- NA



unique(is.nan(as.matrix(new_data)))
#La on nous dis bien qu'il n'y a pas de NaN

new_data









#On commence les analysese


data_panel_1 <- pdata.frame(new_data, index = c("territoire","date"))



#Premier graphique sur la production totale en regardant tous les territoires:

graph_1 <- data_panel_1 %>% 
  ggplot(aes(x = date, y=production_totale, color = territoire)) +
  geom_point() +
  theme_minimal()
graph_1


#On créer un dataframe sur seulement les jours et on affiche

data_semin <- new_data   %>% 
  group_by(territoire,date_semi) %>% 
  summarise(mean_prod = mean(production_totale, na.rm = T),.groups = "drop") %>% 
  ggplot(aes(x = date_semi, y = mean_prod, color = territoire)) +
  geom_point() +
  theme_minimal()

#Maintenant on regarde ce qui se passe pour la réunion et la guiyanne.

plot_re_guy <- new_data   %>% 
  filter(territoire %in% c('Guyane','Réunion')) %>% 
  group_by(territoire,date_semi) %>% 
  summarise(mean_prod = mean(production_totale, na.rm = T),.groups = "drop") %>% 
  ggplot(aes(x = date_semi, y = mean_prod, color = territoire)) +
  geom_point() +
  theme_minimal()
  
plot_re_guy
#On peut voir qu'il se passe quelque chose sur les graphiques, on peut faire une regression

data_re_guy_reg <- new_data   %>% 
  filter(territoire %in% c('Guyane','Réunion'),
         date_semi >= as.POSIXct("2022-01-01", tz = "UTC")) %>% 
  group_by(territoire,date_semi) %>% 
  summarise(mean_prod = mean(production_totale, na.rm = T),.groups = "drop") %>% 
  ggplot(aes(x = date_semi, y = mean_prod, color = territoire)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  theme_minimal()

data_re_guy_reg

#On voit une baisse de la production de la production totale moyenne d'electricité  par jour pour la Réunion, c'est l'inverse pour la Gyuane
#Il faut aller regarder les situation géopolitique et peut petre le prix par megawatth
#On regarde alors l'évolution du pris pour premièrement la guyane:

  

graph_guy_prod <- new_data   %>% 
  filter(territoire %in% c('Guyane'),
         date_semi >= as.POSIXct("2022-01-01", tz = "UTC")) %>% 
  group_by(territoire,date_semi) %>% 
  summarise(mean_prod = mean(production_totale, na.rm = T),.groups = "drop") %>% 
  ggplot(aes(x = date_semi, y = mean_prod, color = territoire)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  theme_minimal()
graph_guy_prod


#finalement on regarde plutôt les deux pays en mêmes temps

graph_guy_price <- new_data   %>% 
  filter(territoire %in% c('Guyane','Réunion'),
         date_semi >= as.POSIXct("2022-01-01", tz = "UTC")) %>% 
  group_by(territoire,date_semi) %>% 
  select(territoire,date_semi,production_totale,cmp_euro_mwh) %>% 
  summarise(mean_price = mean(cmp_euro_mwh, na.rm = T), .groups =  "drop")
  
graph_guy_price

#On remplace les NaN par des 0 puis on retire les lignes sans les 0
graph_guy_price$mean_price <- ifelse(is.nan(graph_guy_price$mean_price),0,graph_guy_price$mean_price )

###### ICI ca ne marche pas car ca retire tout, on va faire un graphique plus large.




#Pas intéressant parce qu'il n'y a pas les données des prix après 2020

graph_large <- new_data %>% 
  filter(territoire %in% c("Guyane","Corse"),
         date <= as.POSIXct("2020-01-01", tz = "UTC")) %>% 
  group_by(territoire,date_semi) %>%
  summarise(mean = mean(cmp_euro_mwh, na.rm = T), .groups =  "drop") %>% 
  ggplot(aes(x = date_semi, y = mean, color = territoire))  +
  geom_point(alpha = 0.5) +
  geom_line() +
  theme_minimal()
graph_large



#On regarde la base apres 2022: 




info_data_2022 <- new_data %>% 
  filter(date_semi >= as.POSIXct("2022-01-01", tz = "UTC")) %>% 
  select(cmp_euro_mwh) %>% 
  unique()
info_data_2022
#Ici on voit qu'il manque les données des prix  à partir de 2022.
