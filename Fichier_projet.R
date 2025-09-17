#premier fichier pour projet

#Cours
library(plm)
data("Grunfeld", package = "plm")
model <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
summary(model)

#Pour trouver la fichier de datagouv


