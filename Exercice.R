library(lubridate)
library(dplyr)
library(ggplot2)

#Cours
library(plm)
data("Grunfeld", package = "plm")
model <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
summary(model)


#Pour le within
mod_FE_within <- plm(inv ~ value + capital,
                     data = Grunfeld,
                     model = "within",
                     effect = "individual")
summary(mod_FE_within)

#Ici un R2ajuste de .75311



#pout le pooling
mod1_pool <- plm(inv ~ value + capital,
                 data = Grunfeld,
                 model = "pooling")
summary(mod1_pool)



#Ici un R2ajuste de .8105



#Ici seulement avec la value
pool_mod <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")

summary(pool_mod)# Ici  0.73299





coef_pool <- coef(pool_mod)
coef_wt <- coef(mod_FE_within)

ggplot(Grunfeld, aes(x = value, y = inv,
                     color = factor(firm))) +
  geom_point() +
  geom_abline(intercept = coef_pool[1],
              slope = coef_pool[2],
              color = "black",
              linewidth = 0.9) +
  geom_abline(intercept = coef_wt[1],
              slope = coef_wt[2],
              color = "pink",
              linewidth = 0.9) +
  labs(x = "Valeur de l’entreprise",
       y = "Investissement",
       color = "Firme")


#Modele avec l'individualité



fd_mod <- plm(inv ~ value, data = Grunfeld, model = "fd",effect = "individual")
coef <- coef(fd_mod)

ggplot(Grunfeld, aes(x = value, y = inv,
                     color = factor(firm))) +
  geom_point() +
  geom_abline(intercept = coef[1],
              slope = coef[2],
              color = "black",
              linewidth = 0.9) +
  geom_abline(intercept = coef_pool[1],
              slope = coef_pool[2],
              color = "orange",
              linewidth = 0.9) +
  geom_abline(intercept = coef_wt[1],
              slope = coef_wt[2],
              color = "pink",
              linewidth = 0.9) +
  labs(x = "Valeur de l’entreprise",
       y = "Investissement",
       color = "Firme")






ggplqot(Grunfeld, aes(x = value + capital, y = inv,
                     color = factor(firm))) +
  geom_point() +
  geom_abline(intercept = coef[1],
              slope = coef[2],
              color = "black",
              linewidth = 0.9) +
  labs(x = "Valeur de l’entreprise",
       y = "Investissement",
       color = "Firme")
  
