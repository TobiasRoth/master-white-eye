#======================================================================================================
#
#  Birds in the Zoo of Basel:
#  Is the probability that an individual is breeding on its nest depend on the 
#  number of people visiting the bird house?
#
#======================================================================================================

#------------------------------------------------------------------------------------------------------
# Settings
#------------------------------------------------------------------------------------------------------
# Libraries
library(readxl)
library(tidyverse)
library(arm)

#------------------------------------------------------------------------------------------------------
# Data preparation
#------------------------------------------------------------------------------------------------------
# Load data
dat <- read_xlsx("Data/Daten_Komplett.xlsx")

#------------------------------------------------------------------------------------------------------
# Nest 1, Sektor 2
#------------------------------------------------------------------------------------------------------
# Daten zusammenstellen
d <- dat %>% 
  filter(Sektor == 2 & Saison == "Sommer") %>% 
  filter(Individuum == "B_r" | Individuum == "P_r") %>% 
  group_by(Datum_Zahl, Uhrzeit_St, Besucher) %>% 
  summarise(Nest = sum(Bemerkung == "Nest", na.rm = TRUE) > 0)

# Modell machen
mod <- glm(Nest ~ Datum_Zahl + I(Datum_Zahl^2) + Uhrzeit_St + I(Uhrzeit_St^2) + Besucher, 
    family = binomial, data = d)
summary(mod)

# Vertrauensbereich berechnen
nsim <- 1000
post <- sim(mod, nsim)@coef
prediction <- function(x){
  plogis(post[, "(Intercept)"] + 
    post[, "Datum_Zahl"] * newdata[x, "Datum_Zahl"] + 
    post[, "I(Datum_Zahl^2)"] * newdata[x, "Datum_Zahl"]^2 + 
    post[, "Uhrzeit_St"] * newdata[x, "Uhrzeit_St"] + 
    post[, "I(Uhrzeit_St^2)"] * newdata[x, "Uhrzeit_St"]^2 + 
    post[, "Besucher"] * newdata[x, "Besucher"])
}

# Wahrscheinlichkeit, dass ein Nest besetzt ist im Tagesverlauf
newdata <- data.frame(Datum_Zahl = 10, Uhrzeit_St = seq(7, 18, 0.1), Besucher = 0)
tt <- map(1:nrow(newdata), prediction)
newdata$y <- map_dbl(tt, mean)
newdata$ylow <- map_dbl(tt, quantile, probs = 0.025)
newdata$yup <- map_dbl(tt, quantile, probs = 0.975)
ggplot(newdata, aes(x = Uhrzeit_St, y = y)) + 
  geom_ribbon(aes(ymin = ylow, ymax = yup), col = "grey", fill = "grey") +
  geom_line() + 
  labs(x = "Time of the day", y = "Probability that nest is occupied",
       title = "Nest 1 (B_r	and P_r)") +
  ylim(0, 1)

# Wahrscheinlichkeit, dass ein Nest besetzt ist im Verlauf der Saison
newdata <- data.frame(Datum_Zahl = seq(1, 25, 0.1), Uhrzeit_St = 12, Besucher = 0)
tt <- map(1:nrow(newdata), prediction)
newdata$y <- map_dbl(tt, mean)
newdata$ylow <- map_dbl(tt, quantile, probs = 0.025)
newdata$yup <- map_dbl(tt, quantile, probs = 0.975)
ggplot(newdata, aes(x = Datum_Zahl, y = y)) + 
  geom_ribbon(aes(ymin = ylow, ymax = yup), col = "grey", fill = "grey") +
  geom_line() + 
  labs(x = "Day of the saison", y = "Probability that nest is occupied",
       title = "Nest 1 (B_r	and P_r)") +
  ylim(0, 1)

#------------------------------------------------------------------------------------------------------
# Nest 2, Sektor 2
#------------------------------------------------------------------------------------------------------
# Daten zusammenstellen
d <- dat %>% 
  filter(Sektor == 2 & Saison == "Sommer") %>% 
  filter(Individuum == "L_l" | Individuum == "W_r") %>% 
  group_by(Datum_Zahl, Uhrzeit_St, Besucher) %>% 
  summarise(Nest = sum(Bemerkung == "Nest", na.rm = TRUE) > 0)

# Modell machen
mod <- glm(Nest ~ Datum_Zahl + I(Datum_Zahl^2) + Uhrzeit_St + I(Uhrzeit_St^2) + Besucher, 
           family = binomial, data = d)
summary(mod)

# Vertrauensbereich berechnen
nsim <- 10000
post <- sim(mod, nsim)@coef
prediction <- function(x){
  plogis(post[, "(Intercept)"] + 
           post[, "Datum_Zahl"] * newdata[x, "Datum_Zahl"] + 
           post[, "I(Datum_Zahl^2)"] * newdata[x, "Datum_Zahl"]^2 + 
           post[, "Uhrzeit_St"] * newdata[x, "Uhrzeit_St"] + 
           post[, "I(Uhrzeit_St^2)"] * newdata[x, "Uhrzeit_St"]^2 + 
           post[, "Besucher"] * newdata[x, "Besucher"])
}

# Wahrscheinlichkeit, dass ein Nest besetzt ist im Tagesverlauf
newdata <- data.frame(Datum_Zahl = 10, Uhrzeit_St = seq(7, 18, 0.1), Besucher = 0)
tt <- map(1:nrow(newdata), prediction)
newdata$y <- map_dbl(tt, mean)
newdata$ylow <- map_dbl(tt, quantile, probs = 0.025)
newdata$yup <- map_dbl(tt, quantile, probs = 0.975)
ggplot(newdata, aes(x = Uhrzeit_St, y = y)) + 
  geom_ribbon(aes(ymin = ylow, ymax = yup), col = "grey", fill = "grey") +
  geom_line() + 
  labs(x = "Time of the day", y = "Probability that nest is occupied",
       title = "Nest 1 (L_l	and W_r)") +
  ylim(0, 1)

# Wahrscheinlichkeit, dass ein Nest besetzt ist im Verlauf der Saison
newdata <- data.frame(Datum_Zahl = seq(1, 25, 0.1), Uhrzeit_St = 12, Besucher = 0)
tt <- map(1:nrow(newdata), prediction)
newdata$y <- map_dbl(tt, mean)
newdata$ylow <- map_dbl(tt, quantile, probs = 0.025)
newdata$yup <- map_dbl(tt, quantile, probs = 0.975)
ggplot(newdata, aes(x = Datum_Zahl, y = y)) + 
  geom_ribbon(aes(ymin = ylow, ymax = yup), col = "grey", fill = "grey") +
  geom_line() + 
  labs(x = "Day of the saison", y = "Probability that nest is occupied",
       title = "Nest 1 (L_l	and W_r)") +
  ylim(0, 1)
