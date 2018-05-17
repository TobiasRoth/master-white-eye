#Fragen/Probleme      Stand: 26.04.18

library(readxl)
setwd("C:/Users/Tabea/Desktop/Masterarbeit_Statistik")

data.komplett <- read_excel("Daten_Komplett.xlsx")
data.komplett <- data.frame(data.komplett)

data.summer <- data.komplett[data.komplett$Saison == "Sommer", ]  #11'159 Beobachtungen
data.winter <- data.komplett[data.komplett$Saison == "Winter", ]  #6'441 Beobachtungen

##1) wie bekomme ich den Zahlenwert (bsp. Sektor 6 = 1608 obs.) dazu? #Kann man hier die BV farbig hervorheben?
tS <- table(data.summer$Art, data.summer$Sektor)
barplot(tS, main = "Anzahl Vögel pr Sektor INSGESAMT \n (Sommer)", xlab = "Sektor", ylab = "#Sichtungen", ylim = c(0,2000))

##2) Wieso stimmen die Zahlen nicht überein? #kann eig. nicht an NA's liegen, denn das wären im hier 3...
data.BVS <- data.summer[data.summer$Art == "BV", ]  #Gezählt: 2'856 / R sagt: 2'860 obs. ???!!!

##3) Wie passe ich die Y-Achse schön an? #sodass sie auch die Zahl des höchsten Balken anzeigt; ODER Zahlen DIREKT über Balken
tBV <- table(data.BVS$Art, data.BVS$Sektor)
barplot(tBV, main = "Anzahl BV/Sektor im Sommer", xlab = "Sektor", ylab = "#BV", ylim = c(0,600), col = "darkgreen")

##4) wie passe ich die X-Achse an, dass sie mir ALLE Sektoren anzeigt, auch wenn dort KEINE Sichtungen waren?
data.STW <- data.winter[data.winter$Art == "ST", ]  #scheinbar nie in Sektoren 1, 2, 8 und 9 gesehen -> Grafik anpassen?!
tST <- table(data.STW$Art, data.STW$Sektor)
barplot(tST, main = "Anzahl ST/Sektor im Winter")

##5) Kann man die Balken in Sommer und Winter mit Farbe unterteilen? (bsp.: Sommer hellgelb und Winter hellblau)?
data.BV <- data.komplett[data.komplett$Art == "BV", ]  #Gezählt: 2'856 / R sagt: 2'860 obs. ???!!
tBV <- table(data.BV$Art, data.BV$Sektor)
barplot(tBV, main = "Anzahl BV/Sektor", xlab = "Sektor", ylab = "#BV", ylim = c(0,750), col = "darkgreen")

##6) Wie kann ich die Barplots für die INDIVIDUEN pro Sektor machen?
  #BV Individuen/Sektor für Sommer:
tBV_ind <- table(data.BVS$Individuum, data.BVS$Sektor)

tBV_B_r <- table(data.BVS$Individuum == "B_r", data.BVS$Sektor) #Was bedeutet das FALSE und TRUE???
barplot(tBV_B_r, main = "Sichtungen von B_r/Sektor \n im Sommer", xlab = "Sektor", ylab = "#Sichtungen", ylim = c(0,450), col = "brown")
#Warum macht R die Balken jetzt für FALSE noch untendrunter???!!!







