#======================================================================================================
#
#  Birds in the Zoo of Basel:
#  First analyses
#
#======================================================================================================

#------------------------------------------------------------------------------------------------------
# Settings
#------------------------------------------------------------------------------------------------------
# Libraries
library(readxl)
library(tidyverse)

#------------------------------------------------------------------------------------------------------
# Data preparation
#------------------------------------------------------------------------------------------------------
# Load data
dat <- read_xlsx("Data/Daten_Komplett.xlsx")

# 





# Datum formatieren (ist ein bisschen mühsam mit Excel)
dat$Datum <- as.POSIXct((dat$Datum + dat$Uhrzeit) * (60*60*24), origin = "1899-12-30", tz = "GMT")
dat$Julian <- as.integer(floor(dat$Datum - as.POSIXct("2017-05-30"))) # 1 = 1.6.2017 
dat$Uhrzeit <- format(dat$Datum, format = "%H:%M", tz = "GMT")
dat$Stunde <- as.integer(format(dat$Datum, format = "%H", tz = "GMT"))
dat$Minute <- as.integer(format(dat$Datum, format = "%M", tz = "GMT"))

# Eindeutige ID für eine Aufnahme (Datum, Stunde und Sektor)
dat$ID <- paste(dat$Julian, dat$Stunde, dat$Sektor, sep = "-")

# Anzahl Datenpunkte pro Art, Individuum und Sektor anschauen
table(dat$Art)
table(dat$Individuum)
table(dat$Sektor)

# Nur Daten von Brillenvögel
dBV <- dat[dat$Art == "BV", c("ID", "Julian", "Stunde", "Minute", "Besucher", "Sektor", "Individuum")]

# Daten für Artanalysen (BV müssen die gleiche Struktur haben wie übrige Arten)
dArten <- dat[dat$Art != "BV", c("ID", "Julian", "Stunde", "Minute", "Besucher", "Sektor", "Art")]
tmp <- data.frame(
  ID = tapply(dBV$ID, dBV$ID, function(x) x[1]),
  Julian = tapply(dBV$Julian, dBV$ID, function(x) x[1]),
  Stunde = tapply(dBV$Stunde, dBV$ID, function(x) x[1]),
  Minute = tapply(dBV$Minute, dBV$ID, function(x) x[1]),
  Besucher = tapply(dBV$Besucher, dBV$ID, function(x) x[1]),
  Sektor = tapply(dBV$Sektor, dBV$ID, function(x) x[1]))
tmp$Art <- "BV"
dArten <- rbind(dArten, tmp)


# Nullhypothese: Arten Arten verteilen sich gleich über die Sektoren
# Chisq-Test: p<0.001 --> Nullhypothese kann verworfen werden
(tmp <- table(dArten$Art, dArten$Sektor))
chisq.test(tmp)
plot(tmp, main = "Verteilung der Arten auf die Sektoren")

# Nullhypothese: Brillenvogel-Individuen verteilen sich gleich über die Sektoren
# Chisq-Test: p<0.001 --> Nullhypothese kann verworfen werden
(tmp <- table(dBV$Ind, dBV$Sektor))
chisq.test(tmp)
plot(tmp, main = "Verteilung der Brillenvogel-Individuen auf die Sektoren")


