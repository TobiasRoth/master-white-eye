#======================================================================================================
#
#  Birds in the Zoo of Basel:
#  Make density maps for all species and White-Eye individuals
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

#------------------------------------------------------------------------------------------------------
# Function to make the plots
#------------------------------------------------------------------------------------------------------
plotsectors <- function(x, individuum, sex = NA) {
  par(mar = c(1,1,2,1))
  rbPal <- colorRampPalette(c('white','red'))
  tcol <- rbPal(50)[as.numeric(cut(x / sum(x, na.rm = TRUE), breaks = seq(0, 1, length.out = 50)))]
  tborder <- "black"
  plot(NA, xlim = c(0, 5), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")
  for(i in 1:5) {
    polygon(c(i-1, i, i, i-1), c(1, 1, 2, 2), border = tborder, col = tcol[i]) 
    text(i-0.93, 1.07, paste("Sect", i), adj = 0, col = "gray")
    text(i-0.5, 1.5, paste("n =", x[i]), adj = 0.5)
  }
  for(i in 6:10) {
    polygon(c(10-i, 11-i, 11-i, 10-i), c(0, 0, 1, 1), border = tborder, col = tcol[i]) 
    text(10.07-i, 0.07, paste("Sect", i), adj = 0, col = "gray")
    text(10.5- i, 0.5, paste("n =", x[i]), adj = 0.5)
  }
  if(!is.na(sex)) mtext(paste0("Name: ", individuum, "; Sex: ", sex), 3)
  if(is.na(sex)) mtext(paste0("Name: ", individuum, " "), 3)
}

#------------------------------------------------------------------------------------------------------
# Make the plots for all White-Eyes
#------------------------------------------------------------------------------------------------------
d <- dat %>% filter(Art == "BV" & !is.na(Individuum)) %>% 
  group_by(Individuum, Sektor, Geschlecht) %>% 
  summarise(AnzBeob = n())

for(i in unique(d$Individuum)){
  pdf(paste0("DensityMaps/WhiteEye_", i, ".pdf"))
  plotsectors(d$AnzBeob[d$Individuum == i], i, d$Geschlecht[d$Individuum == i][1])
  dev.off()
}

#------------------------------------------------------------------------------------------------------
# Make the plots for species
#------------------------------------------------------------------------------------------------------
d <- dat %>% 
  filter(!is.na(Art) & !is.na(Sektor)) %>% 
  group_by(Art, Sektor) %>% 
  summarise(AnzBeob = n())

for(i in unique(d$Art)){
  pdf(paste0("DensityMaps/", i, ".pdf"))
  plotsectors(d$AnzBeob[d$Art == i], i)
  dev.off()
}

