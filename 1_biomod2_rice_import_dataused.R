# Requierd Library
library(biomod2)

# Step 1 Import rice cultivation and current climate data
# 1.1 load species occurrences data
DataSpecies <- read.csv("D:/Biomod2_Rice/rice_extent/rice_extent_csv.csv")
head(DataSpecies)
# the name of studied species
myRespName <- 'RICE'
# the presence/absences data for our species
myResp <- as.numeric(DataSpecies[,myRespName])
# the XY coordinates of species data
myRespXY <- DataSpecies[,c("x_wgs84","y_wgs84")]
# creat study area extent from species data 
coordinates(DataSpecies) <- ~x_wgs84+y_wgs84
studyarea <- extent(DataSpecies)

# 1.2 Import Current Climate data
# 1.2.1 Total precipitation (SEP:OCT)
setwd("D:/Biomod2_Rice/Climate_Data/WC_2_0_30s/wc2.0_30s_prec")
list <- list.files(pattern = "\\.tif$")
prec.stack <- stack(list)
gr.s <- subset(prec.stack, 9:10)
prec.th <- crop(gr.s, studyarea)
prec.tt <- sum(prec.th) #prec.tt (total), tmax.m (mean), tmin.m (mean) and PER rate
plot(prec.tt, main = "Total precipitation (SEP-OCT)")

# 1.2.2 Mean maximum temperature (Tmax) (JUN:SEP)
setwd("D:/Biomod2_Rice/Climate_Data/WC_2_0_30s/wc2.0_30s_tmax")
list <- list.files(pattern = "\\.tif$")
gridstack <- stack(list)
gr.s <- subset(gridstack, 6:9)
gr.th <- crop(gr.s, studyarea)
tmax.m <- mean(gr.th) #prec.tt (total), tmax.m (mean), tmin.m (mean) and PER rate
plot(tmax.m, main = "Mean maximum temperature (JUN:SEP)")

# 1.2.3 Mean minimum temperature (Tmin) (SEP:OCT)
setwd("D:/Biomod2_Rice/Climate_Data/WC_2_0_30s/wc2.0_30s_tmin")
list <- list.files(pattern = "\\.tif$")
gridstack <- stack(list)
gr.s <- subset(gridstack, 9:10)
gr.th <- crop(gr.s, studyarea)
tmin.m <- mean(gr.th) #prec.tt (total), tmax.m (mean), tmin.m (mean) and PER rate
plot(tmin.m, main = "Mean minimum temperature (SEP:OCT)")

# 1.2.4 Precipitation-evapotranspiration ratio (PER) (JUN:SEP)
setwd("D:/Biomod2_Rice/Climate_Data/WC_2_0_30s/wc2.0_30s_tavg")
list <- list.files(pattern = "\\.tif$")
gridstack <- stack(list)
gr.s <- subset(gridstack, 6:9)
tavg <- crop(gr.s, studyarea)
plot(tavg)

# 1.2.5 PER Calculation
prec.t <- subset(prec.stack, 6:9)
prec.s <- crop(prec.t, studyarea)
plot(prec.s)

# PER calculate
H = 15/24
SVP = 6.108*exp((17.27*tavg)/(tavg+237.3))
TM = tavg
  
pe = 715.5*H*SVP*((tavg)/(tavg+273.2))
PER = prec.s/pe
plot(PER)
PER.avg <- sum(PER)
plot(PER.avg, main = "Precipitation \n Evapotranspiration Ratio (JUN:SEP)")

# 1.2.6 load present climate data
myExpl = stack(tmin.m,   # 1. Mean maximum temperature (Tmax) (JUN:SEP)
               tmax.m,   # 2. Mean minimum temperature (Tmin) (SEP:OCT)
               prec.tt,  # 3. Total precipitation (Rain) (SEP:OCT)
               PER.avg)  # 4.Precipitation-evapotranspiration ratio (PER)

# rename layers
names(myExpl) <- c('tmin', 'tmax', 'prec','PER')
plot(myExpl)

# Step 2 Import Future Climate Data

# 2.1 Run R Script to import HadGEM2-CC climate data (import_HadGEM2-CC.R)

# 2.1.1 Mean minimum temp

# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/HadGEM2-CC/rcp45/hg45tn50")
list.tmin.hg45 <- list.files(pattern = "\\.tif$")
tmin.hg45 <- stack(list.tmin.hg45)
tmin.hg45.th <- crop(tmin.hg45, studyarea)
tmin.hg45.mnplt <- tmin.hg45.th/10
tmin.hg45.06.09 <- subset(tmin.hg45.mnplt, 9:12)
tmin.hg45.09 <- subset(tmin.hg45.mnplt,12)
tmin.hg45.10 <- subset(tmin.hg45.mnplt,2)
tmin.hg45 <- mean(stack(tmin.hg45.09, tmin.hg45.10))
plot(tmin.hg45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/HadGEM2-CC/rcp85/hg85tn50")
list.tmin.hg85 <- list.files(pattern = "\\.tif$")
tmin.hg85 <- stack(list.tmin.hg85)
tmin.hg85.th <- crop(tmin.hg85, studyarea)
tmin.hg85.mnplt <- tmin.hg85.th/10
tmin.hg85.06.09 <- subset(tmin.hg85.mnplt, 9:12)
tmin.hg85.09 <- subset(tmin.hg85.mnplt,12)
tmin.hg85.10 <- subset(tmin.hg85.mnplt,2)
tmin.hg85 <- mean(stack(tmin.hg85.09, tmin.hg85.10))
plot(tmin.hg85)

# 2.1.2 Mean maximum temperature

# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/HadGEM2-CC/rcp45/hg45tx50")
list.tmax.hg45 <- list.files(pattern = "\\.tif$")
tmax.hg45.all <- stack(list.tmax.hg45)
tmax.hg45.th <- crop(tmax.hg45.all, studyarea)
tmax.hg45.mnplt <- tmax.hg45.th/10
tmax.hg45.06.09 <- subset(tmax.hg45.mnplt, 9:12)
tmax.hg45 <- mean(tmax.hg45.06.09)
plot(tmax.hg45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/HadGEM2-CC/rcp85/hg85tx50")
list.tmax.hg85 <- list.files(pattern = "\\.tif$")
tmax.hg85.all <- stack(list.tmax.hg85)
tmax.hg85.th <- crop(tmax.hg85.all, studyarea)
tmax.hg85.mnplt <- tmax.hg85.th/10
tmax.hg85.06.09 <- subset(tmax.hg85.mnplt, 9:12)
tmax.hg85 <- mean(tmax.hg85.06.09)
plot(tmax.hg85)

# 2.1.3 Total precipitation
# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/HadGEM2-CC/rcp45/hg45pr50")
list.hg45.prec <- list.files(pattern = "\\.tif$")
prec.hg45.all <- stack(list.hg45.prec)
prec.hg45.th <- crop(prec.hg45.all, studyarea)
prec.hg45.09 <- subset(prec.hg45.th,12)
prec.hg45.10 <- subset(prec.hg45.th,2)
prec.hg45.06.09 <- subset(prec.hg45.th, 9:12)
prec.hg45 <- sum(stack(prec.hg45.09, prec.hg45.10))
plot(prec.hg45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/HadGEM2-CC/rcp85/hg85pr50")
list.hg85.prec <- list.files(pattern = "\\.tif$")
prec.hg85.all <- stack(list.hg85.prec)
prec.hg85.th <- crop(prec.hg85.all, studyarea)
prec.hg85.09 <- subset(prec.hg85.th,12)
prec.hg85.10 <- subset(prec.hg85.th,2)
prec.hg85.06.09 <- subset(prec.hg85.th, 9:12)
prec.hg85 <- sum(stack(prec.hg85.09, prec.hg85.10))
plot(prec.hg85)

# 2.1.4 PER
# PER calculate
# RCP45
H = 15/24
tmean.hg45 <- (tmin.hg45.06.09+tmax.hg45.06.09)/2
SVP.hg.45 = 6.108*exp((17.27*tmean.hg45)/(tmean.hg45+237.3))


pe.hg45 = 715.5*H*SVP.hg.45*((tmean.hg45)/(tmean.hg45+273.2))
PER.hg45 = prec.hg45.06.09/pe.hg45
PER.hg45.sum <- sum(PER.hg45)
plot(PER.hg45.sum)

# RCP85
tmean.hg85 <- (tmin.hg85.06.09+tmax.hg85.06.09)/2
SVP.hg.85 = 6.108*exp((17.27*tmean.hg85)/(tmean.hg85+237.3))


pe.hg85 = 715.5*H*SVP.hg.85*((tmean.hg85)/(tmean.hg85+273.2))
PER.hg85 = prec.hg85.06.09/pe.hg85
PER.hg85.sum <- sum(PER.hg85)
plot(PER.hg85.sum)

# load all climate variables for the HadGEM2-CC.

# RCP45
hg45bi50 = stack(tmin.hg45, # Annual Mean Min Temperature
                 tmax.hg45, # Annual Mean Max Temperature
                 prec.hg45, # Annual Precipitation
                 PER.hg45.sum) # PER

names(hg45bi50) <- c('tmin', 'tmax', 'prec','PER')
plot(hg45bi50, main = "HadGEM2-CC RCP45")

# RCP85
hg85bi50 = stack(tmin.hg85, # Annual Mean Min Temperature
                 tmax.hg85, # Annual Mean Max Temperature
                 prec.hg85, # Annual Precipitation
                 PER.hg85.sum) # PER

names(hg85bi50) <- c('tmin', 'tmax', 'prec','PER')
plot(hg85bi50, main = "HadGEM2-CC RCP85")

# 2.2 Run R Script to import IPSL-CM5A-LR climate data (import_IPSL-CM5A-LR.R)
# 2.2.1 Mean minimum temp

# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/IPSL-CM5A-LR/rcp45/ip45tn50")
list.tmin.ip45 <- list.files(pattern = "\\.tif$")
tmin.ip45 <- stack(list.tmin.ip45)
tmin.ip45.th <- crop(tmin.ip45, studyarea)
tmin.ip45.mnplt <- tmin.ip45.th/10
tmin.ip45.06.09 <- subset(tmin.ip45.mnplt, 9:12)
tmin.ip45.09 <- subset(tmin.ip45.mnplt,12)
tmin.ip45.10 <- subset(tmin.ip45.mnplt,2)
tmin.ip45 <- mean(stack(tmin.ip45.09, tmin.ip45.10))
plot(tmin.ip45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/IPSL-CM5A-LR/rcp85/ip85tn50")
list.tmin.ip85 <- list.files(pattern = "\\.tif$")
tmin.ip85 <- stack(list.tmin.ip85)
tmin.ip85.th <- crop(tmin.ip85, studyarea)
tmin.ip85.mnplt <- tmin.ip85.th/10
tmin.ip85.06.09 <- subset(tmin.ip85.mnplt, 9:12)
tmin.ip85.09 <- subset(tmin.ip85.mnplt,12)
tmin.ip85.10 <- subset(tmin.ip85.mnplt,2)
tmin.ip85 <- mean(stack(tmin.ip85.09, tmin.ip85.10))
plot(tmin.ip85)

# 2.2.2 Mean maximum temperature

# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/IPSL-CM5A-LR/rcp45/ip45tx50")
list.tmax.ip45 <- list.files(pattern = "\\.tif$")
tmax.ip45.all <- stack(list.tmax.ip45)
tmax.ip45.th <- crop(tmax.ip45.all, studyarea)
tmax.ip45.mnplt <- tmax.ip45.th/10
tmax.ip45.06.09 <- subset(tmax.ip45.mnplt, 9:12)
tmax.ip45 <- mean(tmax.ip45.06.09)
plot(tmax.ip45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/IPSL-CM5A-LR/rcp85/ip85tx50")
list.tmax.ip85 <- list.files(pattern = "\\.tif$")
tmax.ip85.all <- stack(list.tmax.ip85)
tmax.ip85.th <- crop(tmax.ip85.all, studyarea)
tmax.ip85.mnplt <- tmax.ip85.th/10
tmax.ip85.06.09 <- subset(tmax.ip85.mnplt, 9:12)
tmax.ip85 <- mean(tmax.ip85.06.09)
plot(tmax.ip85)

# 2.2.3 Total precipitation
# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/IPSL-CM5A-LR/rcp45/ip45pr50")
list.ip45.prec <- list.files(pattern = "\\.tif$")
prec.ip45.all <- stack(list.ip45.prec)
prec.ip45.th <- crop(prec.ip45.all, studyarea)
prec.ip45.09 <- subset(prec.ip45.th,12)
prec.ip45.10 <- subset(prec.ip45.th,2)
prec.ip45.06.09 <- subset(prec.ip45.th, 9:12)
prec.ip45 <- sum(stack(prec.ip45.09, prec.ip45.10))
plot(prec.ip45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/IPSL-CM5A-LR/rcp85/ip85pr50")
list.ip85.prec <- list.files(pattern = "\\.tif$")
prec.ip85.all <- stack(list.ip85.prec)
prec.ip85.th <- crop(prec.ip85.all, studyarea)
prec.ip85.09 <- subset(prec.ip85.th,12)
prec.ip85.10 <- subset(prec.ip85.th,2)
prec.ip85.06.09 <- subset(prec.ip85.th, 9:12)
prec.ip85 <- sum(stack(prec.ip85.09, prec.ip85.10))
plot(prec.ip85)

# 2.2.4 PER
# PER calculate
# RCP45
H = 15/24
tmean.ip45 <- (tmin.ip45.06.09+tmax.ip45.06.09)/2
SVP.ip45 = 6.108*exp((17.27*tmean.ip45)/(tmean.ip45+237.3))


pe.ip45 = 715.5*H*SVP.ip45*((tmean.ip45)/(tmean.ip45+273.2))
PER.ip45 = prec.ip45.06.09/pe.ip45
PER.ip45.sum <- sum(PER.ip45)
plot(PER.ip45.sum)

# RCP85
tmean.ip85 <- (tmin.ip85.06.09+tmax.ip85.06.09)/2
SVP.ip85 = 6.108*exp((17.27*tmean.ip85)/(tmean.ip85+237.3))


pe.ip85 = 715.5*H*SVP.ip85*((tmean.ip85)/(tmean.ip85+273.2))
PER.ip85 = prec.ip85.06.09/pe.ip85
PER.ip85.sum <- sum(PER.ip85)
plot(PER.ip85.sum)

# load all climate variables for the IPSL-CM5A-LR.

# RCP45
ip45bi50 = stack(tmin.ip45, # Annual Mean Min Temperature
                 tmax.ip45, # Annual Mean Max Temperature
                 prec.ip45, # Annual Precipitation
                 PER.ip45.sum) # PER

names(ip45bi50) <- c('tmin', 'tmax', 'prec','PER')
plot(ip45bi50, main = "IPSL-CM5A-LR RCP45")

# RCP85
ip85bi50 = stack(tmin.ip85, # Annual Mean Min Temperature
                 tmax.ip85, # Annual Mean Max Temperature
                 prec.ip85, # Annual Precipitation
                 PER.ip85.sum) # PER

names(ip85bi50) <- c('tmin', 'tmax', 'prec','PER')
plot(ip85bi50, main = "IPSL-CM5A-LR RCP85")

# 2.3 Run R Script to import MIROC-ESM-CHEM climate data
# 2.3.1 Mean minimum temp

# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/MIROC-ESM-CHEM/rcp45/mi45tn50")
list.tmin.mi45 <- list.files(pattern = "\\.tif$")
tmin.mi45 <- stack(list.tmin.mi45)
tmin.mi45.th <- crop(tmin.mi45, studyarea)
tmin.mi45.mnplt <- tmin.mi45.th/10
tmin.mi45.06.09 <- subset(tmin.mi45.mnplt, 9:12)
tmin.mi45.09 <- subset(tmin.mi45.mnplt,12)
tmin.mi45.10 <- subset(tmin.mi45.mnplt,2)
tmin.mi45 <- mean(stack(tmin.mi45.09, tmin.mi45.10))
plot(tmin.mi45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/MIROC-ESM-CHEM/rcp85/mi85tn50")
list.tmin.mi85 <- list.files(pattern = "\\.tif$")
tmin.mi85 <- stack(list.tmin.mi85)
tmin.mi85.th <- crop(tmin.mi85, studyarea)
tmin.mi85.mnplt <- tmin.mi85.th/10
tmin.mi85.06.09 <- subset(tmin.mi85.mnplt, 9:12)
tmin.mi85.09 <- subset(tmin.mi85.mnplt,12)
tmin.mi85.10 <- subset(tmin.mi85.mnplt,2)
tmin.mi85 <- mean(stack(tmin.mi85.09, tmin.mi85.10))
plot(tmin.mi85)

# 2.3.2 Mean maximum temperature

# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/MIROC-ESM-CHEM/rcp45/mi45tx50")
list.tmax.mi45 <- list.files(pattern = "\\.tif$")
tmax.mi45.all <- stack(list.tmax.mi45)
tmax.mi45.th <- crop(tmax.mi45.all, studyarea)
tmax.mi45.mnplt <- tmax.mi45.th/10
tmax.mi45.06.09 <- subset(tmax.mi45.mnplt, 9:12)
tmax.mi45 <- mean(tmax.mi45.06.09)
plot(tmax.mi45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/MIROC-ESM-CHEM/rcp85/mi85tx50")
list.tmax.mi85 <- list.files(pattern = "\\.tif$")
tmax.mi85.all <- stack(list.tmax.mi85)
tmax.mi85.th <- crop(tmax.mi85.all, studyarea)
tmax.mi85.mnplt <- tmax.mi85.th/10
tmax.mi85.06.09 <- subset(tmax.mi85.mnplt, 9:12)
tmax.mi85 <- mean(tmax.mi85.06.09)
plot(tmax.mi85)

# 2.3.3 Total precipitation
# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/MIROC-ESM-CHEM/rcp45/mi45pr50")
list.mi45.prec <- list.files(pattern = "\\.tif$")
prec.mi45.all <- stack(list.mi45.prec)
prec.mi45.th <- crop(prec.mi45.all, studyarea)
prec.mi45.09 <- subset(prec.mi45.th,12)
prec.mi45.10 <- subset(prec.mi45.th,2)
prec.mi45.06.09 <- subset(prec.mi45.th, 9:12)
prec.mi45 <- sum(stack(prec.mi45.09, prec.mi45.10))
plot(prec.mi45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/MIROC-ESM-CHEM/rcp85/mi85pr50")
list.mi85.prec <- list.files(pattern = "\\.tif$")
prec.mi85.all <- stack(list.mi85.prec)
prec.mi85.th <- crop(prec.mi85.all, studyarea)
prec.mi85.09 <- subset(prec.mi85.th,12)
prec.mi85.10 <- subset(prec.mi85.th,2)
prec.mi85.06.09 <- subset(prec.mi85.th, 9:12)
prec.mi85 <- sum(stack(prec.mi85.09, prec.mi85.10))
plot(prec.mi85)

# 2.3.4 PER
# PER calculate
# RCP45
H = 15/24
tmean.mi45 <- (tmin.mi45.06.09+tmax.mi45.06.09)/2
SVP.mi45 = 6.108*exp((17.27*tmean.mi45)/(tmean.mi45+237.3))


pe.mi45 = 715.5*H*SVP.mi45*((tmean.mi45)/(tmean.mi45+273.2))
PER.mi45 = prec.mi45.06.09/pe.mi45
PER.mi45.sum <- sum(PER.mi45)
plot(PER.mi45.sum)

# RCP85
tmean.mi85 <- (tmin.mi85.06.09+tmax.mi85.06.09)/2
SVP.mi85 = 6.108*exp((17.27*tmean.mi85)/(tmean.mi85+237.3))


pe.mi85 = 715.5*H*SVP.mi85*((tmean.mi85)/(tmean.mi85+273.2))
PER.mi85 = prec.mi85.06.09/pe.mi85
PER.mi85.sum <- sum(PER.mi85)
plot(PER.mi85.sum)

# load all climate variables for the MIROC-ESM-CHEM.

# RCP45
mi45bi50 = stack(tmin.mi45, # Annual Mean Min Temperature
                 tmax.mi45, # Annual Mean Max Temperature
                 prec.mi45, # Annual Precipitation
                 PER.mi45.sum) # PER

names(mi45bi50) <- c('tmin', 'tmax', 'prec','PER')
plot(mi45bi50, main = "MIROC-ESM-CHEM RCP45")

# RCP85
mi85bi50 = stack(tmin.mi85, # Annual Mean Min Temperature
                 tmax.mi85, # Annual Mean Max Temperature
                 prec.mi85, # Annual Precipitation
                 PER.mi85.sum) # PER

names(mi85bi50) <- c('tmin', 'tmax', 'prec','PER')
plot(mi85bi50, main = "MIROC-ESM-CHEM RCP85")


# 2.4 Run R Script to import MPI-ESM-LR climate data
# 2.4.1 Mean minimum temp

# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/MPI-ESM-LR/rcp45/mp45tn50")
list.tmin.mp45 <- list.files(pattern = "\\.tif$")
tmin.mp45 <- stack(list.tmin.mp45)
tmin.mp45.th <- crop(tmin.mp45, studyarea)
tmin.mp45.mnplt <- tmin.mp45.th/10
tmin.mp45.06.09 <- subset(tmin.mp45.mnplt, 9:12)
tmin.mp45.09 <- subset(tmin.mp45.mnplt,12)
tmin.mp45.10 <- subset(tmin.mp45.mnplt,2)
tmin.mp45 <- mean(stack(tmin.mp45.09, tmin.mp45.10))
plot(tmin.mp45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/MPI-ESM-LR/rcp85/mp85tn50")
list.tmin.mp85 <- list.files(pattern = "\\.tif$")
tmin.mp85 <- stack(list.tmin.mp85)
tmin.mp85.th <- crop(tmin.mp85, studyarea)
tmin.mp85.mnplt <- tmin.mp85.th/10
tmin.mp85.06.09 <- subset(tmin.mp85.mnplt, 9:12)
tmin.mp85.09 <- subset(tmin.mp85.mnplt,12)
tmin.mp85.10 <- subset(tmin.mp85.mnplt,2)
tmin.mp85 <- mean(stack(tmin.mp85.09, tmin.mp85.10))
plot(tmin.mp85)

# 2.4.2 Mean maximum temperature

# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/MPI-ESM-LR/rcp45/mp45tx50")
list.tmax.mp45 <- list.files(pattern = "\\.tif$")
tmax.mp45.all <- stack(list.tmax.mp45)
tmax.mp45.th <- crop(tmax.mp45.all, studyarea)
tmax.mp45.mnplt <- tmax.mp45.th/10
tmax.mp45.06.09 <- subset(tmax.mp45.mnplt, 9:12)
tmax.mp45 <- mean(tmax.mp45.06.09)
plot(tmax.mp45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/MPI-ESM-LR/rcp85/mp85tx50")
list.tmax.mp85 <- list.files(pattern = "\\.tif$")
tmax.mp85.all <- stack(list.tmax.mp85)
tmax.mp85.th <- crop(tmax.mp85.all, studyarea)
tmax.mp85.mnplt <- tmax.mp85.th/10
tmax.mp85.06.09 <- subset(tmax.mp85.mnplt, 9:12)
tmax.mp85 <- mean(tmax.mp85.06.09)
plot(tmax.mp85)

# 2.4.3 Total precipitation
# RCP45
setwd("D:/Biomod2_Rice/Climate_Data/MPI-ESM-LR/rcp45/mp45pr50")
list.mp45.prec <- list.files(pattern = "\\.tif$")
prec.mp45.all <- stack(list.mp45.prec)
prec.mp45.th <- crop(prec.mp45.all, studyarea)
prec.mp45.09 <- subset(prec.mp45.th,12)
prec.mp45.10 <- subset(prec.mp45.th,2)
prec.mp45.06.09 <- subset(prec.mp45.th, 9:12)
prec.mp45 <- sum(stack(prec.mp45.09, prec.mp45.10))
plot(prec.mp45)

# RCP85
setwd("D:/Biomod2_Rice/Climate_Data/MPI-ESM-LR/rcp85/mp85pr50")
list.mp85.prec <- list.files(pattern = "\\.tif$")
prec.mp85.all <- stack(list.mp85.prec)
prec.mp85.th <- crop(prec.mp85.all, studyarea)
prec.mp85.09 <- subset(prec.mp85.th,12)
prec.mp85.10 <- subset(prec.mp85.th,2)
prec.mp85.06.09 <- subset(prec.mp85.th, 9:12)
prec.mp85 <- sum(stack(prec.mp85.09, prec.mp85.10))
plot(prec.mp85)

# 2.4.4 PER
# PER calculate
# RCP45
H = 15/24
tmean.mp45 <- (tmin.mp45.06.09+tmax.mp45.06.09)/2
SVP.mp45 = 6.108*exp((17.27*tmean.mp45)/(tmean.mp45+237.3))


pe.mp45 = 715.5*H*SVP.mp45*((tmean.mp45)/(tmean.mp45+273.2))
PER.mp45 = prec.mp45.06.09/pe.mp45
PER.mp45.sum <- sum(PER.mp45)
plot(PER.mp45.sum)

# RCP85
tmean.mp85 <- (tmin.mp85.06.09+tmax.mp85.06.09)/2
SVP.mp85 = 6.108*exp((17.27*tmean.mp85)/(tmean.mp85+237.3))


pe.mp85 = 715.5*H*SVP.mp85*((tmean.mp85)/(tmean.mp85+273.2))
PER.mp85 = prec.mp85.06.09/pe.mp85
PER.mp85.sum <- sum(PER.mp85)
plot(PER.mp85.sum)

# load all climate variables for the MIROC-ESM-CHEM.

# RCP45
mp45bi50 = stack(tmin.mp45, # Annual Mean Min Temperature
                 tmax.mp45, # Annual Mean Max Temperature
                 prec.mp45, # Annual Precipitation
                 PER.mp45.sum) # PER

names(mp45bi50) <- c('tmin', 'tmax', 'prec','PER')
plot(mp45bi50, main = "MIROC-ESM-CHEM RCP45")

# RCP85
mp85bi50 = stack(tmin.mp85, # Annual Mean Min Temperature
                 tmax.mp85, # Annual Mean Max Temperature
                 prec.mp85, # Annual Precipitation
                 PER.mp85.sum) # PER

names(mp85bi50) <- c('tmin', 'tmax', 'prec','PER')
plot(mp85bi50, main = "MIROC-ESM-CHEM RCP85")
