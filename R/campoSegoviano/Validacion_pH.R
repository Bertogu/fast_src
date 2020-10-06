###################################################################
###################################################################


rm(list=ls()) # Clean the workspace
gc()
setwd('D://Textura_CyL//CAMPO_SEGOVIANO//src//R')

library(sqldf)
library(rgdal)
library(gdalUtils)
library(sp)
library(maptools)
library(caret)
library(sp)
library(gstat)
library(plyr)
library(ggplot2)
library(randomForest)

set.seed(32323)

etrs89<-"+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)
query<-"SELECT ID_MUESTRA, ARENA, LIMO, ARCILLA, PH,CE,MO_Porc,
N_Porc,Fosforo_pp,Potasio_pp,Magnesio_p,Calcio_ppm,Sodio_ppm,Carbonatos,
CalizaActi,COOR_X_ETR, COOR_Y_ETR, 
aspect_250m, ETP, GDD, LIBREHELADAS, mde_250m, PMed_ABRIL, PMed_AGOSTO, 
PMed_ANUAL, PMed_DICIEMBRE, PMed_ENERO, PMed_FEBRERO, PMed_INVIERNO, 
PMed_JULIO, PMed_JUNIO, PMed_MARZO, PMed_MAYO, PMed_NOVIEMBRE, PMed_OCTUBRE, 
PMed_PRIMAVERA, PMed_SEPTIEMBRE, PMed_VERANO, RADIACION, Rug_250, slope_250m, 
TIERRA_ARABLE, TMed_ABRIL, TMed_AGOSTO, TMed_DICIEMBRE, TMed_ENERO, 
TMed_FEBRERO, TMed_JULIO, TMed_JUNIO, TMed_MARZO, TMed_MAYO, TMed_NOVIEMBRE, 
TMed_OCTUBRE, TMed_SEPTIEMBRE, TMMAX_ABRIL, TMMAX_AGOSTO, TMMAX_DICIEMBRE, 
TMMAX_ENERO, TMMAX_FEBRERO, TMMAX_JULIO, TMMAX_JUNIO, TMMAX_MARZO, TMMAX_MAYO, 
TMMAX_NOVIEMBRE, TMMAX_OCTUBRE, TMMAX_SEPTIEMBRE FROM SG_SAMPLES_COVARIANTS"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))
dbDisconnect(connExp)
nrow(soil.data)

# Elimino las muestras que son anormales
# soil.data <- soil.data[soil.data$ID_MUESTRA != 244,]
# soil.data <- soil.data[soil.data$ID_MUESTRA != 142,]

inTrain <- createDataPartition(y = soil.data$PH, p = 0.80, list = FALSE)
soilData <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

coordinates(soilData.test)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData.test)<-CRS(etrs89)

nrow(soilData.test)

######### VALIDACIÓN DE SIMPLE REGRESIÓN MÚLTIPLE ####################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_LM_pH.tif')
names(covar.grid)[1]<-'SG_LM_pH'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","PH","COOR_X_ETR","COOR_Y_ETR","SG_LM_pH")]
analiza$RESIDUOS <- analiza$PH - analiza$SG_LM_pH

x11()
plot(x=analiza$SG_LM_pH, y=analiza$PH, col='Red',
     main='Ajuste pH estimado y observado', 
     ylab = 'pH observado', xlab = 'pH estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$PH, y=analiza$RESIDUOS, col = "Red",
     xlab = "pH observado", ylab = "Residuos",
     main="Estudio de residuos")
r <- RMSE(obs = analiza$PH, pred = analiza$SG_LM_pH)
abline(h=c(-1*r,r), col='grey', lty=2)
abline(h=0, lty=2 )

RMSE(obs = analiza$PH, pred = analiza$SG_LM_pH) # 0.9788807


# Utilizando todos los datos para la validación


coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_LM_pH.tif')
names(covar.grid)[1]<-'SG_LM_pH'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","PH","COOR_X_ETR","COOR_Y_ETR","SG_LM_pH")]
analiza$RESIDUOS <- analiza$PH - analiza$SG_LM_pH

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$SG_LM_pH, y=analiza$PH, col='Red',
     main='Ajuste pH estimado y observado', 
     ylab = 'pH observado', xlab = 'pH estimado')
abline(0,1, lty=2)


x11()
plot(x=analiza$PH, y=analiza$RESIDUOS, col = "Red",
     xlab = "pH observado", ylab = "Residuos",
     main="Estudio de residuos")
r <- RMSE(obs = analiza$PH, pred = analiza$SG_LM_pH)
abline(h=c(-1*r,r), col='grey', lty=2)
abline(h=0, lty=2 )

RMSE(obs = analiza$PH, pred = analiza$SG_LM_pH) #  0.8481859


############# VALIDACION EXTERNA #############

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, pH, Arcilla_Po, Fosforo_pp, Potasio_pp, 
COOR_X_ETR, COOR_Y_ETR 
FROM Extraccion_SuelosCyL_Etrs89_H30_2
WHERE ID_MUESTRA IN (
SELECT ID_MUESTRA
FROM SuelosCyL_SG_from_CyL)"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_LM_pH.tif')
names(covar.grid)[1]<-'SG_LM_pH'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","pH","COOR_X_ETR","COOR_Y_ETR","SG_LM_pH")]
analiza$RESIDUOS <- analiza$pH - analiza$SG_LM_pH

analiza <- analiza[complete.cases(analiza),]

x11()
plot(x=analiza$SG_LM_pH, y=analiza$pH, col='Red',
     main='Ajuste pH estimado y observado', 
     ylab = 'pH observado', xlab = 'pH estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$pH, y=analiza$RESIDUOS, col = "Red",
     xlab = "pH observado", ylab = "Residuos",
     main="Estudio de residuos")
r <- RMSE(obs = analiza$pH, pred = analiza$SG_LM_pH)
abline(h=0, lty=2 )
abline(h=c(-1*r,r), lty=2 , col='grey')

RMSE(obs = analiza$pH, pred = analiza$SG_LM_pH) # 1.25771

################################################################

###############################################################
########## VALIDACIÓN RANDOM FOREST ###############
###############################################################

################# DATOS DE TEST ##############################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_RF_pH.tif')
names(covar.grid)[1]<-'RF_pH'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")


soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","PH","COOR_X_ETR","COOR_Y_ETR","RF_pH")]
analiza$RESIDUOS <- analiza$PH - analiza$RF_pH

x11()
plot(x=analiza$RF_pH, y=analiza$PH, col='Red',
     xlab = 'pH predicho', ylab = 'pH observado', 
     main = 'Ajuste del modelo sobre los datos de test\nMétodo: Random Forest\n(pH)')
abline(0,1, lty=2)
cor.test(x=analiza$RF_pH, y= analiza$PH, alternative = "two.sided", conf.level = 0.95, method = "pearson")

x11()
plot(x=analiza$RF_pH, y=analiza$RESIDUOS, col = "Red",
     xlab = "pH estimado", ylab = "Residuos",
     main="Estudio de residuos sobre los datos de test\nMétodo: Random Forest\n(pH)")
r <- RMSE(obs = analiza$PH, pred = analiza$RF_pH) # 1.00521
abline(h=0, lty=2 )
abline(h=c(-1*r,r), col='grey', lty=2)
r

nrow(soilData.test)


################# CON TODOS LOS DATOS DE LA MUESTRA ########################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/RF_pH.tif')
names(covar.grid)[1]<-'RF_pH'

proj4string(covar.grid)<-CRS(etrs89)

str(soil.data)
coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","PH","COOR_X_ETR","COOR_Y_ETR","RF_pH")]
analiza$RESIDUOS <- analiza$PH - analiza$RF_pH

x11()
plot(x=analiza$RF_pH, y=analiza$PH, col='Red',
     xlab = 'pH predicho', ylab = 'pH observado', 
     main = 'Ajuste pH entre datos observados y predichos \n(Random Forest)')
abline(0,1, lty=2)

x11()
plot(x=analiza$PH, y=analiza$RESIDUOS, col = "Red",
     xlab = "pH observado", ylab = "Residuos",
     main="Estudio de residuos")
r <- RMSE(obs = analiza$PH, pred = analiza$RF_pH) 
abline(h=0, lty=2 )
abline(h=c(-1*r,r),col='grey', lty=2)
r # 0.6476836

analiza <- analiza[complete.cases(analiza),]


################# VALIDACION EXTERNA ########################
baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)

query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, pH, Arcilla_Po, Fosforo_pp, Potasio_pp, 
COOR_X_ETR, COOR_Y_ETR 
FROM Extraccion_SuelosCyL_Etrs89_H30_2
WHERE ID_MUESTRA IN (
  SELECT ID_MUESTRA
  FROM SuelosCyL_SG_from_CyL)"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/RF_pH.tif')
names(covar.grid)[1]<-'RF_pH'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","pH","COOR_X_ETR","COOR_Y_ETR","RF_pH")]
analiza$RESIDUOS <- analiza$pH - analiza$RF_pH

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$RF_pH, y=analiza$pH, col='Red',
     xlab='pH predicho', ylab='pH observado',
     main = 'Ajuste entre pH observado y predicho \n(muestras base de datos de suelo)')
abline(0,1, lty=2)

x11()
plot(x=analiza$pH, y=analiza$RESIDUOS, col = "Red",
     xlab = "pH observado", ylab = "Residuos",
     main="Estudio de residuos (pH)")
r<-RMSE(obs = analiza$pH, pred = analiza$RF_pH)
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)
r
r<-RMSE(obs = analiza$pH, pred = analiza$RF_pH) # 0.8128874




