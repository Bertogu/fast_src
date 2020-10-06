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

inTrain <- createDataPartition(y = soil.data$LIMO, p = 0.80, list = FALSE)
soilData <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

coordinates(soilData.test)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData.test)<-CRS(etrs89)

nrow(soilData.test)

######### VALIDACIÓN DE SIMPLE KRIGING FOSFORO ####################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/MacroNutrientes/FOSFORO_SK.tif')
names(covar.grid)[1]<-'FOSFORO_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","Fosforo_pp","COOR_X_ETR","COOR_Y_ETR","FOSFORO_SK")]
analiza$RESIDUOS <- analiza$Fosforo_pp - analiza$FOSFORO_SK

x11()
plot(x=analiza$FOSFORO_SK, y=analiza$Fosforo_pp, col='Red',
     main='Ajuste del modelo sobre los datos de test\nMétodo: Simple Kriging\n(ppm de P)', 
     ylab = 'ppm fosforo observado', xlab = 'ppm limo estimado')
abline(0,1, lty=2)

cor.test(x=analiza$FOSFORO_SK, y= analiza$Fosforo_pp, alternative = "two.sided", conf.level = 0.95, method = "pearson")



er<-RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_SK)
x11()
plot(x=analiza$FOSFORO_SK, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm fósforo estimado", ylab = "Residuos",
     main="Dispersión de los residuos.\nMétodo: Simple Kriging\n(ppm de P)")
abline(h=0, lty=2 )
abline(h=c(-1*er,er), col='grey', lty=2)

RMSE(obs = analiza$Fosforo_pp, pred = analiza$FOSFORO_SK)


# Utilizando todos los datos para la validación


coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/FOSFORO_SK.tif')
names(covar.grid)[1]<-'FOSFORO_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","Fosforo_pp","COOR_X_ETR","COOR_Y_ETR","FOSFORO_SK")]
analiza$RESIDUOS <- analiza$Fosforo_pp - analiza$FOSFORO_SK

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$FOSFORO_SK, y=analiza$Fosforo_pp, col='Red',
     main='Ajuste ppm fósforo estimado y observado', 
     ylab = 'ppm fósforo observado', xlab = 'ppm fósforo estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Fosforo_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm fósforo observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$Fosforo_pp, pred = analiza$FOSFORO_SK) # 19.24985


############# VALIDACION EXTERNA #############

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/FOSFORO_SK.tif')
names(covar.grid)[1]<-'FOSFORO_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Fosforo_pp","COOR_X_ETRS89","COOR_Y_ETRS89","FOSFORO_SK")]
analiza$RESIDUOS <- analiza$Fosforo_pp - analiza$FOSFORO_SK

analiza <- analiza[complete.cases(analiza),]
analiza <- analiza[analiza$RESIDUOS > -2000,]
x11()
plot(x=analiza$FOSFORO_SK, y=analiza$Fosforo_pp, col='Red',
     main='Ajuste ppm fósforo estimado y observado', 
     ylab = 'ppm fósforo observado', xlab = 'ppm fósforo estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Fosforo_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm fósforo observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$Fosforo_pp, pred = analiza$FOSFORO_SK) # 33.33149

################################################################

###############################################################
########## VALIDACIÓN RANDOM FOREST ###############
###############################################################

################# DATOS DE TEST ##############################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/MacroNutrientes/RF_FOSFORO.tif')
names(covar.grid)[1]<-'RF_FOSFORO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")


soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","Fosforo_pp","COOR_X_ETR","COOR_Y_ETR","RF_FOSFORO")]
analiza$RESIDUOS <- analiza$Fosforo_pp - analiza$RF_FOSFORO

x11()
plot(x=analiza$RF_FOSFORO, y=analiza$Fosforo_pp, col='Red',
     xlab = 'ppm de fósforo predicho', ylab = 'ppm de fósforo observado', 
     main = 'Ajuste ppm de fósforo con los datos de test\nMétodo: Random Forest\n(ppm de P)')
abline(0,1, lty=2)
cor.test(x=analiza$RF_FOSFORO, y= analiza$Fosforo_pp, alternative = "two.sided", conf.level = 0.95, method = "pearson")

er <- RMSE(obs = analiza$Fosforo_pp, pred = analiza$RF_FOSFORO)
x11()
plot(x=analiza$RF_FOSFORO, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm de fósforo estimado", ylab = "Residuos",
     main="Estudio de residuos\nMétodo: Random Forest\n(ppm de P)")
abline(h=0, lty=2, col='Red' )
abline(h=c(-1*er,er), col='grey', lty=2)

 # 14.80144
nrow(soilData.test)





analiza[analiza$RESIDUOS>30,]







################# CON TODOS LOS DATOS DE LA MUESTRA ########################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/RF_FOSFORO.tif')
names(covar.grid)[1]<-'RF_FOSFORO'

proj4string(covar.grid)<-CRS(etrs89)

str(soil.data)
coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Fosforo_pp","COOR_X_ETR","COOR_Y_ETR","RF_FOSFORO")]
analiza$RESIDUOS <- analiza$Fosforo_pp - analiza$RF_FOSFORO

x11()
plot(x=analiza$RF_FOSFORO, y=analiza$Fosforo_pp, col='Red',
     xlab = 'ppm fósforo predicho', ylab = 'ppm fósforo observado', 
     main = 'Ajuste ppm fósforo entre datos observados y predichos \n(Random Forest)')
abline(0,1, lty=2)

x11()
plot(x=analiza$Fosforo_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm fósforo observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

analiza <- analiza[complete.cases(analiza),]
RMSE(obs = analiza$Fosforo_pp, pred = analiza$RF_FOSFORO) # 14.67949

################# VALIDACION EXTERNA ########################
baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/RF_FOSFORO.tif')
names(covar.grid)[1]<-'RF_FOSFORO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Fosforo_pp","COOR_X_ETRS89","COOR_Y_ETRS89","RF_FOSFORO")]
analiza$RESIDUOS <- analiza$Fosforo_pp - analiza$RF_FOSFORO

analiza <- analiza[complete.cases(analiza),]
analiza <- analiza[analiza$RESIDUOS > -2000,]

x11()
plot(x=analiza$RF_FOSFORO, y=analiza$Fosforo_pp, col='Red',
     xlab='ppm fósforo predicho', ylab='ppm fósforo observado',
     main = 'Ajuste entre ppm fósforo observado y predicho \n(muestras base de datos de suelo)')
abline(0,1, lty=2)

x11()
plot(x=analiza$Fosforo_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm fósforo observado", ylab = "Residuos",
     main="Estudio de residuos (limo)")
abline(h=0, lty=2 )

RMSE(obs = analiza$Fosforo_pp, pred = analiza$RF_FOSFORO) # 29.54874




