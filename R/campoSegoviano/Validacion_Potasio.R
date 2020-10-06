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
##################################################################
######### VALIDACIÓN DE SIMPLE KRIGING FOSFORO ####################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SK_POTASIO.tif')
names(covar.grid)[1]<-'SK_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETR","COOR_Y_ETR","SK_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$SK_POTASIO

x11()
plot(x=analiza$SK_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste del modelo sobre los datos de test\nMétodo: Simple Kriging\n(ppm de potasio)', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)

cor.test(x=analiza$SK_POTASIO, y= analiza$Potasio_pp, alternative = "two.sided", conf.level = 0.95, method = "pearson")



error <- RMSE(obs = analiza$Potasio_pp, pred = analiza$SK_POTASIO)
x11()
plot(x=analiza$SK_POTASIO, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio estimado", ylab = "Residuos",
     main="Dispersión de los residuos\nMétodo: Simple Kriging\n(ppm de potasio)")
abline(h=0, lty=2 )
abline(h=c(-1*error,error), lty=2, col='grey')
error


########Utilizando todos los datos para la validación################
# Utilizando todos los datos para la validación


coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SK_POTASIO.tif')
names(covar.grid)[1]<-'SK_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETR","COOR_Y_ETR","SK_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$SK_POTASIO

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$SK_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste ppm potasio estimado y observado', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Potasio_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio observado", ylab = "Residuos",
     main="Estudio de residuos")
r <- RMSE(obs = analiza$Potasio_pp, pred = analiza$SK_POTASIO)
abline(h=0, lty=2)
abline(h=c(-1*r,r), lty=2, col='grey')

RMSE(obs = analiza$Potasio_pp, pred = analiza$SK_POTASIO) # 63.25058

############# VALIDACION EXTERNA #############

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SK_POTASIO.tif')
names(covar.grid)[1]<-'SK_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETRS89","COOR_Y_ETRS89","SK_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$SK_POTASIO

analiza <- analiza[complete.cases(analiza),]
analiza <- analiza[analiza$RESIDUOS > -2000,]
x11()
plot(x=analiza$SK_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste ppm potasio estimado y observado', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Potasio_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )
r<-RMSE(obs = analiza$Potasio_pp, pred = analiza$SK_POTASIO) # 336.2546
abline(h=c(-1*r,r), col='grey', lty=2)

################################################################
################################################################

########### VALIDACIÓN REGRESION LINEAL MULTIPLE ##############
############ Validacion datos de Test #######################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LM_POTASIO.tif')
names(covar.grid)[1]<-'LM_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETR","COOR_Y_ETR","LM_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$LM_POTASIO

x11()
plot(x=analiza$LM_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste ppm de potasio estimado y observado', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Potasio_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )
error <- RMSE(obs = analiza$Potasio_pp, pred = analiza$LM_POTASIO) #  77.8119
abline(h=c(-1*error,error), lty=2, col='grey')
error
###############################################################
############ Validacion todos los datos #######################

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LM_POTASIO.tif')
names(covar.grid)[1]<-'LM_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETR","COOR_Y_ETR","LM_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$LM_POTASIO

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$LM_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste ppm potasio estimado y observado', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Potasio_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio observado", ylab = "Residuos",
     main="Estudio de residuos")
r <- RMSE(obs = analiza$Potasio_pp, pred = analiza$LM_POTASIO)
abline(h=0, lty=2)
abline(h=c(-1*r,r), lty=2, col='grey')

RMSE(obs = analiza$Potasio_pp, pred = analiza$LM_POTASIO) # 69.62995


##############################################
############# VALIDACION EXTERNA #############


baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LM_POTASIO.tif')
names(covar.grid)[1]<-'LM_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETRS89","COOR_Y_ETRS89","LM_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$LM_POTASIO

analiza <- analiza[complete.cases(analiza),]
analiza <- analiza[analiza$RESIDUOS > -2000,]
x11()
plot(x=analiza$LM_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste ppm potasio estimado y observado', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Potasio_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )
r<-RMSE(obs = analiza$Potasio_pp, pred = analiza$LM_POTASIO) # 336.388
abline(h=c(-1*r,r), col='grey', lty=2)
r
################################################################



################## VALIDACIÓN RANDOM FOREST ##################

################# VALIDACIÓN TEST ###########################
covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/MacroNutrientes/RF_POTASIO.tif')
names(covar.grid)[1]<-'RF_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETR","COOR_Y_ETR","RF_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$RF_POTASIO

x11()
plot(x=analiza$RF_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste del modelo sobre los datos de test\nMétodo: Random Forest\n(ppm de K)', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)
cor.test(x=analiza$RF_POTASIO, y= analiza$Potasio_pp, alternative = "two.sided", conf.level = 0.95, method = "pearson")

x11()
plot(x=analiza$RF_POTASIO, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio estimado", ylab = "Residuos",
     main="Estudio de residuos sobre los datos de test\nMétodo: Random Forest\n(ppm de K)")
abline(h=0, lty=2 )
error <- RMSE(obs = analiza$Potasio_pp, pred = analiza$RF_POTASIO) #  56.10016
abline(h=c(-1*error,error), lty=2, col='grey')
error
##############################################################

############ Validacion todos los datos #######################

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/RF_POTASIO.tif')
names(covar.grid)[1]<-'RF_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETR","COOR_Y_ETR","RF_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$RF_POTASIO

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$RF_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste ppm potasio estimado y observado', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Potasio_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio observado", ylab = "Residuos",
     main="Estudio de residuos")
r <- RMSE(obs = analiza$Potasio_pp, pred = analiza$RF_POTASIO)
abline(h=0, lty=2)
abline(h=c(-1*r,r), lty=2, col='grey')

RMSE(obs = analiza$Potasio_pp, pred = analiza$RF_POTASIO) # 53.43888
##############################################################

############# VALIDACION EXTERNA #############

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/RF_POTASIO.tif')
names(covar.grid)[1]<-'RF_POTASIO'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Potasio_pp","COOR_X_ETRS89","COOR_Y_ETRS89","RF_POTASIO")]
analiza$RESIDUOS <- analiza$Potasio_pp - analiza$RF_POTASIO

analiza <- analiza[complete.cases(analiza),]
analiza <- analiza[analiza$RESIDUOS > -2000,]
x11()
plot(x=analiza$RF_POTASIO, y=analiza$Potasio_pp, col='Red',
     main='Ajuste ppm potasio estimado y observado', 
     ylab = 'ppm potasio observado', xlab = 'ppm potasio estimado')
abline(0,1, lty=2)

analiza[analiza$RESIDUOS>1500,]
summary(analiza$RESIDUOS)


x11()
plot(x=analiza$Potasio_pp, y=analiza$RESIDUOS, col = "Red",
     xlab = "ppm potasio observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )
r<-RMSE(obs = analiza$Potasio_pp, pred = analiza$RF_POTASIO) # 312.5572
abline(h=c(-1*r,r), col='grey', lty=2)
r

##############################################################


############################################################
















