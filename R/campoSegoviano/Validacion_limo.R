###################################################################
###################################################################
######### VALIDACIÓN DE SIMPLE KRIGING LIMO ####################

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
query<-"SELECT ID_MUESTRA, ARENA, LIMO, ARCILLA, COOR_X_ETR, COOR_Y_ETR, 
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

######### VALIDACIÓN DE SIMPLE KRIGING LIMO ####################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_SK.tif')
names(covar.grid)[1]<-'LIMO_DIF_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)


analiza <- soilData.test[,c("ID_MUESTRA","LIMO","COOR_X_ETR","COOR_Y_ETR","LIMO_DIF_SK")]
analiza$RESIDUOS <- analiza$LIMO - analiza$LIMO_DIF_SK

x11()
plot(x=analiza$LIMO_DIF_SK, y=analiza$LIMO, col='Red',
     main='Ajuste % de limo estimado y observado', 
     ylab = '% Limo observado', xlab = '% limo estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$LIMO, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Limo observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$LIMO, pred = analiza$LIMO_DIF_SK) # 5.25


# Utilizando todos los datos para la validación


coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_SK.tif')
names(covar.grid)[1]<-'LIMO_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","LIMO","COOR_X_ETR","COOR_Y_ETR","LIMO_SK")]
analiza$RESIDUOS <- analiza$LIMO - analiza$LIMO_SK

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$LIMO_SK, y=analiza$LIMO, col='Red',
     main='Ajuste % de limo estimado y observado', 
     ylab = '% Limo observado', xlab = '% limo estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$LIMO, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$LIMO, pred = analiza$LIMO_SK) # 5.430894


############# VALIDACION EXTERNA #############

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_SK.tif')
names(covar.grid)[1]<-'LIMO_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Limo_Porc","COOR_X_ETRS89","COOR_Y_ETRS89","LIMO_SK")]
analiza$RESIDUOS <- analiza$Limo_Porc - analiza$LIMO_SK

analiza <- analiza[complete.cases(analiza),]

x11()
plot(x=analiza$LIMO_SK, y=analiza$Limo_Porc, col='Red',
     main='Ajuste % de limo estimado y observado', 
     ylab = '% Limo observado', xlab = '% limo estimado')
abline(0,1, lty=2)

x11()
plot(x=analiza$Limo_Porc, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Limo observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$Limo_Porc, pred = analiza$LIMO_SK) # 13.09

#################################################################
#################################################################

###############################################################
########## VALIDACIÓN REGRESIÓN LINEAL MULTIPLE ###############
###############################################################

################# DATOS DE TEST ##############################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_LM.tif')
names(covar.grid)[1]<-'LIMO_LM'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)


analiza <- soilData.test[,c("ID_MUESTRA","LIMO","COOR_X_ETR","COOR_Y_ETR","LIMO_LM")]
analiza$RESIDUOS <- analiza$LIMO - analiza$LIMO_LM

x11()
plot(x=analiza$LIMO_LM, y=analiza$LIMO, col='Red',
     main='Ajuste % de limo estimado y observado', 
     ylab = '% Limo observado', xlab = '% limo estimado')
abline(0,1)

x11()
plot(x=analiza$LIMO_LM, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos (arena)")
abline(h=0, lty=2 )

RMSE(obs = analiza$LIMO, pred = analiza$LIMO_LM) # 7.582609

########### CON TODOS LOS DATOS DE MUESTRA ##############
str(soil.data)

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_LM.tif')
names(covar.grid)[1]<-'LIMO_LM'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","LIMO","COOR_X_ETR","COOR_Y_ETR","LIMO_LM")]
analiza$RESIDUOS <- analiza$LIMO - analiza$LIMO_LM

x11()
plot(x=analiza$LIMO_LM, y=analiza$LIMO, col='Red',
     main = 'Ajuste entre % arena observado y predicho',
     xlab = '% Limo predicho', ylab = '% Limo observado')
abline(0,1, lty=2)

x11()
plot(x=analiza$LIMO_LM, y=analiza$RESIDUOS, col = "Red",
     xlab = "% limo observado", ylab = "Residuos",
     main="Estudio de residuos (limo)")
abline(h=0, lty=2 )

analiza <- analiza[complete.cases(analiza),]
RMSE(obs = analiza$LIMO, pred = analiza$LIMO_LM) # 7.582609

################# VALIDACIÓN EXTERNA ################

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_LM.tif')
names(covar.grid)[1]<-'LIMO_LM'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)



analiza <- soil.data[,c("ID_MUESTRA","Limo_Porc","COOR_X_ETRS89","COOR_Y_ETRS89","LIMO_LM")]
analiza$RESIDUOS <- analiza$Limo_Porc - analiza$LIMO_LM

analiza <- analiza[complete.cases(analiza),]

x11()
plot(x=analiza$LIMO_LM, y=analiza$Limo_Porc, col='Red',
     xlab='% Limo predicho', ylab='% Limo observado',
     main = 'Ajuste entre % limo observado y predicho')
abline(0,1, lty=2)

x11()
plot(x=analiza$Limo_Porc, y=analiza$RESIDUOS, col = "Red",
     xlab = "% limo observado", ylab = "Residuos",
     main="Estudio de residuos (limo)")
abline(h=0, lty=2 )

RMSE(obs = analiza$Limo_Porc, pred = analiza$LIMO_LM) # 23.67251

#####################################################


###############################################################
########## VALIDACIÓN RANDOM FOREST ###############
###############################################################

################# DATOS DE TEST ##############################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_RF.tif')
names(covar.grid)[1]<-'LIMO_RF'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")


soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","LIMO","COOR_X_ETR","COOR_Y_ETR","LIMO_RF")]
analiza$RESIDUOS <- analiza$LIMO - analiza$LIMO_RF

x11()
plot(x=analiza$LIMO_RF, y=analiza$LIMO, col='Red',
     xlab = '% limo predicho', ylab = '% limo observado', 
     main = 'Ajuste % limo entre datos observados y predichos \n(Random Forest)')
abline(0,1, lty=2)

x11()
plot(x=analiza$LIMO, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Limo observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$LIMO, pred = analiza$LIMO_RF) # 4.604231
nrow(soilData.test)


################# CON TODOS LOS DATOS DE LA MUESTRA ########################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_RF.tif')
names(covar.grid)[1]<-'LIMO_RF'

proj4string(covar.grid)<-CRS(etrs89)


str(soil.data)
coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")


soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","LIMO","COOR_X_ETR","COOR_Y_ETR","LIMO_RF")]
analiza$RESIDUOS <- analiza$LIMO - analiza$LIMO_RF

x11()
plot(x=analiza$LIMO_RF, y=analiza$LIMO, col='Red',
     xlab = '% limo predicho', ylab = '% limo observado', 
     main = 'Ajuste % limo entre datos observados y predichos \n(Random Forest)')
abline(0,1, lty=2)

x11()
plot(x=analiza$LIMO, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Limo observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

analiza <- analiza[complete.cases(analiza),]
RMSE(obs = analiza$LIMO, pred = analiza$LIMO_RF) # 4.81513

################# VALIDACION EXTERNA ########################
baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_RF.tif')
names(covar.grid)[1]<-'LIMO_RF'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Limo_Porc","COOR_X_ETRS89","COOR_Y_ETRS89","LIMO_RF")]
analiza$RESIDUOS <- analiza$Limo_Porc - analiza$LIMO_RF

analiza <- analiza[complete.cases(analiza),]

x11()
plot(x=analiza$LIMO_RF, y=analiza$Limo_Porc, col='Red',
     xlab='% Limo predicho', ylab='% Limo observado',
     main = 'Ajuste entre % limo observado y predicho \n(muestras base de datos de suelo)')
abline(0,1, lty=2)

x11()
plot(x=analiza$Limo_Porc, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Limo observado", ylab = "Residuos",
     main="Estudio de residuos (limo)")
abline(h=0, lty=2 )

RMSE(obs = analiza$Limo_Porc, pred = analiza$LIMO_RF) # 12.82383


