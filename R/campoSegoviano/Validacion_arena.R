###################################################################
###################################################################
######### VALIDACIÓN DE SIMPLE KRIGING ARENA ####################

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
soil.data <- soil.data[soil.data$ID_MUESTRA != 244,]
soil.data <- soil.data[soil.data$ID_MUESTRA != 142,]

inTrain <- createDataPartition(y = soil.data$ARENA, p = 0.80, list = FALSE)
soilData <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

coordinates(soilData.test)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData.test)<-CRS(etrs89)

nrow(soilData.test)

######### VALIDACIÓN DE SIMPLE KRIGING ARENA ####################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/Textura/ARENA_SK.tif')
names(covar.grid)[1]<-'ARENA_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)


analiza <- soilData.test[,c("ID_MUESTRA","ARENA","COOR_X_ETR","COOR_Y_ETR","ARENA_SK")]
analiza$RESIDUOS <- analiza$ARENA - analiza$ARENA_SK


x11()
plot(x=analiza$ARENA_SK, y=analiza$ARENA, col='red', 
     main = 'Ajuste del modelo sobre los datos de test\nMétodo: Simple Kriging\n(% arena)',
     xlab='% de arena estimado', ylab ='% de arena observado')
abline(0,1, lty=2)

cor.test(x=analiza$ARENA_SK, y= analiza$ARENA, alternative = "two.sided", conf.level = 0.95, method = "pearson")



r<-RMSE(obs = analiza$ARENA, pred = analiza$ARENA_SK) # 6.01654
x11()
plot(x=analiza$ARENA_SK, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena estimado", ylab = "Residuos",
     main="Estudio de residuos\nMétodo: Simple Kriging\n(% arena)")
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)



x11()
plot(x=analiza$ARENA_SK, y=analiza$ARENA)
abline(0,1)

x11()
plot(x=analiza$ARENA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$ARENA, pred = analiza$ARENA_SK) # 6.01654


# Utilizando todos los datos para la validación


coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/ARENA_SK.tif')
names(covar.grid)[1]<-'ARENA_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","ARENA","COOR_X_ETR","COOR_Y_ETR","ARENA_SK")]
analiza$RESIDUOS <- analiza$ARENA - analiza$ARENA_SK

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$ARENA_SK, y=analiza$ARENA)
abline(0,1)

x11()
plot(x=analiza$ARENA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$ARENA, pred = analiza$ARENA_SK) # 5.430894

############# VALIDACION EXTERNA #############

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/ARENA_SK.tif')
names(covar.grid)[1]<-'ARENA_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Arena_Porc","COOR_X_ETRS89","COOR_Y_ETRS89","ARENA_SK")]
analiza$RESIDUOS <- analiza$Arena_Porc - analiza$ARENA_SK

analiza <- analiza[complete.cases(analiza),]

x11()
plot(x=analiza$ARENA_SK, y=analiza$Arena_Porc)
abline(0,1)

x11()
plot(x=analiza$Arena_Porc, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$Arena_Porc, pred = analiza$ARENA_SK) # 11.63213

#################################################################
#################################################################

###############################################################
########## VALIDACIÓN REGRESIÓN LINEAL MULTIPLE ###############
###############################################################

################# DATOS DE TEST ##############################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARENA_LM.tif')
names(covar.grid)[1]<-'SG_ARENA_LM'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)


analiza <- soilData.test[,c("ID_MUESTRA","ARENA","COOR_X_ETR","COOR_Y_ETR","SG_ARENA_LM")]
analiza$RESIDUOS <- analiza$ARENA - analiza$SG_ARENA_LM

x11()
plot(x=analiza$SG_ARENA_LM, y=analiza$ARENA, col='red', 
     main = 'Ajuste del modelo sobre los datos de test\nMétodo: Regresión Lineal Múltiple\n(% arena)',
     xlab='% de arena estimado', ylab ='% de arena observado')
abline(0,1, lty=2)
cor.test(x=analiza$SG_ARENA_LM, y= analiza$ARENA, alternative = "two.sided", conf.level = 0.95, method = "pearson")

er <- RMSE(obs = analiza$ARENA, pred = analiza$SG_ARENA_LM) # 7.582609
x11()
plot(x=analiza$ARENA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% arena observado", ylab = "Residuos",
     main="Estudio de residuos\nMétodo: Regresión Lineal Múltiple\n(% arena)")
abline(h=0, lty=2 )
abline(h=c(-1*er,er), lty=2, col='grey')

RMSE(obs = analiza$ARENA, pred = analiza$SG_ARENA_LM) # 7.582609

########### CON TODOS LOS DATOS DE MUESTRA ##############
str(soil.data)

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARENA_LM.tif')
names(covar.grid)[1]<-'SG_ARENA_LM'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","ARENA","COOR_X_ETR","COOR_Y_ETR","SG_ARENA_LM")]
analiza$RESIDUOS <- analiza$ARENA - analiza$SG_ARENA_LM

x11()
plot(x=analiza$SG_ARENA_LM, y=analiza$ARENA, col='Red',
     main = 'Ajuste entre % arena observado y predicho',
     xlab = '% Arena predicho', ylab = '% Arena observado')
abline(0,1, lty=2)

x11()
plot(x=analiza$ARENA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos (arena)")
abline(h=0, lty=2 )

analiza <- analiza[complete.cases(analiza),]
RMSE(obs = analiza$ARENA, pred = analiza$SG_ARENA_LM) # 7.582609

################# VALIDACIÓN EXTERNA ################

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)



covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARENA_LM.tif')
names(covar.grid)[1]<-'SG_ARENA_LM'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)



analiza <- soil.data[,c("ID_MUESTRA","Arena_Porc","COOR_X_ETRS89","COOR_Y_ETRS89","SG_ARENA_LM")]
analiza$RESIDUOS <- analiza$Arena_Porc - analiza$SG_ARENA_LM

analiza <- analiza[complete.cases(analiza),]

x11()
plot(x=analiza$SG_ARENA_LM, y=analiza$Arena_Porc, col='Red',
     xlab='% Arena predicho', ylab='% Arena observado',
     main = 'Ajuste entre % arena observado y predicho')
abline(0,1)

x11()
plot(x=analiza$Arena_Porc, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos (arena)")
abline(h=0, lty=2 )

RMSE(obs = analiza$Arena_Porc, pred = analiza$SG_ARENA_LM) # 23.67251

#####################################################


###############################################################
########## VALIDACIÓN RANDOM FOREST ###############
###############################################################

################# DATOS DE TEST ##############################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARENA_RF_COOR.tif')
names(covar.grid)[1]<-'SG_ARENA_RF_COOR'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")


soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)

analiza <- soilData.test[,c("ID_MUESTRA","ARENA","COOR_X_ETR","COOR_Y_ETR","SG_ARENA_RF_COOR")]
analiza$RESIDUOS <- analiza$ARENA - analiza$SG_ARENA_RF_COOR


x11()
plot(x=analiza$SG_ARENA_RF_COOR, y=analiza$ARENA, col='Red',
     xlab = '% arena predicho', ylab = '% arena observado', 
     main = 'Ajuste del modelo sobre los datos de test\nMétodo: Random Forest\n(% arena)')
abline(0,1, lty=2)

cor.test(x = analiza$SG_ARENA_RF_COOR, y = analiza$ARENA, alternative = "two.sided", conf.level = 0.95, method = "pearson")
er <- RMSE(obs = analiza$ARENA, pred = analiza$SG_ARENA_RF_COOR) # 6.01654

x11()
plot(x=analiza$SG_ARENA_RF_COOR, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena estimado", ylab = "Residuos",
     main="Estudio de residuos\nMétodo: Random Forest\n(% arena)")
abline(h=0, lty=2 )
abline(h=c(-1*er,er), lty=2, col='grey')


nrow(soilData.test)


################# CON TODOS LOS DATOS DE LA MUESTRA ########################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARENA_RF_COOR.tif')
names(covar.grid)[1]<-'SG_ARENA_RF_COOR'

proj4string(covar.grid)<-CRS(etrs89)


str(soil.data)
coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")


soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","ARENA","COOR_X_ETR","COOR_Y_ETR","SG_ARENA_RF_COOR")]
analiza$RESIDUOS <- analiza$ARENA - analiza$SG_ARENA_RF_COOR

x11()
plot(x=analiza$SG_ARENA_RF_COOR, y=analiza$ARENA, col='Red',
     xlab = '% arena predicho', ylab = '% arena observado', 
     main = 'Ajuste % arena entre datos observados y predichos \n(Random Forest)')
abline(0,1, lty=2)

x11()
plot(x=analiza$ARENA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

analiza <- analiza[complete.cases(analiza),]
RMSE(obs = analiza$ARENA, pred = analiza$SG_ARENA_RF_COOR) # 5.91

################# VALIDACION EXTERNA ########################
baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARENA_RF_COOR.tif')
names(covar.grid)[1]<-'SG_ARENA_RF_COOR'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Arena_Porc","COOR_X_ETRS89","COOR_Y_ETRS89","SG_ARENA_RF_COOR")]
analiza$RESIDUOS <- analiza$Arena_Porc - analiza$SG_ARENA_RF_COOR

analiza <- analiza[complete.cases(analiza),]

x11()
plot(x=analiza$SG_ARENA_RF_COOR, y=analiza$Arena_Porc, col='Red',
     xlab='% Arena predicho', ylab='% Arena observado',
     main = 'Ajuste entre % arena observado y predicho \n(muestras base de datos de suelo)')
abline(0,1, lty=2)

x11()
plot(x=analiza$Arena_Porc, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arena observado", ylab = "Residuos",
     main="Estudio de residuos (arena)")
abline(h=0, lty=2 )

RMSE(obs = analiza$Arena_Porc, pred = analiza$SG_ARENA_RF_COOR) # 21.7983





