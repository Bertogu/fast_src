###################################################################
###################################################################
######### VALIDACIÓN DE SIMPLE KRIGING ARCILLA ####################

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
soil.data <- soil.data[soil.data$ID_MUESTRA != 244,] #tiene una suma de textura mayor que 100.10
soil.data <- soil.data[soil.data$ARCILLA >= 2,]


inTrain <- createDataPartition(y = soil.data$ARCILLA, p = 0.80, list = FALSE)
soilData <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

coordinates(soilData.test)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData.test)<-CRS(etrs89)

nrow(soilData.test)

######### VALIDACIÓN DE SIMPLE KRIGING ARCILLA ####################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARCILLA_SK.tif')
names(covar.grid)[1]<-'SG_ARCILLA_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)


analiza <- soilData.test[,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_SK")]
analiza$RESIDUOS <- analiza$ARCILLA - analiza$SG_ARCILLA_SK

x11()
plot(x=analiza$SG_ARCILLA_SK, y=analiza$ARCILLA, col='red', 
     main = 'Ajuste del modelo Simple Kriging con los datos de test.\n(% Arilla)',
     xlab='% Arcilla estimado', ylab ='% de Arcilla observado')
abline(0,1, lty=2)

cor.test(x=analiza$SG_ARCILLA_SK, y= analiza$ARCILLA, alternative = "two.sided", conf.level = 0.95, method = "pearson")



r<-RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_SK) # 6.01654
x11()
plot(x=analiza$SG_ARCILLA_SK, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla estimados", ylab = "Residuos",
     main="Estudio de residuos (% Arcilla)\nMétodo: Simple Krigiging")
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)

RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_SK) # 6.01654


# Utilizando todos los datos para la validación


coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARCILLA_SK.tif')
names(covar.grid)[1]<-'SG_ARCILLA_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_SK")]
analiza$RESIDUOS <- analiza$ARCILLA - analiza$SG_ARCILLA_SK

analiza = analiza[complete.cases(analiza),]


x11()
plot(x=analiza$SG_ARCILLA_SK, y=analiza$ARCILLA)
abline(0,1)

x11()
plot(x=analiza$ARCILLA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_SK) # 6.01654
analiza[209,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_SK")]

##############################
## VALIDACION EXTERNA
##############################

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARCILLA_SK.tif')
names(covar.grid)[1]<-'SG_ARCILLA_SK'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Arcilla_Po","COOR_X_ETRS89","COOR_Y_ETRS89","SG_ARCILLA_SK")]
analiza$RESIDUOS <- analiza$Arcilla_Po - analiza$SG_ARCILLA_SK

analiza = analiza[complete.cases(analiza),]

x11()
plot(x=analiza$SG_ARCILLA_SK, y=analiza$Arcilla_Po)
abline(0,1)

x11()
plot(x=analiza$Arcilla_Po, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$Arcilla_Po, pred = analiza$SG_ARCILLA_SK) # 11.63213

analiza[209,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_SK")]


#############################################################################
######## VALIDACIÓN ARCILLA REGRESIÓN LINEAL MULTIPLE CON COORDENADAS #######
#############################################################################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/Textura/ARCILLA_LM_COOR.tif')
names(covar.grid)[1]<-'SG_ARCILLA_LM_COOR'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)


analiza <- soilData.test[,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_LM_COOR")]
analiza$RESIDUOS <- analiza$ARCILLA - analiza$SG_ARCILLA_LM_COOR

x11()
plot(x=analiza$SG_ARCILLA_LM_COOR, y=analiza$ARCILLA, col='red',
     main='Ajuste del modelo sobre los datos de test\nMétodo: regresión lineal múltiple\n(% Arcilla)',
     xlab = '% Arcilla estimado', ylab = '% Arcilla observado')
abline(0,1, lty=2)
cor.test(x=analiza$SG_ARCILLA_LM_COOR, y= analiza$ARCILLA, alternative = "two.sided", conf.level = 0.95, method = "pearson")


x11()
plot(x=analiza$ARCILLA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla observado", ylab = "Residuos",
     main="Estudio de residuos sobre los datos de test\nMétodo: regresion lineal mulitiple\n(% arcilla)")
er <- RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_LM_COOR)
abline(h=0, lty=2 )
abline(h=c(-1*er,er), lty=2, col='grey')
er


nrow(analiza)
# Utilizando todos los datos para la validación


coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARCILLA_LM_COOR.tif')
names(covar.grid)[1]<-'SG_ARCILLA_LM_COOR'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_LM_COOR")]
analiza$RESIDUOS <- analiza$ARCILLA - analiza$SG_ARCILLA_LM_COOR

analiza = analiza[complete.cases(analiza),]


x11()
plot(x=analiza$SG_ARCILLA_LM_COOR, y=analiza$ARCILLA, 
     col='Red', xlab='% Arcilla predicho', ylab='% Arcilla observado', 
     main = 'Ajuste entre % Arcilla observado y predicho')
abline(0,1, lty=2)

x11()
plot(x=analiza$ARCILLA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla observado", ylab = "Residuos",
     main="Estudio de residuos \n(regresion lineal mulitiple con coordenadas)")
abline(h=0, lty=2 )

RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_LM_COOR) 
analiza[209,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_SK")]


##############################
## VALIDACION EXTERNA
##############################

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/Textura/ARCILLA_LM_COOR.tif')
names(covar.grid)[1]<-'SG_ARCILLA_LM_COOR'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Arcilla_Po","COOR_X_ETRS89","COOR_Y_ETRS89","SG_ARCILLA_LM_COOR")]
analiza$RESIDUOS <- analiza$Arcilla_Po - analiza$SG_ARCILLA_LM_COOR

analiza = analiza[complete.cases(analiza),]

x11()
plot(x=analiza$SG_ARCILLA_LM_COOR, y=analiza$Arcilla_Po,
     col='Red', xlab='% Arcilla predicho', ylab='% Arcilla observado', 
     main = 'Ajuste entre % Arcilla observado y predicho')
abline(0,1)

x11()
plot(x=analiza$Arcilla_Po, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$Arcilla_Po, pred = analiza$SG_ARCILLA_LM_COOR) # 14.01771






#############################################################################
######## VALIDACIÓN ARCILLA RANDOM FOREST #######
#############################################################################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/Textura/ARCILLA_RF.tif')
names(covar.grid)[1]<-'SG_ARCILLA_RF'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)
names(soilData.test)


analiza <- soilData.test[,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_RF")]
analiza$RESIDUOS <- analiza$ARCILLA - analiza$SG_ARCILLA_RF



x11()
plot(x=analiza$SG_ARCILLA_RF, y=analiza$ARCILLA, col='red',
     main='Ajuste del modelo sobre los datos de test\nMétodo: Random Forest\n(% arcilla)', 
     xlab = '% Arcilla estimado', ylab = '% Arcilla observado')
abline(0,1, lty=2)

cor.test(x=analiza$SG_ARCILLA_RF, y= analiza$ARCILLA, alternative = "two.sided", conf.level = 0.95, method = "pearson")



x11()
plot(x=analiza$SG_ARCILLA_RF, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla estimado", ylab = "Residuos",
     main="Estudio de residuos sobre los datos de test\nMétodo: Random forest\n (% arcilla)")
abline(h=0, lty=2 )
er<-RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_RF) # 5.953924
abline(h=c(-1*er,er), lty=2, col='grey')

RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_RF) # 5.953924

nrow(analiza)
# Utilizando todos los datos para la validación


coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARCILLA_RF.tif')
names(covar.grid)[1]<-'SG_ARCILLA_RF'


proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)


analiza <- soil.data[,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_RF")]
analiza$RESIDUOS <- analiza$ARCILLA - analiza$SG_ARCILLA_RF

analiza <- analiza[complete.cases(analiza),]


x11()
plot(x=analiza$SG_ARCILLA_RF, y=analiza$ARCILLA, 
     col='Red', xlab='% Arcilla predicho', ylab='% Arcilla observado', 
     main = 'Ajuste entre % Arcilla observado y predicho')
abline(0,1, lty=2)

x11()
plot(x=analiza$ARCILLA, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla observado", ylab = "Residuos",
     main="Estudio de residuos \n(Random forest)")
abline(h=0, lty=2 )

RMSE(obs = analiza$ARCILLA, pred = analiza$SG_ARCILLA_RF) 
analiza[209,c("ID_MUESTRA","ARCILLA","COOR_X_ETR","COOR_Y_ETR","SG_ARCILLA_SK")]


##############################
## VALIDACION EXTERNA
##############################

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")

connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

nrow(soil.data)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/SG_ARCILLA_RF.tif')
names(covar.grid)[1]<-'SG_ARCILLA_RF'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)
names(soil.data)

analiza <- soil.data[,c("ID_MUESTRA","Arcilla_Po","COOR_X_ETRS89","COOR_Y_ETRS89","SG_ARCILLA_RF")]
analiza$RESIDUOS <- analiza$Arcilla_Po - analiza$SG_ARCILLA_RF

analiza <- analiza[complete.cases(analiza),]

x11()
plot(x=analiza$SG_ARCILLA_RF, y=analiza$Arcilla_Po,
     col='Red', xlab='% Arcilla predicho', ylab='% Arcilla observado', 
     main = 'Ajuste entre % Arcilla observado y predicho')
abline(0,1)

x11()
plot(x=analiza$Arcilla_Po, y=analiza$RESIDUOS, col = "Red",
     xlab = "% Arcilla observado", ylab = "Residuos",
     main="Estudio de residuos")
abline(h=0, lty=2 )

RMSE(obs = analiza$Arcilla_Po, pred = analiza$SG_ARCILLA_RF) # 12.26673

















































######### VALIDACIÓN DE LA ARENA ####################

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/ARENA_SK.tif')
names(covar.grid)[1]<-'ARENA_SK'

covar.grid$ARENA_RK<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/ARENA_RK.tif')$band1
covar.grid$ARENA_RF_COOR<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/ARENA_RF_COOR.tif')$band1

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)

analiza <- soilData.test[,c("ID_MUESTRA","ARENA","COOR_X_ETR","COOR_Y_ETR","ARENA_SK","ARENA_RK","ARENA_RF_COOR")]

x11()
plot(x=analiza$ARENA_SK, y=analiza$ARENA, main = "Arena SK")
abline(0,1)
# cor(analiza$ARENA_SK,analiza$ARENA)
RMSE(obs = analiza$ARENA,pred = analiza$ARENA_SK)

x11()
plot(x=analiza$ARENA_RK, y=analiza$ARENA, main = "Arena RK")
abline(0,1)
# cor(analiza$ARENA_RK,analiza$ARENA)
RMSE(obs = analiza$ARENA,pred = analiza$ARENA_RK)

x11()
plot(x=analiza$ARENA_RF_COOR, y=analiza$ARENA, main = "Arena RF")
abline(0,1)
# cor(analiza$ARENA_RF_COOR,analiza$ARENA)
RMSE(obs = analiza$ARENA,pred = analiza$ARENA_RF_COOR)


######### VALIDACIÓN DEL LIMO ####################

coordinates(soilData)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData)<-CRS(etrs89)

covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/LIMO_DIFERENCIA_RF.tif')
names(covar.grid)[1]<-'LIMO_DIFERENCIA_RF'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData,y=covar.grid)

soilData<-as(soilData,Class="data.frame")

soilData<-cbind(soilData,overDataCov)
rm(overDataCov)

analiza <- soilData[,c("ID_MUESTRA","LIMO","COOR_X_ETR","COOR_Y_ETR","LIMO_DIFERENCIA_RF")]

x11()
plot(x=analiza$LIMO_DIFERENCIA_RF, y=analiza$LIMO, main = "Arena SK")
abline(0,1)
# cor(analiza$ARENA_SK,analiza$ARENA)
RMSE(obs = analiza$LIMO, pred = analiza$LIMO_DIFERENCIA_RF)

x11()
plot(x=analiza$ARENA_RK, y=analiza$ARENA, main = "Arena RK")
abline(0,1)
# cor(analiza$ARENA_RK,analiza$ARENA)
RMSE(obs = analiza$ARENA,pred = analiza$ARENA_RK)

x11()
plot(x=analiza$ARENA_RF_COOR, y=analiza$ARENA, main = "Arena RF")
abline(0,1)
# cor(analiza$ARENA_RF_COOR,analiza$ARENA)
RMSE(obs = analiza$ARENA,pred = analiza$ARENA_RF_COOR)




######### VALIDACIÓN DEL FOSFORO ####################

set.seed(32323)

etrs89<-"+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)
query<-"SELECT ID_MUESTRA,Fosforo_pp, COOR_X_ETR, COOR_Y_ETR, aspect_250m, ETP, GDD, LIBREHELADAS, mde_250m, 
PMed_ABRIL, PMed_AGOSTO, PMed_ANUAL, PMed_DICIEMBRE, PMed_ENERO, PMed_FEBRERO, 
PMed_INVIERNO, PMed_JULIO, PMed_JUNIO, PMed_MARZO, PMed_MAYO, PMed_NOVIEMBRE, 
PMed_OCTUBRE, PMed_PRIMAVERA, PMed_SEPTIEMBRE, PMed_VERANO, RADIACION, Rug_250, 
slope_250m, TIERRA_ARABLE, TMed_ABRIL, TMed_AGOSTO, TMed_DICIEMBRE, TMed_ENERO, 
TMed_FEBRERO, TMed_JULIO, TMed_JUNIO, TMed_MARZO, TMed_MAYO, TMed_NOVIEMBRE, 
TMed_OCTUBRE, TMed_SEPTIEMBRE, TMMAX_ABRIL, TMMAX_AGOSTO, TMMAX_DICIEMBRE, 
TMMAX_ENERO, TMMAX_FEBRERO, TMMAX_JULIO, TMMAX_JUNIO, TMMAX_MARZO, TMMAX_MAYO, 
TMMAX_NOVIEMBRE, TMMAX_OCTUBRE, TMMAX_SEPTIEMBRE
FROM SG_SAMPLES_COVARIANTS"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

dbDisconnect(connExp)

inTrain <- createDataPartition(y = soil.data$Fosforo_pp, p = (1 - 0.09),list = FALSE)
soilData <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

coordinates(soilData)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData)<-CRS(etrs89)


covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/FOSFORO_RF_COOR.tif')
names(covar.grid)[1]<-'FOSFORO_RF_COOR'

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData,y=covar.grid)

soilData<-as(soilData,Class="data.frame")

soilData<-cbind(soilData,overDataCov)
rm(overDataCov)

names(soilData)
analiza <- soilData[,c("ID_MUESTRA","COOR_X_ETR","COOR_Y_ETR","Fosforo_pp","FOSFORO_RF_COOR")]

analiza$RESIDUOS <- soilData$Fosforo_pp - soilData$FOSFORO_RF_COOR

x11()
plot(x=analiza$RESIDUOS, y=analiza$Fosforo_pp, main = "Fosforo Random Forest")
abline(0,1)
# cor(analiza$ARENA_SK,analiza$ARENA)
RMSE(obs = analiza$Fosforo_pp, pred = analiza$FOSFORO_RF_COOR)

cor(analiza$Fosforo_pp, analiza$FOSFORO_RF_COOR)^2


covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/FOSFORO_RF_COOR.tif')
names(covar.grid)[1]<-'FOSFORO_RF_COOR'

proj4string(covar.grid)<-CRS(etrs89)

coordinates(soilData.test)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData.test)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)

names(soilData.test)
analiza <- soilData.test[,c("ID_MUESTRA","COOR_X_ETR","COOR_Y_ETR","Fosforo_pp","FOSFORO_RF_COOR")]

analiza$RESIDUOS <- soilData.test$Fosforo_pp - soilData.test$FOSFORO_RF_COOR

x11()
plot(x=analiza$RESIDUOS, y=analiza$Fosforo_pp, main = "Fosforo Random Forest")
abline(0,1)
# cor(analiza$ARENA_SK,analiza$ARENA)
RMSE(obs = analiza$Fosforo_pp, pred = analiza$FOSFORO_RF_COOR)

cor(analiza$Fosforo_pp, analiza$FOSFORO_RF_COOR)^2



######### VALIDACIÓN DEL POTASIO ####################

set.seed(32323)

etrs89<-"+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

query<- "SELECT Potasio_pp, COOR_X_ETR, COOR_Y_ETR, aspect_250m, ETP, GDD, 
LIBREHELADAS, mde_250m, PMed_ABRIL, PMed_AGOSTO, PMed_ANUAL, PMed_DICIEMBRE, 
PMed_ENERO, PMed_FEBRERO, PMed_INVIERNO, PMed_JULIO, PMed_JUNIO, PMed_MARZO, 
PMed_MAYO, PMed_NOVIEMBRE, PMed_OCTUBRE, PMed_PRIMAVERA, PMed_SEPTIEMBRE, 
PMed_VERANO, RADIACION, Rug_250, slope_250m, TIERRA_ARABLE, TMed_ABRIL, 
TMed_AGOSTO, TMed_DICIEMBRE, TMed_ENERO, TMed_FEBRERO, TMed_JULIO, TMed_JUNIO, 
TMed_MARZO, TMed_MAYO, TMed_NOVIEMBRE, TMed_OCTUBRE, TMed_SEPTIEMBRE, 
TMMAX_ABRIL, TMMAX_AGOSTO, TMMAX_DICIEMBRE, TMMAX_ENERO, TMMAX_FEBRERO, 
TMMAX_JULIO, TMMAX_JUNIO, TMMAX_MARZO, TMMAX_MAYO, TMMAX_NOVIEMBRE, 
TMMAX_OCTUBRE, TMMAX_SEPTIEMBRE FROM SG_SAMPLES_COVARIANTS"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

dbDisconnect(connExp)

inTrain <- createDataPartition(y = soil.data$Potasio_pp, p = 0.8, list = FALSE)
soilData <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

coordinates(soilData.test)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData.test)<-CRS(etrs89)


covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/K_BK_EXP1M_VAR.tif')
names(covar.grid)[1]<-'K_BK_EXP1M_VAR'
covar.grid$K_BK_EXP1M<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/K_BK_EXP1M.tif')$band1
covar.grid$K_BK_EXP_VAR<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/K_BK_EXP_VAR.tif')$band1
covar.grid$K_BK_EXP<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/K_BK_EXP.tif')$band1

proj4string(covar.grid)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)

names(soilData.test)

analiza <- soilData.test[,c("COOR_X_ETR","COOR_Y_ETR","Potasio_pp",
                            "K_BK_EXP1M_VAR","K_BK_EXP1M","K_BK_EXP_VAR","K_BK_EXP")]


summary(analiza$Potasio_pp)
summary(analiza$K_BK_EXP1M_VAR)
summary(analiza$K_BK_EXP1M)
summary(analiza$K_BK_EXP_VAR)
summary(analiza$K_BK_EXP)

RMSE(obs = analiza$Potasio_pp, pred = analiza$K_BK_EXP1M_VAR)
RMSE(obs = analiza$Potasio_pp, pred = analiza$K_BK_EXP1M)
RMSE(obs = analiza$Potasio_pp, pred = analiza$K_BK_EXP_VAR)
RMSE(obs = analiza$Potasio_pp, pred = analiza$K_BK_EXP)


analiza$RESIDUOS <- analiza$Potasio_pp - analiza$POTASIO_KR
x11()
hist(analiza$RESIDUOS)

x11()
plot(x=analiza$RESIDUOS, y=analiza$Potasio_pp, main = "Potasio kriging",
     xlim = c(min(analiza$RESIDUOS),max(analiza$RESIDUOS)),
     ylim = c(0, max(analiza$Potasio_pp)))
abline(0,1)
# cor(analiza$ARENA_SK,analiza$ARENA)
RMSE(obs = analiza$Potasio_pp, pred = analiza$POTASIO_KR)

x11()
plot(x=analiza$POTASIO_KR, y=analiza$RESIDUOS)
abline(h=0, lty = 2)



cor(analiza$Fosforo_pp, analiza$FOSFORO_RF_COOR)^2


covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/FOSFORO_RF_COOR.tif')
names(covar.grid)[1]<-'FOSFORO_RF_COOR'

proj4string(covar.grid)<-CRS(etrs89)

coordinates(soilData.test)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soilData.test)<-CRS(etrs89)

overDataCov<-over(x=soilData.test,y=covar.grid)

soilData.test<-as(soilData.test,Class="data.frame")

soilData.test<-cbind(soilData.test,overDataCov)
rm(overDataCov)

names(soilData.test)
analiza <- soilData.test[,c("ID_MUESTRA","COOR_X_ETR","COOR_Y_ETR","Fosforo_pp","FOSFORO_RF_COOR")]

analiza$RESIDUOS <- soilData.test$Fosforo_pp - soilData.test$FOSFORO_RF_COOR

x11()
plot(x=analiza$RESIDUOS, y=analiza$Fosforo_pp, main = "Fosforo Random Forest")
abline(0,1)
# cor(analiza$ARENA_SK,analiza$ARENA)
RMSE(obs = analiza$Fosforo_pp, pred = analiza$FOSFORO_RF_COOR)

cor(analiza$Fosforo_pp, analiza$FOSFORO_RF_COOR)^2







baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
sqlite=dbDriver("SQLite")


connExp <- dbConnect(sqlite, dbname = baseDeDatos, loadable.extensions=TRUE)
query<-"SELECT ID_MUESTRA, Arena_Porc, Limo_Porc, Arcilla_Po, Fosforo_pp, Potasio_pp, COOR_X_ETRS89, COOR_Y_ETRS89 FROM SuelosCyL_SG_from_CyL"


soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

dbDisconnect(connExp)


covar.grid<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/ARCILLA_RF_COOR.tif')
names(covar.grid)[1]<-'ARCILLA_RF_COOR'
covar.grid$ARENA_RF_COOR<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/ARENA_RF_COOR.tif')$band1
covar.grid$FOSFORO_RF_COOR<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/FOSFORO_RF_COOR.tif')$band1
covar.grid$K_BK_EXP_VAR<-readGDAL('D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/Out/K_BK_EXP_VAR.tif')$band1


proj4string(covar.grid)<-CRS(etrs89)
names(soil.data)
coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)

overDataCov<-over(x=soil.data,y=covar.grid)

soil.data<-as(soil.data,Class="data.frame")

soil.data<-cbind(soil.data,overDataCov)
rm(overDataCov)


soil.data[,][soil.data[,] == -9999.000] <- NA
soil.data = soil.data[complete.cases(soil.data),]

RMSE(obs = soil.data$Arena_Porc, pred = soil.data$ARENA_RF_COOR)
RMSE(obs = soil.data$Arcilla_Po, pred = soil.data$ARCILLA_RF_COOR)
RMSE(obs = soil.data$Fosforo_pp, pred = soil.data$FOSFORO_RF_COOR)
RMSE(obs = soil.data$Potasio_pp, pred = soil.data$K_BK_EXP_VAR)

soil.data$RESI_ARENA <- soil.data$Arena_Porc-soil.data$ARENA_RF_COOR
soil.data$RESI_ARCILLA <- soil.data$Arcilla_Po-soil.data$ARCILLA_RF_COOR
soil.data$RESI_FOSFORO <- soil.data$Fosforo_pp-soil.data$FOSFORO_RF_COOR
soil.data$RESI_POTASIO <- soil.data$Potasio_pp-soil.data$K_BK_EXP_VAR

x11()
plot(x=soil.data$Arena_Porc, y=soil.data$RESI_ARENA)
abline(h=0, lty=2)

x11()
plot(x=soil.data$Arcilla_Po, y=soil.data$RESI_ARCILLA)
abline(h=0, lty=2)


x11()
plot(x=soil.data$Fosforo_pp, y=soil.data$RESI_FOSFORO)
abline(h=0, lty=2)


x11()
plot(x=soil.data$Fosforo_pp, y=soil.data$FOSFORO_RF_COOR)
abline(0,1, lty=2)









x11()
plot(x=soil.data$Potasio_pp, y=soil.data$RESI_POTASIO)
abline(h=0, lty=2)







x11()
hist(soil.data$Arena_Porc)

































