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

nrow(soil.data)



inTrain <- createDataPartition(y = soil.data$Fosforo_pp, p = 0.8,list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

nrow(soil.data)
nrow(soilData.test)


# a <- soil.data
# a <- data.frame("Fosforo_pp" = soil.data$Fosforo_pp)
# a$log_1_P <- log1p(a$Fosforo_pp)
# a$expm_1_P <- expm1(a$log_1_P)


names(soil.data)

p <- soil.data[,'Fosforo_pp']
# names(soil.data[,3:53])
covariants <- soil.data[,3:53]

rf <- randomForest(x = covariants, y=p, ntree = 3001, mtry = 16, importance = TRUE)

x11()
varImpPlot(rf, main = 'Random Forest: Importancia de las variables (ppm de P)')


x11()
plot(rf$predicted, rf$y, col='red',
     main='Ajuste del modelo con los datos de entrenamiento\nMétodo: Random Forest\n(ppm de P)',
     xlab = 'ppm de P estimado', ylab='ppm de P observado')
abline(0,1)
cor.test(x=rf$predicted, y= rf$y, alternative = "two.sided", conf.level = 0.95, method = "pearson")

rf$Residuos <- rf$y - rf$predicted
er<-RMSE(obs = rf$y, pred = rf$predicted)
x11()
plot(x = rf$predicted, y = rf$Residuos, main= "Estudio de residuos\nMétodo: Random forest\n(ppm de P)",
     col = "Red", xlab = "Valores estimados (ppm de P)", ylab = "Residuos")
abline(h=0, lty=2)
abline(h=c(-1*er,er), lty=2, col='grey')
er













cor(rf$predicted, rf$y)^2

# rf$exp1_Fosforo_pred <- expm1(rf$predicted)
# rf$exp1_Fosforo_obser <- expm1(rf$y)

RMSE(obs = rf$y, pred = rf$predicted)

summary(soil.data$Fosforo_pp)
x11()
hist(soil.data$Fosforo_pp)

x11()
hist(rf$predicted)

names(soilData.test[,3:53])
p.rf <- predict(rf, newdata = soilData.test[,3:53])
soilData.test$PREDICHO_RF <- p.rf

RMSE(obs = soilData.test$Fosforo_pp,pred = soilData.test$PREDICHO_RF)


# compruebo si hay correlación espacial en los residuos
soil.data$RESIDUOS <- rf$y - rf$predicted
names(soil.data)

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

v <- variogram(RESIDUOS~1, data = soil.data)
x11()
plot(v, col='Red', main='Fósforo (ppm) \nSemivariograma residuos Random forest',
     xlab='Distancia', ylab='Semivarianza')


# Cargo los datos con las localizaciones para la interpolación

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)
query<-"SELECT COOR_X_ETRS89, COOR_Y_ETRS89, aspect_250m, ETP, GDD, 
LIBREHELADAS, mde_250m, PMed_ABRIL, PMed_AGOSTO, PMed_ANUAL, PMed_DICIEMBRE, 
PMed_ENERO, PMed_FEBRERO, PMed_INVIERNO, PMed_JULIO, PMed_JUNIO, PMed_MARZO, 
PMed_MAYO, PMed_NOVIEMBRE, PMed_OCTUBRE, PMed_PRIMAVERA, PMed_SEPTIEMBRE, 
PMed_VERANO, RADIACION, Rug_250, slope_250m, TIERRA_ARABLE, TMed_ABRIL, 
TMed_AGOSTO, TMed_DICIEMBRE, TMed_ENERO, TMed_FEBRERO, TMed_JULIO, TMed_JUNIO, 
TMed_MARZO, TMed_MAYO, TMed_NOVIEMBRE, TMed_OCTUBRE, TMed_SEPTIEMBRE, TMMAX_ABRIL, 
TMMAX_AGOSTO, TMMAX_DICIEMBRE, TMMAX_ENERO, TMMAX_FEBRERO, TMMAX_JULIO, 
TMMAX_JUNIO, TMMAX_MARZO, TMMAX_MAYO, TMMAX_NOVIEMBRE, TMMAX_OCTUBRE, TMMAX_SEPTIEMBRE
FROM SG_LOCATIONS_COVARIANTS_2"


localizaciones <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

dbDisconnect(connExp)

localizaciones[,][localizaciones[,] == -9999.000] <- NA
localizaciones = localizaciones[complete.cases(localizaciones),]

names(localizaciones)[names(localizaciones) == "COOR_X_ETRS89"] <- "COOR_X_ETR"
names(localizaciones)[names(localizaciones) == "COOR_Y_ETRS89"] <- "COOR_Y_ETR"

Sg_Fosforo.rf <- predict(rf, newdata = localizaciones)

localizaciones$PREDIC_RF <- Sg_Fosforo.rf

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

names(localizaciones)


dbWriteTable(connExp, "RF_COOR_FOSFORO", localizaciones[,c("COOR_X_ETR","COOR_Y_ETR","PREDIC_RF")])
dbDisconnect(connExp)

x11()
getTree(rf,k=1, labelVar = FALSE)















