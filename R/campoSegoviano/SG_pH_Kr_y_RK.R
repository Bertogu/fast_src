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
query<- "SELECT ID_MUESTRA, LOCALIDAD, ARENA, LIMO, ARCILLA, USDA, PH, CE, 
MO_Porc, N_Porc, Fosforo_pp, Potasio_pp, Magnesio_p, Calcio_ppm, Sodio_ppm, 
Carbonatos, CalizaActi, COOR_X_ETR, COOR_Y_ETR, Campaña AS season, Laboratori, LAB_COD, 
aspect_250m, ETP, GDD, LIBREHELADAS, mde_250m, PMed_ABRIL, PMed_AGOSTO, 
PMed_ANUAL, PMed_DICIEMBRE, PMed_ENERO, PMed_FEBRERO, PMed_INVIERNO, PMed_JULIO, 
PMed_JUNIO, PMed_MARZO, PMed_MAYO, PMed_NOVIEMBRE, PMed_OCTUBRE, PMed_PRIMAVERA, 
PMed_SEPTIEMBRE, PMed_VERANO, RADIACION, Rug_250, slope_250m, TIERRA_ARABLE, 
TMed_ABRIL, TMed_AGOSTO, TMed_DICIEMBRE, TMed_ENERO, TMed_FEBRERO, TMed_JULIO, 
TMed_JUNIO, TMed_MARZO, TMed_MAYO, TMed_NOVIEMBRE, TMed_OCTUBRE, 
TMed_SEPTIEMBRE, TMMAX_ABRIL, TMMAX_AGOSTO, TMMAX_DICIEMBRE, 
TMMAX_ENERO, TMMAX_FEBRERO, TMMAX_JULIO, TMMAX_JUNIO, TMMAX_MARZO, 
TMMAX_MAYO, TMMAX_NOVIEMBRE, TMMAX_OCTUBRE, TMMAX_SEPTIEMBRE
FROM SG_SAMPLES_COVARIANTS_CODLAB"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))
dbDisconnect(connExp)
nrow(soil.data)

soil.data$LAB_COD <- as.factor(soil.data$LAB_COD)
summary(soil.data$PH)

x11()
h<-hist(soil.data$PH, col='lightblue',
     main='Histograma pH', xlab='pH', ylab = 'Frecuencia', freq = TRUE,
     breaks = c(min(soil.data$PH),5.5,6.5,7.5,8.5,max(soil.data$PH)))
text(h$mids,h$counts+2,labels = h$counts, col='black' )

x11()
hist(soil.data$PH, col='lightblue',main='Histograma de pH', 
     xlab='pH', ylab = 'Frecuencia', nclass = 20, freq = TRUE)

soil.data$Laboratori[soil.data$Laboratori == "INSTITUTO DE LA VIÑA Y LA VID - ULE"] <- 'ULE'

x11()
boxplot(soil.data$PH~soil.data$season)
abline(h=mean(soil.data$PH), col='Red', lty=2)
text(x=1,y=9,labels = c(count(soil.data$Laboratori[soil.data$LAB_COD==2])))


x11()
boxplot(soil.data$PH)
abline(h=mean(soil.data$PH), col='Red', lty=2)
abline(h=median(soil.data$PH), col='Red', lty=2)









inTrain <- createDataPartition(y = soil.data$PH, p = 0.80,list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]



# arcillaTrend <- ARCILLA ~ COOR_X_ETR+COOR_Y_ETR+aspect_250m+ETP+GDD+LIBREHELADAS+mde_250m+PMed_ABRIL+PMed_AGOSTO+PMed_ANUAL+PMed_DICIEMBRE+PMed_ENERO+PMed_FEBRERO+PMed_INVIERNO+PMed_JULIO+PMed_JUNIO+PMed_MARZO+PMed_MAYO+PMed_NOVIEMBRE+PMed_OCTUBRE+PMed_PRIMAVERA+PMed_SEPTIEMBRE+PMed_VERANO+RADIACION+Rug_250+slope_250m+TIERRA_ARABLE+TMed_ABRIL+TMed_AGOSTO+TMed_DICIEMBRE+TMed_ENERO+TMed_FEBRERO+TMed_JULIO+TMed_JUNIO+TMed_MARZO+TMed_MAYO+TMed_NOVIEMBRE+TMed_OCTUBRE+TMed_SEPTIEMBRE+TMMAX_ABRIL+TMMAX_AGOSTO+TMMAX_DICIEMBRE+TMMAX_ENERO+TMMAX_FEBRERO+TMMAX_JULIO+TMMAX_JUNIO+TMMAX_MARZO+TMMAX_MAYO+TMMAX_NOVIEMBRE+TMMAX_OCTUBRE+TMMAX_SEPTIEMBRE

phTrend <- PH~COOR_X_ETR+COOR_Y_ETR+aspect_250m+ ETP+ GDD+ LIBREHELADAS+ mde_250m+ PMed_ABRIL+ PMed_AGOSTO+ 
  PMed_ANUAL+ PMed_DICIEMBRE+ PMed_ENERO+ PMed_FEBRERO+ PMed_INVIERNO+ PMed_JULIO+ 
  PMed_JUNIO+ PMed_MARZO+ PMed_MAYO+ PMed_NOVIEMBRE+ PMed_OCTUBRE+ PMed_PRIMAVERA+ 
  PMed_SEPTIEMBRE+ PMed_VERANO+ RADIACION+ Rug_250+ slope_250m+ TIERRA_ARABLE+ 
  TMed_ABRIL+ TMed_AGOSTO+ TMed_DICIEMBRE+ TMed_ENERO+ TMed_FEBRERO+ TMed_JULIO+ 
  TMed_JUNIO+ TMed_MARZO+ TMed_MAYO+ TMed_NOVIEMBRE+ TMed_OCTUBRE+ 
  TMed_SEPTIEMBRE+ TMMAX_ABRIL+ TMMAX_AGOSTO+ TMMAX_DICIEMBRE+ 
  TMMAX_ENERO+ TMMAX_FEBRERO+ TMMAX_JULIO+ TMMAX_JUNIO+ TMMAX_MARZO+ 
  TMMAX_MAYO+ TMMAX_NOVIEMBRE+ TMMAX_OCTUBRE+ TMMAX_SEPTIEMBRE

### SIN COORDENADAS
# phTrend <- PH~aspect_250m+ ETP+ GDD+ LIBREHELADAS+ mde_250m+ PMed_ABRIL+ PMed_AGOSTO+ 
#   PMed_ANUAL+ PMed_DICIEMBRE+ PMed_ENERO+ PMed_FEBRERO+ PMed_INVIERNO+ PMed_JULIO+ 
#   PMed_JUNIO+ PMed_MARZO+ PMed_MAYO+ PMed_NOVIEMBRE+ PMed_OCTUBRE+ PMed_PRIMAVERA+ 
#   PMed_SEPTIEMBRE+ PMed_VERANO+ RADIACION+ Rug_250+ slope_250m+ TIERRA_ARABLE+ 
#   TMed_ABRIL+ TMed_AGOSTO+ TMed_DICIEMBRE+ TMed_ENERO+ TMed_FEBRERO+ TMed_JULIO+ 
#   TMed_JUNIO+ TMed_MARZO+ TMed_MAYO+ TMed_NOVIEMBRE+ TMed_OCTUBRE+ 
#   TMed_SEPTIEMBRE+ TMMAX_ABRIL+ TMMAX_AGOSTO+ TMMAX_DICIEMBRE+ 
#   TMMAX_ENERO+ TMMAX_FEBRERO+ TMMAX_JULIO+ TMMAX_JUNIO+ TMMAX_MARZO+ 
#   TMMAX_MAYO+ TMMAX_NOVIEMBRE+ TMMAX_OCTUBRE+ TMMAX_SEPTIEMBRE


m <- lm(phTrend, data = soil.data)
step.lm <- step(m, direction = "both" )
conCoor <- step.lm$call
# sinCoor <- step.lm$call
summary(step.lm)

x11()
plot(step.lm$fitted.values, soil.data$PH, 
     xlab='Valores ajustado pH', ylab="Valores observados de pH",
     main = 'Ajuste del modelo de regresión lineal multiple (pH)',
     col = 'Red')
abline(0,1, lty = 2)

x11()
plot(step.lm$fitted.values, resid(step.lm), 
     xlab="Valores ajjustados de pH", ylab = "Residual (observados - ajustados)",
     main='Dispersión de los residuos del modelo lineal', col = 'Red')
abline(0,0, lty=2)


x11()
hist(resid(step.lm), col='light blue', main='Histograma de los residuos pH',
     xlab ='pH', ylab='Frecuencia')

x11()
qqnorm(resid(step.lm), col='Red', main='Plot de normalidad Q-Q \nvalores de pH',
       xlab='Cuantiles teóricos', ylab='Cuantiles de la muestra')
qqline(resid(step.lm), lty=2)


soil.data$lm_Residuals <- step.lm$residuals
names(soil.data)
coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

# El variograma de los residuos no muestra ningún tipo de correlación espacial

c = 25000
w = 2000

v <- variogram(lm_Residuals~1, data = soil.data)
x11()
plot(v, col='Red', 
     xlab = 'Distancia', ylab = 'Semivarianza', 
     main='Semivariograma de los residuos pH')


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


coordinates(localizaciones)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(localizaciones)<-CRS(etrs89)

names(soil.data)
str(soil.data)

soil.data <- as(soil.data, Class="data.frame")
localizaciones <- as(localizaciones, Class="data.frame")
names(localizaciones)[names(localizaciones)=='COOR_X_ETRS89'] <- 'COOR_X_ETR'
names(localizaciones)[names(localizaciones)=='COOR_Y_ETRS89'] <- 'COOR_Y_ETR'

pH.lmPred <- predict(step.lm, newdata = localizaciones)

localizaciones$lm_pH_PRED <- pH.lmPred

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "LM_COOR_PH", localizaciones[,c("COOR_X_ETR","COOR_Y_ETR","lm_pH_PRED")])
dbDisconnect(connExp)




















