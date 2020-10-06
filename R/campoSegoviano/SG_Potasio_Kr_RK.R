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

nrow(soil.data)

summary(soil.data$Potasio_pp)
x11()
hist(soil.data$Potasio_pp, col="lightblue", main='Histograma ppm potasio',
     xlab='Potasio (ppm)', ylab='Frecuencia')
abline(v=c(mean(soil.data$Potasio_pp) - sd(soil.data$Potasio_pp),mean(soil.data$Potasio_pp) + sd(soil.data$Potasio_pp)),
       lty=2, col='grey')
abline(v=mean(soil.data$Potasio_pp), col='Red', lty=2)

x11()
qqnorm(soil.data$Potasio_pp, col="Red", main='Q-Q plot de normalidad\nPotasio (ppm)',
       xlab='Cuantiles teóricos', ylab='Cuantiles de la muestra')
qqline(soil.data$Potasio_pp, lty=2)


soil.data$log_K <- log(soil.data$Potasio_pp)

x11()
hist(soil.data$log_K, col='lightblue',
     main='Histograma potasio transformado\n(logaritmo ppm)', xlab='Potasio log(ppm)', ylab = 'Frecuencia')
abline(v=mean(soil.data$log_K), col='red', lty=2)
abline(v=c(mean(soil.data$log_K)-sd(soil.data$log_K),mean(soil.data$log_K)+sd(soil.data$log_K)), 
       col='grey', lty=2)

x11()
qqnorm(soil.data$log_K, col="Red", main='Q-Q plot normalidad potasio transformado\n(logaritmo ppm)',
       xlab='Cuantiles teóricos', ylab='Cuantiles de la muestra')
qqline(soil.data$log_K, lty=2)

inTrain <- createDataPartition(y = soil.data$log_K, p = 0.8, list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

#######################################################
########## INTERPOLACION SIMPLE KRIGING ###############
#######################################################

c = 48000
w = 4000
vlog_K <- variogram(log_K~1, data = soil.data, cutoff = c, width = w)
# x11()
# plot(vlog_K)


n = 0.15
p = 0.12
r = 25000

vgmlog_K <- vgm(nugget = n, psill = p, range = r, model = "Sph")
x11()
plot(vlog_K,vgmlog_K, col = 'Red', main='Semivariograma del potasio\n(trans. logarítmica)',
     xlab='Distancia', ylab='Semivarianza')

### Ajuste de los semivariogramas 

vgFitLog_K <- fit.variogram(vlog_K,vgmlog_K)
attr(vgFitLog_K,"SSErr")


Potasio.CV <- krige.cv(formula = log_K~1, soil.data, model=vgFitLog_K ,nfold=nrow(soil.data))

Potasio.CV$Potasio_bk <- exp(Potasio.CV$var1.pred + 0.5*Potasio.CV$var1.var)
Potasio.CV$Potasio_Obs <- soil.data$Potasio_pp
Potasio.CV$ResiduoLineal <- Potasio.CV$Potasio_Obs- Potasio.CV$Potasio_bk






x11()
plot(x=Potasio.CV$var1.pred, y=Potasio.CV$observed, col='red',
     main='Ajuste del modelo con los datos de entrenamiento\nMétodo: Simple Kriging\n(logaritmo ppm de potasio)',
     xlab = 'log. ppm de potasio estimado', ylab='log. ppm de potasio observado')
abline(c(0,1), lty=2)
cor.test(x=Potasio.CV$observed, y= Potasio.CV$var1.pred, alternative = "two.sided", conf.level = 0.95, method = "pearson")

r<-RMSE(obs = Potasio.CV$observed, pred = Potasio.CV$var1.pred)
x11()
plot(x=Potasio.CV$var1.pred, y=Potasio.CV$residual, col='red',
     main='Dispersión de los residuos\nMétodo: Simple Kriging\n(logaritmo ppm de potasio)',
     xlab='log. ppm de potasio estimado', ylab = 'Residuos')
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)
r



x11()
plot(x=Potasio.CV$Potasio_bk, y=Potasio.CV$Potasio_Obs, col='red',
     main='Ajuste del modelo con los datos de entrenamiento\nMétodo: Simple Kriging\n(ppm de potasio)',
     xlab = 'ppm de potasio estimado', ylab='ppm de potasio observado')
abline(c(0,1), lty=2)
cor.test(x=Potasio.CV$Potasio_bk, y= Potasio.CV$Potasio_Obs, alternative = "two.sided", conf.level = 0.95, method = "pearson")


r<-RMSE(obs = Potasio.CV$Potasio_Obs, pred = Potasio.CV$Potasio_bk)
x11()
plot(x=Potasio.CV$Potasio_bk, y=Potasio.CV$ResiduoLineal, col='red',
     main='Dispersión de los residuos\nMétodo: Simple Kriging\n(ppm de potasio)',
     xlab='ppm de potasio estimado', ylab = 'Residuos')
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)
r















#### INTERPOLACIÓN CON SIMPLE KRIGING ########


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


KrLog_K <- krige(log_K~1, locations = soil.data, newdata = localizaciones, 
                   model = vgFitLog_K, debug.level = -1)

# localizaciones$K_BK_EXP <- exp(KrLog_K$var1.pred)
localizaciones$K_PRED_PPM <- exp(KrLog_K$var1.pred + 0.5*KrLog_K$var1.var)

localizaciones <- as(localizaciones, Class = 'data.frame')

names(localizaciones)


baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "SK_POTASIO", 
             localizaciones[,c("COOR_X_ETRS89","COOR_Y_ETRS89","K_PRED_PPM")])
dbDisconnect(connExp)

#######################################################


#######################################################
########### REGRESIÓN LINEAL MULTIPLE #################
#######################################################


###### SIN COORDENADAS ###################
K_trend <- Potasio_pp~aspect_250m+ ETP+ GDD+ 
  LIBREHELADAS+ mde_250m+ PMed_ABRIL+ PMed_AGOSTO+ PMed_ANUAL+ PMed_DICIEMBRE+ 
  PMed_ENERO+ PMed_FEBRERO+ PMed_INVIERNO+ PMed_JULIO+ PMed_JUNIO+ PMed_MARZO+ 
  PMed_MAYO+ PMed_NOVIEMBRE+ PMed_OCTUBRE+ PMed_PRIMAVERA+ PMed_SEPTIEMBRE+ 
  PMed_VERANO+ RADIACION+ Rug_250+ slope_250m+ TIERRA_ARABLE+ TMed_ABRIL+ 
  TMed_AGOSTO+ TMed_DICIEMBRE+ TMed_ENERO+ TMed_FEBRERO+ TMed_JULIO+ TMed_JUNIO+ 
  TMed_MARZO+ TMed_MAYO+ TMed_NOVIEMBRE+ TMed_OCTUBRE+ TMed_SEPTIEMBRE+ 
  TMMAX_ABRIL+ TMMAX_AGOSTO+ TMMAX_DICIEMBRE+ TMMAX_ENERO+ TMMAX_FEBRERO+ 
  TMMAX_JULIO+ TMMAX_JUNIO+ TMMAX_MARZO+ TMMAX_MAYO+ TMMAX_NOVIEMBRE+ 
  TMMAX_OCTUBRE+ TMMAX_SEPTIEMBRE
# Residual standard error: 68.97 on 175 degrees of freedom
# Multiple R-squared:  0.4723,	Adjusted R-squared:  0.406 
# F-statistic:  7.12 on 22 and 175 DF,  p-value: 4.279e-15

K_trend <- Potasio_pp~COOR_X_ETR+ COOR_Y_ETR+ aspect_250m+ ETP+ GDD+ 
  LIBREHELADAS+ mde_250m+ PMed_ABRIL+ PMed_AGOSTO+ PMed_ANUAL+ PMed_DICIEMBRE+ 
  PMed_ENERO+ PMed_FEBRERO+ PMed_INVIERNO+ PMed_JULIO+ PMed_JUNIO+ PMed_MARZO+ 
  PMed_MAYO+ PMed_NOVIEMBRE+ PMed_OCTUBRE+ PMed_PRIMAVERA+ PMed_SEPTIEMBRE+ 
  PMed_VERANO+ RADIACION+ Rug_250+ slope_250m+ TIERRA_ARABLE+ TMed_ABRIL+ 
  TMed_AGOSTO+ TMed_DICIEMBRE+ TMed_ENERO+ TMed_FEBRERO+ TMed_JULIO+ TMed_JUNIO+ 
  TMed_MARZO+ TMed_MAYO+ TMed_NOVIEMBRE+ TMed_OCTUBRE+ TMed_SEPTIEMBRE+ 
  TMMAX_ABRIL+ TMMAX_AGOSTO+ TMMAX_DICIEMBRE+ TMMAX_ENERO+ TMMAX_FEBRERO+ 
  TMMAX_JULIO+ TMMAX_JUNIO+ TMMAX_MARZO+ TMMAX_MAYO+ TMMAX_NOVIEMBRE+ 
  TMMAX_OCTUBRE+ TMMAX_SEPTIEMBRE

m <- lm(K_trend, data = soil.data)
step.lm <- step(m, direction = "both" )
summary(step.lm)

# Residual standard error: 69.04 on 175 degrees of freedom
# Multiple R-squared:  0.4713,	Adjusted R-squared:  0.4048 
# F-statistic:  7.09 on 22 and 175 DF,  p-value: 4.973e-15

step.lm$call

### graficas del ajuste de la regresión lineal múltiple
x11()
plot(step.lm$fitted.values, soil.data$Fosforo_pp, xlab='Fitted (%)', ylab="observed (%)")
abline(0,1)
x11()
hist(resid(step.lm))

cor(step.lm$fitted.values, soil.data$Potasio_pp)^2

x11()
hist(step.lm$residuals)

#### comprobación correlación espacial de los residuos
soil.data$Residuos <- step.lm$residuals
coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)


c = 48000
w = 4000
vlog_ResiduosK <- variogram(Residuos~1, data = soil.data, cutoff = c, width = w)
x11()
plot(vlog_ResiduosK, col='Red', main='Semivariograma de los residuos\nPotasio (ppm)',
     xlab='Distancia', ylab='Semivarianza')
##############################################################


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

potasio.lmPred <- predict(step.lm, newdata = localizaciones)

localizaciones$K_PRED_LM <- potasio.lmPred

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "LM_COOR_POTASIO", localizaciones[,c("COOR_X_ETR","COOR_Y_ETR","K_PRED_LM")])
dbDisconnect(connExp)








