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

summary(soil.data$ARCILLA)
x11()
hist(soil.data$ARCILLA, breaks = 20)


# Elimino las muestras que son anormales
soil.data <- soil.data[soil.data$ID_MUESTRA != 244,] #tiene una suma de textura mayor que 100.10
soil.data <- soil.data[soil.data$ARCILLA > 2,]


inTrain <- createDataPartition(y = soil.data$ARCILLA, p = 0.80,list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

nrow(soil.data)
summary(soil.data$ARCILLA)
x11()
hist(soil.data$ARCILLA, col="light blue",
     xlab = "% Arcilla", ylab="Frecuencia",
     main = "HISTOGRAMA % DE ARCILLA")
abline(v=summary(soil.data$ARCILLA)[3], lty=2, col="red")
abline(v=c(mean(soil.data$ARCILLA)-sd(soil.data$ARCILLA), 
           mean(soil.data$ARCILLA)+sd(soil.data$ARCILLA)),
       lty = 2, col='grey')
x11()
qqnorm(soil.data$ARCILLA, col="red", main='Q-Q plot de normalidad',
       xlab ="Quantiles teoricos", ylab = "Quantiles de la muestra")
qqline(soil.data$ARCILLA, lty=2)


# Interpolación con Kriging
coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

gsClay <- gstat(formula = ARCILLA~1, data = soil.data)

a<- seq(from = 3000, to= 35000, by=3000)

c = 35000
w = 3500
vgClay <- variogram(gsClay, cutoff=c, width = w)

x11()
plot(vgClay)


n = 20
p = 32
r = 15000

vgmClay <- vgm(nugget = n, psill = p, range = r, model = "Sph")
x11()
plot(vgClay, vgmClay,main="Arcilla: Semivariogram ajustado (esférico)", 
     col='Red', xlab='Distancia', ylab='Semivarianza')

vgFitClay <- fit.variogram(vgClay,vgmClay)
attr(vgFitClay,"SSErr")

Arcilla.CV <- krige.cv(formula = ARCILLA~1, soil.data, nfold=nrow(soil.data))
str(Arcilla.CV)
r<-RMSE(obs = Arcilla.CV$observed, pred = Arcilla.CV$var1.pred)
r

cor.test(x=Arcilla.CV$var1.pred, y= Arcilla.CV$observed, alternative = "two.sided", conf.level = 0.95, method = "pearson")



x11()
plot(x=Arcilla.CV$var1.pred, y=Arcilla.CV$observed, col='red',
     main='Ajuste del modelo en la validación cruzada\nMétodo: Simple Kriging\n(% Arcilla)',
     xlab = '% Arcilla estimado', ylab='% Arcilla observado')
abline(c(0,1), lty=2)

x11()
plot(x=Arcilla.CV$observed, y=Arcilla.CV$residual, col='red',
     main='Dispersión de los residuos del modelo\nMétodo: Simple Kriging\n(% Arcilla)',
     xlab='% Arcilla observado', ylab = 'Residuos')
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)




x11()
plot(x=Arcilla.CV$var1.pred, y=Arcilla.CV$residual, col='red',
     main='Dispersión de los residuos del modelo\nMétodo: Simple Kriging\n(% Arcilla)',
     xlab='% Arcilla estimado', ylab = 'Residuos')
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)












# vgmClay <- vgm(nugget = n, psill = p, range = r, model = "Gau")
# vgmClay <- vgm(nugget = n, psill = p, range = r, model = "Exp")


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

# interpolación con simple kriging
arcilla.Krig <- krige(formula = ARCILLA~1, locations = soil.data, newdata = localizaciones, model = vgFitClay, debug.level = -1)

str(arcilla.Krig)

localizaciones$SK_PRED <- arcilla.Krig@data$var1.pred
localizaciones$SK_VAR <- arcilla.Krig@data$var1.var
localizaciones$SK_SD <- sqrt(arcilla.Krig@data$var1.var)

summary(localizaciones$SK_PRED)
x11()
hist(localizaciones$SK_PRED, probability = TRUE)

summary(localizaciones$SK_SD)
x11()
hist(localizaciones$SK_SD)

localizaciones<-as(localizaciones,Class="data.frame")

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "SK_ARCILLA", localizaciones[,c("COOR_X_ETRS89","COOR_Y_ETRS89","SK_PRED","SK_VAR","SK_SD")])
dbDisconnect(connExp)


#######################################################
######## REGRESSION KRIGING ##########################
#######################################################
names(soil.data)
# Regresión lineal multiple con todas las variables
arcillaTrend <- ARCILLA ~ aspect_250m+ETP+GDD+LIBREHELADAS+mde_250m+PMed_ABRIL+PMed_AGOSTO+PMed_ANUAL+PMed_DICIEMBRE+PMed_ENERO+PMed_FEBRERO+PMed_INVIERNO+PMed_JULIO+PMed_JUNIO+PMed_MARZO+PMed_MAYO+PMed_NOVIEMBRE+PMed_OCTUBRE+PMed_PRIMAVERA+PMed_SEPTIEMBRE+PMed_VERANO+RADIACION+Rug_250+slope_250m+TIERRA_ARABLE+TMed_ABRIL+TMed_AGOSTO+TMed_DICIEMBRE+TMed_ENERO+TMed_FEBRERO+TMed_JULIO+TMed_JUNIO+TMed_MARZO+TMed_MAYO+TMed_NOVIEMBRE+TMed_OCTUBRE+TMed_SEPTIEMBRE+TMMAX_ABRIL+TMMAX_AGOSTO+TMMAX_DICIEMBRE+TMMAX_ENERO+TMMAX_FEBRERO+TMMAX_JULIO+TMMAX_JUNIO+TMMAX_MARZO+TMMAX_MAYO+TMMAX_NOVIEMBRE+TMMAX_OCTUBRE+TMMAX_SEPTIEMBRE
# arcillaTrend <- ARCILLA ~ COOR_X_ETR+COOR_Y_ETR+aspect_250m+ETP+GDD+LIBREHELADAS+mde_250m+PMed_ABRIL+PMed_AGOSTO+PMed_ANUAL+PMed_DICIEMBRE+PMed_ENERO+PMed_FEBRERO+PMed_INVIERNO+PMed_JULIO+PMed_JUNIO+PMed_MARZO+PMed_MAYO+PMed_NOVIEMBRE+PMed_OCTUBRE+PMed_PRIMAVERA+PMed_SEPTIEMBRE+PMed_VERANO+RADIACION+Rug_250+slope_250m+TIERRA_ARABLE+TMed_ABRIL+TMed_AGOSTO+TMed_DICIEMBRE+TMed_ENERO+TMed_FEBRERO+TMed_JULIO+TMed_JUNIO+TMed_MARZO+TMed_MAYO+TMed_NOVIEMBRE+TMed_OCTUBRE+TMed_SEPTIEMBRE+TMMAX_ABRIL+TMMAX_AGOSTO+TMMAX_DICIEMBRE+TMMAX_ENERO+TMMAX_FEBRERO+TMMAX_JULIO+TMMAX_JUNIO+TMMAX_MARZO+TMMAX_MAYO+TMMAX_NOVIEMBRE+TMMAX_OCTUBRE+TMMAX_SEPTIEMBRE
m <- lm(arcillaTrend, data = soil.data)
step.lm <- step(m, direction = "both" )
summary(step.lm)
#####################################################
# Residual standard error: 4.914 on 162 degrees of freedom
# Multiple R-squared:  0.6171,	Adjusted R-squared:  0.5486 
# F-statistic: 9.004 on 29 and 162 DF,  p-value: < 2.2e-16
#####################################################
step.lm$call

# trend <- ARCILLA ~ ETP + mde_250m + PMed_AGOSTO + PMed_DICIEMBRE + 
#   PMed_ENERO + PMed_MARZO + PMed_NOVIEMBRE + PMed_OCTUBRE + 
#   PMed_VERANO + RADIACION + Rug_250 + slope_250m + TMed_ABRIL + 
#   TMed_DICIEMBRE + TMed_ENERO + TMed_FEBRERO + TMed_JULIO + 
#   TMed_JUNIO + TMed_MARZO + TMed_MAYO + TMed_NOVIEMBRE + TMed_SEPTIEMBRE + 
#   TMMAX_AGOSTO + TMMAX_JUNIO + TMMAX_MARZO + TMMAX_NOVIEMBRE + 
#   TMMAX_SEPTIEMBRE + PMed_PRIMAVERA + LIBREHELADAS

# # Trend utilizando las coordenadas como variable explicativa
trend <- ARCILLA ~ ARCILLA ~ COOR_X_ETR + COOR_Y_ETR + mde_250m + PMed_AGOSTO + 
  PMed_DICIEMBRE + PMed_ENERO + PMed_INVIERNO + PMed_MARZO + 
  PMed_MAYO + PMed_NOVIEMBRE + PMed_PRIMAVERA + PMed_VERANO + 
  Rug_250 + slope_250m + TMed_ABRIL + TMed_AGOSTO + TMed_DICIEMBRE + 
  TMed_ENERO + TMed_FEBRERO + TMed_JULIO + TMed_JUNIO + TMed_MAYO + 
  TMed_NOVIEMBRE + TMed_SEPTIEMBRE + TMMAX_ABRIL + TMMAX_AGOSTO + 
  TMMAX_DICIEMBRE + TMMAX_ENERO + TMMAX_JUNIO + TMMAX_NOVIEMBRE + 
  TMMAX_OCTUBRE + TMMAX_SEPTIEMBRE

### graficas del ajuste de la regresión lineal múltiple
x11()
plot(step.lm$fitted.values, soil.data$ARCILLA, 
     xlab='Valores ajustado (% arcilla)', ylab="Valores observados (% arcilla)",
     main = 'Ajuste del modelo con los datos de entrenamiento\nMétodo: regresión lineal múltiple\n (% arcilla)',
     col = 'Red')
abline(0,1, lty = 2)

x11()
plot(step.lm$fitted.values, resid(step.lm), 
     xlab="Valores estimados (% arcilla)", ylab = "Residuos",
     main='Dispersión de los residuos\nMétodo: regresión lineal multiple', col = 'Red')
abline(0,0, lty=2)
r <- RMSE(obs = soil.data$ARCILLA, pred = step.lm$fitted.values)
abline(h=c(r, -1*r), col='grey', lty=2)

cor.test(x=step.lm$fitted.values, y= soil.data$ARCILLA, alternative = "two.sided", conf.level = 0.95, method = "pearson")

x11()
hist(resid(step.lm), col='light blue', 
     main = 'Distribución de los residuos del ajuste\nMétodo: regresión lineal múltiple\n(% arcilla)',
     xlab = 'Residuos del ajuste (% de arcilla)', ylab = 'Frecuencia')
abline(v=mean(resid(step.lm)), col='Red', lty=2)
abline(v=c(mean(resid(step.lm))-sd(resid(step.lm)), mean(resid(step.lm))+sd(resid(step.lm))), col='grey', lty=2)


x11()
qqnorm(resid(step.lm), col='Red', main = 'Plot Q-Q de normalida de los residuos\nMétodo: regresión lineal múltiple\n(% arcilla)',
       xlab='Cuantiles teóricos', ylab='Cuantiles de la muestra')
qqline(resid(step.lm), lty=2)


soil.data$lm_Residuals <- step.lm$residuals

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

# El variograma de los residuos no muestra ningún tipo de correlación espacial

c = 25000
w = 2000

v <- variogram(lm_Residuals~1, data = soil.data, cutoff=c, width=w)
x11()
plot(v, col='Red', 
     xlab = 'Distancia', ylab = 'Semivarianza', 
     main='Semivariograma de los residuos\nMétodo: regresión lineal múltiples\n (% arcilla)')

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

arcilla.lmPred <- predict(step.lm, newdata = localizaciones)

localizaciones$lm_PRED <- arcilla.lmPred

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

names(localizaciones)

dbWriteTable(connExp, "LM_COOR_ARCILLA", localizaciones[,c("COOR_X_ETR","COOR_Y_ETR","lm_PRED")])
dbDisconnect(connExp)


### lm utilizando las coordenadas como regresores


arcilla.lmPred_coord <- predict(step.lm, newdata = localizaciones)

localizaciones$lm_PRED_coor<- arcilla.lmPred_coord
summary(localizaciones$lm_PRED)

x11()
hist(localizaciones$lm_PRED)

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

localizaciones<-as(localizaciones,Class="data.frame")
dbWriteTable(connExp, "LM_COOR_ARCILLA", localizaciones)
dbDisconnect(connExp)

