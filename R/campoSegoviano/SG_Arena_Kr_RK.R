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


soil.data$SUMA_TEXTURA <- soil.data$ARENA + soil.data$LIMO + soil.data$ARCILLA
soil.data <- soil.data[soil.data$ID_MUESTRA != 244,]
soil.data <- soil.data[soil.data$ID_MUESTRA != 142,]

summary(soil.data$ARENA)
x11()
hist(soil.data$ARENA,col="light blue",
     xlab = "% Arena", ylab="Frecuencia",
     main = "HISTOGRAMA % DE ARENA")
abline(v=summary(soil.data$ARENA)[3], lty=2, col="red")
abline(v=c(mean(soil.data$ARENA)-sd(soil.data$ARENA), 
           mean(soil.data$ARENA)+sd(soil.data$ARENA)),
       lty = 2, col='grey')
x11()
qqnorm(soil.data$ARENA, col='red',
       xlab ="Quantiles teoricos", ylab = "Quantiles de la muestra",
       main = 'Q-Q plot de normalidad')
qqline(soil.data$ARENA, lty=2)



inTrain <- createDataPartition(y = soil.data$ARENA, p = 0.8,list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]


#############################################################
############ SIMPLE KRIGING #################################
#############################################################

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

names(soil.data)

c = 28000
w = 4500
vArena <- variogram(ARENA~1, data = soil.data, cutoff = c, width = w)
x11()
plot(vArena)

n = 55
p = 150
r = 12500

vgmArena <- vgm(nugget = n, psill = p, range = r, model = "Sph")
x11()
plot(vArena,vgmArena, col="Red", main="Semivariograma ajustado de tipo esférico\n(% arena)",
     xlab="Distancia", ylab="semivarianza")

vgFitArena <- fit.variogram(vArena, vgmArena)
attr(vgFitArena,"SSErr")

Arena.CV <- krige.cv(formula = ARENA~1, soil.data, model=vgFitArena ,nfold=nrow(soil.data))

r<-RMSE(obs = Arena.CV$observed, pred = Arena.CV$var1.pred)
r
cor.test(x=Arena.CV$var1.pred, y= Arena.CV$observed, alternative = "two.sided", conf.level = 0.95, method = "pearson")

x11()
plot(x=Arena.CV$var1.pred, y=Arena.CV$observed, col='red',
     main='Ajuste del modelo con los datos de entrenamiento\nMétodo: Simple Kriging\n(% arena)',
     xlab = '% Arena estimado', ylab='% Arena observado')
abline(c(0,1), lty=2)

x11()
plot(x=Arena.CV$var1.pred, y=Arena.CV$residual, col='red',
     main='Dispersión de los residuos\nMétodo: Simple Kriging\n(% Arena)',
     xlab='% Arena estimado', ylab = 'Residuos')
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)


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

arena.Krig <- krige(formula = ARENA~1, locations = soil.data, 
                    newdata = localizaciones, 
                    model = vgFitArena, debug.level = -1)

arena.Krig<-as(arena.Krig, Class="data.frame")

names(localizaciones)[names(localizaciones) == "COOR_X_ETRS89"] <- "COOR_X_ETR"

names(arena.Krig)[names(arena.Krig) == "var1.pred"] <- "ARENA_SK"
names(arena.Krig)[names(arena.Krig) == "var1.var"] <- "VAR_ARENA_SK"
arena.Krig$SD_ARENA_SK <- sqrt(arena.Krig$VAR_ARENA_SK)

summary(arena.Krig$ARENA_SK)


x11()
hist(arena.Krig$ARENA_SK, col = 'lightblue', freq = FALSE,
     main='Distribucion de los valores interpolados\n(% Arena)',
     xlab='% Arena')

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "SK_ARENA", arena.Krig)
dbDisconnect(connExp)




#####################################################
############ REGRESSION KRIGING #####################
#####################################################

summary(soil.data$ARENA)
x11()
hist(soil.data$ARENA)

x11()
qqnorm(soil.data$ARENA)
qqline(soil.data$ARENA)


# Regresión lineal multiple con todas las variables
# arenaTrend <- ARENA ~ aspect_250m+ETP+GDD+LIBREHELADAS+mde_250m+PMed_ABRIL+PMed_AGOSTO+PMed_ANUAL+PMed_DICIEMBRE+PMed_ENERO+PMed_FEBRERO+PMed_INVIERNO+PMed_JULIO+PMed_JUNIO+PMed_MARZO+PMed_MAYO+PMed_NOVIEMBRE+PMed_OCTUBRE+PMed_PRIMAVERA+PMed_SEPTIEMBRE+PMed_VERANO+RADIACION+Rug_250+slope_250m+TIERRA_ARABLE+TMed_ABRIL+TMed_AGOSTO+TMed_DICIEMBRE+TMed_ENERO+TMed_FEBRERO+TMed_JULIO+TMed_JUNIO+TMed_MARZO+TMed_MAYO+TMed_NOVIEMBRE+TMed_OCTUBRE+TMed_SEPTIEMBRE+TMMAX_ABRIL+TMMAX_AGOSTO+TMMAX_DICIEMBRE+TMMAX_ENERO+TMMAX_FEBRERO+TMMAX_JULIO+TMMAX_JUNIO+TMMAX_MARZO+TMMAX_MAYO+TMMAX_NOVIEMBRE+TMMAX_OCTUBRE+TMMAX_SEPTIEMBRE
arenaTrend <- ARENA ~ COOR_X_ETR+COOR_Y_ETR+aspect_250m+ETP+GDD+LIBREHELADAS+mde_250m+PMed_ABRIL+PMed_AGOSTO+PMed_ANUAL+PMed_DICIEMBRE+PMed_ENERO+PMed_FEBRERO+PMed_INVIERNO+PMed_JULIO+PMed_JUNIO+PMed_MARZO+PMed_MAYO+PMed_NOVIEMBRE+PMed_OCTUBRE+PMed_PRIMAVERA+PMed_SEPTIEMBRE+PMed_VERANO+RADIACION+Rug_250+slope_250m+TIERRA_ARABLE+TMed_ABRIL+TMed_AGOSTO+TMed_DICIEMBRE+TMed_ENERO+TMed_FEBRERO+TMed_JULIO+TMed_JUNIO+TMed_MARZO+TMed_MAYO+TMed_NOVIEMBRE+TMed_OCTUBRE+TMed_SEPTIEMBRE+TMMAX_ABRIL+TMMAX_AGOSTO+TMMAX_DICIEMBRE+TMMAX_ENERO+TMMAX_FEBRERO+TMMAX_JULIO+TMMAX_JUNIO+TMMAX_MARZO+TMMAX_MAYO+TMMAX_NOVIEMBRE+TMMAX_OCTUBRE+TMMAX_SEPTIEMBRE
m <- lm(arenaTrend, data = soil.data)
step.lm <- step(m, direction = "both" )
summary(step.lm)
#####################################################
# Residual standard error: 9.146 on 199 degrees of freedom
# Multiple R-squared:  0.6086,	Adjusted R-squared:  0.5614 
# F-statistic: 12.89 on 24 and 199 DF,  p-value: < 2.2e-16
#####################################################
step.lm$call

trend <- ARENA ~ aspect_250m + ETP + PMed_AGOSTO + PMed_ANUAL + 
  PMed_DICIEMBRE + PMed_ENERO + PMed_JULIO + PMed_JUNIO + PMed_NOVIEMBRE + 
  PMed_SEPTIEMBRE + PMed_VERANO + RADIACION + TMed_ABRIL + 
  TMed_FEBRERO + TMed_JULIO + TMed_JUNIO + TMed_MARZO + TMed_NOVIEMBRE + 
  TMed_SEPTIEMBRE + TMMAX_AGOSTO + TMMAX_DICIEMBRE + TMMAX_ENERO + 
  TMMAX_JULIO + TMMAX_JUNIO + TMMAX_NOVIEMBRE + TMMAX_SEPTIEMBRE


# Trend utilizando las coordenadas como variable explicativa

trend <- ARENA ~ COOR_X_ETR + aspect_250m + GDD + LIBREHELADAS + 
  PMed_ABRIL + PMed_AGOSTO + PMed_JULIO + PMed_NOVIEMBRE + 
  PMed_SEPTIEMBRE + PMed_VERANO + RADIACION + TMed_FEBRERO + 
  TMed_JULIO + TMed_JUNIO + TMed_NOVIEMBRE + TMed_OCTUBRE + 
  TMed_SEPTIEMBRE + TMMAX_AGOSTO + TMMAX_DICIEMBRE + TMMAX_ENERO + 
  TMMAX_JULIO + TMMAX_JUNIO + TMMAX_NOVIEMBRE + TMMAX_SEPTIEMBRE

### graficas del ajuste de la regresión lineal múltiple
x11()
plot(step.lm$fitted.values, soil.data$ARENA, 
     xlab='% Arena estimado', ylab='% Arena observado',
     main = 'Ajuste del modelo con los datos de entrenamiento\nMétodo: Regresión Lineal Múltiple\n(% arena)',
     col = 'Red')
abline(0,1, lty = 2)
cor.test(x=step.lm$fitted.values, y=soil.data$ARENA, alternative = "two.sided", conf.level = 0.95, method = "pearson")


er <- RMSE(obs = soil.data$ARENA, pred = step.lm$fitted.values) 

x11()
plot(step.lm$fitted.values, resid(step.lm), 
     xlab="% Arena estimado", ylab = "Residuos",
     main= 'Estudio de los residuos\nMétodo: regresión lineal múltiple\n(% arena)', 
     col = 'Red')
abline(0,0, lty=2)
abline(h=c(-1*er,er), col='grey', lty=2)


x11()
hist(resid(step.lm), col='lightblue',
     main = 'Histograma de los residuos (% arena)', freq = TRUE,
     xlab='Residuos', ylab='Frecuencia')
abline(v=mean(resid(step.lm)), lty=2, col='red')
abline(v=c(mean(resid(step.lm))-sd(resid(step.lm)),mean(resid(step.lm))+sd(resid(step.lm))),
       col='grey', lty=2)


x11()
qqnorm(resid(step.lm), col='Red', 
       xlab='Cuantiles teóricos', ylab='Cuantiles de la muestra',
       main = 'Plot de normalidad Q-Q de los residuos del modelo\n(% arena)')
qqline(resid(step.lm), lty=2)


# El variograma de los residuos no muestra ningún tipo de correlación espacial

soil.data$lm_Residuals <- step.lm$residuals
coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

c = 22000
w = 2000
vResdArena <- variogram(lm_Residuals~1, data = soil.data, cutoff = c, width = w)
x11()
plot(vResdArena, col='Red', 
     main='Semivariograma de los residuos (% Arena).\nMétodo: regresión lineal multiple ',
     xlab ='Distancia', ylab='Semivarianza')


#####################################################
############ REGRESIÓN LINEAL MULTIPLE ##############
######### (COORDENADAS COMO REGRESORES) #############
#####################################################

# Regresión lineal multiple con todas las variables
arenaTrend <- ARENA ~ COOR_X_ETR+COOR_Y_ETR+aspect_250m+ETP+GDD+LIBREHELADAS+mde_250m+PMed_ABRIL+PMed_AGOSTO+PMed_ANUAL+PMed_DICIEMBRE+PMed_ENERO+PMed_FEBRERO+PMed_INVIERNO+PMed_JULIO+PMed_JUNIO+PMed_MARZO+PMed_MAYO+PMed_NOVIEMBRE+PMed_OCTUBRE+PMed_PRIMAVERA+PMed_SEPTIEMBRE+PMed_VERANO+RADIACION+Rug_250+slope_250m+TIERRA_ARABLE+TMed_ABRIL+TMed_AGOSTO+TMed_DICIEMBRE+TMed_ENERO+TMed_FEBRERO+TMed_JULIO+TMed_JUNIO+TMed_MARZO+TMed_MAYO+TMed_NOVIEMBRE+TMed_OCTUBRE+TMed_SEPTIEMBRE+TMMAX_ABRIL+TMMAX_AGOSTO+TMMAX_DICIEMBRE+TMMAX_ENERO+TMMAX_FEBRERO+TMMAX_JULIO+TMMAX_JUNIO+TMMAX_MARZO+TMMAX_MAYO+TMMAX_NOVIEMBRE+TMMAX_OCTUBRE+TMMAX_SEPTIEMBRE
m <- lm(arenaTrend, data = soil.data)
step.lm <- step(m, direction = "both" )
summary(step.lm)
###########################################################
# Residual standard error: 9.487 on 172 degrees of freedom
# Multiple R-squared:  0.5987,	Adjusted R-squared:  0.5427 
# F-statistic: 10.69 on 24 and 172 DF,  p-value: < 2.2e-16
###########################################################
step.lm$call

trend <- ARENA ~ COOR_X_ETR + aspect_250m + GDD + LIBREHELADAS + 
  PMed_ABRIL + PMed_AGOSTO + PMed_JULIO + PMed_NOVIEMBRE + 
  PMed_SEPTIEMBRE + PMed_VERANO + RADIACION + TMed_FEBRERO + 
  TMed_JULIO + TMed_JUNIO + TMed_NOVIEMBRE + TMed_OCTUBRE + 
  TMed_SEPTIEMBRE + TMMAX_AGOSTO + TMMAX_DICIEMBRE + TMMAX_ENERO + 
  TMMAX_JULIO + TMMAX_JUNIO + TMMAX_NOVIEMBRE + TMMAX_SEPTIEMBRE

x11()
plot(step.lm$fitted.values, soil.data$ARENA, 
     xlab='Valores ajustados (% arena)', ylab='Valores observa (% arena)',
     main = 'Ajuste del modelo de regresión múltiple (% arena)',
     col = 'Red')
abline(0,1, lty = 2)

x11()
plot(step.lm$fitted.values, resid(step.lm), 
     xlab="Valores ajustados (% arena)", ylab = "Residuos (observados - ajustados) (%)",
     main= 'Dispersión de los residuos del modelo lineal', col = 'Red')
abline(0,0, lty=2)

x11()
hist(resid(step.lm), col='lightblue',
     main = 'Histograma de los residuos (arena)', xlab='Residuos', freq = FALSE)
x11()
qqnorm(resid(step.lm), col='Red', 
       xlab='Cuantiles teóricos', ylab='Cuantiles de la muestra',
       main = 'Normal Q-Q Plot (arena)')
qqline(resid(step.lm), lty=2)

summary(step.lm$fitted.values)


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


names(localizaciones)[names(localizaciones) == 'COOR_X_ETRS89'] <- 'COOR_X_ETR'
names(localizaciones)[names(localizaciones) == 'COOR_Y_ETRS89'] <- 'COOR_Y_ETR'

trend.Arena <- predict(step.lm, newdata = localizaciones)

localizaciones$ARENA_LM <- trend.Arena

lmArena <- localizaciones[,c("COOR_X_ETR","COOR_Y_ETR","ARENA_LM")]

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "LM_ARENA", lmArena)
dbDisconnect(connExp)













###############################################################
################  REGRESSION KRIGING ##########################
###############################################################

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




trend.Arena <- predict(step.lm, newdata = localizaciones)


coordinates(localizaciones)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(localizaciones)<-CRS(etrs89)

rkArena <- krige(lm_Residuals~1, locations = soil.data, newdata = localizaciones, 
                 model = vgFitResdArena, debug.level = -1)

rkArena$trend <- trend.Arena
rkArena$PREDICCION <- rkArena$trend + rkArena$var1.pred


baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)



rkArena<-as(rkArena,Class="data.frame")
dbWriteTable(connExp, "REGRESION_KRIGING_ARENA", rkArena)
dbDisconnect(connExp)










# interpolación con simple kriging
arena.Krig <- krige(formula = ARENA~1, locations = soil.data, newdata = localizaciones, model = vgFitClay, debug.level = -1)


localizaciones$SK_PRED <- arena.Krig@data$var1.pred
localizaciones$SK_VAR <- arena.Krig@data$var1.var
localizaciones$SK_SD <- sqrt(arena.Krig@data$var1.var)

summary(localizaciones$SK_PRED)
x11()
hist(localizaciones$SK_PRED)

summary(localizaciones$SK_SD)
x11()
hist(localizaciones$SK_SD)


baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

localizaciones<-as(localizaciones,Class="data.frame")
dbWriteTable(connExp, "SIMPLEKRIGE_ARENA", localizaciones)
dbDisconnect(connExp)








# Interpolación con Kriging

gsClay <- gstat(formula = ARENA~1, data = soil.data)


vgClay <- variogram(gsClay, cutoff = 30000, width = 2500)

x11()
plot(vgClay, plot.nu=FALSE)

n = 50
p = 150
r = 12000

# vgmClay <- vgm(nugget = n, psill = p, range = r, model = "Gau")
# vgmClay <- vgm(nugget = n, psill = p, range = r, model = "Exp")
vgmClay <- vgm(nugget = n, psill = p, range = r, model = "Sph")
x11()
plot(vgClay,vgmClay)

vgFitClay <- fit.variogram(vgClay,vgmClay)
attr(vgFitClay,"SSErr")


















