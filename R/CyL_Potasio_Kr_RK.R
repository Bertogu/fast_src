rm(list=ls()) # Clean the workspace
gc()

library(sqldf)
# library(rgdal)
# library(gdalUtils)
library(sp)
library(maptools)
library(caret)

library(gstat)
library(plyr)
library(ggplot2)

set.seed(32323)

etrs89<-"+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# baseDeDatos <- "/media/alberto/DATOS/Trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite"
baseDeDatos <- "D:/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite"

connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

# query<-"SELECT ID_MUESTRA, COOR_X_ETRS89, COOR_Y_ETRS89, P_INTERPOLA, CRAD_UK, ARCILLA, ARENA, 
# FC_UK, KSAT_UK, LIMO, MO, pH_RASTER, SAT_UK, WP_UK FROM FOSFORO_SAMPLES_COVARIANTS"

query <- "SELECT ID_MUESTRA, POTASIO_PPM, COOR_X_ETRS89, COOR_Y_ETRS89, ARCILLA, ARENA, ETP, FC_UK, GDD, KSAT_UK, LIBREHELADAS, LIMO, mde_250m, MO, pH_RASTER, PMed_ABRIL, PMed_AGOSTO, PMed_ANUAL, PMed_DICIEMBRE, PMed_ENERO, PMed_FEBRERO, PMed_INVIERNO, PMed_JUNIO, PMed_MARZO, PMed_MAYO, PMed_NOVIEMBRE, PMed_OCTUBRE, PMed_PRIMAVERA, PMed_SEPTIEMBRE, PMed_VERANO, RADIACION, Rug_250, SAT_UK, slope_250m, TIERRA_ARABLE, TMed_ABRIL, TMed_AGOSTO, TMed_DICIEMBRE, TMed_ENERO, TMed_JULIO, TMed_JUNIO, TMed_MARZO, TMed_MAYO, TMed_NOVIEMBRE, TMed_OCTUBRE, TMed_SEPTIEMBRE, TMMAX_ABRIL, TMMAX_AGOSTO, TMMAX_DICIEMBRE, TMMAX_ENERO, TMMAX_FEBRERO, TMMAX_JULIO, TMMAX_JUNIO, TMMAX_MARZO, TMMAX_MAYO, TMMAX_NOVIEMBRE, TMMAX_OCTUBRE, TMMAX_SEPTIEMBRE, WP_UK, CRAD_UK, PMed_JULIO, TMed_FEBRERO FROM POTASIO_SAMPLES_COVARIANTS"

soil.data <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

dbDisconnect(connExp)
str(soil.data)

summary(soil.data$POTASIO_PPM)

# x11()
hist(soil.data$POTASIO_PPM, col='Lightblue', 
     main = 'Histograma ppm potasio', 
     xlab = 'Potasio (ppm)', ylab = 'Frecuencia', freq = TRUE)
abline(v=mean(soil.data$POTASIO_PPM), col='Red', lty=2)
abline(v=median(soil.data$POTASIO_PPM), col='grey', lty=2)

# x11()
qqnorm(soil.data$POTASIO_PPM, col='Red', 
       xlab='Cuantiles teoricos', ylab='Cuantiles de la muestra',
       main = 'Q-Q plot de normalidad\npotasio(ppm)')
qqline(soil.data$POTASIO_PPM, lty=2)

summary(log1p(soil.data$POTASIO_PPM))

soil.data$logK <- log1p(soil.data$POTASIO_PPM)

# x11()
hist(soil.data$logK, col='Lightblue',
     main = 'Histograma potasio transformado \n(logaritmo ppm)', 
     xlab = 'Potasio (ppm)', ylab='Frecuencia', freq = TRUE)
abline(v=mean(soil.data$logK), col='Red', lty=2)
abline(v=c(mean(soil.data$logK)-sd(soil.data$logK),mean(soil.data$logK)+sd(soil.data$logK))
       ,col='grey', lty=2)

# x11()
qqnorm(soil.data$logK, col='Red', main = 'Q-Q plot normalidad potasio transformado\n(logaritmo ppm)')
qqline(soil.data$logK, lty=2)


inTrain <- createDataPartition(y = soil.data$logK, p = 0.8, list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]
nrow(soil.data)
nrow(soilData.test)

# names(soil.data)

############################################################
#################  SIMPLE KRIGING ##########################
############################################################


coordinates(soil.data)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(soil.data)<-CRS(etrs89)


c = 150000
w = 1500

vlogK <- variogram(logK~1, data = soil.data, cutoff=c, width = w)

x11()
plot(vlogK)


n = 0.21
p = 0.18
r = 77000

vgmLogK <- vgm(nugget = n, psill = p, range = r, model = "Sph")
# x11()
plot(vlogK,vgmLogK, col = 'Red',
     main='Semivariograma del potasio \n(trans. logarítmica)',
     xlab = 'Distancia', ylab='Semivarianza')

vgFitLogK <- fit.variogram(vlogK,vgmLogK)
attr(vgFitLogK,"SSErr")


logK.CV <- krige.cv(formula = logK~1, soil.data, model=vgFitLogK ,nfold=10)

logK.CV$K_PREDICT_bk <- expm1(logK.CV$var1.pred + 0.5*logK.CV$var1.var)
logK.CV$K_INTERPOLA <- soil.data$POTASIO_PPM


# x11()
plot(x=logK.CV$var1.pred, y=logK.CV$observed, col='red',
     main='Ajuste del modelo con los datos de entrenamiento\nMétodo: Ordinary Kriging\n(logaritmo ppm de potasio)',
     xlab = 'log. ppm de potasio estimado', ylab='log. ppm de potasio observado')
abline(c(0,1), lty=2)
cor.test(x=logK.CV$observed, y= logK.CV$var1.pred, alternative = "two.sided", conf.level = 0.95, method = "pearson")

r<-RMSE(obs = logK.CV$observed, pred = logK.CV$var1.pred)
# x11()
plot(x=logK.CV$var1.pred, y=logK.CV$residual, col='red',
     main='Dispersión de los residuos\nMétodo: Ordinary Kriging\n(logaritmo ppm de potasio)',
     xlab='log. ppm de f?potasio estimado', ylab = 'Residuos')
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)



# x11()
plot(x=logK.CV$K_PREDICT_bk, y=logK.CV$K_INTERPOLA, col='red',
     main='Ajuste del modelo con los datos de entrenamiento\nMétodo: Ordinary Kriging\n(ppm P)',
     xlab = 'ppm de K estimado', ylab='ppm de K observado')
abline(c(0,1), lty=2)
cor.test(x=logK.CV$K_PREDICT_bk, y= logK.CV$K_INTERPOLA, alternative = "two.sided", conf.level = 0.95, method = "pearson")



logK.CV$Residos_bk <- logK.CV$K_INTERPOLA - logK.CV$K_PREDICT_bk
r<-RMSE(obs = logK.CV$K_INTERPOLA, pred = logK.CV$K_PREDICT_bk)
# x11()
plot(x=logK.CV$K_PREDICT_bk, y=logK.CV$Residos_bk, col='red',
     main='Dispersión de los residuos\nMétodo: Ordinary Kriging\n(ppm de K)',
     xlab='ppm de K estimado', ylab = 'Residuos')
abline(h=0, lty=2)
abline(h=c(-1*r,r), col='grey', lty=2)
r
hist(logK.CV$Residos_bk)



# Cargo los datos con las localizaciones para la interpolaci?n

# baseDeDatos <- "/media/alberto/DATOS/Trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite"
baseDeDatos <- "D:/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite"

connExp <- dbConnect(SQLite(), dbname = baseDeDatos)
query<-"SELECT COOR_X_ETRS89, COOR_Y_ETRS89 FROM PTOS_INTERPOLAR"


localizaciones <- as.data.frame(fetch(dbSendQuery(conn = connExp, query), n=-1))

dbDisconnect(connExp)

# localizaciones[,][localizaciones[,] == -9999.000] <- NA
# localizaciones = localizaciones[complete.cases(localizaciones),]
# 
# names(localizaciones)[names(localizaciones)=='COOR_X_ETRS89'] <- "COOR_X_ETR"
# names(localizaciones)[names(localizaciones)=='COOR_Y_ETRS89'] <- "COOR_Y_ETR"

coordinates(localizaciones)<- ~COOR_X_ETRS89 + COOR_Y_ETRS89
proj4string(localizaciones)<-CRS(etrs89)


kLogK <- krige(logK~1, locations = soil.data, newdata = localizaciones, 
               model = vgFitLogK, debug.level = -1)

str(kLogK)

kLogK$K_PREDICT <- expm1(kLogK$var1.pred)
kLogK$K_PREDICT_CORR <- expm1(kLogK$var1.pred + 0.5*kLogK$var1.var)

str(kLogP)
DF.kLogK <- as(kLogK, "data.frame")

# baseDeDatos <- "/media/alberto/DATOS/Trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite"
baseDeDatos <- "D:/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "OK_POTASIO_new", DF.kLogk[,c('COOR_X_ETR','COOR_Y_ETR','P_PREDICT','P_PREDICT_CORR')])
dbWriteTable(connExp, "OK_POTASIO_250m", DF.kLogk)
dbDisconnect(connExp)


############################################################


############################################################
############## REGRESION LINEAL MULTIPLE ###################
############################################################

inTrain <- createDataPartition(y = soil.data$P_INTERPOLA, p = 0.8, list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]


# TRENDS SIN COORDENADAS
# P_trend <- Fosforo_pp~aspect_250m+ ETP+ GDD+ LIBREHELADAS+ mde_250m+ PMed_ABRIL+ PMed_AGOSTO+ PMed_ANUAL+ PMed_DICIEMBRE+ PMed_ENERO+ PMed_FEBRERO+ PMed_INVIERNO+ PMed_JULIO+ PMed_JUNIO+ PMed_MARZO+ PMed_MAYO+ PMed_NOVIEMBRE+ PMed_OCTUBRE+ PMed_PRIMAVERA+ PMed_SEPTIEMBRE+ PMed_VERANO+ RADIACION+ Rug_250+ slope_250m+ TIERRA_ARABLE+ TMed_ABRIL+ TMed_AGOSTO+ TMed_DICIEMBRE+ TMed_ENERO+ TMed_FEBRERO+ TMed_JULIO+ TMed_JUNIO+ TMed_MARZO+ TMed_MAYO+ TMed_NOVIEMBRE+ TMed_OCTUBRE+ TMed_SEPTIEMBRE+ TMMAX_ABRIL+ TMMAX_AGOSTO+ TMMAX_DICIEMBRE+ TMMAX_ENERO+ TMMAX_FEBRERO+ TMMAX_JULIO+ TMMAX_JUNIO+ TMMAX_MARZO+ TMMAX_MAYO+ TMMAX_NOVIEMBRE+ TMMAX_OCTUBRE+ TMMAX_SEPTIEMBRE

# P_trend <- P_INTERPOLA ~ ARCILLA + ARENA + LIMO + MO + FC_UK + KSAT_UK +CRAD_UK + SAT_UK + WP_UK + pH_RASTER

P_trend <- P_INTERPOLA ~ COOR_X_ETRS89+COOR_Y_ETRS89+ARCILLA+ARENA+ETP+FC_UK+GDD+KSAT_UK+LIBREHELADAS+LIMO+mde_250m+MO+pH_RASTER+PMed_ABRIL+PMed_AGOSTO+PMed_ANUAL+PMed_DICIEMBRE+PMed_ENERO+PMed_FEBRERO+PMed_INVIERNO+PMed_JUNIO+PMed_MARZO+PMed_MAYO +PMed_NOVIEMBRE+PMed_OCTUBRE+PMed_PRIMAVERA+PMed_SEPTIEMBRE+PMed_VERANO+RADIACION+Rug_250+SAT_UK+slope_250m+TIERRA_ARABLE+TMed_ABRIL+TMed_AGOSTO+TMed_DICIEMBRE+TMed_ENERO+TMed_JULIO+TMed_JUNIO+TMed_MARZO+TMed_MAYO+TMed_NOVIEMBRE+TMed_OCTUBRE+TMed_SEPTIEMBRE+TMMAX_ABRIL+TMMAX_AGOSTO+TMMAX_DICIEMBRE+TMMAX_ENERO+TMMAX_FEBRERO+TMMAX_JULIO+TMMAX_JUNIO+TMMAX_MARZO+TMMAX_MAYO+TMMAX_NOVIEMBRE+TMMAX_OCTUBRE+TMMAX_SEPTIEMBRE+WP_UK+CRAD_UK+PMed_JULIO+TMed_FEBRERO

# P_trend <- COOR_X_ETRS89 + COOR_Y_ETRS89 + CRAD_UK + ARCILLA + ARENA + FC_UK + KSAT_UK + LIMO + MO + pH_RASTER + SAT_UK + WP_UK

m <- lm(P_trend, data = soil.data)
step.lm <- step(m, direction = "both" )
summary(step.lm)
#####################################################
# Residual standard error: 19.81 on 174 degrees of freedom
# Multiple R-squared:  0.3756,	Adjusted R-squared:  0.293 
# F-statistic:  4.55 on 23 and 174 DF,  p-value: 2.59e-09
#####################################################
step.lm$call


gml.P <- glm(P_trend, family= Gamma, data = soil.data)
step.gml.P <- step(m, direction = "both" )
summary(step.gml.P)


x11()
plot(step.gml.P$fitted.values, soil.data$P_INTERPOLA, xlab='Fitted (%)', ylab="observed (%)")
abline(0,1)
x11()
hist(resid(step.gml.P))

cor(step.gml.P$fitted.values, soil.data$P_INTERPOLA)^2









# TREND CON COORDENADAS
P_trend <- Fosforo_pp~COOR_X_ETR+ COOR_Y_ETR+aspect_250m+ ETP+ GDD+ LIBREHELADAS+ mde_250m+ PMed_ABRIL+ PMed_AGOSTO+ PMed_ANUAL+ PMed_DICIEMBRE+ PMed_ENERO+ PMed_FEBRERO+ PMed_INVIERNO+ PMed_JULIO+ PMed_JUNIO+ PMed_MARZO+ PMed_MAYO+ PMed_NOVIEMBRE+ PMed_OCTUBRE+ PMed_PRIMAVERA+ PMed_SEPTIEMBRE+ PMed_VERANO+ RADIACION+ Rug_250+ slope_250m+ TIERRA_ARABLE+ TMed_ABRIL+ TMed_AGOSTO+ TMed_DICIEMBRE+ TMed_ENERO+ TMed_FEBRERO+ TMed_JULIO+ TMed_JUNIO+ TMed_MARZO+ TMed_MAYO+ TMed_NOVIEMBRE+ TMed_OCTUBRE+ TMed_SEPTIEMBRE+ TMMAX_ABRIL+ TMMAX_AGOSTO+ TMMAX_DICIEMBRE+ TMMAX_ENERO+ TMMAX_FEBRERO+ TMMAX_JULIO+ TMMAX_JUNIO+ TMMAX_MARZO+ TMMAX_MAYO+ TMMAX_NOVIEMBRE+ TMMAX_OCTUBRE+ TMMAX_SEPTIEMBRE

m <- lm(P_trend, data = soil.data)
step.lm <- step(m, direction = "both" )
summary(step.lm)
#####################################################
# Residual standard error: 19.84 on 173 degrees of freedom
# Multiple R-squared:  0.3772,	Adjusted R-squared:  0.2908 
# F-statistic: 4.365 on 24 and 173 DF,  p-value: 4.753e-09
#####################################################

############################################################






### graficas del ajuste de la regresi?n lineal m?ltiple
x11()
plot(step.lm$fitted.values, soil.data$Fosforo_pp, xlab='Fitted (%)', ylab="observed (%)")
abline(0,1)
x11()
hist(resid(step.lm))

cor(step.lm$fitted.values, soil.data$ARENA)^2


x11()
qqnorm(resid(step.lm))
qqline(resid(step.lm))

x11()
plot(step.lm$fitted.values, resid(step.lm), xlab="Fitted (%)", ylab = "Residual (%)")
abline(0,0)

soil.data$lm_Residuals <- step.lm$residuals

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)

# El variograma de los residuos no muestra ning?n tipo de correlaci?n espacial

sqrt((473154.3736-58277.8522)^2 + (4589838.89-4517413.19)^2)/4

c = 22000
w = 2000
vResdArena <- variogram(lm_Residuals~1, data = soil.data, cutoff = c, width = w)

x11()
plot(vResdArena)

n = 58
p = 22
r = 10000
# vgmResidArena <- vgm(nugget = n, psill = p, range = r, model = "Gau")
# x11()
# plot(vArena,vgmResidArena)

# vgmResidArena <- vgm(nugget = n, psill = p, range = r, model = "Exp")
# x11()
# plot(vArena,vgmResidArena)

vgmResidArena <- vgm(nugget = n, psill = p, range = r, model = "Sph")
x11()
plot(vResdArena,vgmResidArena)

vgFitResdArena <- fit.variogram(vResdArena,vgmResidArena)
attr(vgFitResdArena,"SSErr")

###############################################################
################  REGRESSION KRIGING ##########################
###############################################################

# Cargo los datos con las localizaciones para la interpolaci?n

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



# interpolaci?n con simple kriging
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








# Interpolaci?n con Kriging

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




