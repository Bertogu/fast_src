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


inTrain <- createDataPartition(y = soil.data$ARENA, p = 0.8, list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]

arena <- soil.data$ARENA
covariants <- soil.data[,5:55]

length(names(soil.data[,5:55]))/3

rf <- randomForest(x = covariants, y= arena, ntree = 5001, mtry = 4, importance = TRUE)


for (i in 4:17){
  rf <- randomForest(x = covariants, y= arena, ntree = 5001, mtry = i, importance = TRUE)
  residuos <- (rf$y - rf$predicted)
  mean.squared.residuals <- mean(residuos^2)
  vari.expli <- 1-((var(residuos))/((var(rf$y))))
  
  print(rf)
  print(mean.squared.residuals)
  print(vari.expli)

}
i=13

for (a in seq(501,5001,500)){
  rf <- randomForest(x = covariants, y= arena, ntree = a, mtry = 13, importance = TRUE)
  print(rf)
  
}
a=3501


rf <- randomForest(x = covariants, y= arena, ntree = 3501, mtry = 13, importance = TRUE)
rf





str(rf, max.level = 2)

x11()
varImpPlot(rf, main='Random Forest: Importancia de las variables (% arena)')
summary(rf)

x11()
plot(rf$predicted, rf$y, main="Ajuste del modelo con los datos de entrenamiento\nMétodo: Random forest\n(% arena)",
     xlab="Valores ajustados (% Arena)", ylab = "Valores observados (% Arena)",
     col = "Red")
abline(0,1, lty=2)
cor.test(x=rf$predicted, y= rf$y, alternative = "two.sided", conf.level = 0.95, method = "pearson")



rf$Residuos <- rf$y - rf$predicted
er<-RMSE(obs = rf$y, pred = rf$predicted)
x11()
plot(x = rf$predicted, y = rf$Residuos, main= "Estudio de residuos\nMétodo: Random forest\n(% arena)",
     col = "Red", xlab = "Valores estimados (% arena)", ylab = "Residuos")
abline(h=0, lty=2)
abline(h=c(-1*er,er), lty=2, col='grey')

summary(rf$Residuos)
summary(rf.Filtrado$Residuos)

x11()
plot(x = rf.Filtrado$predicted, y = rf.Filtrado$Residuos, main= "Estudio de residuos\nMétodo: Random forest\n(% arena)",
     col = "Red", xlab = "Valores estimados (% arena)", ylab = "Residuos")
abline(h=0, lty=2)

# p.rf <- predict(rf, newdata = soilData.test)
# soilData.test$PREDICHO_RF <- p.rf
# 
# RMSE(obs = soilData.test$ARCILLA,pred = soilData.test$PREDICHO_RF)


# compruebo si hay correlación espacial en los residuos
soil.data$RESIDUOS <- rf$y - rf$predicted
names(soil.data)

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)
str(soil.data)

v <- variogram(RESIDUOS~1, data = soil.data)
x11()
plot(v, col="Red", main="Semivariograma de los residuos random forest (% arena)\nMétodo: Random Forest",
     xlab = 'Distancia', ylab = 'Semivarianza')


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

Sg_Arena.rf <- predict(rf, newdata = localizaciones)

localizaciones$PREDIC_RF <- Sg_Arena.rf


summary(localizaciones$PREDIC_RF)
x11()
hist(localizaciones$PREDIC_RF)

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "RF_COOR_ARENA", localizaciones[,c("COOR_X_ETR","COOR_Y_ETR","PREDIC_RF")])
dbDisconnect(connExp)




















