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

# Elimino las muestras que son anormales
soil.data <- soil.data[soil.data$ID_MUESTRA != 244,] #tiene una suma de textura mayor que 100.10
soil.data <- soil.data[soil.data$ARCILLA >= 2,]


inTrain <- createDataPartition(y = soil.data$ARCILLA, p = 0.80,list = FALSE)
soil.data <- soil.data[inTrain,]
soilData.test <- soil.data[-inTrain,]
names(soil.data)
arcilla <- soil.data$ARCILLA
covariants <- soil.data[,5:55]



#############################################################
# df.prec.RF <- data.frame(nVariable = integer(),
#                          nArboles=integer(),
#                          MeanSqErr = double(),
#                          VariExpli = double(),
#                          CoefCor = double(),
#                          pValue = double())
# n=1
# for (i in 15:19){
#   for (j in seq(501,7001,500)){
#     print (paste(n,i,j, sep = ";"))
#     rf <- randomForest(x = covariants, y= arcilla, ntree = j, mtry = i, importance = TRUE)
#     residuos <- (rf$y - rf$predicted)
#     mean.squared.residuals <- mean(residuos^2)
#     vari.expli <- 1-((var(residuos))/((var(rf$y))))
# 
#     coef.corre <- cor.test(x=rf$predicted, y= rf$y, alternative = "two.sided", conf.level = 0.95, method = "pearson")
#     correla <- coef.corre$estimate
#     pValor <- coef.corre$p.value
#     df.prec.RF[n,]<-c(i,j,mean.squared.residuals,vari.expli,correla,pValor)
#     n=n+1
# 
#   }
# }
##############################################################################


rf <- randomForest(x = covariants, y= arcilla, ntree = 4501, mtry = 15, importance = TRUE)
rf

cor.test(x=rf$predicted, y= rf$y, alternative = "two.sided", conf.level = 0.95, method = "pearson")


str(rf, max.level = 2)
x11()
varImpPlot(rf, main="Random Forest: importancia de las variables (% arcilla)")
importance(rf)

x11()
plot(rf$predicted, rf$y, main="Ajuste del modelo con los datos de entrenamiento\nMétodo: Random Forest\n(% arcilla)",
     xlab="Valores estimados (% arcilla)", ylab = "Valores observados (% arcilla)",
     col = "Red")
abline(0,1, lty=2)

rf$Residuos <- rf$y - rf$predicted
r<-RMSE(obs = rf$y,pred = rf$predicted)
x11()
plot(x = rf$predicted, y = rf$Residuos, main= "Dispersión de los residuos\nMétodo: Random Forest\n(% arcilla)",
     col = "Red", xlab = "Valores estimados (% arcilla)", ylab = "Residuos")
abline(h=0, lty=2)
abline(h=c(-1*r,r),col='grey', lty=2)

cor(rf$predicted, rf$y)^2
RMSE(obs = rf$y,pred = rf$predicted)

p.rf <- predict(rf, newdata = soilData.test)
soilData.test$PREDICHO_RF <- p.rf
nrow(soilData.test)
RMSE(obs = soilData.test$ARCILLA,pred = soilData.test$PREDICHO_RF)


# compruebo si hay correlación espacial en los residuos
soil.data <- as(soil.data, "data.frame")

soil.data$RESIDUOS <- rf$y - rf$predicted
names(soil.data)

coordinates(soil.data)<- ~COOR_X_ETR + COOR_Y_ETR
proj4string(soil.data)<-CRS(etrs89)
str(soil.data)


v <- variogram(RESIDUOS~1, data = soil.data)
x11()
plot(v, col = "Red", main="Semivariograma de los residuos\nMétodo: Random Forest\n(% arcilla)",
     xlab = "Distancia", ylab="Semivarianza")



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

Sg_Arcilla.rf <- predict(rf, newdata = localizaciones)


localizaciones$PREDIC_RF <- Sg_Arcilla.rf

names(localizaciones)

baseDeDatos <- "../../DB/CampoSegoviano_Suelos.sqlite"
connExp <- dbConnect(SQLite(), dbname = baseDeDatos)

dbWriteTable(connExp, "RF_COOR_ARCILLA", localizaciones[,c("COOR_X_ETR","COOR_Y_ETR","PREDIC_RF")])
dbDisconnect(connExp)





