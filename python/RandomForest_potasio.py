#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 18 10:07:02 2020

@author: alberto
"""

import sys
# import gdal
import geopandas as gpd
import pandas as pd
import numpy as np
import scipy 
import matplotlib as plt
import time

# import rasterio
import sqlite3

# from affine import Affine
from os import listdir
from os.path import isfile, join

# import pykrige.kriging_tools as kt
# from pykrige.ok import OrdinaryKriging

# import shapely.wkb
from shapely.wkt import loads, dumps
import sklearn.metrics as metrics
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor







# =============================================================================
# DEFINICIÓN DE LAS FUNCIONES
# =============================================================================




def FromSpatialite2PandasGeoPandas(_dbIn, _sql, _geoDF = False , _geoField = 'GEO_WKT', _epsg = 'EPSG:25830'):
    '''
    Lee una tabla de la base de datos spatialite. Puede ser Geográfica o no.
    Si es Geo se carga el módulo espacial para las funciones st.
    Si no la carga es normal
    Arg:
        - _dbIn (str): base de datos
        - _sql (str): sentencia sql para leer las entidades que sea
        - _geoDF (bool): Si la tabla se volcará en un geoPandasDF
        - _geoField (str): el campo de geometría. Tiene que ser tipo texto.
        - _epsg (str): SRS en forma "EPSG:XXXX"
    Return:
        - DataFrame
        o
        - GeoPandasDataframe con los datos
    '''
    
    conexion = sqlite3.connect(_dbIn)
    
    if _geoDF == True:
        
        conexion.enable_load_extension(True)
        conexion.execute("SELECT load_extension('mod_spatialite')") 
        
        selectedFOIs = pd.read_sql_query(_sql, conexion)
        
        selectedFOIs[_geoField] = selectedFOIs[_geoField].apply(loads)
        selectedFOIs = gpd.GeoDataFrame(selectedFOIs, geometry=_geoField)
        selectedFOIs.crs = _epsg
        
    else:
        selectedFOIs = pd.read_sql_query(_sql, conexion)
        
    
    return selectedFOIs


def devuelveConexionSpatialite(_dbIn):
    '''
    Connect to spatialite database enabling the spatial module

    Parameters
    ----------
    _dbIn : str
        str with the path of the db.

    Returns
    -------
    conexion : connection
        the connection to the datebase.

    '''
    
    conexion = sqlite3.connect(_dbIn)
    conexion.enable_load_extension(True)
    conexion.execute("SELECT load_extension('mod_spatialite')") 
    
    return conexion




if __name__ == '__main__':
    
    
# =============================================================================
    dbIn = 'D:/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'
# =============================================================================

# =============================================================================
    # dbIn = '/media/alberto/DATOS/Trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'
# =============================================================================

    sqlSentence = "SELECT ID_MUESTRA, POTASIO_PPM, COOR_X_ETRS89, COOR_Y_ETRS89, ARCILLA, ARENA, ETP, FC_UK, GDD, KSAT_UK, LIBREHELADAS, LIMO, mde_250m, MO, pH_RASTER, PMed_ABRIL, PMed_AGOSTO, PMed_ANUAL, PMed_DICIEMBRE, PMed_ENERO, PMed_FEBRERO, PMed_INVIERNO, PMed_JUNIO, PMed_MARZO, PMed_MAYO, PMed_NOVIEMBRE, PMed_OCTUBRE, PMed_PRIMAVERA, PMed_SEPTIEMBRE, PMed_VERANO, RADIACION, Rug_250, SAT_UK, slope_250m, TIERRA_ARABLE, TMed_ABRIL, TMed_AGOSTO, TMed_DICIEMBRE, TMed_ENERO, TMed_JULIO, TMed_JUNIO, TMed_MARZO, TMed_MAYO, TMed_NOVIEMBRE, TMed_OCTUBRE, TMed_SEPTIEMBRE, TMMAX_ABRIL, TMMAX_AGOSTO, TMMAX_DICIEMBRE, TMMAX_ENERO, TMMAX_FEBRERO, TMMAX_JULIO, TMMAX_JUNIO, TMMAX_MARZO, TMMAX_MAYO, TMMAX_NOVIEMBRE, TMMAX_OCTUBRE, TMMAX_SEPTIEMBRE, WP_UK, CRAD_UK, PMed_JULIO, TMed_FEBRERO FROM POTASIO_SAMPLES_COVARIANTS"
    
    # Cargo los datos
    pd_rdmForest = FromSpatialite2PandasGeoPandas(dbIn, sqlSentence, False)
    
    # La variable a predecir y las covariantes
    y_k_ppm = pd_rdmForest.loc[:,'POTASIO_PPM'].values
    X_covariants = pd_rdmForest.iloc[:,2:].values
    
    # Dividir los datos en train y test. En este caso 70, 30.
    X_train, X_test, y_train, y_test = train_test_split(X_covariants, y_k_ppm, test_size = 0.3, random_state=0)
    
    # escalo las covariantes
    sc = StandardScaler()
    X_train = sc.fit_transform(X_train)
    X_test = sc.fit_transform(X_test)
    
    
# =============================================================================
#     # Bucle para establecer el nº de árboles en randomforest
#     # lstError = []
#     # for i in range(2001,5201,200):
#         
#     #     # se prueban desde 100 árboles hasta 5001        
#     #     start_time = time.time()
# 
#     #     # Define los parámetros para crear el Random forest y lo aplica a los datos
#     #     # de entrenamiento
#     #     regressor = RandomForestRegressor(n_estimators = i, random_state= 0)
#     #     regressor.fit(X_train, y_train)
#         
#     #     # Aplico el modelo a los datos de test
#     #     y_pred = regressor.predict(X_test)
#         
#         
#     #     lstError.append((i, metrics.mean_absolute_error(y_test, y_pred), 
#     #                       np.sqrt(metrics.mean_squared_error(y_test, y_pred)),time.time()-start_time))
#         
#     # dfArbolesError = pd.DataFrame.from_records(lstError, columns=['N_ARBOLES','ERROR_ABSOLUTO', 'RMSE','TIEMPO_PROCESO'])    
# =============================================================================
    


    # El RMSE menor se obtiene con 1500 árboles
    # Genero el modelo con 1501 árboles
    regressor = RandomForestRegressor(n_estimators = 1501, random_state= 0)
    regressor.fit(X_train, y_train)
    
    # Aplico el modelo a los datos de test
    y_pred = regressor.predict(X_test)
    
    ErrorAbsolutoMedio_test = metrics.mean_absolute_error(y_test, y_pred) 
    ErrorCuadradoMedio_test = metrics.mean_squared_error(y_test, y_pred)
    RMSE_test = np.sqrt(metrics.mean_squared_error(y_test, y_pred))   


    sqlLocations = "SELECT COOR_X_ETRS89,COOR_Y_ETRS89,ARCILLA,ARENA,CRAD_UK,ETP,FC_UK,GDD,KSAT_UK,LIBREHELADAS,LIMO,mde_250m,MO,pH_RASTER,PMed_ABRIL,PMed_AGOSTO,PMed_ANUAL,PMed_DICIEMBRE,PMed_ENERO,PMed_FEBRERO,PMed_INVIERNO,PMed_JULIO,PMed_JUNIO,PMed_MARZO,PMed_MAYO,PMed_NOVIEMBRE,PMed_OCTUBRE,PMed_PRIMAVERA,PMed_SEPTIEMBRE,PMed_VERANO,RADIACION,Rug_250,SAT_UK,slope_250m,TIERRA_ARABLE,TMed_ABRIL,TMed_AGOSTO,TMed_DICIEMBRE,TMed_ENERO,TMed_FEBRERO,TMed_JULIO,TMed_JUNIO,TMed_MARZO,TMed_MAYO,TMed_NOVIEMBRE,TMed_OCTUBRE,TMed_SEPTIEMBRE,TMMAX_ABRIL,TMMAX_AGOSTO,TMMAX_DICIEMBRE,TMMAX_ENERO,TMMAX_FEBRERO,TMMAX_JULIO,TMMAX_JUNIO,TMMAX_MARZO,TMMAX_MAYO,TMMAX_NOVIEMBRE,TMMAX_OCTUBRE,TMMAX_SEPTIEMBRE,WP_UK FROM LOCATION_COVARIANTS"
    
    # Cargo los datos
    pd_rdmForest_locations = FromSpatialite2PandasGeoPandas(dbIn, sqlLocations, False)
    
    pd_rdmForest_locations.columns
    
    Covar_CyL = pd_rdmForest_locations.loc[:,:].values
    
    k_pred_CyL = regressor.predict(Covar_CyL)
    
    
    k_pred_CyL2db = pd_rdmForest_locations.loc[:,['COOR_X_ETRS89','COOR_Y_ETRS89']]
    k_pred_CyL2db['K_PREDICT_RF'] = k_pred_CyL
    
    k_pred_CyL2db.loc[:,'K_PREDICT_RF'].describe()
    
    
    k_pred_CyL2db.to_sql('RF_POTASIO_COOR_SIN_STANDARD', devuelveConexionSpatialite(dbIn), if_exists='replace', index=False)
    











