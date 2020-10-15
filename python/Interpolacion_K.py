#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct  9 18:22:50 2020

@author: alberto
"""

# =============================================================================
# IMPORTAR BIBLIOTECAS
# =============================================================================

import sys
import gdal
import geopandas as gpd
import pandas as pd
import numpy as np
import scipy 
import matplotlib as plt

import rasterio
import sqlite3


from affine import Affine
from os import listdir
from os.path import isfile, join

# import pykrige.kriging_tools as kt
# from pykrige.ok import OrdinaryKriging

import shapely.wkb
from shapely.wkt import loads, dumps
# import sklearn.metrics as metrics



# =============================================================================
# DEFINICIÓN DE LAS FUNCIONES
# =============================================================================

def world2Pixel(geoMatrix, x, y):
    
  """
  Uses a gdal geomatrix (gdal.GetGeoTransform()) to calculate
  the pixel location of a geospatial coordinate
  """
  ulX = geoMatrix[0]
  ulY = geoMatrix[3]
  xDist = geoMatrix[1]
  yDist = geoMatrix[5]
  rtnX = geoMatrix[2]
  rtnY = geoMatrix[4]
  columna = int((x - ulX) / xDist)
  fila = int((ulY - y) / xDist)
  return (fila, columna)

def pixel2World(geoMatrix, f, c): 
    
    ulX = geoMatrix[0]
    ulY = geoMatrix[3]
    xDist = geoMatrix[1]
    yDist = geoMatrix[5]
    rtnX = geoMatrix[2]
    rtnY = geoMatrix[4]
    Xmundo = (ulX + ((c + 1) * xDist)) - xDist/2
    Ymundo = (ulY + (f * yDist)) + yDist/2
    
    return (int(Xmundo), int(Ymundo))
    print ('hola')


def leeFoisFromSpatialite(_dbIn, _sql):
    # dbIn = '/media/alberto/DATOS/Trabajo/Monitor_2020/data/BD/FOIS_MNT20_20200420_GEO.sqlite'
    conexion = sqlite3.connect(_dbIn)
    conexion.enable_load_extension(True)
    conexion.execute("SELECT load_extension('mod_spatialite')") 
    # sqlSentence = "SELECT FOI_ID, COD_CULTIVO_MONITOR, AREA, ASWKT(Geometry) AS GEO_WKT FROM MNT20_FOI_BUFFER_POLI WHERE FOI_ID = 2020565667"
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


def FromSpatialite2GeoPandas(_dbIn, _sql, _geoField, _epsg):
    '''
    Lee una tabla geográfica de la base de datos spatialite.
    Por eso es necesario cargar el módulo espacial.
    Arg:
        - _dbIn (str): base de datos
        - _sql (str): sentencia sql para leer las entidades que sea
        - _geoField (str): el campo de geometría. Tiene que ser tipo texto.
        - _epsg (str): SRS en forma "EPSG:XXXX"
    Return:
        - GeoPandasDataframe con los datos
    '''
    
    conexion = sqlite3.connect(_dbIn)
    conexion.enable_load_extension(True)
    conexion.execute("SELECT load_extension('mod_spatialite')") 
    
    selectedFOIs = pd.read_sql_query(_sql, conexion)

    selectedFOIs[_geoField] = selectedFOIs[_geoField].apply(loads)
    selectedFOIs = gpd.GeoDataFrame(selectedFOIs, geometry=_geoField)
    selectedFOIs.crs = _epsg
    
    
    return selectedFOIs




def GetImgsValueFromXYCoor(_dirImg, _lstImgNames, _gpdPtos, _geoField):
    '''
    Escribe en el geopandasDataframe un campo por cada imagen en la lista
    con el valor del pixel correspondiente a la coordenada X Y de la muestra.
    Dependencies:
        - world2Pixel()
    Args:
        - _dirImg (str): directorio con la/s imágen/es, 
        - _lstImgNames (lst): lista con el nombre de las imágenes
        - _gpdPtos (gpd): gpdf almenos con un campo con la geometría (puntos)
        - _geoField (str): campo de _gpdPtos que contiene la geometría
    Returns:
        - gpddf: el mismo gpdf con las columnas añadidas para cada imagen
    
    '''
    
    for img in _lstImgNames:

        imgNDVI_1 = gdal.Open(_dirImg+img)
        
        geoInformacion = imgNDVI_1.GetGeoTransform()

        ArNDVI_1 = imgNDVI_1.GetRasterBand(1).ReadAsArray()
        
        print(img)
        
        pds_FC = _gpdPtos[_geoField].apply(lambda t: world2Pixel(geoInformacion, t.x, t.y))
        _gpdPtos[img.split('.')[0]]=pds_FC.apply(lambda x: ArNDVI_1[x[0],x[1]])
    
    return _gpdPtos



# =============================================================================


if __name__ == '__main__':
    
    
# =============================================================================
    # dbIn = 'D:/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'
    # dirImagenes = 'D:/FaST_2020/Data/Raster/interpola/'
# =============================================================================

    dbIn = '/media/alberto/DATOS/Trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'
    dirImagenes = '/media/alberto/DATOS/Trabajo/FaST_2020/Data/Raster/'
    
    
    sqlSentence = "SELECT ID_MUESTRA, ORIGEN, SEASON, LABORATORIO, MO_PORC, MATERIA_ORGANICA, ARENA_PORC, LIMO_PORC, ARCILLA_PORC, TEXTURA, pH, ACIDEZ_BASICIDAD, CARBONATOS_PORC, NITROGENO_PORC, POTASIO_PPM, POTASIO, CALCIO_PPM, MAGNESIO_PPM, SODIO_PPM, COOR_X_ETRS89, COOR_Y_ETRS89, PUBLICOS, TEXTCALCU, GRUPO_TEXTURA, TIPO, P_OLSEN_PPM, P_BRAY_PPM, P_BRAY, ASTEXT(Geometry) AS GEO_WKT FROM PTOS_GEORREF_PARCEL_INTERPOLA"
    
    gpd_ptos = FromSpatialite2GeoPandas(dbIn, sqlSentence, 'GEO_WKT', "EPSG:25830")
    
    # Nº total de registros = 15813
    len(gpd_ptos)
    
    # Nº de muestras 0.0 < PORTASIO_PPM < 116518 -> 11449
    gpd_ptos.loc[(gpd_ptos['POTASIO_PPM']>0.0) & (gpd_ptos['POTASIO_PPM']<116518.0),'POTASIO_PPM'].describe()
    
    # Eliminio los valores extremos
    gpd_ptos = gpd_ptos.loc[(gpd_ptos['POTASIO_PPM']>0.0) & (gpd_ptos['POTASIO_PPM']<116518.0),:]
    
    
    gpd_ptos.loc[:,'POTASIO_PPM'].describe() # Nº de muestras 11449.0
    gpd_ptos.loc[gpd_ptos['POTASIO_PPM']>900.0,'POTASIO_PPM'].describe() # Nº de muestras 47 (0.41% de los datos)
    
    # Elimino todos las muestras con valores > de 900.0 ppm_K
    gpd_ptos = gpd_ptos.loc[gpd_ptos['POTASIO_PPM']<900.0,:]
    
    gpd_ptos.loc[gpd_ptos['POTASIO_PPM']<900.0,'POTASIO_PPM'].describe()
    gpd_ptos.loc[:,'POTASIO_PPM'].quantile([0.05, 0.15, 0.25, 0.50, 0.75, 0.90, 0.95, 0.97, 0.99])
    

# =============================================================================
# según las tablas:
#     * SECANO:
#         - Muy bajo < 41.5
#         - Muy alto >=273.9
#     * REGADIO:
#         - Muy bajo < 49.8
#         - muy alto >= 290.5
# =============================================================================
    
    # Para secano
    len(gpd_ptos)
    # Nº de muestras POTASIO_PPM < 41.5 = 97. Total = 11402. % = 0.85%
    len(gpd_ptos.loc[gpd_ptos['POTASIO_PPM']<41.5,'POTASIO_PPM']) 
    # Nº de muestras POTASIO_PPM >= 290.5 = 2661 Total = 11402. % = 23.34%
    len(gpd_ptos.loc[gpd_ptos['POTASIO_PPM']>=273.9,'POTASIO_PPM'])
    
    # Para Regadío
    len(gpd_ptos)
    # Nº de muestras POTASIO_PPM < 49.8 = 184. Total = 11402. % = 1.62%
    len(gpd_ptos.loc[gpd_ptos['POTASIO_PPM']<49.8,'POTASIO_PPM']) 
    # Nº de muestras POTASIO_PPM >= 290.5 = 2332. Total = 11402. % = 20.45%
    len(gpd_ptos.loc[gpd_ptos['POTASIO_PPM']>=290.5,'POTASIO_PPM'])
    
    lstImgNames = [allfiles for allfiles in [f for f in listdir(dirImagenes) if isfile(join(dirImagenes, f))] if len(allfiles.split('.')) < 3 and allfiles.split('.')[1] == 'tif']
    gpd_ptos = GetImgsValueFromXYCoor(dirImagenes, lstImgNames, gpd_ptos, 'GEO_WKT')
    
    # Convierto el campo de geometría a texto y vuelco la tabla a la db   
    gpd_ptos['GEO_WKT']=gpd_ptos['GEO_WKT'].apply(dumps)
    gpd_ptos.to_sql('POTASIO_SAMPLES_COVARIANTS', devuelveConexionSpatialite(dbIn), if_exists='replace', index=False)
    
    
    # plt.pyplot.hist(gpd_ptos.loc[:,'POTASIO_PPM'], bins=50, color='Red')
    # plt.show()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
