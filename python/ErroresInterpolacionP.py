# -*- coding: utf-8 -*-
"""
Created on Fri Oct  2 12:37:00 2020

@author: ita-gutgaral
"""

import sys
import gdal
import geopandas as gpd
import pandas as pd
import numpy as np
import scipy 
import matplotlib as plt

import rasterio
import sqlite3
#from sqlite3 import dbapi2 as sqlite

from affine import Affine
from os import listdir
from os.path import isfile, join

# import pykrige.kriging_tools as kt
# from pykrige.ok import OrdinaryKriging

# from sqlalchemy import create_engine, event
import shapely.wkb
from shapely.wkt import loads, dumps
import sklearn.metrics as metrics


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


def fosforaInterpola (_olsen,_bray,_ph):
    '''
    Actualiza los valores de P en el campo que se utilizará para la interpola.
    Si existe valor de P para ambos métodos y el ph>= 7.0 utiliza el P_Olsen
    Sino P_bray
    Args:
        - _olsen: valor de P_Olsen
        - _bray: valor de P_Bray
        - _ph: valor de pH
    Retruns:
        - valor de P
    '''
    
    if (_olsen !=-9999 and _bray == -9999) or (_olsen !=-9999 and _bray != -9999 and _ph >= 7.0):
        valor=_olsen

    else:
        valor=_bray
    
    return valor


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




if __name__ == '__main__':
    
    dbIn = 'D:/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'
    dirImagenes = 'D:/FaST_2020/Data/Raster/interpola/'
    sqlSentence = "SELECT ID_MUESTRA, ORIGEN, SEASON, LABORATORIO, MO_PORC, MATERIA_ORGANICA, ARENA_PORC, LIMO_PORC, ARCILLA_PORC, TEXTURA, pH, ACIDEZ_BASICIDAD, CARBONATOS_PORC, NITROGENO_PORC, POTASIO_PPM, POTASIO, CALCIO_PPM, MAGNESIO_PPM, SODIO_PPM, COOR_X_ETRS89, COOR_Y_ETRS89, PUBLICOS, TEXTCALCU, GRUPO_TEXTURA, TIPO, P_OLSEN_PPM, P_BRAY_PPM, P_BRAY, ASTEXT(Geometry) AS GEO_WKT FROM PTOS_GEORREF_PARCEL_INTERPOLA"
    
    # Cargo los datos de las muestras en un geopandas Dataframe
    gpd_ptos = FromSpatialite2GeoPandas(dbIn, sqlSentence, 'GEO_WKT', "EPSG:25830")
     
    # Selección de las muestras que tienen algún dato de Fósforo
    gpd_ptos = gpd_ptos[~((gpd_ptos['P_OLSEN_PPM']==-9999) & (gpd_ptos['P_BRAY_PPM']==-9999))]
    
    # Creo el campo con los valores que se utilizarán para la interpolación
    gpd_ptos['P_INTERPOLA'] = gpd_ptos.apply(lambda x: fosforaInterpola (x['P_OLSEN_PPM'],x['P_BRAY_PPM'],x['pH']), axis=1)  
    
    # Selecciono los valores que están entre los percentiles 5 y 95
    gpd_ptos = gpd_ptos.loc[(gpd_ptos['P_INTERPOLA']>=gpd_ptos['P_INTERPOLA'].quantile(0.05)) & (gpd_ptos['P_INTERPOLA']<=gpd_ptos['P_INTERPOLA'].quantile(0.95)),:]    

    # Leo las imagenes (tif) del directorio
    lstImgNames = [allfiles for allfiles in [f for f in listdir(dirImagenes) if isfile(join(dirImagenes, f))] if len(allfiles.split('.')) < 3 and allfiles.split('.')[1] == 'tif']
    
    # Intersecto los puntos con todas las imagenes del directorio
    gpd_ptos = GetImgsValueFromXYCoor(dirImagenes, lstImgNames, gpd_ptos, 'GEO_WKT')
    
    gpd_ptos.columns
    


    #
    # with sqlite3.connect(dbIn) as conn:
    #     conn.enable_load_extension(True)
    #     conn.load_extension("mod_spatialite")
    # #
    
    # removeColumns = ['ORIGEN', 'SEASON', 'LABORATORIO', 'MO_PORC', 'MATERIA_ORGANICA', 'ARENA_PORC', 'LIMO_PORC', 'ARCILLA_PORC', 'TEXTURA', 'pH', 'ACIDEZ_BASICIDAD','CARBONATOS_PORC','NITROGENO_PORC', 'POTASIO_PPM', 'POTASIO', 'CALCIO_PPM','MAGNESIO_PPM', 'SODIO_PPM', 'PUBLICOS', 'TEXTCALCU', 'GRUPO_TEXTURA', 'TIPO', 'P_OLSEN_PPM','P_BRAY_PPM', 'P_BRAY', 'GEO_WKT']    
    # gpd_ptos.drop(removeColumns, axis = 1).to_sql('P_ERRORES_INTERPOLA', conn, if_exists='replace', index=False)
    #


    gpd_random_ptos = gpd_ptos.sample(frac=0.3)
    gpd_random_ptos = gpd_random_ptos.loc[gpd_random_ptos['RF_FOSFORO_250m']>0.0,:]
    len(gpd_random_ptos)
    gpd_random_ptos.columns



    rmse_RF = metrics.mean_squared_error(gpd_random_ptos['P_INTERPOLA'],gpd_random_ptos['RF_FOSFORO_250m'])
    rmse_SK = metrics.mean_squared_error(gpd_random_ptos['P_INTERPOLA'],gpd_random_ptos['SK_FOSFORO_CyL_250m'])


    # plt.pyplot.scatter(gpd_random_ptos['P_INTERPOLA'], gpd_random_ptos['RF_FOSFORO_250m'], color="blue", label="original", s=0.7)
    # plt.pyplot.scatter( gpd_random_ptos['RF_FOSFORO_250m'], gpd_random_ptos['RF_FOSFORO_250m']- gpd_random_ptos['P_INTERPOLA'], color="blue", label="original", s=0.7)
    
    plt.pyplot.scatter(gpd_random_ptos['P_INTERPOLA'], gpd_random_ptos['SK_FOSFORO_CyL_250m'], color="blue", label="original", s=0.7)
    # plt.pyplot.scatter( gpd_random_ptos['SK_FOSFORO_CyL_250m'], gpd_random_ptos['SK_FOSFORO_CyL_250m']- gpd_random_ptos['P_INTERPOLA'], color="blue", label="original", s=0.7)
    
    # plt.plot(x, yhat, color="red", label="predicted")
    plt.legend()
    plt.show()









