# -*- coding: utf-8 -*-
"""
Editor de Spyder


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


def fosforaInterpola (_olsen,_bray,_ph):
    
    if (_olsen !=-9999 and _bray == -9999) or (_olsen !=-9999 and _bray != -9999 and _ph >= 7.0):
        valor=_olsen

    else:
        valor=_bray
    
    return valor




if __name__ == '__main__':
    
    dbIn = '/home/alberto/Dropbox/trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'
    # dbIn = 'D:/Dropbox/trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'
    # sqlSentence = "SELECT FOI_ID, COD_CULTIVO_MONITOR, AREA, ASWKT(Geometry) AS GEO_WKT FROM MNT20_FOI_BUFFER_POLI WHERE FOI_ID = 2020502723"
    sqlSentence = "SELECT ID_MUESTRA, ORIGEN, SEASON, LABORATORIO, MO_PORC, MATERIA_ORGANICA, ARENA_PORC, LIMO_PORC, ARCILLA_PORC, TEXTURA, pH, ACIDEZ_BASICIDAD, CARBONATOS_PORC, NITROGENO_PORC, POTASIO_PPM, POTASIO, CALCIO_PPM, MAGNESIO_PPM, SODIO_PPM, COOR_X_ETRS89, COOR_Y_ETRS89, PUBLICOS, TEXTCALCU, GRUPO_TEXTURA, TIPO, P_OLSEN_PPM, P_BRAY_PPM, P_BRAY, ASTEXT(Geometry) AS GEO_WKT FROM PTOS_GEORREF_PARCEL_INTERPOLA"
    df_ptos = leeFoisFromSpatialite(dbIn, sqlSentence)
    df_ptos['GEO_WKT'] = df_ptos['GEO_WKT'].apply(loads)
    gpd_ptos = gpd.GeoDataFrame(df_ptos, geometry='GEO_WKT')
    gpd_ptos.crs = "EPSG:25830"
     
    # Selección de las muestras que tienen algún dato de Fósforo
    gpd_ptos = gpd_ptos[~((gpd_ptos['P_OLSEN_PPM']==-9999) & (gpd_ptos['P_BRAY_PPM']==-9999))]
    gpd_ptos['P_INTERPOLA'] = gpd_ptos.apply(lambda x: fosforaInterpola (x['P_OLSEN_PPM'],x['P_BRAY_PPM'],x['pH']), axis=1)  
    
    gpd_ptos = gpd_ptos.loc[(gpd_ptos['P_INTERPOLA']>=gpd_ptos['P_INTERPOLA'].quantile(0.05)) & (gpd_ptos['P_INTERPOLA']<=gpd_ptos['P_INTERPOLA'].quantile(0.95)),:]    


    # gpd_samples = gpd.read_file('D:/Textura_CyL/CAMPO_SEGOVIANO/DB/PTOS_ANALISIS.shp')
    
    # dirImagenes = "D:/Textura_CyL/CAMPO_SEGOVIANO/Suelo/RASTER/"
    dirImagenes = '/media/alberto/DATOS/Trabajo/FaST_2020/Data/Raster/'
    # dirImagenes = 'D:/FaST_2020/Data/Raster/'
    lstImgNames = [allfiles for allfiles in [f for f in listdir(dirImagenes) if isfile(join(dirImagenes, f))] if len(allfiles.split('.')) < 3 and allfiles.split('.')[1] == 'tif']
    for img in lstImgNames:
    #    print(img)
        # imgNDVI_1 = gdal.Open("D:/FaST_2020/Data/Raster/{0}".format(img))
        imgNDVI_1 = gdal.Open(dirImagenes+img)
        
        geoInformacion = imgNDVI_1.GetGeoTransform()
        
        BandNDVI_1 = imgNDVI_1.GetRasterBand(1)
        ArNDVI_1 = imgNDVI_1.GetRasterBand(1).ReadAsArray()
        
        print(img)
        
        pds_FC = gpd_ptos['GEO_WKT'].apply(lambda t: world2Pixel(geoInformacion, t.x, t.y))
        # pds_FC = gpd_samples['geometry'].apply(lambda t: world2Pixel(geoInformacion, t.x, t.y))
        # gpd_samples[img.split('.')[0]]=pds_FC.apply(lambda x: ArNDVI_1[x[0],x[1]])
        gpd_ptos[img.split('.')[0]]=pds_FC.apply(lambda x: ArNDVI_1[x[0],x[1]])




    
    # DB_PATH = 'D:/Textura_CyL/CAMPO_SEGOVIANO/DB/CampoSegoviano_Suelos.sqlite'
    #
    #
    with sqlite3.connect(dbIn) as conn:
        conn.enable_load_extension(True)
        conn.load_extension("mod_spatialite")
    #
    
    
    gpd_ptos.drop(['GEO_WKT'], axis = 1).to_sql('FOSFORO_SAMPLES_COVARIANTS_2', conn, if_exists='replace', index=False)
    #
    














