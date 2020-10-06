#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 11 17:10:36 2020

@author: alberto
"""
# Para que funcione la librería esda he tenido que modificar el fichero
# C:\Users\ita-gutgaral\Anaconda3\envs\GeoStats37_v8\Lib\site-packages\libpysal\examples\remotes.py
# añadiendo las direcciones del proxy a cañón y modificando los parámetros de requests.get(url, proxies=proxies)
import esda 
import pandas as pd
import geopandas as gpd
from geopandas import GeoDataFrame
import libpysal as lps
import numpy as np
import matplotlib.pyplot as plt
from shapely.geometry import Point
from shapely.wkt import loads, dumps
import sqlite3

def leeFoisFromSpatialite(_dbIn, _sql):
    # dbIn = '/media/alberto/DATOS/Trabajo/Monitor_2020/data/BD/FOIS_MNT20_20200420_GEO.sqlite'
    conexion = sqlite3.connect(_dbIn)
    conexion.enable_load_extension(True)
    conexion.execute("SELECT load_extension('mod_spatialite')") 
    # sqlSentence = "SELECT FOI_ID, COD_CULTIVO_MONITOR, AREA, ASWKT(Geometry) AS GEO_WKT FROM MNT20_FOI_BUFFER_POLI WHERE FOI_ID = 2020565667"
    selectedFOIs = pd.read_sql_query(_sql, conexion)
    return selectedFOIs








if __name__ == '__main__':
    
    dbIn = '/home/alberto/Dropbox/trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'
    # sqlSentence = "SELECT FOI_ID, COD_CULTIVO_MONITOR, AREA, ASWKT(Geometry) AS GEO_WKT FROM MNT20_FOI_BUFFER_POLI WHERE FOI_ID = 2020502723"
    sqlSentence = "select ID_MUESTRA, Publicos as PUBLICO, Origen as ORIGEN, Campaña as SEASON, MO_Porc as MO_PORC, Arena_Porc as ARENA_PORC, Arcilla_Porc as ARCILLA_PORC, Textura as TEXTURA, pH, Potasio_ppm as K_PPM, P_Olsen_ppm as P_OLSEN_PPM, CalizaActiva_Porc as CALIZA_POR, ST_ASTEXT(SHAPE) AS GEO_WKT from PTOS_GeorrefParcel"
    df_ptos = leeFoisFromSpatialite(dbIn, sqlSentence)
    df_ptos['GEO_WKT'] = df_ptos['GEO_WKT'].apply(loads)
    gpd_ptos = gpd.GeoDataFrame(df_ptos, geometry='GEO_WKT')
    gpd_ptos.crs = "EPSG:25830"
    
    gpd_ptos.dtypes
    gpd_ptos.plot(column='P_OLSEN_PPM')
    
    gpd_P.columns
    
    gpd_P = gpd_ptos.query('P_OLSEN_PPM > 0.0').copy()
    len(gpd_P)
    

    gpd_P['P_OLSEN_PPM'].describe()
    
    gpd_P[gpd_P['pH'] >= 7.0]['P_OLSEN_PPM'].describe()
    gpd_P['P_OLSEN_PPM'].quantile((0.05))

    len(gpd_P[gpd_P['P_OLSEN_PPM'] < 5 ])/len(gpd_P)
    len(gpd_P[(gpd_P['P_OLSEN_PPM'] >= 5.0) & (gpd_P['P_OLSEN_PPM'] < 10.0)])/len(gpd_P)
    len(gpd_P[gpd_P['P_OLSEN_PPM'] >= 10.0 ])/len(gpd_P)
    plt.hist(gpd_P[gpd_P['P_OLSEN_PPM'] < 102.0]['P_OLSEN_PPM'], bins=100)
    len(gpd_P[gpd_P['P_OLSEN_PPM'] >= 102.0 ])/len(gpd_P)
    
    gpd_P['P_OLSEN_PPM'].quantile(0.98)


    
    




