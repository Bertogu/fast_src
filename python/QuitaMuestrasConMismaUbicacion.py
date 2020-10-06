#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 18 18:08:26 2020

@author: alberto
"""

# from datetime import datetime, timedelta
import pandas as pd
import numpy as np
import sqlite3
# import matplotlib.pyplot as plt
# from matplotlib import dates as mpl_date
# from matplotlib import style

# import matplotlib.dates as mdates
# import matplotlib.cbook as cbook
# import matplotlib.ticker as mtick




def conexSpatialite(_BaseDatos):

    # conexSqliteIn = sqlite3.connect('/media/alberto/DATOS/Trabajo/Monitor_2020/data/BD/REPLICA_ORACLE.sqlite')
    conexSqliteIn = sqlite3.connect(_BaseDatos)
    
    
    conexSqliteIn.enable_load_extension(True)
    conexSqliteIn.execute("SELECT load_extension('mod_spatialite')") 
    
    return conexSqliteIn


if __name__ == '__main__':
       
    # BaseDatos='D:/Monitor_2020/data/VEC_DATA/FOIs/FOI_20200814.sqlite'
    BaseDatos='/home/alberto/Dropbox/trabajo/FaST_2020/Data/BD/PTOS_BD_Suelos_CyL.sqlite'   
    conexLectura = conexSpatialite(BaseDatos)
 
    sql_MuestraUnica = "SELECT DISTINCT(ID_MUESTRA_1) FROM PTOS_MISMAS_COOR_2"
    df_MuestraUnica = pd.read_sql_query(sql_MuestraUnica, conexLectura)
    
    sql_MuestrasGeo = "SELECT ID_MUESTRA, Campaña as SEASON FROM PTOS_GeorrefParcel"
    df_MuestrasGeo = pd.read_sql_query(sql_MuestrasGeo, conexLectura)
    
    sql_unaMuestra = "SELECT ID_MUESTRA_1, ID_MUESTRA_2 FROM PTOS_MISMAS_COOR_2"
    df_unaMuestra = pd.read_sql_query(sql_unaMuestra, conexLectura)
    
    # lstIdMuestras = df_unaMuestra.loc[:,'ID_MUESTRA_2'].values.tolist()
    # lstIdMuestras.append(df_unaMuestra.loc[0,'ID_MUESTRA_1'])
    
    
    
    df_MuestrasOut = pd.DataFrame(columns = ['ID_MUESTRA'])
    for i in df_MuestraUnica.index:
        # print(type(df_MuestraUnica.loc[i,'ID_MUESTRA_1']))
        # df_unaMuestra[df_unaMuestra['ID_MUESTRA_1'] == df_MuestraUnica.loc[i,'ID_MUESTRA_1']].loc[:,'ID_MUESTRA_2'].values.tolist()
        # df_MuestraUnica.loc[i,'ID_MUESTRA_1']
        lstIdMuestras = df_unaMuestra[df_unaMuestra['ID_MUESTRA_1'] == df_MuestraUnica.loc[i,'ID_MUESTRA_1']].loc[:,'ID_MUESTRA_2'].values.tolist()
        lstIdMuestras.append(df_unaMuestra[df_unaMuestra['ID_MUESTRA_1'] == df_MuestraUnica.loc[i,'ID_MUESTRA_1']].reset_index(drop=True).loc[0,'ID_MUESTRA_1'])

        # a = df_MuestrasGeo[df_MuestrasGeo['ID_MUESTRA'].isin(lstIdMuestras)].sort_values(by=['SEASON'],ascending=False).reset_index(drop=True).loc[1:,'ID_MUESTRA'].to_frame()        
        df_MuestrasOut = df_MuestrasOut.append(df_MuestrasGeo[df_MuestrasGeo['ID_MUESTRA'].isin(lstIdMuestras)].sort_values(by=['SEASON'],ascending=False).reset_index(drop=True).loc[1:,'ID_MUESTRA'].to_frame())

    
    df_MuestrasOut = pd.DataFrame.from_dict({'ID_MUESTRA':df_MuestrasOut.loc[:,'ID_MUESTRA'].unique().tolist()})
    df_MuestrasOut.to_sql(name = 'MUESTRAS_OUT', con = conexLectura, if_exists = 'replace', index = False)
























