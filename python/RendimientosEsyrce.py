# -*- coding: utf-8 -*-
"""
Created on Fri Jun 19 10:24:03 2020

@author: ita-gutgaral
"""

# import esda
import pandas as pd
import geopandas as gpd
from geopandas import GeoDataFrame
# import libpysal as lps
import numpy as np
import matplotlib.pyplot as plt
from shapely.geometry import Point
from shapely.wkt import loads, dumps
import sqlite3




def leeFoisFromSpatialite(_dbIn, _sql):
    # dbIn = '/media/alberto/DATOS/Trabajo/Monitor_2020/data/BD/FOIS_MNT20_20200420_GEO.sqlite'
    conexion = sqlite3.connect(_dbIn)
    # conexion.enable_load_extension(True)
    # conexion.execute("SELECT load_extension('mod_spatialite')") 
    # sqlSentence = "SELECT FOI_ID, COD_CULTIVO_MONITOR, AREA, ASWKT(Geometry) AS GEO_WKT FROM MNT20_FOI_BUFFER_POLI WHERE FOI_ID = 2020565667"
    selectedFOIs = pd.read_sql_query(_sql, conexion)
    return selectedFOIs





if __name__ == '__main__':
    
    dicProv = {'CYL':'CYL','05':'AVILA', '09':'BURGOS', '24':'LEON', '34':'PALENCIA', '37':'SALAMANCA', '40':'SEGOVIA', '42':'SORIA', '47':'VALLADOLID', '49':'ZAMORA'}
    df_resultado = pd.DataFrame(columns=['ADMINISTRATIVO','CULTIVO_ESYRCE','EXPLOTACION','NUM_PARCELAS', 'RTO_MIN', 'RTO_MAX', 'RTO_MEDIO', 'RTO_STD', 'PERCEN_10','PERCEN_15','PERCEN_20','PERCEN_25','PERCEN_30','PERCEN_35','PERCEN_40','PERCEN_45','PERCEN_50','PERCEN_55','PERCEN_60','PERCEN_65','PERCEN_70','PERCEN_75','PERCEN_80','PERCEN_85','PERCEN_90','PERCEN_95'])

    dbIn = 'D:/FaST_2020/Data/BD/FAST_CROPS.sqlite'
    # sqlSentence = "SELECT FOI_ID, COD_CULTIVO_MONITOR, AREA, ASWKT(Geometry) AS GEO_WKT FROM MNT20_FOI_BUFFER_POLI WHERE FOI_ID = 2020502723"
    sqlSentence = "SELECT GRC, CUL, DES_CUL, C_PRODUCTO, C_VARIEDAD, D_PRODUCTO_PAC, DESCRIPCION FROM ESYRCE_PAC WHERE C_PRODUCTO > 0"
    df_cultivos = leeFoisFromSpatialite(dbIn, sqlSentence)

    sqlSentence = "SELECT GRC, CUL, DES_CUL FROM ESYRCE_CULTIVOS"
    df_CropName = leeFoisFromSpatialite(dbIn, sqlSentence)
    
    sqlSentence = "SELECT NUM, PAR, CS, GRC, CUL, EB, MD, SRI, MRI, OBS, RTO, MI, EL, DL, CA, CP, YEA, SUP FROM ESYRCE_2001TO2018 WHERE RTO > 1"
    df_CropRtos = leeFoisFromSpatialite(dbIn, sqlSentence)
    
    lstExplo = ['S','R']
    lstCrops = list(df_cultivos['CUL'].unique())
    for ex in range(0,len(lstExplo)):    
        df_CropRtosExp = df_CropRtos[(df_CropRtos['CUL'].isin(lstCrops)) & (df_CropRtos['SRI'] == lstExplo[ex])].copy()
    
        exp = df_CropRtosExp.groupby('CUL').agg(NUM_PARCELAS=pd.NamedAgg(column='RTO', aggfunc='count'), 
                                                                      RTO_MIN=pd.NamedAgg(column='RTO', aggfunc='min'),
                                                                      RTO_MEDIO=pd.NamedAgg(column='RTO', aggfunc='mean'),
                                                                      RTO_MAX=pd.NamedAgg(column='RTO', aggfunc='max'),
                                                                      RTO_STD=pd.NamedAgg(column='RTO', aggfunc='std'),
                                                                      PERCEN_10=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.10)),
                                                                      PERCEN_15=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.15)),
                                                                      PERCEN_20=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.20)),
                                                                      PERCEN_25=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.25)),
                                                                      PERCEN_30=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.30)),
                                                                      PERCEN_35=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.35)),
                                                                      PERCEN_40=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.40)),
                                                                      PERCEN_45=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.45)),
                                                                      PERCEN_50=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.50)),
                                                                      PERCEN_55=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.55)),
                                                                      PERCEN_60=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.60)),
                                                                      PERCEN_65=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.65)),
                                                                      PERCEN_70=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.70)),
                                                                      PERCEN_75=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.75)),
                                                                      PERCEN_80=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.80)),
                                                                      PERCEN_85=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.85)),
                                                                      PERCEN_90=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.90)),
                                                                      PERCEN_95=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.95)))
    
        exp['EXPLOTACION'] = lstExplo[ex]
        exp['ADMINISTRATIVO'] = 'CYL'
        exp.to_sql(name = 'ESYRCE_RTOS_CULTIVOS_3', con = sqlite3.connect(dbIn), if_exists = 'append')
    


    for ex in range(0,len(lstExplo)):    
        df_CropRtosExp = df_CropRtos[(df_CropRtos['CUL'].isin(lstCrops)) & (df_CropRtos['SRI'] == lstExplo[ex])].copy()
    
        exp = df_CropRtosExp.groupby(['CUL','CP']).agg(NUM_PARCELAS=pd.NamedAgg(column='RTO', aggfunc='count'), 
                                                                      RTO_MIN=pd.NamedAgg(column='RTO', aggfunc='min'),
                                                                      RTO_MEDIO=pd.NamedAgg(column='RTO', aggfunc='mean'),
                                                                      RTO_MAX=pd.NamedAgg(column='RTO', aggfunc='max'),
                                                                      RTO_STD=pd.NamedAgg(column='RTO', aggfunc='std'),
                                                                      PERCEN_10=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.10)),
                                                                      PERCEN_15=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.15)),
                                                                      PERCEN_20=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.20)),
                                                                      PERCEN_25=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.25)),
                                                                      PERCEN_30=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.30)),
                                                                      PERCEN_35=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.35)),
                                                                      PERCEN_40=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.40)),
                                                                      PERCEN_45=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.45)),
                                                                      PERCEN_50=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.50)),
                                                                      PERCEN_55=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.55)),
                                                                      PERCEN_60=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.60)),
                                                                      PERCEN_65=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.65)),
                                                                      PERCEN_70=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.70)),
                                                                      PERCEN_75=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.75)),
                                                                      PERCEN_80=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.80)),
                                                                      PERCEN_85=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.85)),
                                                                      PERCEN_90=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.90)),
                                                                      PERCEN_95=pd.NamedAgg(column='RTO', aggfunc=lambda x: np.quantile(x, q= 0.95)))
    
        exp['EXPLOTACION'] = lstExplo[ex]
        exp.reset_index(inplace=True)
        
        exp['ADMINISTRATIVO'] = exp.apply(lambda x: dicProv[x['CP']],axis=1)
        exp.drop('CP', inplace=True, axis=1)
        exp.to_sql(name = 'ESYRCE_RTOS_CULTIVOS_3', con = sqlite3.connect(dbIn), if_exists = 'append',index=False)


