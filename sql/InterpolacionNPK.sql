## SELECCIONA LOS PUNTOS QUE TIENEN LAS MISMAS COORDENADAS
SELECT tbl1.ID_MUESTRA, tbl2.ID_MUESTRA
FROM PTOS_GeorrefParcel tbl1
INNER JOIN PTOS_GeorrefParcel tbl2 
ON ST_Equals(tbl1.Shape, tbl2.Shape) = 1
WHERE tbl1.ID_MUESTRA <> tbl2.ID_MUESTRA
ORDER BY tbl1.ID_MUESTRA





SELECT COUNT(*) FROM PTOS_GeorrefParcel
WHERE ID_MUESTRA NOT IN (
	SELECT DISTINCT("ID_MUESTRA_1")
	FROM "PTOS_MISMAS_COOR_2"
)
AND
ID_MUESTRA NOT IN (
	SELECT DISTINCT("ID_MUESTRA_2")
	FROM "PTOS_MISMAS_COOR_2"
)


SELECT tbl1.ID_MUESTRA, tbl2.ID_MUESTRA 
FROM PTOS_GeorrefParcel tbl1, 
(
	SELECT ID_MUESTRA, Shape
	FROM PTOS_GeorrefParcel
	WHERE ID_MUESTRA = '0001_7.4_1.59_0.12'
) tbl2
WHERE ST_Equals(tbl1.Shape, tbl2.Shape) = 1


SELECT * 
FROM PTOS_GeorrefParcel
WHERE tbl1.ID_MUESTRA = '0001_7.4_1.59_0.12'


SELECT tbl1.ID_MUESTRA, tbl2.ID_MUESTRA 
FROM PTOS_GeorrefParcel tbl1, 
(
	SELECT ID_MUESTRA, Shape
	FROM PTOS_GeorrefParcel
	WHERE ID_MUESTRA = '1995_2591'
) tbl2
WHERE ST_Equals(tbl1.Shape, tbl2.Shape) = 1
AND tbl1.ID_MUESTRA <> tbl2.ID_MUESTRA
ORDER BY 


SELECT ID_MUESTRA, Campaña
FROM PTOS_GeorrefParcel
WHERE ID_MUESTRA IN ("1995_2591","1996_1719","2004_2441","2015_2005","2016_1593","2017_2018","2007_0736","2008_1054","2010_0569","2011_1109","2013_1063","2014_1159")
ORDER BY Campaña desc



CREATE TABLE PTOS_GEORREF_PARCEL_ALL (
ID_MUESTRA TEXT,
U_F_N_Lab REAL,
U_F_P_Lab REAL, 
U_F_K_Lab REAL, 
U_F_Mg_Lab REAL, 
U_F_N_Tec REAL, 
U_F_P_Tec REAL, 
U_F_K_Tec REAL, 
U_F_Mg_Tec REAL, 
ORIGEN REAL, 
SEASON INTEGER, 
LABORATORIO TEXT, 
MO_PORC REAL, 
MATERIA_ORGANICA TEXT, 
ARENA_PORC REAL, 
LIMO_PORC REAL, 
ARCILLA_PORC REAL,
TEXTURA TEXT, 
VALORACION_SUELO TEXT, 
DA_kg_m3 INTEGER, 
pH REAL, 
ACIDEZ_BASICIDAD TEXT, 
CARBONATOS_PORC REAL, 
CALIZA_ACTIVA_PORC REAL, 
CONDUCTIVIDAD TEXT, 
NITROGENO_PORC REAL, 
POTASIO_PPM REAL, 
POTASIO TEXT, 
CALCIO_PPM REAL, 
CALCIO TEXT, 
MAGNESIO_PPM REAL, 
MAGNESIO TEXT, 
SODIO_PPM REAL, 
SODIO TEXT, 
H_pF2_5GravPorc REAL, 
H_pF2_7GravPorc REAL, 
H_pF4_2GravPorc REAL, 
COOR_X_ETRS89 REAL, 
COOR_Y_ETRS89 REAL,
GRUESO_PORC REAL, 
CIC_cmol_kg REAL, 
PUBLICOS INTEGER, 
OBSERVACIONES TEXT, 
CAJA_EDAFOTECA INTEGER, 
TEXTCALCU TEXT, 
H_CC_Vol_Prov REAL, 
H_CC_Vol REAL, 
H_PM_Vol_Prov REAL,
H_PM_Vol REAL, 
PERME_1 REAL,
PERME_2 REAL, 
PERME_3 REAL,
PERME_mm_h REAL, 
GRUPO_TEXTURA TEXT, 
TIPO INTEGER, 
BORO_PPM REAL, 
BORO TEXT, 
CE_1_5_Agua_dS_m REAL, 
CE_ExtSat_dS_m REAL, 
C_N float64 REAL, 
Fe_EDTA_ppm REAL, 
IPC REAL, 
Zn_DTPA_ppm REAL, 
ProfIni_Muestra_cm INTEGER, 
ProfFin_Muestra_cm INTEGER, 
ProfLimiteAgric_cm INTEGER, 
P_OLSEN_PPM REAL, 
P_OLSEN TEXT, 
P_BRAY_PPM REAL, 
P_BRAY TEXT, 
pH_KCl REAL, 
CIC_meq_100g REAL, 
MANGANESOT_PPM REAL, 
HIEROT_PPM REAL, 
COBRET_PPM REAL, 
ZINCT_ppm REAL, 
BOROT_PPM REAL, 
Ca_Mg_RELAC REAL, 
K_Mg_RELAC REAL, 
K_CIC_PORC REAL, 
Mg_CIC_PORC REAL, 
Ca_CIC_PORC REAL, 
PSI_NaInterc_PORC REAL, 
IRFC_TendFormarCostra REAL)
SELECT AddGeometryColumn('PTOS_GEORREF_PARCEL_ALL', 'Geometry', 25830, 'POINT', 'XY');
UPDATE PTOS_GEORREF_PARCEL_ALL SET Geometry = SetSrid(Geometry, 25830);


INSERT INTO PTOS_GEORREF_PARCEL_ALL
SELECT "ID_MUESTRA", "U_F_N_Lab", "U_F_P_Lab", "U_F_K_Lab", "U_F_Mg_Lab", "U_F_N_Tec", "U_F_P_Tec", "U_F_K_Tec", "U_F_Mg_Tec", "Origen", "Campaña", "Laboratorio", "MO_Porc", "MateriaOrganica", "Arena_Porc", "Limo_Porc", "Arcilla_Porc", "Textura", "ValoracionSuelo", "DA_kg_m3", "pH", "AcidezBasicidad", "Carbonatos_Porc", "CalizaActiva_Porc", "Conductividad", "Nitrogeno_Porc", "Potasio_ppm", "Potasio", "Calcio_ppm", "Calcio", "Magnesio_ppm", "Magnesio", "Sodio_ppm", "Sodio", "H_pF2_5GravPorc", "H_pF2_7GravPorc", "H_pF4_2GravPorc", "COOR_X_ETRS89", "COOR_Y_ETRS89", "Gruesos_Porc", "CIC_cmol_kg", "Publicos", "Observaciones", "Caja_EDAFOTECA", "TextCalcu", "H_CC_Vol_Prov", "H_CC_Vol", "H_PM_Vol_Prov", "H_PM_Vol", "PERME_1", "PERME_2", "PERME_3", "Perme_mm_h", "GrupoTextura", "Tipo", "Boro_ppm", "Boro", "CE_1_5_Agua_dS_m", "CE_ExtSat_dS_m", "C_N", "Fe_EDTA_ppm", "IPC", "Zn_DTPA_ppm", "ProfIni_Muestra_cm", "ProfFin_Muestra_cm", "ProfLimiteAgric_cm", "P_Olsen_ppm", "P_Olsen", "P_Bray_ppm", "P_Bray", "pH_KCl", "CIC_meq_100g", "ManganesoT_ppm", "HierroT_ppm", "CobreT_ppm", "ZincT_ppm", "BoroT_ppm", "Ca_Mg_Relac", "K_Mg_Relac", "K_CIC_Porc", "Mg_CIC_Porc", "Ca_CIC_Porc", "PSI_NaInterc_Porc", "IRFC_TendFormarCostra", "Shape"
FROM "PTOS_GeorrefParcel"


DROP TABLE PTOS_GEORREF_PARCEL_ALL
SELECT DiscardGeometryColumn('PTOS_GEORREF_PARCEL_ALL', 'Geometry');



CREATE TABLE PTOS_GEORREF_PARCEL_INTERPOLA (
ID_MUESTRA TEXT,
U_F_N_Lab REAL,
U_F_P_Lab REAL, 
U_F_K_Lab REAL, 
U_F_Mg_Lab REAL, 
U_F_N_Tec REAL, 
U_F_P_Tec REAL, 
U_F_K_Tec REAL, 
U_F_Mg_Tec REAL, 
ORIGEN REAL, 
SEASON INTEGER, 
LABORATORIO TEXT, 
MO_PORC REAL, 
MATERIA_ORGANICA TEXT, 
ARENA_PORC REAL, 
LIMO_PORC REAL, 
ARCILLA_PORC REAL,
TEXTURA TEXT, 
VALORACION_SUELO TEXT, 
DA_kg_m3 INTEGER, 
pH REAL, 
ACIDEZ_BASICIDAD TEXT, 
CARBONATOS_PORC REAL, 
CALIZA_ACTIVA_PORC REAL, 
CONDUCTIVIDAD TEXT, 
NITROGENO_PORC REAL, 
POTASIO_PPM REAL, 
POTASIO TEXT, 
CALCIO_PPM REAL, 
CALCIO TEXT, 
MAGNESIO_PPM REAL, 
MAGNESIO TEXT, 
SODIO_PPM REAL, 
SODIO TEXT, 
H_pF2_5GravPorc REAL, 
H_pF2_7GravPorc REAL, 
H_pF4_2GravPorc REAL, 
COOR_X_ETRS89 REAL, 
COOR_Y_ETRS89 REAL,
GRUESO_PORC REAL, 
CIC_cmol_kg REAL, 
PUBLICOS INTEGER, 
OBSERVACIONES TEXT, 
CAJA_EDAFOTECA INTEGER, 
TEXTCALCU TEXT, 
H_CC_Vol_Prov REAL, 
H_CC_Vol REAL, 
H_PM_Vol_Prov REAL,
H_PM_Vol REAL, 
PERME_1 REAL,
PERME_2 REAL, 
PERME_3 REAL,
PERME_mm_h REAL, 
GRUPO_TEXTURA TEXT, 
TIPO INTEGER, 
BORO_PPM REAL, 
BORO TEXT, 
CE_1_5_Agua_dS_m REAL, 
CE_ExtSat_dS_m REAL, 
C_N float64 REAL, 
Fe_EDTA_ppm REAL, 
IPC REAL, 
Zn_DTPA_ppm REAL, 
ProfIni_Muestra_cm INTEGER, 
ProfFin_Muestra_cm INTEGER, 
ProfLimiteAgric_cm INTEGER, 
P_OLSEN_PPM REAL, 
P_OLSEN TEXT, 
P_BRAY_PPM REAL, 
P_BRAY TEXT, 
pH_KCl REAL, 
CIC_meq_100g REAL, 
MANGANESOT_PPM REAL, 
HIEROT_PPM REAL, 
COBRET_PPM REAL, 
ZINCT_ppm REAL, 
BOROT_PPM REAL, 
Ca_Mg_RELAC REAL, 
K_Mg_RELAC REAL, 
K_CIC_PORC REAL, 
Mg_CIC_PORC REAL, 
Ca_CIC_PORC REAL, 
PSI_NaInterc_PORC REAL, 
IRFC_TendFormarCostra REAL)
SELECT AddGeometryColumn('PTOS_GEORREF_PARCEL_INTERPOLA', 'Geometry', 25830, 'POINT', 'XY');



INSERT INTO PTOS_GEORREF_PARCEL_INTERPOLA
SELECT "ID_MUESTRA", "U_F_N_Lab", "U_F_P_Lab", "U_F_K_Lab", "U_F_Mg_Lab", "U_F_N_Tec", "U_F_P_Tec", "U_F_K_Tec", "U_F_Mg_Tec", "Origen", "Campaña", "Laboratorio", "MO_Porc", "MateriaOrganica", "Arena_Porc", "Limo_Porc", "Arcilla_Porc", "Textura", "ValoracionSuelo", "DA_kg_m3", "pH", "AcidezBasicidad", "Carbonatos_Porc", "CalizaActiva_Porc", "Conductividad", "Nitrogeno_Porc", "Potasio_ppm", "Potasio", "Calcio_ppm", "Calcio", "Magnesio_ppm", "Magnesio", "Sodio_ppm", "Sodio", "H_pF2_5GravPorc", "H_pF2_7GravPorc", "H_pF4_2GravPorc", "COOR_X_ETRS89", "COOR_Y_ETRS89", "Gruesos_Porc", "CIC_cmol_kg", "Publicos", "Observaciones", "Caja_EDAFOTECA", "TextCalcu", "H_CC_Vol_Prov", "H_CC_Vol", "H_PM_Vol_Prov", "H_PM_Vol", "PERME_1", "PERME_2", "PERME_3", "Perme_mm_h", "GrupoTextura", "Tipo", "Boro_ppm", "Boro", "CE_1_5_Agua_dS_m", "CE_ExtSat_dS_m", "C_N", "Fe_EDTA_ppm", "IPC", "Zn_DTPA_ppm", "ProfIni_Muestra_cm", "ProfFin_Muestra_cm", "ProfLimiteAgric_cm", "P_Olsen_ppm", "P_Olsen", "P_Bray_ppm", "P_Bray", "pH_KCl", "CIC_meq_100g", "ManganesoT_ppm", "HierroT_ppm", "CobreT_ppm", "ZincT_ppm", "BoroT_ppm", "Ca_Mg_Relac", "K_Mg_Relac", "K_CIC_Porc", "Mg_CIC_Porc", "Ca_CIC_Porc", "PSI_NaInterc_Porc", "IRFC_TendFormarCostra", "Shape"
FROM "PTOS_GeorrefParcel"
WHERE ID_MUESTRA NOT IN (
SELECT ID_MUESTRA 
FROM MUESTRAS_OUT)


SELECT tbl1.ID_MUESTRA, tbl2.ID_MUESTRA
FROM PTOS_GEORREF_PARCEL_INTERPOLA tbl1
JOIN PTOS_GEORREF_PARCEL_INTERPOLA tbl2 
ON ST_Equals(tbl1.Geometry, tbl2.Geometry) = 1
WHERE tbl1.ID_MUESTRA <> tbl2.ID_MUESTRA
ORDER BY tbl1.ID_MUESTRA




CREATE TABLE PTOS_INTERPOLAR(
ID INTEGER,
COOR_X_ETRS89 INTEGER,
COOR_Y_ETRS89 INTEGER
)
SELECT AddGeometryColumn('PTOS_INTERPOLAR', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO PTOS_INTERPOLAR
SELECT id, x(Geometry) as X, y(Geometry) as Y, Geometry FROM ptos_interpolado_2




CREATE TABLE SK_FOSFORO_250m (
COOR_X_ETRS89 REAL,
COOR_Y_ETRS89 REAL,
LOG_P_INTER REAL,
VAR_LOG_P_INTER REAL,
P_PREDICT REAL,
P_PREDICT_CORR REAL
)
SELECT AddGeometryColumn('SK_FOSFORO_250m', 'Geometry', 25830, 'POINT', 'XY');



INSERT INTO SK_FOSFORO_250m
SELECT COOR_X_ETRS89,COOR_Y_ETRS89, "var1.pred" as LOG_P_INTER, "var1.var" as VAR_LOG_P_INTER, P_PREDICT,P_PREDICT_CORR, MakePoint(COOR_X_ETRS89, COOR_Y_ETRS89, 25830) as Geometry FROM a."SK_FOSFORO_250m"

CREATE TABLE "FOSFORO_SAMPLES_COVARIANTS_2" (
"ID_MUESTRA" TEXT,
  "ORIGEN" TEXT,
  "SEASON" INTEGER,
  "LABORATORIO" TEXT,
  "MO_PORC" REAL,
  "MATERIA_ORGANICA" TEXT,
  "ARENA_PORC" REAL,
  "LIMO_PORC" REAL,
  "ARCILLA_PORC" REAL,
  "TEXTURA" TEXT,
  "pH" REAL,
  "ACIDEZ_BASICIDAD" TEXT,
  "CARBONATOS_PORC" REAL,
  "NITROGENO_PORC" REAL,
  "POTASIO_PPM" REAL,
  "POTASIO" TEXT,
  "CALCIO_PPM" REAL,
  "MAGNESIO_PPM" REAL,
  "SODIO_PPM" REAL,
  "COOR_X_ETRS89" REAL,
  "COOR_Y_ETRS89" REAL,
  "PUBLICOS" INTEGER,
  "TEXTCALCU" TEXT,
  "GRUPO_TEXTURA" TEXT,
  "TIPO" INTEGER,
  "P_OLSEN_PPM" REAL,
  "P_BRAY_PPM" REAL,
  "P_BRAY" TEXT,
  "P_INTERPOLA" REAL,
  "ARCILLA" REAL,
  "ARENA" REAL,
  "ETP" REAL,
  "FC_UK" REAL,
  "GDD" REAL,
  "KSAT_UK" REAL,
  "LIBREHELADAS" REAL,
  "LIMO" REAL,
  "mde_250m" REAL,
  "MO" REAL,
  "pH_RASTER" REAL,
  "PMed_ABRIL" REAL,
  "PMed_AGOSTO" REAL,
  "PMed_ANUAL" REAL,
  "PMed_DICIEMBRE" REAL,
  "PMed_ENERO" REAL,
  "PMed_FEBRERO" REAL,
  "PMed_INVIERNO" REAL,
  "PMed_JUNIO" REAL,
  "PMed_MARZO" REAL,
  "PMed_MAYO" REAL,
  "PMed_NOVIEMBRE" REAL,
  "PMed_OCTUBRE" REAL,
  "PMed_PRIMAVERA" REAL,
  "PMed_SEPTIEMBRE" REAL,
  "PMed_VERANO" REAL,
  "RADIACION" REAL,
  "Rug_250" REAL,
  "SAT_UK" REAL,
  "slope_250m" REAL,
  "TIERRA_ARABLE" REAL,
  "TMed_ABRIL" REAL,
  "TMed_AGOSTO" REAL,
  "TMed_DICIEMBRE" REAL,
  "TMed_ENERO" REAL,
  "TMed_JULIO" REAL,
  "TMed_JUNIO" REAL,
  "TMed_MARZO" REAL,
  "TMed_MAYO" REAL,
  "TMed_NOVIEMBRE" REAL,
  "TMed_OCTUBRE" REAL,
  "TMed_SEPTIEMBRE" REAL,
  "TMMAX_ABRIL" REAL,
  "TMMAX_AGOSTO" REAL,
  "TMMAX_DICIEMBRE" REAL,
  "TMMAX_ENERO" REAL,
  "TMMAX_FEBRERO" REAL,
  "TMMAX_JULIO" REAL,
  "TMMAX_JUNIO" REAL,
  "TMMAX_MARZO" REAL,
  "TMMAX_MAYO" REAL,
  "TMMAX_NOVIEMBRE" REAL,
  "TMMAX_OCTUBRE" REAL,
  "TMMAX_SEPTIEMBRE" REAL,
  "WP_UK" REAL,
  "CRAD_UK" REAL,
  "PMed_JULIO" REAL,
  "TMed_FEBRERO" REAL
)

INSERT INTO FOSFORO_SAMPLES_COVARIANTS_2
SELECT * FROM a."FOSFORO_SAMPLES_COVARIANTS_2"



CREATE TABLE rtk_ogrFromWKT_4258_remake (
Usuario TEXT,
Fecha TEXT,
Velocidad TEXT,
wkt TEXT,
wkt_fine TEXT)
SELECT AddGeometryColumn('rtk_ogrFromWKT_4258_remake', 'Geometry', 4258, 'POINT', 'XY');

INSERT INTO rtk_ogrFromWKT_4258_remake
SELECT Usuario, Fecha, Velocidad, wkt, ASTEXT(MakePoint(y("Geometry"),y("Geometry"), 4258)) as wkt_fine, MakePoint(y("Geometry"),y("Geometry"), 4258) as Geometry
FROM rtk_ogrFromWKT_4258



CREATE TABLE rtk_ogrFromWKT_4258_remake2 (
Usuario TEXT,
Fecha TEXT,
Velocidad TEXT,
wkt TEXT,
wkt_fine TXT
)
SELECT AddGeometryColumn('rtk_ogrFromWKT_4258_remake2', 'Geometry', 4258, 'POINT', 'XY');

INSERT INTO rtk_ogrFromWKT_4258_remake2
SELECT Usuario, Fecha, Velocidad, wkt, ASTEXT(makepoint(y(Geometry),x(Geometry),4258)) as wkt_fine ,makepoint(y(Geometry),x(Geometry),4258) as Geometry
FROM rtk_ogrFromWKT_4258



CREATE TABLE RF_FOSFORO_250m (
ID INTEGER,
COOR_X_ETRS89 REAL,
COOR_Y_ETRS89 REAL,
PREDIC_RF REAL
)
SELECT AddGeometryColumn('RF_FOSFORO_250m', 'Geometry', 25830, 'POINT', 'XY');



INSERT INTO RF_FOSFORO_250m
SELECT ID, COOR_X_ETRS89, COOR_Y_ETRS89, PREDIC_RF, MakePoint(COOR_X_ETRS89, COOR_Y_ETRS89, 25830) as Geometry FROM a."RF_COOR_FOSFORO"


SELECT A.ID_MUESTRA, A.P_INTERPOLA, MAKEPOINT(COOR_X_ETRS89, COOR_Y_ETRS89, 25830) as A.Geometry
FROM FOSFORO_SAMPLES_COVARIANTS_2

SELECT PK_UID, DN_PK, PROVINCIA, Geometry
FROM SIGPAC_ConMuestra_25830



CREATE TABLE FOSFORO_PARCELAS (
PK_UID INTEGER, 
DN_PK INTEGER, 
PROVINCIA INTEGER,
ID_MUESTRA TEXT,
P_PPM REAL)
SELECT AddGeometryColumn('FOSFORO_PARCELAS', 'Geometry', 25830, 'MULTIPOLYGON', 'XY');

INSERT INTO FOSFORO_PARCELAS
SELECT B.PK_UID AS PK_UID, B.DN_PK AS DN_PK, B.PROVINCIA AS PROVINCIA, A.ID_MUESTRA AS ID_MUESTRA, A.P_INTERPOLA AS P_ppm, B.Geometry AS Geometry
FROM 
(SELECT ID_MUESTRA, P_INTERPOLA, MAKEPOINT(COOR_X_ETRS89, COOR_Y_ETRS89, 25830) as Geometry
FROM FOSFORO_SAMPLES_COVARIANTS_2
) A, SIGPAC_ConMuestra_25830 B
WHERE ST_WITHIN(A.Geometry, B.Geometry)


SELECT DiscardGeometryColumn('FOSFORO_PARCELAS', 'Geometry');
DROP TABLE FOSFORO_PARCELAS


CREATE TABLE SK_POTASIO_250m (
COOR_X_ETRS89 REAL,
COOR_Y_ETRS89 REAL,
LOG_K_INTER REAL,
VAR_LOG_K_INTER REAL,
K_PREDICT, 
K_PREDICT_CORR,
)
SELECT AddGeometryColumn('SK_POTASIO_250m', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO SK_POTASIO_250m (COOR_X_ETRS89,COOR_Y_ETRS89,LOG_K_INTER,VAR_LOG_K_INTER,K_PREDICT, K_PREDICT_CORR,Geometry)
SELECT COOR_X_ETRS89, COOR_Y_ETRS89, "var1.pred","var1.var", K_PREDICT, K_PREDICT_CORR, MAKEPOINT(COOR_X_ETRS89, COOR_Y_ETRS89, 25830) as Geometry FROM a."OK_POTASIO_250m"




CREATE TABLE RF_POTASIO_250m (
COOR_X_ETRS89 REAL,
COOR_Y_ETRS89 REAL,
K_PREDICT_RF REAL
)
SELECT AddGeometryColumn('RF_POTASIO_250m', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO RF_POTASIO_250m (COOR_X_ETRS89, COOR_Y_ETRS89, K_PREDICT_RF, Geometry)
SELECT COOR_X_ETRS89, COOR_Y_ETRS89, K_PREDICT_RF, MAKEPOINT(COOR_X_ETRS89, COOR_Y_ETRS89, 25830) as Geometry FROM a."RF_POTASIO_COOR"










a."RF_POTASIO_COOR"





















