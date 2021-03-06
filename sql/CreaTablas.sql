CREATE TABLE "ESYRCE_2001TO2018" (
NUM INTEGER,
PAR INTEGER,
CS TEXT,
GRC TEXT,
CUL TEXT,
EB INTEGER,
MD TEXT,
SRI TEXT,
MRI INTEGER,
OBS TEXT,
RTO DOUBLE,
MI TEXT,
EL INTEGER,
DL INTEGER,
CA TEXT,
CP TEXT,
YEA INTEGER,
SUP DOUBLE,
HUS INTEGER,
X DOUBLE,
Y DOUBLE)
INSERT INTO ESYRCE_2001TO2018
SELECT * FROM a."ESYRCE_2001TO2018"




CREATE TABLE "IACS_FERTILICALC_CULITVOS" (
C_PRODUCTO INTEGER, 
C_VARIEDAD INTEGER, 
D_PRODUCTO_PAC TEXT, 
ES TEXT, 
Name TEXT, 
Type TEXT, 
Harvest_product TEXT, 
prod_DM_PORC REAL, 
prod_N_typical REAL, 
P_porc_prod_dm REAL, 
K_porc_prod_dm REAL, 
Residue_product REAL, 
res_DM_porc REAL, 
res_N_typical REAL, 
P_porc_res_dm REAL, 
K_porc_res_dm REAL, 
Nfix_code REAL, 
N_min REAL, 
N_max REAL, 
HI REAL, 
Fres_porc REAL, 
N_Fondo_porc REAL, 
N_Fondo_Descripcion TEXT, 
N_Coberteras_Num_Habitual TEXT, 
N_Coberteras_Descripcion TEXT)



INSERT INTO IACS_FERTILICALC_CULITVOS
SELECT "C_PRODUCTO", "C_VARIEDAD", "D_PRODUCTO_PAC", "ES", "Name", "Type", "Harvest_product", "prod_DM%", "prod_N typical", "P(%prod_dm)", "K(%prod_dm)", "Residue_product", "res_DM%", "res_N typical", "P(%res_dm)", "K(%res_dm)", "Nfix_code", "N min", "N max", "HI", "Fres(%)", "N_Fondo (%)", "N_Fondo_Descripcion", "N_Coberteras_Num_Habitual", "N_Coberteras_Descripcion"
FROM "iacs_fertilicalc_cultivos"

(20,21,23,24,25,28,62,63,64,65,66,114,115,144,145,146,150,199,237,350,400,500,600,700,750,800,850,999)

CREATE TABLE ESYRCE_CULTIVOS(
GRC TEXT,
CUL TEXT,
DES_CUL TEXT
)
INSERT INTO ESYRCE_CULTIVOS (GRC, CUL, DES_CUL)
SELECT GRC, CUL, DES_CUL FROM "CULTIVOS_ESYRCE"

CREATE TABLE ESYRCE_GRUPOS_CULTIVOS(
GRC TEXT,
DES_GRC TEXT,
CON_PAC TEXT
)

INSERT INTO ESYRCE_GRUPOS_CULTIVOS (GRC, DES_GRC, CON_PAC)
SELECT GRC, DES_GRC, CON_PAC FROM "GRUPO_CULTIVOS"



CREATE TABLE ESYRCE_CODIGOS_COMPLEMENTARIOS(
COD_COMPLE TEXT,
DESC_CODIGO_COMPLE TEXT
)

INSERT INTO ESYRCE_CODIGOS_COMPLEMENTARIOS (COD_COMPLE, DESC_CODIGO_COMPLE)
SELECT COD_COMPLE, DESC_CODIGO_COMPLE FROM "COD_COMPLE"


CREATE TABLE ESYRCE_PAC (
GRC TEXT,
CUL TEXT,
DES_CUL TEXT,
C_PRODUCTO INTEGER,
C_VARIEDAD INTEGER,
D_PRODUCTO_PAC TEXT,
DESCRIPCION TEXT
)
INSERT INTO ESYRCE_PAC (GRC, CUL, DES_CUL, C_PRODUCTO, C_VARIEDAD, D_PRODUCTO_PAC, DESCRIPCION)
SELECT GRC, CUL, DES_CUL, C_PRODUCTO, " C_VARIEDAD", D_PRODUCTO_PAC, DESCRIPCION FROM "COD_ESYRCE_PAC"

-- CULTIVOS QUE TIENEN CORRESPONDENCIA DIRECTA ESYRCE - PAC
SELECT GRC, CUL, DES_CUL, C_PRODUCTO, C_VARIEDAD, D_PRODUCTO_PAC, DESCRIPCION FROM ESYRCE_PAC WHERE C_PRODUCTO > 0


-- SELECCIÓN DE LOS CULTIVOS A PROCESAR
SELECT NUM, PAR, CS, GRC, CUL, EB, MD, SRI, MRI, OBS, RTO, MI, EL, DL, CA, CP, YEA, SUP FROM ESYRCE_2001TO2018 WHERE RTO > 1 AND SRI = "R" AND CP = "05" AND CUL LIKE "TB%"

-- SELECCION DEL NOMBRE DE CULTIVO
SELECT GRC, CUL, DES_CUL FROM ESYRCE_CULTIVOS


SELECT ADMINISTRATIVO, A.CUL AS CULTIVO_ESYRCE, DES_CUL, EXPLOTACION, NUM_PARCELAS, RTO_MIN, RTO_MAX, RTO_MEDIO, RTO_STD, PERCEN_10, PERCEN_15, PERCEN_20, PERCEN_25, PERCEN_30, PERCEN_35, PERCEN_40, PERCEN_45, PERCEN_50, PERCEN_55, PERCEN_60, PERCEN_65, PERCEN_70, PERCEN_75, PERCEN_80, PERCEN_85, PERCEN_90, PERCEN_95
FROM ESYRCE_RTOS_CULTIVOS_3 A
LEFT JOIN ESYRCE_CULTIVOS B
ON A.CUL=B.CUL





CREATE TABLE LIMITE_CyL_MULTIPOL (
NOMBRE TEXT,
COD_COMUNIDAD INTEGER
)
SELECT AddGeometryColumn('LIMITE_CyL_MULTIPOL', 'Geometry', 25830, 'MULTIPOLYGON', 'XY');

INSERT INTO LIMITE_CyL_MULTIPOL (Geometry)
SELECT Geometry FROM a."Limite_CyL_poligono"

UPDATE LIMITE_CyL_MULTIPOL
SET NOMBRE = 'Castilla y León', COD_COMUNIDAD = 10


CREATE TABLE DUERO_MASAS_SUB_SUPERIOR_CYL (
ID INTEGER,
NAME TEXT,
AREA REAL)
SELECT AddGeometryColumn('DUERO_MASAS_SUB_SUPERIOR_CYL', 'Geometry', 25830, 'POLYGON', 'XY');

INSERT INTO DUERO_MASAS_SUB_SUPERIOR_CYL 
SELECT A.ID, A.feature_na, A.area, A.Geometry
FROM Duero_Masa_Subterranea_Superior A, LIMITE_CyL_MULTIPOL B
WHERE st_within(A.Geometry, B.Geometry)


CREATE TABLE "PROMEDIO_NITRATOS_INSIDE" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia"TEXT,
NumDatosPr INTEGER,
Parametro"TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT)
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_INSIDE', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO PROMEDIO_NITRATOS_INSIDE
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,A.Geometry
FROM PROMEDIO_NITRATOS A, DUERO_MASAS_SUB_SUPERIOR_CYL B
WHERE ST_WITHIN(A.Geometry, B.Geometry)

CREATE TABLE "PROMEDIO_NITRATOS_OUTSIDE" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia"TEXT,
NumDatosPr INTEGER,
Parametro"TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT)
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_OUTSIDE', 'Geometry', 25830, 'POINT', 'XY');


INSERT INTO PROMEDIO_NITRATOS_OUTSIDE (Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry)
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry
FROM PROMEDIO_NITRATOS
WHERE CodRedCont NOT IN (
	SELECT DISTINCT(CodRedCont)
	FROM "PROMEDIO_NITRATOS_INSIDE"
)




CREATE TABLE "VOR_NITRATOS_CyL" (
COD_RED_CONT TEXT,
MASA_AGUA TEXT,
TERMINO_MUNICIPAL TEXT,
PROVINCIA TEXT,
NITRATOS_PRO DOUBLE,
UNIDAD_MEDIDA TEXT)
SELECT AddGeometryColumn('VOR_NITRATOS_CyL', 'Geometry', 25830, 'MULTIPOLYGON', 'XY');


INSERT INTO VOR_NITRATOS_CyL (COD_RED_CONT,MASA_AGUA,TERMINO_MUNICIPAL,PROVINCIA,NITRATOS_PRO,UNIDAD_MEDIDA, Geometry)
SELECT CodRedCont,Masa_Agua_,TerminoMun,Provincia,NITRAT_PRO,UnidadDeMe,Geometry
FROM VOR_NITRATOS_OUTSIDE_2


INSERT INTO VOR_NITRATOS_CyL (COD_RED_CONT,MASA_AGUA,TERMINO_MUNICIPAL,PROVINCIA,NITRATOS_PRO,UNIDAD_MEDIDA, Geometry)
SELECT CodRedCont,Masa_Agua_,TerminoMun,Provincia,NITRAT_PRO,UnidadDeMe,Geometry
FROM VOR_NITRATOS_INSIDE_2


#######################################################
ME QUEDO CON PÁRAMOS Y RAÑAS -> NO ALUVIAL


CREATE TABLE "PROMEDIO_NITRATOS_INSIDE_PARAMOS" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia"TEXT,
NumDatosPr INTEGER,
Parametro"TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT)
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_INSIDE_PARAMOS', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO PROMEDIO_NITRATOS_INSIDE_PARAMOS
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,A.Geometry
FROM PROMEDIO_NITRATOS A, 
(SELECT * FROM DUERO_MASAS_SUB_SUPERIOR_CYL WHERE NAME NOT LIKE 'Aluvial%') B
WHERE ST_WITHIN(A.Geometry, B.Geometry) 


CREATE TABLE "PROMEDIO_NITRATOS_OUTSIDE_PARAMOS" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia TEXT,
NumDatosPr INTEGER,
Parametro TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT)
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_OUTSIDE_PARAMOS', 'Geometry', 25830, 'POINT', 'XY');

# Los que están fuera de los paramos
INSERT INTO PROMEDIO_NITRATOS_OUTSIDE_PARAMOS (Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry)
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry
FROM PROMEDIO_NITRATOS
WHERE CodRedCont NOT IN (
	SELECT A.CodRedCont
	FROM PROMEDIO_NITRATOS A, 
	(SELECT * FROM DUERO_MASAS_SUB_SUPERIOR_CYL WHERE ID IN (400015,400019,400032,400043,400044,400029)) B
	WHERE ST_WITHIN (A.Geometry,B.Geometry)
)



CREATE TABLE "PROMEDIO_NITRATOS_INSIDE_PARAMO_400015" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia TEXT,
NumDatosPr INTEGER,
Parametro TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT)
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_INSIDE_PARAMO_400015', 'Geometry', 25830, 'POINT', 'XY');



INSERT INTO PROMEDIO_NITRATOS_INSIDE_PARAMO_400015 (Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry)
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry
FROM PROMEDIO_NITRATOS
WHERE CodRedCont IN (
	SELECT A.CodRedCont
	FROM PROMEDIO_NITRATOS A, 
		(SELECT * FROM DUERO_MASAS_SUB_SUPERIOR_CYL WHERE ID IN (400015)) B
	WHERE ST_WITHIN (A.Geometry,B.Geometry)
)



BEGIN TRANSACTION;
CREATE TABLE "PROMEDIO_NITRATOS_INSIDE_PARAMO_400019" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia TEXT,
NumDatosPr INTEGER,
Parametro TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT);
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_INSIDE_PARAMO_400019', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO PROMEDIO_NITRATOS_INSIDE_PARAMO_400019 (Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry)
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry
FROM PROMEDIO_NITRATOS
WHERE CodRedCont IN (
	SELECT A.CodRedCont
	FROM PROMEDIO_NITRATOS A, 
		(SELECT * FROM DUERO_MASAS_SUB_SUPERIOR_CYL WHERE ID IN (400019)) B
	WHERE ST_WITHIN (A.Geometry,B.Geometry)
);


CREATE TABLE "PROMEDIO_NITRATOS_INSIDE_PARAMO_400032" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia TEXT,
NumDatosPr INTEGER,
Parametro TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT);
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_INSIDE_PARAMO_400032', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO PROMEDIO_NITRATOS_INSIDE_PARAMO_400032 (Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry)
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry
FROM PROMEDIO_NITRATOS
WHERE CodRedCont IN (
	SELECT A.CodRedCont
	FROM PROMEDIO_NITRATOS A, 
		(SELECT * FROM DUERO_MASAS_SUB_SUPERIOR_CYL WHERE ID IN (400032)) B
	WHERE ST_WITHIN (A.Geometry,B.Geometry)
);


CREATE TABLE "PROMEDIO_NITRATOS_INSIDE_PARAMO_400043" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia TEXT,
NumDatosPr INTEGER,
Parametro TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT);
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_INSIDE_PARAMO_400043', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO PROMEDIO_NITRATOS_INSIDE_PARAMO_400043 (Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry)
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry
FROM PROMEDIO_NITRATOS
WHERE CodRedCont IN (
	SELECT A.CodRedCont
	FROM PROMEDIO_NITRATOS A, 
		(SELECT * FROM DUERO_MASAS_SUB_SUPERIOR_CYL WHERE ID IN (400043)) B
	WHERE ST_WITHIN (A.Geometry,B.Geometry)
);

CREATE TABLE "PROMEDIO_NITRATOS_INSIDE_PARAMO_400044" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia TEXT,
NumDatosPr INTEGER,
Parametro TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT);
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_INSIDE_PARAMO_400044', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO PROMEDIO_NITRATOS_INSIDE_PARAMO_400044 (Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry)
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry
FROM PROMEDIO_NITRATOS
WHERE CodRedCont IN (
	SELECT A.CodRedCont
	FROM PROMEDIO_NITRATOS A, 
		(SELECT * FROM DUERO_MASAS_SUB_SUPERIOR_CYL WHERE ID IN (400044)) B
	WHERE ST_WITHIN (A.Geometry,B.Geometry)
);


CREATE TABLE "PROMEDIO_NITRATOS_INSIDE_PARAMO_400029" (
Confederac TEXT,
CoorX_Etrs DOUBLE,
CoorY_Etrs DOUBLE,
CodRedCont TEXT,
Masa_Agua_ TEXT,
TerminoMun TEXT,
Provincia TEXT,
NumDatosPr INTEGER,
Parametro TEXT,
Parametro2 TEXT,
NITRAT_PRO DOUBLE,
UnidadDeMe TEXT,
LimiteCuan TEXT);
SELECT AddGeometryColumn('PROMEDIO_NITRATOS_INSIDE_PARAMO_400029', 'Geometry', 25830, 'POINT', 'XY');

INSERT INTO PROMEDIO_NITRATOS_INSIDE_PARAMO_400029 (Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry)
SELECT Confederac,CoorX_Etrs,CoorY_Etrs,CodRedCont,Masa_Agua_,TerminoMun,Provincia,Parametro2,NITRAT_PRO,UnidadDeMe,LimiteCuan,Geometry
FROM PROMEDIO_NITRATOS
WHERE CodRedCont IN (
	SELECT A.CodRedCont
	FROM PROMEDIO_NITRATOS A, 
		(SELECT * FROM DUERO_MASAS_SUB_SUPERIOR_CYL WHERE ID IN (400029)) B
	WHERE ST_WITHIN (A.Geometry,B.Geometry)
);
COMMIT;




CREATE TABLE VOR_NITRATOS_PARAMOS_CyL (
CODREDCON TEXT,
MASA_AGUA TEXT, 
TERMINO_MUNI TEXT, 
PROVINCIA TEXT, 
PARAMETRO TEXT, 
NITRATO_PRO REAL, 
UNIDAD_MEDIDA TEXT, 
LIMITE_CUAN TEXT, 
)
SELECT AddGeometryColumn('VOR_NITRATOS_PARAMOS_CyL', 'Geometry', 25830, 'MULTIPOLYGON', 'XY');

INSERT INTO VOR_NITRATOS_PARAMOS_CyL (CODREDCON,MASA_AGUA, TERMINO_MUNI, PROVINCIA, PARAMETRO, NITRATO_PRO, UNIDAD_MEDIDA, LIMITE_CUAN, Geometry)
SELECT (codredcont ,masa_agua_ ,terminomun ,provincia ,parametro2 ,nitrat_pro ,unidaddeme ,limitecuan, Geometry) FROM Vor_Nitratos_inside_paramos_Clip


INSERT INTO VOR_NITRATOS_PARAMOS_CyL (CODREDCON,MASA_AGUA, TERMINO_MUNI, PROVINCIA, PARAMETRO, NITRATO_PRO, UNIDAD_MEDIDA, LIMITE_CUAN, Geometry)
SELECT (codredcont ,masa_agua_ ,terminomun ,provincia ,parametro2 ,nitrat_pro ,unidaddeme ,limitecuan, Geometry) FROM Vor_Nitratos_outside_paramos_SymDiff_CyL

















































