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






ADMINISTRATIVO	CULTIVO_ESYRCE	DES_CUL	EXPLOTACION	NUM_PARCELAS	RTO_MIN	RTO_MAX	RTO_MEDIO	RTO_STD	PERCEN_10	PERCEN_15	PERCEN_20	PERCEN_25	PERCEN_30	PERCEN_35	PERCEN_40	PERCEN_45	PERCEN_50	PERCEN_55	PERCEN_60	PERCEN_65	PERCEN_70	PERCEN_75	PERCEN_80	PERCEN_85	PERCEN_90	PERCEN_95

GRC	CUL	DES_CUL	C_PRODUCTO	C_VARIEDAD	D_PRODUCTO_PAC	DESCRIPCION

PASTIZAL
PRADO NATURAL
PRADO NATURAL EN REGADIO
PASTIZAL MATORRAL
PASTIZAL ALTA MONTÑA
PASTIZAL CON ARBOLADO
PASTIZAL MATORRAL CON ARBOLADO
FRONDOSAS CRECIMIENTO LENTO
FRONDOSAS CRECIMIENTO RAPIDO
MATORRAL
CONIFERAS
CONIFERAS Y FONDOSAS
ERIAL
NO AGRICOLA
AGUAS INTERIORES
BALDIO
ROCAS, PEDREGALES, ARENALES
ESPARTIZAL
MATORRAL
BARBECHO
BARBECHO REGADIO
HUERTO FAMILIAR
PRADERA POLIFITAS
VIVEROS
HUERTAS VACIAS
ALMENDRO ABANDONADO

ALMENDRO NO COMERCIAL, MANZANO NO COMERCIAL, LOMBARDA, COL FORRAJERA, MAIZ FORRAJERO, NABO FORRAJERO, REMOLACHA FORRAJERA

OTROS FRUTALES,OTRAS PLANTAS DE ESCARDA FORRAJERA,OTROS CULTIVOS INDUSTRIALES,CONDIMENTOS,OTRAS OLEAGINOSAS,OTRAS HORTALIZAS,FLORES Y ORNAMENTALES


































