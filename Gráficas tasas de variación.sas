LIBNAME TFG 'C:\Users\mcarm\Documents\GitHub\TFG';
* Reseteo opciones gráficas;
GOPTION RESET = ALL;
/* --------------------------------------------------------*/
/*     				IMPORTAMOS LOS DATOS	  			   */
/* --------------------------------------------------------*/
PROC IMPORT DATAFILE = 'C:\Users\mcarm\Documents\GitHub\TFG\CCAALecturaDatos'
	DBMS = SAV OUT=TFG.CCAA REPLACE;
RUN;
PROC IMPORT DATAFILE = 'C:\Users\mcarm\Documents\GitHub\TFG\CCAASECTORLecturaDatos'
	DBMS = SAV OUT=TFG.CCAA_SECTOR REPLACE;
RUN;
PROC PRINT DATA=TFG.CCAA; RUN;
PROC PRINT DATA=TFG.CCAA_SECTOR; RUN;

* Macro ordenar data;
%macro ordenar (dt, var);
PROC SORT DATA = &dt;
	BY &var;
RUN;
%mend;

ODS GRAPHICS ON;
/* ----------------------------------------------------------*/
/*      TASAS VARIACIÓN POR RENTA PER CÁPITA - REGIONAL	     */
/* ----------------------------------------------------------*/
* Andalucía, Extremadura, Castilla-La Mancha, Región de Murcia;
DATA BAJO_R;
	SET TFG.CCAA;
	IF (CCAA='Andalucia')|(CCAA='Extremadura')|(CCAA='Castilla-La Mancha')|(CCAA='Murcia, Region de');
		RETAIN CCAA;
RUN;
* %ordenar(BAJO_R,CCAA); * Si tenemos desordenados los datos;
PROC GPLOT DATA = BAJO_R;
	TITLE "Comunidades Autónomas con menor renta per cápita";
	*FOOTNOTE1 J=l "Gráfico de elaboración propia.";
	AXIS1 LABEL=("Año" JUSTIFY=RIGHT);
	AXIS2 LABEL=("Tasa Variación" JUSTIFY=RIGHT);
	PLOT TVar_Prod*Anio=CCAA / VREF = 0 HAXIS=AXIS1 VAXIS=AXIS2;
	SYMBOL1 INTERPOL=line WIDTH=1 C=steelblue;
	SYMBOL2 INTERPOL=line WIDTH=1 C=red;
	SYMBOL3 INTERPOL=line WIDTH=1 C=orange;
	SYMBOL4 INTERPOL=line WIDTH=1 C=brown;
	WHERE Anio > 2008;
RUN;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;

* Cataluña, Comunidad de Madrid, País Vasco, Comunidad Foral de Navarra;
DATA ALTO_R;
	SET TFG.CCAA;
	IF (CCAA='Catalunia')|(CCAA='Madrid, Comunidad de')|(CCAA='Pais Vasco')|(CCAA='Navarra, Comunidad Foral de');
		RETAIN CCAA;
RUN;
* %ordenar(ALTO_R,CCAA); * Si tenemos desordenados los datos;
PROC GPLOT DATA = ALTO_R;
	TITLE "Comunidades Autónomas con mayor renta per cápita";
	*FOOTNOTE J=l "Gráfico de elaboración propia.";
	AXIS1 LABEL=("Año" JUSTIFY=RIGHT);
	AXIS2 LABEL=("Tasa Variación" JUSTIFY=RIGHT);
	PLOT TVar_Prod*Anio=CCAA / VREF = 0 HAXIS=AXIS1 VAXIS=AXIS2;
	SYMBOL1 INTERPOL=line WIDTH=1 C=STPPK;
	SYMBOL2 INTERPOL=line WIDTH=1 C=tomato;
	SYMBOL3 INTERPOL=line WIDTH=1 C=vilg;
	SYMBOL4 INTERPOL=line WIDTH=1 C=grey;
	WHERE Anio > 2008;
RUN;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;
/* ----------------------------------------------------------*/
/*      			 REPRESENTACIÓN CONJUNTA			     */
/* ----------------------------------------------------------*/
PROC GSLIDE;
	TITLE H=2 JUSTIFY=CENTER "Tasa de Variación de la productividad" ;
	FOOTNOTE H=1 JUSTIFY=LEFT "Gráficos de elaboración propia.";
RUN;
QUIT;
PROC GREPLAY IGOUT=work.gseg GOUT=work.gseg TC=work.tmplt NOFS;
	TDEF PK1 1 / LLX=0 LLY=0 		/*full panel for main titles*/
				ULX=0 ULY=100
				LRX=100 LRY=0
				URX=100 URY=100
			2 / LLX=0 LLY=50 		/* upper panel*/
				ULX=0 ULY=88
				LRX=100 LRY=50
				URX=100 URY=88
			3 / LLX=0 LLY=6 		/* lower panel*/
				ULX=0 ULY=44
				LRX=100 LRY=6
				URX=100 URY=44
			;
	RUN;
QUIT;
PROC GREPLAY IGOUT=work.gseg GOUT=work.gseg TC=work.tmplt NOFS;
	TEMPLATE PK1;
	TREPLAY 1:gslide
		2:gplot
		3:gplot1
		;
QUIT;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;


/* ----------------------------------------------------------*/
/* TASAS VARIACIÓN POR RENTA PER CÁPITA - REGIONAL-SECTORIAL */
/* ----------------------------------------------------------*/
* Andalucía, Extremadura, Castilla-La Mancha, Región de Murcia;
DATA BAJO_RS;
	SET TFG.CCAA_SECTOR;
	IF (CCAA='Andalucia')|(CCAA='Extremadura')|(CCAA='Castilla-La Mancha')|(CCAA='Murcia, Region de');
		RETAIN CCAA;
RUN;
* %ordenar(BAJO_RS,CCAA); * Si tenemos desordenados los datos;
PROC GPLOT DATA = BAJO_RS;
	AXIS1 LABEL=("Año" JUSTIFY=RIGHT);
	AXIS2 LABEL=("Tasa Variación" JUSTIFY=RIGHT);
	PLOT TVar_Prod*Anio=Sector / VREF = 0 HAXIS=AXIS1 VAXIS=AXIS2;
	SYMBOL1 INTERPOL=line WIDTH=1 C=green;
	SYMBOL2 INTERPOL=line WIDTH=1 C=red;
	SYMBOL3 INTERPOL=line WIDTH=1 C=orange;
	SYMBOL4 INTERPOL=line WIDTH=1 C=steelblue;
	WHERE Anio > 2011;
	BY CCAA;
RUN;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;

* Cataluña, Comunidad de Madrid, País Vasco, Comunidad Foral de Navarra;
DATA ALTO_RS;
	SET TFG.CCAA_SECTOR;
	IF (CCAA='Catalunia')|(CCAA='Madrid, Comunidad de')|(CCAA='Pais Vasco')|(CCAA='Navarra, Comunidad Foral de');
		RETAIN CCAA;
RUN;
* %ordenar(ALTO_RS,SECTOR); * Si tenemos desordenados los datos;
PROC GPLOT DATA = ALTO_RS;
	AXIS1 LABEL=("Año" JUSTIFY=RIGHT);
	AXIS2 LABEL=("Tasa Variación" JUSTIFY=RIGHT);
	PLOT TVar_Prod*Anio=Sector / VREF = 0 HAXIS=AXIS1 VAXIS=AXIS2;
	SYMBOL1 INTERPOL=line WIDTH=1 C=green;
	SYMBOL2 INTERPOL=line WIDTH=1 C=red;
	SYMBOL3 INTERPOL=line WIDTH=1 C=orange;
	SYMBOL4 INTERPOL=line WIDTH=1 C=steelblue;
	WHERE Anio > 2011;
	BY CCAA;
RUN;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;

* Sacamos Madrid sin la agricultura para poder ver bien los otros sectores;
DATA MADRID;
	SET TFG.CCAA_SECTOR;
	IF (CCAA='Madrid, Comunidad de');
		RETAIN CCAA;
	IF (Sector='Industria')|(Sector='Construccion')|(Sector='Servicios');
		RETAIN Sector;
RUN;
PROC GPLOT DATA = MADRID;
	TITLE "Tasas de variación de Madrid sin la agricultura";
	FOOTNOTE J=l "Gráfico de elaboración propia.";
	AXIS1 LABEL=("Año" JUSTIFY=RIGHT);
	AXIS2 LABEL=("Tasa Variación" JUSTIFY=RIGHT);
	PLOT TVar_Prod*Anio=Sector / VREF = 0 HAXIS=AXIS1 VAXIS=AXIS2;
	SYMBOL1 INTERPOL=line WIDTH=1 C=red;
	SYMBOL2 INTERPOL=line WIDTH=1 C=orange;
	SYMBOL3 INTERPOL=line WIDTH=1 C=steelblue;
	WHERE Anio > 2011;
RUN;
TITLE; * Quitar el título;
FOOTNOTE;

/* ----------------------------------------------------------*/
/*      			 REPRESENTACIÓN CONJUNTA 1			     */
/* ----------------------------------------------------------*/
PROC GSLIDE;
	TITLE H=2 JUSTIFY=CENTER "Tasa de Variación de la productividad";
	FOOTNOTE H=1 JUSTIFY=LEFT "Gráficos de elaboración propia.";
RUN;
QUIT;
PROC GREPLAY IGOUT=work.gseg GOUT=work.gseg TC=work.tmplt NOFS;
	TDEF PK1 1 / LLX=0 LLY=0 		/*full panel for main titles */
				 ULX=0 ULY=100
		 		 LRX=100 LRY=0
		 		 URX=100 URY=100
			 2 / LLX=0 LLY=50 		/*left upper panel*/
		 		 ULX=0 ULY=88
		 		 LRX=50 LRY=50
		 		 URX=50 URY=88
			 3 / LLX=50 LLY=50 		/*right upper panel*/
		 		 ULX=50 ULY=88
		 		 LRX=100 LRY=50
		 		 URX=100 URY=88
			 4 / LLX=0 LLY=6 		/*left lower panel*/
		 		 ULX=0 ULY=44
		 		 LRX=50 LRY=6
		 		 URX=50 URY=44
			 5 / LLX=50 LLY=6 		/*right lower panel*/
		 		 ULX=50 ULY=44
		 		 LRX=100 LRY=6
		 		 URX=100 URY=44
			 ;
	RUN;
QUIT;
PROC GREPLAY IGOUT=work.gseg GOUT=work.gseg TC=work.tmplt NOFS;
	TEMPLATE PK1;
	TREPLAY 1:gslide
		2:gplot2
		3:gplot3
		4:gplot4
		5:gplot5
		;
QUIT;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;
/* ----------------------------------------------------------*/
/*      			 REPRESENTACIÓN CONJUNTA 2			     */
/* ----------------------------------------------------------*/
PROC GSLIDE;
	TITLE2 H=2 JUSTIFY=CENTER "Tasa de Variación de la productividad";
	FOOTNOTE H=1 JUSTIFY=LEFT "Gráficos de elaboración propia.";
RUN;
QUIT;
PROC GREPLAY IGOUT=work.gseg GOUT=work.gseg TC=work.tmplt NOFS;
	TDEF PK1 1 / LLX=0 LLY=0 		/*full panel for main titles */
				 ULX=0 ULY=100
		 		 LRX=100 LRY=0
		 		 URX=100 URY=100
			 2 / LLX=0 LLY=50 		/*left upper panel*/
		 		 ULX=0 ULY=88
		 		 LRX=50 LRY=50
		 		 URX=50 URY=88
			 3 / LLX=50 LLY=50 		/*right upper panel*/
		 		 ULX=50 ULY=88
		 		 LRX=100 LRY=50
		 		 URX=100 URY=88
			 4 / LLX=0 LLY=6 		/*left lower panel*/
		 		 ULX=0 ULY=44
		 		 LRX=50 LRY=6
		 		 URX=50 URY=44
			 5 / LLX=50 LLY=6 		/*right lower panel*/
		 		 ULX=50 ULY=44
		 		 LRX=100 LRY=6
		 		 URX=100 URY=44
			 ;
	RUN;
QUIT;
PROC GREPLAY IGOUT=work.gseg GOUT=work.gseg TC=work.tmplt NOFS;
	TEMPLATE PK1;
	TREPLAY 1:gslide
		2:gplot6
		3:gplot7
		4:gplot8
		5:gplot9
		;
QUIT;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;



/* ----------------------------------------------------------*/
/*      	 TASAS VARIACIÓN NIVEL REGIONAL, COMPLETO	     */
/* ----------------------------------------------------------*/
* %ordenar(TFG.CCAA,CCAA); * Si tenemos desordenados los datos;
PROC GPLOT DATA = TFG.CCAA;
	TITLE "Tasa de Variación de la productividad";
	FOOTNOTE1 J=l "Gráfico de elaboración propia.";
	AXIS1 LABEL=("Año" JUSTIFY=RIGHT);
	AXIS2 LABEL=("Tasa Variación" JUSTIFY=RIGHT);
	PLOT TVar_Prod*Anio / VREF = 0 HAXIS=axis1 VAXIS=axis2;
	SYMBOL INTERPOL=line WIDTH=1 C=steelblue;
	WHERE Anio > 2008;
	BY CCAA;
RUN;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;


/* ----------------------------------------------------------*/
/*     TASAS VARIACIÓN NIVEL REGIONAL-SECTORIAL, COMPLETO    */
/* ----------------------------------------------------------*/
* %ordenar(TFG.CCAA_SECTOR,CCAA); * Si tenemos desordenados los datos;
PROC GPLOT DATA = TFG.CCAA_SECTOR ;
	TITLE "Tasa de Variación de la productividad";
	FOOTNOTE1 J=l "Gráfico de elaboración propia.";
	AXIS1 LABEL=("Año" JUSTIFY=RIGHT);
	AXIS2 LABEL=("Tasa Variación" JUSTIFY=RIGHT);
	PLOT TVar_Prod*Anio=Sector / VREF = 0 LEGEND HAXIS=axis1 VAXIS=axis2;
	SYMBOL1 INTERPOL=line WIDTH=1 C=green;
	SYMBOL2 INTERPOL=line WIDTH=1 C=red;
	SYMBOL3 INTERPOL=line WIDTH=1 C=orange;
	SYMBOL4 INTERPOL=line WIDTH=1 C=steelblue;
	WHERE Anio > 2011;
	BY CCAA;
RUN;
TITLE; * Quitar el título;
FOOTNOTE; * Quitar footnote;

