# _______________________________________________________________________________ #
# Facultad de Estudios Estadísticos - UCM
# TFG - Carmen Alcántara
# Curso académico 2019/2020
# _______________________________________________________________________________ #

# Librerías necesarias
library(dplyr) # para filtrar y seleccionar variables
library(ggplot2) # para gráficos
library(tidyverse) # para gráficos
library(seasonal) # para ajustar la tendencia
library(tseries) # para hacer el test de Jarque-Bera y ADF
library(lmtest) # para hacer el contraste de Ramsey
library(stargazer) # para presentar mejor los modelos
library(forecast) # para sacar ACF y PACF directamente
library(lmtest) # para el test de Durbin-Watson 
library(urca) # para ur.test ()
library(dynlm) # para hacer lm con objetos que son series temporales
library(devtools)
devtools::install_github("KevinKotze/tsm") # para la librería TSM
library(tsm) # para la función BND (descomposición Beveridge-Nelson)
library(mFilter) # para hacer la descomposición de Hodrick-Prescott
library(strucchange) # para el test de cambio estructural

#=================================================================================#
#                          CARGAMOS LOS DATA FRAMES CREADOS                       #
#=================================================================================#
setwd("~/Desktop")
# Cargamos los datos de nuestras series
load(file="TFGdataokun.RData")
class(okun)  # data.frame
pib <- window(okun[,2], start=1980, end=2019.75)
ocu <- window(okun[,3], start=1980,  end=2019.75)



#=================================================================================#
#                    GRÁFICAS DEL CRECIMIENTO DEL PIB VS EMPLEO                   #
#=================================================================================#
#====================== 1. VARIACIÓN TRIMESTRAL PIB Y EMPLEO =====================#
ggplot(okun) +
  geom_col (aes(tiempo, T1ocu.ts, fill="OCUPADOS")) +
  geom_line(aes(x=tiempo, y=T1pib.ts, colour="PIB"), size=1) +
  theme_bw() +
  theme (legend.position="bottom") +
  scale_color_manual(values=c("PIB"="black", "OCUPADOS")) +
  labs(colour=" ", fill=" ")



#=================================================================================#
#                              DESCOMPOSICIÓN C/P Y L/P                           #
#=================================================================================#

#============================= 1. LOG PIB Y LOG OCU ==============================#
# Cogemos el logaritmo tanto del PIB como de los ocupados.
lpib0 <- log(pib)
locu0 <- log(ocu)

# A pesar de que cogimos los datos ajustados de estacionalidad y calendario,
# los ajustados estacionalmente al pasarlos a logaritmos para asegurandos de
# que no está presente la componente estacional. 
lpib <- trend(seas(lpib0)) 
locu <- trend(seas(locu0))

# Comprobamos que sean semejantes.
rbind(lpib0,lpib)[,1:10]
rbind(locu0,locu)[,1:10]
# Efectivamente, por lo que comentábamos antes, son muy parecidos.

# Con esto, pasamos a descomponer las series del PIB y del empleo en una parte
# a corto plazo y otra a largo plazo a partir del filtro de Hodrick-Prescott
# y el de Beveridge-Nelson, para compararlos posteriormente.


#========================= 2. FILTRO DE HODRICK-PRESCOTT =========================#
# Aplicamos el filtro de Hodrick-Prescott tanto al pib como a los ocupados:

# PIB:
# Cogemos freq=1600 porque es lo que se recomienda para datos trimestrales
hp.decomP <- hpfilter(lpib, freq = 1600, type = "lambda")
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(lpib, ylab = "")  # plot time series
lines(hp.decomP$trend, col = "red")  # include HP trend
legend("topleft", legend = c("data", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(hp.decomP$cycle, ylab = "")  # plot cycle
legend("topleft", legend = c("HPcycle"), lty = 1, col = c("black"), 
       bty = "n")

# OCU:
# Cogemos freq=1600 porque es lo que se recomienda para datos trimestrales
hp.decomO <- hpfilter(locu, freq = 1600, type = "lambda")
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(locu, ylab = "")  # plot time series
lines(hp.decomO$trend, col = "red")  # include HP trend
legend("topleft", legend = c("data", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(hp.decomO$cycle, ylab = "")  # plot cycle
legend("topleft", legend = c("HPcycle"), lty = 1, col = c("black"), 
       bty = "n")


#========================= 3. FILTRO DE BEVERIDGE-NELSON =========================# 
# Pasamos a hacer ahora el filtro de Beveridge-Nelson tanto al pib como a los ocupados:

# PIB:
bn.decompP <- bnd(lpib, nlag = 8)  # apply the BN decomposition that creates dataframe
bn.trendP <- ts(bn.decompP[, 1], start = c(1980, 1), frequency = 4)  # first column contains trend
bn.cycleP <- ts(bn.decompP[, 2], start = c(1980, 1), frequency = 4)  # second column contains cycle
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(lpib, ylab = "")
lines(bn.trendP, col = "red")
 legend("topleft", legend = c("data", "BNtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(bn.cycleP, ylab = "")
legend("bottomleft", legend = c("BNcycle"), lty = 1, col = c("black"), 
       bty = "n")

# OCU:
bn.decompO <- bnd(locu, nlag = 8)  # apply the BN decomposition that creates dataframe
bn.trendO <- ts(bn.decompO[, 1], start = c(1980, 1), frequency = 4)  # first column contains trend
bn.cycleO <- ts(bn.decompO[, 2], start = c(1980, 1), frequency = 4)  # second column contains cycle
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(locu, ylab = "")
lines(bn.trendO, col = "red")
legend("bottomright", legend = c("data", "BNtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(bn.cycleO, ylab = "")
legend("bottomleft", legend = c("BNcycle"), lty = 1, col = c("black"), 
       bty = "n")


#================================ 4. CONCLUSIONES ================================#
# Comparamos las descomposiciones
# PIB:
combP1 <- ts.union(hp.decomP$trend, bn.trendP, lpib)
combP2 <- ts.union(hp.decomP$cycle, bn.cycleP)
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 2, 1), cex = 0.8)
plot.ts(combP1, ylab = "", plot.type = "single", col = c("blue", "red","black"))
  legend("topleft", legend = c("hp-filter", "bn-filter", "data"), lty = 1, col = c("blue", "red","black"), bty = "n")
plot.ts(combP2, ylab = "", plot.type = "single", col = c("blue", "red"))
  legend("bottomleft", legend = c("hp-filter", "bn-filter"), lty = 1, col = c("blue", "red"), bty = "n")
  abline(h=0, lty=2)

# OCU:
combO1 <- ts.union(hp.decomO$trend, bn.trendO, locu)
combO2 <- ts.union(hp.decomO$cycle, bn.cycleO)
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 2, 1), cex = 0.8)
plot.ts(combO1, ylab = "", plot.type = "single", col = c("blue", "red","black"))
  legend("topleft", legend = c("hp-filter", "bn-filter", "data"), lty = 1, col = c("blue", "red","black"), bty = "n")
plot.ts(combO2, ylab = "", plot.type = "single", col = c("blue", "red"))
  legend("bottomleft", legend = c("hp-filter","bn-decomp"), lty = 1, col = c("blue", "red"), bty = "n")
  abline(h=0, lty=2)

# El filtro de HP se ajusta bastante en ambos casos a largo plazo (tendencia), mientras
# que BN no. En cambio, a corto plazo (ciclo), BN muestra mejor las épocas de crisis,
# estando HP más suavizado.
  
# Como lo que nos interesa en la tendencia, nos quedaremos con los resultados
# obtenidos con el filtro de HP.
  

#=========================== 5. CREACIÓN GAP VARIABLES ===========================# 
# Vamos a crear las GAP variables, del PIB y del empleo, para utilizarlas más adelante.
# Esto lo vamos a hacer a partir de la diferencia de la serie con respecto a su componente
# permanente, es decir, la tendencia (L/P) obtenida con Hodrick-Prescott.
gappib <- lpib - hp.decomP$trend
gapocu <- locu - hp.decomO$trend

  
# _______________________________________________________________________________ #


#=================================================================================#
#                                   LEY DE OKUN (I)                               # 
#=================================================================================#

#=========================== 1. REPRESENTACIÓN CONJUNTA ==========================# 
# Como vemos que tienen una relación positiva y están correlacionadas, vamos a
# ver si esta relación es real o podría ser espuria.

# Como queremos representar ambas series conjuntamente para ver si tienen un 
# comportamiento similar, debemos utilizar los logaritmos calculados antes, 
# puesto que tienen escalas diferentes el PIB y el empleo.

# Miramos la evolución gráficamente.
par(mfrow = c(1, 1))
plot(scale(locu), type="l", col="red")
lines(scale(lpib), type="l", col="black")
legend("bottom", legend = c("ocu", "pib"),lty=c(1), cex=0.65, lwd=c(1,1),
       bty="n",  col=c("red", "black"))
# Observando la gráfica, podríamos decir que están cointegradas,
# puesto que  tienen una evolución muy similar. Por lo tanto, vamos
# a comprobar si esta relación es real o espuria.


#====================== 2. AUTOCORRELACIÓN DE LOS RESIDUOS =======================#
# Primero miramos si el parámetro del PIB es significativo o no.
modelo.lm <- lm(gapocu~gappib)
stargazer(modelo.lm, type="text")
# El parámetro del PIB nos sale significativo, mientras que la constante no.

# Hacemos el test de Ljung-Box para ver si los residuos están 
# autocorrelados o no.
# H0: las autocorrelaciones hasta orden p son iguales a cero
checkresiduals(modelo.lm, test="LB")
ggtsdisplay(modelo.lm$residuals) 
# Gráfico de los residuos: se ve que hay cierta tendencia.
# ACF: el parámetro correspondiente al primer retardo está
# muy próximo a 1, además de decrecer lentamente.
# Histograma: no siguen una normal.
# PACF: el parámetro del primer retardo es significativamente
# distinto de cero.
# Ljung-Box: tenemos un p-valor < 2.2e-16.
# Con todo esto, rechazamos H0, por lo que los residuos están
# autocorrelados.


# Con estos resultados, podemos sospechar que tenemos alguna raíz
# unitaria, por lo que podría ser que la serie no fuese estacionaria.


#=================== 3. RAÍCES UNITARIAS / SERIE ESTACIONARIA ====================# 
# Para hacer esto de forma más precisa, utilizamos el Test de Dickey-fuller
# H0 : Hay una raíz unitaria / Serie no estacionaria
# H1 : No hay una raíz unitaria / Serie estacionaria

# 1. LPIB y LOCU
adf.test(na.omit(lpib)) # p-valor = 0.6414, por lo que acepto H0, hay una raíz unitaria
adf.test(na.omit(locu)) # p-valor = 0.2105, por lo que acepto H0, hay una raíz unitaria

# Probamos a repetir el test con una diferencia, para ver si así la serie pasa    
# a ser estacionaria.
adf.test(na.omit(diff(lpib, differences=1))) # p-valor = 0.09907, por lo que rechazo H0, ya no hay una raíz unitaria
adf.test(na.omit(diff(locu, differences=1))) # p-valor = 0.04998, por lo que rechazo H0, ya no hay una raíz unitaria


# 2. GAPPIB y GAPOCU
adf.test(na.omit(gappib)) # p-valor = 0.0103, por lo que rechazo H0, no tenemos raíz unitaria
adf.test(na.omit(gapocu)) # p-valor = 0.01, por lo que rechazo H0, no tenemos raíz unitaria
# (esto es buena señal)


# Además de las raices unitarias, tenemos que ver también si hay tendencia
# y si la constante es igual a cero o no. Para ello, hacemos los contrastes con la librería urca.


#==================== 4. SERIE DEL PIB SIN DIFERENCIAS (URCA) ====================# 
# NUESTRO NIVEL DE SIGNIFICACIÓN ES DEL 10%

# Modelo completo (con drift y trend) - MODELO SELECCIONADO
tru <- ur.df(lpib, type = c("trend"), lags = 12, 
             selectlags = c("BIC")) # máximo de 12 retardos, con el término bayesiano
summary(tru)
#             Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept)  3.657e-02  2.100e-02   1.742     0.083753  ----> RECHAZO H0       
# tt           4.182e-05  3.416e-05   1.224     0.222860  ----> ACEPTO H0     

# TAU3 (H0: gamma=0) : estadístico = -1.62 > -3.13 ----> ACEPTO H0
# PHI2 (H0: gamma=drift=trend=0) : estadístico = 3.53 < 4.07  ----> ACEPTO H0
# PHI3 (H0: gamma=trend=0) : estadístico = 2.12 < 5.47  ----> ACEPTO H0

# Todo esto nos indica que debemos quedarnos con el modelo sin tendencia y quizá sin constante,
# pero en la representación que hicimos se veía claramente que la tendencia está presente.

# De esta forma nos quedamos con este modelo y vemos que hay que diferenciar la serie
# del PIB al menos una vez.


#===================== 5. SERIE DEL OCU SIN DIFERENCIAS (URCA) ===================# 
# Modelo completo (con drift y trend) - MODELO SELECCIONADO
tru <- ur.df(locu, type = c("trend"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
#             Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept) 4.332e-02   1.460e-02   2.967     0.00355   ----> RECHAZO H0   
# tt          1.567e-05   5.861e-06   2.674     0.00841   ----> RECHAZO H0    

# TAU3 (H0: gamma=0) : estadístico = -2.95 > -3.13 ----> ACEPTO H0
# PHI2 (H0: gamma=drift=trend=0) : estadístico = 3.41 < 4.07  ----> ACEPTO H0
# PHI3 (H0: gamma=trend=0) : estadístico = 4.37 < 5.47  ----> ACEPTO H0

# En este caso los contrastes nos llevan a quedarnos con un modelo sin tendencia
# ni constante, mientras que la significación individual nos indica lo contrario.
# Como en la representación que hicimos se veía claramente que la tendencia está presente,
# nos quedaremos con este modelo.

# De esta forma vemos que también hay que diferenciar la serie del empleo una vez por lo menos.


#================ 6. SERIE DEL PIB EN PRIMERAS DIFERENCIAS (URCA) ================# 
# Representamos la serie para hacernos una idea:
plot(diff(lpib,differences=1))
# Ya no vemos que esté presente la tendencia.

# Modelo completo (con drift y trend)
tru <- ur.df(diff(lpib,differences=1), type = c("trend"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
#             Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept) 2.512e-03   1.063e-03   2.362     0.019531 ----> RECHAZO H0    
# tt          -1.210e-05  9.407e-06   -1.286    0.200524 ----> ACEPTO H0     

# TAU3 (H0: gamma=0) : estadístico = -3.78 < -3.13 ----> RECHAZO H0
# PHI2 (H0: gamma=drift=trend=0) : estadístico = 4.79 > 4.07  ----> RECHAZO H0
# PHI3 (H0: gamma=trend=0) : estadístico = 7.19 > 5.47  ----> RECHAZO H0

# Como nos indica la significación individual, no tendremos tendencia
# pero sí constante, por lo que nuestro modelo será el que incluya la
# constante sin tendencia.

# Modelo con drift pero sin trend - MODELO SELECCIONADO
tru <- ur.df(diff(lpib,differences=1), type = c("drift"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
# TAU2 (H0: gamma=0) : estadístico = -3.55 < -2.57  ----> RECHAZO H0
# PHI1 (H0: gamma=drift=0): estadístico = 6.33 > 3.81   ----> RECHAZO H0

# Por lo tanto, ya no tenemos raíz unitaria. La serie del PIB es estacionaria
# y tiene un orden de integración de I(1).


#================ 7. SERIE DE OCU EN PRIMERAS DIFERENCIAS (URCA) =================# 
# Representamos la serie para hacernos una idea:
plot(diff(locu,differences=1))
# Ya no vemos que esté presente la tendencia.

# Modelo completo (con drift y trend)
tru <- ur.df(diff(locu,differences=1), type = c("trend"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
#             Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept) 1.640e-04   2.597e-04   0.631     0.5288  ----> ACEPTO H0    
# tt          -1.782e-07  2.624e-06   -0.068    0.9459  ----> ACEPTO H0         

# TAU3 (H0: gamma=0) : estadístico = -2.26 > -3.13 ----> ACEPTO H0
# PHI2 (H0: gamma=drift=trend=0) : estadístico = 1.75 < 4.07  ----> ACEPTO H0
# PHI3 (H0: gamma=trend=0) : estadístico = 2.58 < 5.47  ----> ACEPTO H0

# Todo ello nos indica que debemos fijarnos en el modelo sin tendencia ni constante.

# Modelo sin drift y sin trend - MODELO SELECCIONADO
tru <- ur.df(diff(locu,differences=1), type = c("none"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
# TAU1 (H0: gamma=0) : estadístico = -1.94 < -1.62 ----> RECHAZO H0

# Por lo tanto, basándonos en el modelo sin tendencia ni constante, ya no tenemos raíz unitaria.
# La serie de empleo es estacionaria, con un orden de integración de I(1).


#=================== 8. SERIE DE GAPPIB SIN DIFERENCIAS (URCA) ===================# 
# Representamos la serie para hacernos una idea:
plot(gappib)
# No vemos que esté presente ninguna tendencia.

# Modelo completo (con drift y trend)
tru <- ur.df(gappib, type = c("trend"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
#             Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept) -7.566e-05  7.505e-04   -0.101    0.919845 ----> ACEPTO H0    
# tt          2.548e-07   7.825e-06   0.033     0.974071 ----> ACEPTO H0         

# TAU3 (H0: gamma=0) : estadístico = -3.91 < -3.13 ----> RECHAZO H0
# PHI2 (H0: gamma=drift=trend=0) : estadístico = 5.10 > 4.07  ----> RECHAZO H0
# PHI3 (H0: gamma=trend=0) : estadístico = 7.66 > 5.47  ----> RECHAZO H0

# Como nos indica la significación individual, no tendremos ni tendencia
# ni constante, por lo que nuestro modelo será el simple.

# Modelo sin drift y sin trend - MODELO SELECCIONADO
tru <- ur.df(gappib, type = c("none"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
# TAU1 (H0: gamma=0) : estadístico = -3.93 < -1.62 ----> RECHAZO H0

# Por lo tanto, basándonos en el modelo sin tendencia ni constante, no tenemos raíz unitaria.
# La serie de GAPPIB es estacionaria, con un orden de integración de I(0).


#=================== 9. SERIE DE GAPOCU SIN DIFERENCIAS (URCA) ===================# 
# Representamos la serie para hacernos una idea:
plot(gapocu)
# No vemos que esté presente ninguna tendencia.

# Modelo completo (con drift y trend)
tru <- ur.df(gapocu, type = c("trend"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
#             Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept) -1.430e-04  2.138e-04   -0.669    0.504855 ----> ACEPTO H0    
# tt          1.420e-06   2.229e-06   0.637     0.525061 ----> ACEPTO H0         

# TAU3 (H0: gamma=0) : estadístico = -3.80 < -3.13 ----> RECHAZO H0
# PHI2 (H0: gamma=drift=trend=0) : estadístico = 4.92 > 4.07  ----> RECHAZO H0
# PHI3 (H0: gamma=trend=0) : estadístico = 7.38 > 5.47  ----> RECHAZO H0

# Como nos indica la significación individual, no tendremos ni tendencia
# ni constante, por lo que nuestro modelo será el simple.

# Modelo sin drift y sin trend - MODELO SELECCIONADO
tru <- ur.df(gapocu, type = c("none"), lags = 12, 
             selectlags = c("BIC"))
summary(tru)
# TAU1 (H0: gamma=0) : estadístico = -3.80 < -1.62 ----> RECHAZO H0

# Por lo tanto, basándonos en el modelo sin tendencia ni constante, no tenemos raíz unitaria.
# La serie de GAPOCU es estacionaria, con un orden de integración de I(0).


# Esto es buena señal, puesto que lpib y locu salen I(1), lo que quiere decir que están 
# cointegradas y puedo usar gap. Estos resultados equivaldrían a que gappib y
# gapocu salgan I(0), que es lo que obtenemos, por lo que podemos usar este modelo. 

# Además de esto, podríamos comprobar que los errores, tanto del modelo con locu y lpib como
# el modelo con gapocu y gappic, tienen un orden de integración I(0), pero al cumplirse
# lo indicado anteriormente no hace falta.


# _______________________________________________________________________________ #


#=================================================================================#
#                                       "GAP"                                     # 
#=================================================================================#
# Vamos a hacer la diferencia de la serie con respecto a su componente permanente (gap variables).
# Esto es equivalente a ECM, por eso se necesita que las series sean I(1).
# Estimamos la ecuación:  Et – Et* = β(Yt – Yt*) + εt^ey , β > 0
# Hacemos la diagnosis de residuos para ver si está OK (sobre todo autocorrelación).
modelo1 <- lm(gapocu ~ gappib, na.action=NULL)
summary(modelo1)
# Nos sale que la constante no es significativa, por lo que probaremos a eliminarla.
AIC1 <- AIC(modelo1) # -1220.979

# Para guardarnos las gráficas del análisis de los residuos:
setwd("~/Desktop/RESIDUOS TFG")
# Análisis de los residuos
png(filename="residuos_1.png")
ggtsdisplay(modelo1$residuals, main="Residuos modelo sin retardos (AIC= -1220.979)")
dev.off()
# Obtenemos que los residuos están autocorrelados, puesto que tanto en la ACF como
# en la PACF hay retardos que son significativamente distintos de cero, lo cual es mala señal.


# Probamos si mejor con la ecuación que tiene retardos de Y:
# Et – Et*= β0 + β1(Yt – Yt*) + β2(Yt–1 – Yt–1*) + εt ;
# Hay que ver cuántos retardos metemos de Y, si uno o más. Podemos usar también los criterios AIC/BIC.
modelo2 <- dynlm(gapocu ~ gappib + L(gappib,1))
summary(modelo2)
# La constante vuelve a salir no significativa.
AIC2 <- AIC(modelo2) # -1224.308
# Análisis de los residuos
png(filename="residuos_2.png")
ggtsdisplay(modelo2$residuals, main="Residuos modelo con un retardo (AIC= -1224.308)")
dev.off()
# Obtenemos que los residuos están autocorrelados, puesto que tanto en la ACF como
# en la PACF hay retardos que son significativamente distintos de cero, igual que en el modelo sin retardos.

# Como vemos que ambos modelos muestran autocorrelación en los residuos, probamos
# distintos modelos ARIMA (tabla completa en el TFG).


# Probando a introducir retardos en el modelo, no se soluciona el problema de la autocorrelación,
# por lo que quizá se deba a las reformas laborales realizadas en España en el periodo al
# que pertenecen nuestros datos.


#=================================================================================#
#                                CAMBIO ESTRUCTURAL                               # 
#=================================================================================#
# Como a lo largo del tiempo se han realizado varias reformas laborales, vamos a ver
# si estas pueden ser las que estén ocasionando la autocorrelación en los residuos.

# Las reformas laborales que se han realizado dentro de nuestra base de datos han sido:
# 1980: Estatuo de los trabajadores
# 1984: Inicio de la temporalidad
# 1988: Nuevos contratos temporales y basura
# 1992: Reforma del seguro desempleo
# 1994: Reforma importante que da entrada entre otras a las empresas de trabajo temporal (ETT)
# 1997: Reforma pactada
# 2001-2002: Reforma de Aznar con huelga y marcha atr´as
# 2006: Reforma leve
# 2009: Reforma leve
# 2010: Reforma importante ante la crisis, continuada en 2012 con otra reforma importe

# Analizamos el cambio estructural para nuestra variable dependiente, gapocu. 
summary(lm(gapocu ~ 1))
break_point <- breakpoints(gapocu ~ 1)
# Representamos estos "break_points"
plot(break_point)
# Vemos que se podrían llegar a considerar hasta 5 cambios estructurales.
summary(break_point)
# Mirando el valor del BIC, el mejor de todos es en el que se consideran 4 cambios estructurales,
# puesto que es el que menor valor tiene.
# En este caso, los cambios estructurales se producen en el primer trimestre del 1987 y
# de 1993, y en el cuarto trimestre de 2005 y 2011.


# Como tenemos cambio estructural, creamos variables dummies para introducir en nuestro
# modelo y que quede de esta forma indicado.


# Dummies
d1 <- c(rep(0, break_point$breakpoints[1]),  rep(1, length(gapocu) - break_point$breakpoints[1]))
d2 <- c(rep(0, break_point$breakpoints[2]),  rep(1, length(gapocu) - break_point$breakpoints[2]))
d3 <- c(rep(0, break_point$breakpoints[3]),  rep(1, length(gapocu) - break_point$breakpoints[3]))
# d4 <- c(rep(0, break_point$breakpoints[4]),  rep(1, length(gapocu) - break_point$breakpoints[4]))

# Convertimos las dummies en series temporales
d1.ts <- ts(d1, start=c(1980, 1), end=c(2019, 4), frequency=4)
d2.ts <- ts(d2, start=c(1980, 1), end=c(2019, 4), frequency=4)
d3.ts <- ts(d3, start=c(1980, 1), end=c(2019, 4), frequency=4)
gappib.ts <- ts(gappib, start=c(1980, 1), end=c(2019, 4), frequency=4)

# Guardamos juntas todas las variables, incluyendo las interacciones de las dummies con la
# variable gappic y su retardo.
series <- ts.intersect(gapocu, gappib, stats::lag(gappib,-1), d1.ts, d2.ts, d3.ts, d1.ts*stats::lag(gappib,-1),
                       d2.ts*stats::lag(gappib,-1), d3.ts*stats::lag(gappib,-1), dframe=FALSE)


# Para hacernos una idea del modelo que podríamos que tener que ajustar, utilizamos
# la función de auto.arima
modelo.auto <- auto.arima(series[,1], xreg= series[,2:9], stepwise=FALSE, trace = TRUE, seasonal=TRUE)
# Nos indica que tendríamos que hacer un ARIMA(3,0,0)(0,0,2)[4]

# Usando esto como referencia, vamos a ir probando ajustes del modelo, tratando de
# encontrar un modelo que no presente autocorrelación en los residuos.

# EL MEJOR MODELO OBTENIDO ES:
# ARIMA (2,0,3)(0,0,2) sin constante 
modeloDEF <- Arima(series[,1], order = c(2,0,3), seasonal = c(0,0,2), xreg = series[,c(2,3,7,8,9)], include.mean = FALSE)
summary(modeloDEF)
AIC2modeloDEF <- AIC(modeloDEF) # -1731.136
# Análisis de los residuos
png(filename="residuos_modeloDEF.png")
ggtsdisplay(modeloDEF$residuals, main="Residuos ARIMA (2,0,3)(0,0,2) sin cte (AIC= -1731.136)")
dev.off()
# Residuos autocorrelados

# De todos los modelos probados, incluido el que nos sugería el auto.arima, el que mejor AIC tiene es
# un ARIMA (2,0,3)(0,0,2) sin constente. La autocorrelación se corrige bastante, saliendo significativamente
# distintos de cero algunos retardos a partir de, aprox, el 18, pero estos podríamos ignorarlos.

# Por el AIC y la poca diferencia que hay entre las ACF y PACF de estos modelos, el mejor sería el 28,
# que corresponde a un ARIMA (3,0,3) sin constante, aunque no termina de corregirse la autocorrelación del todo.



