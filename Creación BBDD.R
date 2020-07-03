# _______________________________________________________________________________ #
# Facultad de Estudios Estadísticos - UCM
# TFG - Carmen Alcántara
# Curso académico 2019/2020
# _______________________________________________________________________________ #

#=================================================================================#
#                         COGEMOS LOS DATOS Y CREAMOS LA BBDD                     #
#=================================================================================#
# Vamos a leer los datos del Instituto Nacional de Estadística (INE)
library(pxR) # para leer datos desde archivos .px pc-axis del INE
library(dplyr) # para filtrar y seleccionar variables

#========================== 1. DATA FRAME PIB, BASE 2010 =========================# 
url <-"https://www.ine.es/jaxiT3/files/t/es/px/30679.px?nocab=1" # (índices)
# Cogemos los índices en vez de los datos reales porque el efecto monetario varía con el tiempo
datos <- as.data.frame(read.px(url))   # como un data.frame
# Creamos la variable "pib" para convertirla luego a serie temporal
pib <- datos%>%
  dplyr::filter(Niveles.y.tasas=="Dato base")%>%
  dplyr::filter(Agregados.macroeconómicos=="Producto interior bruto a precios de mercado")%>%
  dplyr::filter(Tipo.de.dato=="Datos ajustados de estacionalidad y calendario")%>%
  dplyr::mutate(ANO=as.numeric(substr(Periodo,1,4)))%>%
  dplyr::mutate(TRIM=as.numeric(substr(Periodo,6,6)))%>%
  dplyr::mutate(tiempo=ANO+TRIM*0.25-0.25)%>%
  dplyr::filter(tiempo < 2020)%>%
  dplyr::arrange(tiempo)%>%
  dplyr::select(tiempo, value)

# Tenemos datos de 1995.T1 - 2019.T4
head(pib)
tail(pib)

# Guardamos hasta dónde llega esta serie, para utilizarlo más adelante
tiempo_max<-max(pib$tiempo)
tiempo_max

# Convertimos los datos en una serie temporal
pib10.ts <- ts(pib$value, start=c(1995,1), frequency=4)

# Sacamos su tasa de crecimiento trimestral (T1) y anual (T4)
T110.ts <- pib10.ts/stats::lag(pib10.ts, k=-1)-1
T410.ts <- pib10.ts/stats::lag(pib10.ts, k=-4)-1

#========================== 2. DATA FRAME PIB, BASE 1995 =========================#
url <- "http://www.ine.es/jaxiT3/files/t/es/px/3156.px?nocab=1"
datos<-as.data.frame(read.px(url))   # como un data.frame
# Creamos la variable "pib" para convertirla luego a serie temporal
pib <- datos%>%
  dplyr::filter(Niveles.y.Tasas=="Dato base")%>%
  dplyr::filter(PIB.a.precios.de.mercado..oferta=="Producto interior bruto")%>%
  dplyr::filter(Datos=="Ciclotendencia")%>%
  dplyr::filter(Precios=="Precios constantes")%>%
  dplyr::mutate(ANO=as.numeric(substr(Periodo,1,4)))%>%
  dplyr::mutate(TRIM=as.numeric(substr(Periodo,6,6)))%>%
  dplyr::mutate(tiempo=ANO+TRIM*0.25-0.25)%>%
  dplyr::filter(tiempo < 2020)%>%
  dplyr::arrange(tiempo)%>%
  dplyr::select(tiempo, value)

# Tenemos datos de 1980.T1 - 2004.T4
head(pib)
tail(pib)
# Convertimos los datos en una serie temporal
pib95.ts <- ts(pib$value, start=c(1980,1), frequency = 4)

# Sacamos su tasa de crecimiento trimestral (T1) y anual (T4)
T195.ts <- pib95.ts/stats::lag(pib95.ts, k=-1)-1
T495.ts <- pib95.ts/stats::lag(pib95.ts, k=-4)-1


#============================= 3. ENLAZAMOS LAS SERIES ===========================#
# Vamos a unir ambas series a través de la tasa de crecimiento trimestral
# Creamos la variable tiempo de la serie completa:
tiempo <- ts(seq(1980.00,tiempo_max, by=0.25), start = c(1980,1), frequency = 4)
# Unimos las tasas para ver hasta dónde tenemos datos en cada una de ellas
ts.union(T195.ts,T110.ts)

# Con esto, ya paso a reconstruir la serie de manera recursiva
fusionpib <- ts.union(tiempo, T195.ts,T110.ts, pib10.ts, dframe = TRUE)
fusionpib$T1 <- fusionpib$T110.ts
fusionpib$T1[tiempo>1980 & tiempo<=1995] <- fusionpib$T195.ts[tiempo>1980 & tiempo<=1995]

# Represento la serie indicando los "trozos" correspondiente a cada año base.
plot(fusionpib$T1)
lines(fusionpib$T110.ts, col="red")
lines(window(fusionpib$T195.ts, start=1980, end=1995), col="blue")
abline(h=0, lty=2)  

# Reconstruyo recursivamente el pib con la tasa de crecimiento trimestral a través de:
# TC.PIB(t) = [PIB(t)/PIB(t-1)] - 1 .  Despejamos y obtenemos : PIB(t-1) = PIB(t) / [1 + TC.PIB(t)]
fusionpib$pib <- fusionpib$pib10.ts
for(i in seq(1994.75, 1980, by=-0.25)){
  fusionpib$pib[tiempo==i] <- fusionpib$pib[tiempo==(i+0.25)]/(1+fusionpib$T1[tiempo==(i+0.25)])
}
fusionpib$pib


#================== 4. CALCULO LAS TASAS DE LA SERIE RECONSTRUIDA ================#
# Guardamos la nueva serie reconstruida
pib.ts <- fusionpib$pib
# Sacamos su tasa de crecimiento trimestral (T1) y anual (T4)
T1.ts <- pib.ts/stats::lag(pib.ts, k=-1)-1
T4.ts <- pib.ts/stats::lag(pib.ts, k = -4)-1


#======================== 5. DATA FRAME EMPLEO, BASE 2010 ========================# 
url <- "https://ine.es/jaxiT3/files/t/es/px/30684.px?nocab=1"
datos <- as.data.frame(read.px(url), stringsAsFactor=FALSE)
# Creamos la variable "ocu" para convertirla luego a serie temporal
ocu<-datos%>%
  dplyr::filter(Niveles.y.Tasas =="Dato base")%>%
  dplyr::filter(Ramas.de.actividad=="Total")%>%
  dplyr::filter(Tipo.de.dato=="Datos ajustados de estacionalidad y calendario")%>%
  dplyr::filter(Población=="Puestos de trabajo equivalentes a tiempo completo")%>%
  dplyr::filter(Situación.profesional=="Ocupado/a")%>%
  dplyr::mutate(ANO=as.numeric(substr(Periodo,1,4)))%>%
  dplyr::mutate(TRIM=as.numeric(substr(Periodo,6,7)))%>%
  dplyr::mutate(tiempo=ANO+TRIM*0.25-0.25)%>%
  dplyr::filter(tiempo < 2020)%>%
  dplyr::arrange(tiempo)%>%
  dplyr::select(tiempo, value)

# Tenemos datos de 1995.T1 - 2019.T4
head(ocu)
tail(ocu)

# Guardamos hasta dónde llega esta serie, para utilizarlo más adelante
tiempo_max<-max(ocu$tiempo)
tiempo_max

# Convertimos los datos en una serie temporal
ocu10.ts<-ts(ocu$value, start=c(1995,1), frequency = 4)
plot(ocu10.ts)

# Sacamos su tasa de crecimiento trimestral (T1) y anual (T4)
T1ocu10.ts<-ocu10.ts/stats::lag(ocu10.ts, k = -1)-1
T4ocu10.ts<-ocu10.ts/stats::lag(ocu10.ts, k = -4)-1


#======================== 6. DATA FRAME EMPLEO, BASE 1995 ========================# 
url<-"http://www.ine.es/jaxiT3/files/t/es/px/3160.px?nocab=1"
datos<-as.data.frame(read.px(url), stringsAsFactor=FALSE)  #como un data.frame
# Creamos la variable "ocu" para convertirla luego a serie temporal
ocu<-datos%>%
  dplyr::filter(Niveles.y.Tasas =="Dato base")%>%
  dplyr::filter(Empleo.por.Rama.de.actividad=="Ocupados. Total ramas")%>%
  dplyr::filter(Datos=="Ciclotendencia")%>%
  dplyr::filter(Población=="Puestos de trabajo equivalentes a tiempo completo")%>%
  dplyr::mutate(ANO=as.numeric(substr(Periodo,1,4)))%>%
  dplyr::mutate(TRIM=as.numeric(substr(Periodo,6,6)))%>%
  dplyr::mutate(tiempo=ANO+TRIM*0.25-0.25)%>%
  dplyr::arrange(tiempo)%>%
  dplyr::select(tiempo, value)

# Tenemos datos de 1980.T1 - 2004.T4
head(ocu)
tail(ocu)

# Convertimos los datos en una serie temporal
ocu95.ts<-ts(ocu$value, start=c(1980,1), frequency = 4)

# Sacamos su tasa de crecimiento trimestral (T1) y anual (T4)
T1ocu95.ts<-ocu95.ts/stats::lag(ocu95.ts, k = -1)-1
T4ocu95.ts<-ocu95.ts/stats::lag(ocu95.ts, k = -4)-1


#============================= 7. ENLAZAMOS LAS SERIES ===========================#
# Vamos a unir ambas series a través de la tasa de crecimiento trimestral
# Creo la variable tiempo de la serie completa:
tiempo <- ts(seq(1980.00, tiempo_max, by=0.25), start = c(1980, 1), frequency = 4)
# Unimos las tasas para ver hasta dónde tenemos datos en cada una de ellas
ts.union(T1ocu95.ts,T1ocu10.ts)

# Con esto, ya paso a reconstruir la serie de manera recursiva
fusionocu <- ts.union(tiempo,T1ocu95.ts,T1ocu10.ts,ocu10.ts, dframe = TRUE)
fusionocu$T1ocu<-fusionocu$T1ocu10.ts
fusionocu$T1ocu[tiempo>1980 &tiempo<=1995]<-fusionocu$T1ocu95.ts[tiempo>1980 &tiempo<=1995]

# Represento la serie indicando los "trozos" correspondiente a cada año base.
plot(fusionocu$T1ocu)
lines(fusionocu$T1ocu10.ts, col="red")
lines(window(fusionocu$T1ocu95.ts,start=1980, end=1995), col="blue")
abline(h=0,lty=2)  

# Reconstruyo recursivamente el pib con la tasa de crecimiento trimestral a través de:
# TC.ocu(t) = [ocu(t)/ocu(t-1)] - 1  .  Despejamos y obtenemos : ocu(t-1) = ocu(t) / [1 + TC.ocu(t)]
fusionocu$ocu <- fusionocu$ocu10.ts
for(i in seq(1994.75, 1980, by=-0.25)){
  fusionocu$ocu[tiempo==i] <- fusionocu$ocu[tiempo==(i+0.25)]/(1+fusionocu$T1ocu[tiempo==(i+0.25)])
}
fusionocu$ocu


#================== 8. CALCULO LAS TASAS DE LA SERIE RECONSTRUIDA ================#
# Guardamos la nueva serie reconstruida
ocu.ts<-fusionocu$ocu
# Sacamos su tasa de crecimiento trimestral (T1) y anual (T4)
T1ocu.ts <- ocu.ts/stats::lag(ocu.ts, k=-1)-1
T4ocu.ts <- ocu.ts/stats::lag(ocu.ts, k = -4)-1


#======================== 9. GUARDAMOS LAS SERIES OBTENIDAS ======================#
# Fusionamos los datos en un solo data frame.
okun<-ts.union(tiempo=time(pib.ts),pib.ts,ocu.ts,"T1pib.ts"=T1.ts,"T4pib.ts"=T4.ts,
               T1ocu.ts,T4ocu.ts, dframe = TRUE)

# Guardamos los datos: .RData format
save(okun, file = "TFGdataokun.RData")

