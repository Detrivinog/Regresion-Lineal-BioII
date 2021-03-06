#Ejercicio de Regresi�n Lineal Simple
#David Esteban trivi�o
#
#La dureza de los �rboles es dif�cil de medir directamente, 
#sin embargo la densidad si es relativamente f�cil de medir. 
#Por ello es de gran inter�s disponer de un modelo que permita 
#predecir la dureza de un �rbol a partir de su densidad. 
#Por este motivo se ha tomado una muestra de 36 eucaliptos 
#australianos y se les midi� su densidad (X) y su dureza (Y ). 
#Las variables x[Kg/m3] y[kg]
densidad=c(247,248,273,284,284,290,303,327,356,385,388,393,394,399,403,406,407,
    407,429,458,469,482,515,515,534,560,565,573,576,592,598,660,674,688,691,691)
dureza=c(484,427,413,517,549,648,587,704,979,914,1070,1020,1210,989,1160,1010,1100,
    1130,1270,1180,1400,1760,1710,2010,1880,1980,1820,2020,1980,2310,1940,3260,
    2700,2890,2740,3140)
datos=data.frame(dureza,densidad)
write.csv2(datos, "Densidad.csv")


pairs(dureza~densidad) #Funci�n mas �til al tener mas variables regresoras
#Puede ser remplazada por una gr�fica de dispersi�n con plot()
plot(dureza~densidad,xlab="Densidad (Kg/m^3)",ylab="Dureza (Kg)")
#Correlaci�n Lineal Pearson
cor(datos) 
#Modelo de la regresi�n
regresion<-lm(dureza~densidad,data=datos)
summary(regresion) #Tabla de resumen
str(regresion) #
#Tabla Analisis de Varianza
anova(regresion)
#Gr�fica con la recta regresora
plot(densidad,dureza)
abline(regresion)
#Gr�ficas que fundamentan los supuesto de regresi�n
#Residuales, residuales estandarizados, palancamiento y Cuantiles
par(mfrow=c(2,2))
plot(regresion)
#
##Intervalo de confianza para B0 y B1 al 95%
confint(regresion,level=.95)
#
#Para la respuesta media
nuevas.den<-data.frame(densidad=seq(250,690,15))
nuevas.den #Nuevos datos de Densidades segun la secuencia de arriba
##Dureza estimada para cada dato de la densidad en nuevas.den
predict(regresion,nuevas.den) 
#Dureza estimada al 95%, con los intervalos de confianza
predict(regresion, nuevas.den,level=0.95,interval="confidence")
#Matriz de estimaci�n con los intervalos de confianza
IC<-predict(regresion,nuevas.den, interval="confidence")
#Grafica de estimaci�n media
par(mfrow=c(1,1))
plot(densidad,dureza,xlab="Densidad (Kg/m^3)",ylab="Dureza (Kg)", main="Respuesta Media",
     col="forestgreen")
abline(regresion) #linea regresora
lines(nuevas.den$densidad,IC[,2],lty=5,col=4) #Linea del intevalo inferior de confianza
lines(nuevas.den$densidad,IC[,3],lty=5,col=4) #Linea del intervalo superior de confianza
#Matriz de estimaci�n con los intervalos de predicci�n
IC.predict<-predict(regresion,nuevas.den, interval="prediction")
lines(nuevas.den$densidad,IC.predict[,2],lty=3,col="red") #Linea del intevalo inferior de predicci�n
lines(nuevas.den$densidad,IC.predict[,3],lty=3,col="red") #Linea del intevalo superior de predicci�n

IC[,1]-IC[,2]

#Dato numero 32 
