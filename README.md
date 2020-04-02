---
title: "Regresion Lineal"
author: "David Triviño"
date: "16 de febrero de 2020"
output: 
  github_document:
  toc: true
    toc_depth: 2
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
require(pander)
```

## Descripción del problema

La dureza de los árboles es dif?cil de medir directamente, 
sin embargo la densidad si es relativamente fácil de medir. 
Por ello es de gran inter?s disponer de un modelo que permita 
predecir la dureza de un ?rbol a partir de su densidad. 
Por este motivo se ha tomado una muestra de 36 eucaliptos 
australianos y se les midió su densidad (X) y su dureza (Y). 
Las variables x[Kg/m3] y[kg]

```{r}
densidad=c(247,248,273,284,284,290,303,327,356,385,388,393,394,399,403,406,407,
    407,429,458,469,482,515,515,534,560,565,573,576,592,598,660,674,688,691,691)
dureza=c(484,427,413,517,549,648,587,704,979,914,1070,1020,1210,989,1160,1010,1100,
    1130,1270,1180,1400,1760,1710,2010,1880,1980,1820,2020,1980,2310,1940,3260,
    2700,2890,2740,3140)
datos=data.frame(dureza,densidad)
```

## Análisis exploratorio

```{r pressure}
pairs(dureza~densidad)
```

```{r}
plot(dureza~densidad,xlab="Densidad (Kg/m^3)",ylab="Dureza (Kg)")
```

```{r}
cor(datos) 
```

## Modelo lineal

```{r, results='hide'}
regresion<-lm(dureza~densidad,data=datos)
summary(regresion)
anova(regresion)
```

```{r, echo=FALSE}
pander(summary(regresion))
pander(anova(regresion))
```

### Figura del modelo

```{r}
{plot(densidad,dureza)
abline(regresion)}
```

### Residuales

```{r}
{par(mfrow=c(2,2))
plot(regresion)}
```

### Intervalos de confianza para B_0 y B_1

```{r}
confint(regresion,level=.95)
```

### Predichos y estimados

```{r}
nuevas.den<-data.frame(densidad=seq(250,690,15))
IC<-predict(regresion,nuevas.den, interval="confidence")
IC.predict<-predict(regresion,nuevas.den, interval="prediction")
```

```{r, echo=FALSE}
{plot(densidad,dureza,xlab="Densidad (Kg/m^3)",ylab="Dureza (Kg)", main="Respuesta Media",
     col="forestgreen")
abline(regresion)
lines(nuevas.den$densidad,IC[,2],lty=5,col=4) 
lines(nuevas.den$densidad,IC[,3],lty=5,col=4)
lines(nuevas.den$densidad,IC.predict[,2],lty=3,col="red") 
lines(nuevas.den$densidad,IC.predict[,3],lty=3,col="red")} 

```