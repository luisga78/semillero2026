
# Enero 30 de 2019
# 
# Primera sesion de practica con R project

# ----- Paquete necesarios que deben ser cargados -----

#para ejecutar líneas uso ctrl + enter

require(readxl) 

# library(readxl)

require(writexl)

require(tidyverse)

require(lattice) # Graficos

require(ggplot2) # Graficos

require(multcompView) # test de Tukey

require(lsmeans) # test de Tukey

library(multcomp)


# ----- Importación base de datos -----

# Camino para importacion de la base de datos

setwd("D:/Usb16gmarzo2013/backup4gfeb2013/Clases Unal/Genética Cuantitativa/2019/2/Prácticas R/1")

# Borrar todo el historial de analisis

rm(list=ls()) 

# Importacion de la base de datos

ej1 <- read.csv(file = "Población1-estudiantes.csv",
            header = TRUE,
            sep = ";",
            dec = ".")

ej1 <- read_xlsx("Población1-estudiantes.xlsx")

# ctrl + shift + c  para señalar todo el texto y dejarlo como comentario
# ej1=read.csv(file.choose(),header=T,sep=";",dec=",")
# ej1


# Para verificar como estan clasificadas las variables de la base de datos

class(ej1$PDEST)
class(ej1$PNAC)
class(ej1$EDAD)
class(ej1$EPOCA)
class(ej1$PARTO)
class(ej1$SEXO)
class(ej1$GC)

# Para convertir en factor

ej1$PARTO <- as.factor(ej1$PARTO)
class(ej1$PARTO)

ej1$GC <- as.factor(ej1$GC)
class(ej1$GC)


# ----- Estadísticas básicos -----

# Medias variables de interes

mean(ej1$PDEST)
summary(ej1$PDEST)

mean(ej1$PNAC)
summary(ej1$PNAC)

mean(ej1$EDAD)
summary(ej1$EDAD)

# Medias peso al destete para cada nivel dentro de cada factor

sapply(split(ej1$PDEST, ej1$EPOCA), mean)
sapply(split(ej1$PDEST, ej1$PARTO), mean)
sapply(split(ej1$PDEST, ej1$SEXO), mean)
sapply(split(ej1$PDEST, ej1$GC), mean)


# Desviacion estandar variables de interes

sd(ej1$PDEST)
sd(ej1$PNAC)
sd(ej1$EDAD)

# Varianza variables de interes

var(ej1$PDEST)
var(ej1$PNAC)
var(ej1$EDAD)


# Otra manera pero que permite vincular a una tabla todas las estimativas

ej1a <- ej1 %>% # ctrl + shift + M
    group_by(PARTO) %>%  
    summarise(medpd = mean(PDEST), desvpd = sd(PDEST), varpd = var(PDEST), 
              minpd = min(PDEST),  maxpd = max(PDEST), mediana = median(PDEST),
              n = length(PDEST))

write_xlsx(ej1a, "Resumenpd_np.xlsx")

# Correlacion entre dos variables

cor(ej1$PNAC, ej1$PDEST)

cor.test(ej1$PNAC, ej1$PDEST)

cor <- ej1 %>% 
    dplyr::select(PDEST, PNAC, EDAD)

require(corrplot)

corrplot::corrplot.mixed(cor(cor), lower="ellipse", upper="number")


# ----- Gráficos -----
# Histograma para cada variable

x11()
par(mfrow=c(1,2))
hist(ej1$PNAC, prob=TRUE, xlab="P. Nacimiento", ylab="Densidad", main="Histograma P. Nacimiento")
curve(dnorm(x, mean=mean(ej1$PNAC), sd=sd(ej1$PNAC)), add=TRUE,col="red")

hist(ej1$PDEST, prob=TRUE, xlab="P. Destete", ylab="Densidad", main="Histograma P. Destete")
curve(dnorm(x, mean=mean(ej1$PDEST), sd=sd(ej1$PDEST)), add=TRUE,col="red")

#Box Plot para cada variable

par(mfrow=c(1,2))
boxplot(ej1$PNAC, main="Box Plot P. Nacimiento", ylab="P. Nacimiento")
boxplot(ej1$PDEST, main="Box Plot P. Destete", ylab="P. Destete")

# Boxplot para peso al destete por nivel para cada factor

x11()
par(mfrow=c(2,2))
boxplot(ej1$PDEST ~ ej1$SEXO, col="blue", main="Box Plot P. Destete por Sexo", ylab="P. Destete")
boxplot(ej1$PDEST ~ ej1$EPOCA, col="blue", main="Box Plot P. Destete por Época", ylab="P. Destete")
boxplot(ej1$PDEST ~ ej1$PARTO, col="blue", main="Box Plot P. Destete por Parto", ylab="P. Destete")
boxplot(ej1$PDEST ~ ej1$GC, col="blue", main="Box Plot P. Destete por GC", ylab="P. Destete")


# Construcción del gráfico de hojas y ramas

stem(ej1$PDEST)

# Coeficiente de variacion

cvd=sd(ej1$PDEST)/mean(ej1$PDEST)*100; cvd 
cvn=sd(ej1$PNAC)/mean(ej1$PNAC)*100; cvn 


#Regresión con EDAD
lm(ej1$PDEST~ej1$EDAD)
summary(lm(ej1$PDEST~ej1$EDAD))

# Grafico de regresion

x11()
plot(ej1$PDEST~ej1$EDAD)
abline(lm(ej1$PDEST~ej1$EDAD))

# Valores predichos del modelo de regresion

lm.pdest1<-lm(ej1$PDEST~ej1$EDAD)
fitted(lm.pdest1)
resid(lm.pdest1)

summary(lm.pdest1)


# Anava pdest explicado por factores fijos


resultado <- aov(PDEST ~ EPOCA + PARTO + SEXO, # modelo estadistico utilizado
                ej1)

anova(resultado)
summary(resultado)

# ----- Modelo Lineal -----

# Modelo lineal pdest explicado por factores fijos

resultado <- lm(PDEST ~ PNAC + EDAD + EPOCA + PARTO + SEXO + GC, # modelo estadistico utilizado
                 ej1)

anova(resultado)
summary(resultado)

# Coeficiente de determinacion

rcuadradom1=summary(resultado)$r.squared
rcuadradom1

# Modelo lineal pdest explicado por factores fijos sin covariables

resultado1 <- aov(PDEST ~ EPOCA + PARTO + SEXO + GC, #modelo estadístico utilizado
   ej1)

# resultado1<- lm(PDEST ~ EPOCA + PARTO + SEXO, #modelo estadístico utilizado
#                  ej1)

anova(resultado1)
summary(resultado1)

 rcuadradom2=summary(resultado1)$r.squared
 rcuadradom2


# ----- Verificando las presuposiciones del modelo -----

# Obtencion de los residuos

res_ord <-residuals(resultado1)  # residuos ordinarios
res_pad<-rstandard(resultado1)       # residuos estandarizados
res_stud<-rstudent(resultado1)        # residuos studentizados
res1 <- data.frame(res_ord,res_pad,res_stud)

# Juntando vector de residuos con archivo de datos

ej2<-cbind(ej1, res1)
ej2

# Calculo de suma de cuadradros del residuo (SCResiduo)

ej2$rescuad = ej2$res_ord * ej2$res_ord 
sum(ej2$rescuad)


## (1) Pruebas de homogeneidad de varianzas

# Metodo Grafico

x11()
par(mfrow=c(2,3),pty="s")

plot(res_ord, col="blue", cex=0.7, xlab="Indices", ylab="Residuos Ordinarios")
abline(h=0,col="red", lwd=2)         
plot(res_pad, col="blue", cex=0.7, xlab="Indices", ylab="Residuos Estandarizados")
abline(h=0,col="red", lwd=2)         
plot(res_stud, col="blue", cex=0.7, xlab="Indices", ylab="Residuos estudentizados")
abline(h=0,col="red", lwd=2)          

plot(predict(resultado1),res_ord)
abline(h=0,col="red", lwd=2)
plot(predict(resultado1),res_pad)
abline(h=0,col="red", lwd=2)    
plot(predict(resultado1),res_stud)
abline(h=0,col="red", lwd=2)    


## 2) Verificando la Normalidad de los residuos

# Metodo grafico

x11()
par(mfrow=c(2,3),pty="s")

qqnorm(res_ord, col="blue", cex=0.7, xlab="Cuantiles teoricos",  ylab="Cuantiles muestrales")
qqline(res_ord,col="red", lwd=2)  
qqnorm(res_pad, col="blue", cex=0.7, xlab="Cuantiles teoricos",  ylab="Cuantiles muestrales")
qqline(res_pad,col="red", lwd=2)  
qqnorm(res_stud, col="blue", cex=0.7, xlab="Cuantiles teoricos",  ylab="Cuantiles muestrales")
qqline(res_stud,col="red", lwd=2)  

hist(res_ord,main="Residuos ordinarios",freq=F, ylim=c(0,0.5))
lines(seq(-3,4,0.01),dnorm(seq(-3,4,0.01)),col="red",lwd=3)
hist(res_pad,main="Residuos estandarizados",freq=F)
lines(seq(-3,4,0.01),dnorm(seq(-3,4,0.01)),col="red",lwd=3)
hist(res_stud,main="Residuos estudentizados",freq=F)
lines(seq(-3,4,0.01),dnorm(seq(-3,4,0.01)),col="red",lwd=3)

# Teste de Shapiro-Wilk para normalidad univariada

shapiro.test(res_ord)
shapiro.test(res_pad)
shapiro.test(res_stud)

# Grafico de residuos por orden de parto
x11()
boxplot(ej2$res_pad ~ ej2$PARTO, data=ej2)
abline(h=mean(ej2$res_pad),col="red")

# Histograma de residuos por orden de parto

histogram(~ej2$res_pad | ej2$PARTO, data = ej2, col = "green",
          xlab = "errores", ylab = "Probabilidad", type = "density",
          panel = function(x,...)
          {panel.histogram(x,...)
            panel.mathdensity(dmath = dnorm, col = "red",
                              args = list(mean=mean(x), sd = sd(x)))
          })

# Grafico de distribucion de residuos por orden de parto

densityplot(~ej2$res_pad | ej2$PARTO, ylab="Probabilidad", xlab="Errores")


#prueba de homogeneidad de varianzas

ej2$r.lev=abs(resid(resultado1))

prueba.levene=aov(r.lev~PARTO+GC+EPOCA+SEXO, ej2)

summary(prueba.levene)


# Test de Tukey

require(multcomp)

# Para epoca

ep = lsmeans(resultado1, 
             ~ EPOCA)

cld(ep,
    alpha=0.05,
    Letters=letters,      
    adjust="tukey") 

plot(ep,col="blue")

lsmeans(resultado1, list(pairwise ~ EPOCA))

# Orden de parto

np = lsmeans(resultado1, 
             ~ PARTO)

cld(np,
    alpha=0.05,
    Letters=letters,      
    adjust="tukey") 

plot(np,col="blue")

lsmeans(resultado1, list(pairwise ~ PARTO))

# Sexo

sx = lsmeans(resultado1, 
             ~ SEXO)

cld(sx,
    alpha=0.05,
    Letters=letters,      
    adjust="tukey") 

plot(sx,col="blue")

lsmeans(resultado1, list(pairwise ~ SEXO))

# Grupo contemporaneo

gc = lsmeans(resultado1, 
             ~ GC)

cld(gc,
    alpha=0.05,
    Letters=letters,      
    adjust="tukey") 

plot(gc,col="blue")

lsmeans(resultado1, list(pairwise ~ GC))

