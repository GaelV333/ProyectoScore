########## ----------------- AJUSTE DE PARAMETROS A DISTRIBUCION -------------------------

install.packages("spatstat.explore")
install.packages("VGAM")
install.packages("actuar")
install.packages("MASS")
install.packages("fitdistrplus")
install.packages("ggplot2")
install.packages("moments")
install.packages("nortest")
install.packages("survival")
install.packages("momentfit")
install.packages("vcd")
install.packages("fdth")
install.packages("ExtDist")
install.packages("dplyr")
install.packages("readxl")
install.packages("ks")
install.packages("kdensity")
install.packages("EnvStats")
library(actuar)
library(MASS)
library(fitdistrplus)
library(ggplot2)
library(moments)
library(nortest)
library(survival)
library(momentfit)
library(vcd)
library(fdth)
library(ExtDist)
library(dplyr)
library(readxl)
library(ks)
library(kdensity)

BIMBO <- read_excel("C:/Users/l/Downloads/JAGAPRO.xlsx", 
                    sheet = "BIMBO")
BIMBO2 = na.omit(BIMBO)
BIMBO2

Utilidad_neta = BIMBO2 %>% select(Utilidad_nt)
Deuda_total = BIMBO2 %>% select(Deudat)
Ingresos_tr = BIMBO2 %>% select(Ingresos_trimestrales)
Capital_cont = BIMBO2 %>% select(Capital_cont)
View(Utilidad_neta)

#Hacer vector
Utilidad_neta = as.numeric(Utilidad2$Utilidad_nt) 
Deuda_total = as.numeric(BIMBO2$Deudat)
Ingresos_tr = as.numeric(BIMBO2$Ingresos_trimestrales)
Capital_cont = as.numeric(BIMBO2$Capital_cont)

# Utilidad Neta positiva
Utilidad2 = BIMBO2 %>%  filter(Utilidad_nt>0)
View(Utilidad2)

descdist(data = Utilidad_neta, graph = TRUE,boot=1000)

############################################## Algoritmo todas las distr

#1. Ajustar la muestra con la funci?n *fitdist* y escoger la distribuci?n deseada, a trav?s del m?todo *MLE*.
#2. Llamar el resumen del ajuste con *summary*.
#3. Obtener los IC con la funci?n *confint*.
#4. Realizar la prueba **KS** y **AD**, con la funci?n *ks.test* y *ad.test*, empleando una nueva muestra con los par?metros estimados.
#5. Cambiar cada respectiva variable según se este analizando

######### Ajuste LogNormal
fitlogn<-fitdist(Capital_cont, "lnorm", method = "mle")
summary(fitlogn)
confint(fitlogn)
gofstat(fitlogn)
mu<-fitlogn$estimate[1]
sigmaln<-fitlogn$estimate[2]
mu
sigmaln
ks.test(Capital_cont,"plnorm", meanlog=mu, sdlog<-sigmaln)
ad.test(plnorm(Capital_cont, meanlog=mu, sdlog<-sigmaln ))


######## Ajuste Pareto
fitpareto<-fitdist(Capital_cont, "pareto")
summary(fitpareto)
confint(fitpareto)
gofstat(fitpareto)
alphap<-fitpareto$estimate[1]
thetap<-fitpareto$estimate[2]
alphap
thetap
ks.test(Capital_cont,"ppareto", shape=alphap, scale=thetap)
ad.test(ppareto(Capital_cont,shape=alphap , scale=thetap))

#Ajuste Weibull
fitweibull<-fitdist(Capital_cont, "weibull", method = "mle")
summary(fitweibull)
gofstat(fitweibull)
confint(fitweibull)
taow<-fitweibull$estimate[1]
thetaw<-fitweibull$estimate[2]
taow
thetaw
ks.test(Capital_cont,"pweibull", shape=taow, scale=thetaw)
ad.test(pweibull(Capital_cont,shape=taow , scale=thetaw))

#Ajuste LogGamma
fitlgamma<-fitdist(Capital_cont, "lgamma", method = "mle")
summary(fitlgamma)
confint(fitlgamma)
gofstat(fitlgamma)
alphalg<-fitlgamma$estimate[1]
betalg<-fitlgamma$estimate[2]
alphalg
betalg
ks.test(Capital_cont,"plgamma", shapelog=fitlgamma$estimate[1], ratelog=fitlgamma$estimate[2])
ad.test(plgamma(Capital_cont,shapelog=fitlgamma$estimate[1], ratelog=fitlgamma$estimate[2]))

#Ajuste LogLogistica
fitllog<-fitdist(Capital_cont, "llogis", method = "mle")
summary(fitllog)
confint(fitllog)
gofstat(fitllog)
alphall<-fitllog$estimate[1]
betall<-fitllog$estimate[2]
alphall
betall
ks.test(Capital_cont,"pllogis", shape=fitllog$estimate[1], scale=fitllog$estimate[2])
ad.test(pllogis(Capital_cont,shape=fitllog$estimate[1], scale=fitllog$estimate[2]))

#Graficar ajustes
cdfcomp(list(fitpareto, fitlogn,fitweibull,fitlgamma,fitllog))#fitinvw,fitburr2,fitgamma2

############# Creamos lista de los ajustes
modelos <- list(fitlogn, fitpareto, fitweibull, fitlgamma, fitllog)

# Extraer loglik, aic y bic de cada modelo
loglikfit <- sapply(modelos, function(x) x$loglik)
aicfit <- sapply(modelos, function(x) x$aic)
bicfit <- sapply(modelos, function(x) x$bic)

#Combinamos en una matriz
salida <- rbind(loglikfit, aicfit, bicfit)
# Asignar nombres a las columnas y filas
colnames(salida) <- c("logn", "pareto", "weibull", "lgamma", "llog")
rownames(salida) <- c("Loglik", "AIC", "BIC")
sal = as.data.frame(salida)
View(sal)
# En orden de aparicion busco el más cercano a 0 y comparo entre ellos para ver cual se ajusta mejor

########################### ---------- Ajuste No Paramétrico
kunif<-density(Deuda_total, bw="ucv", kernel="rectangular")
ktriang<-density(Deuda_total, bw="ucv", kernel="triangular")
# ktrian<-kdensity(reclas, start="triangular", kernel="triangular")
kgamma<-kdensity(Deuda_total, start="gamma", kernel="gamma")
coef(kgamma) #iguales que los de la EMV =D

# Determinar el l?mite superior del eje y
y_kunif <- kunif$y
y_ktriang <- ktriang$y
y_max <- max(c(y_kunif, y_ktriang))

# Crear el histograma, se corre junto con la leyenda.
hist(Deuda_total, main="Valor reclamaciones", col="grey", probability=TRUE, xlim=c(min(Deuda_total), max(Deuda_total)), ylim=c(0, y_max))
# Agregar las l?neas de densidad
lines(kunif, col="black")
lines(ktriang, col="green",lwd=2)
#lines(kgamma, col="red",lwd=2)
lines(density(Deuda_total), col="blue",lwd=2)
#leyenda
legend("topright", legend=c("Uniforme", "Triangular", "Densidad Emp?rica"), col=c("black", "green", "blue"), lty=1)
