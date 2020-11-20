## Title: "LB-SPR"
## rEVISADO por Marta Cousido 16-10-2020
##Loading the Package
install.packages("LBSPR")
## You can install the development version of the package from GitHub using the `devtools` package:
install.packages("devtools")
devtools::install_github("AdrianHordyk/LBSPR")
## LB-SPR Analisis de Sensibilidad
library(LBSPR) 
setwd("C:/cristina/disco viejo/ELASMO/2019/IMPRESS/LBSPR")
##Los datos del ciclo vital recogidos de trabajos previos en la zona de estudio para la raya santiaguesa son:
## {M/k}^{LIT}$=1.5 
## L_{inf}^{LIT}$=75
## M = 0.3
## L50=56.2 
## L95=62 
## Con la informacion anterior creamos un objecto "LB_pars".
PinPars <- new("LB_pars")
PinPars@Linf <- 75
PinPars@L50 <- 56.2 
PinPars@L95 <- 62
PinPars@MK <- 1.5 
PinPars@M <- 0.3 
PinPars@L_units <- "cm"
## Para poder aplicar el metodo necesitamos leer el archivo csv con las frecuencias de tallas y crear con dicha informacion un objecto "LB_lengths".
setwd("C:/Cristina/disco viejo/ELASMO/2019/IMPRESS/LBSPR")
## Pin_freq <- read.csv ("rjn9afreq.csv", sep=",", header=TRUE)
## str(Pin_freq)
PinLenFreq1 <- new("LB_lengths", LB_pars=PinPars, file="rjn9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq1@L_units <- PinPars@L_units
## A partir de la informacion creada ya podemos aplicar nuestro modelo. El *ESCENARIO DE REFERENCIA* es el que utiliza los parametros descritos con anterioridad. Los resultados derivados de modificaciones en los valores de $L_{inf}$ y ${M/k}$ se compararan siempre con este escenario.
Fit1 <- LBSPRfit(PinPars, PinLenFreq1)
## En las siguientes secciones vemos los resultamos considerando modificaciones de dichos parametros.
## ESCENARIOS MODIFICADOS USANDO LOS DIFERENTES VALORES PARA EL PARáMETRO @MK
## Primero infraestimando (${M/k}^{LIT}$ multiplicado por 0.75):
Pin1ParsMK <-PinPars
Pin1ParsMK@MK <- 0.75*1.5
PinLenFreq1MK <- new("LB_lengths", LB_pars=Pin1ParsMK, file="rjn9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq1MK@L_units <- Pin1ParsMK@L_units
Fit1MK <- LBSPRfit(Pin1ParsMK, PinLenFreq1MK)
## 2.Segundo sobrestimando (${M/k}^{LIT}$ multiplicado por 1.25):
Pin2ParsMK <-PinPars
Pin2ParsMK@MK <- 1.25*1.5
PinLenFreq2MK <- new("LB_lengths", LB_pars=Pin2ParsMK, file="rjn9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq2MK@L_units <- Pin2ParsMK@L_units
Fit2MK <- LBSPRfit(Pin2ParsMK, PinLenFreq2MK)

## ESCENARIOS MODIFICADOS USANDO LOS DIFERENTES VALORES PARA Linf
## 1. Primero infraestimando ($L_{inf}^{LIT}$ multiplicado por 0.75):
Pin1ParsLinf <-PinPars
Pin1ParsLinf@Linf <- 0.75*75 
PinLenFreq1Linf <- new("LB_lengths", LB_pars=Pin1ParsLinf, file="rjn9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq1Linf@L_units <- Pin1ParsLinf@L_units
Fit1Linf <- LBSPRfit(Pin1ParsLinf, PinLenFreq1Linf)
## 2. Segundo sobrestimando ($L_{inf}^{LIT}$ multiplicado por 1.25):
Pin2ParsLinf <-PinPars
Pin2ParsLinf@Linf <- 1.25*75
PinLenFreq2Linf <- new("LB_lengths", LB_pars=Pin2ParsLinf, file="rjn9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq2Linf@L_units <- Pin2ParsLinf@L_units

## Si 1.25* Linf es superior a la talla maxima observada dara error.En este caso La TL obs es 70 cm y la Linf es 93.75 
PinLenFreq1@LMids

## Se puede solucionar si añadimos a la distribucion de tallas de las capturas el valor 94 con frecuencia 0.
PinLenFreq2Linf@LMids=seq(32,94,by=1)
A=PinLenFreq2Linf@LData
for(i in 1:19){A=rbind(A,(PinLenFreq2Linf@LData[44,]))}
A[45:63,1:11]=rep(0,11)
tail(A)
PinLenFreq2Linf@LData=A
Pin2ParsLinf <-PinPars
Pin2ParsLinf@Linf <-1.25*75
Fit2Linf <- LBSPRfit(Pin2ParsLinf, PinLenFreq2Linf)

## RESUMEN DE LA INFORMACION. Creamos dos graficos en los cuales veremos las estimaciones de F/M y de SPR para los diferentes escenarios.
## Grafico SPR:
smoothEsts1 <- data.frame(Fit1@Ests)
smoothEsts1MK <- data.frame(Fit1MK@Ests)
smoothEsts2MK <- data.frame(Fit2MK@Ests)
smoothEsts1Linf <- data.frame(Fit1Linf@Ests)
smoothEsts2Linf <- data.frame(Fit2Linf@Ests)

spr1=Fit1@SPR
sprmk1=Fit1MK@SPR
sprmk2=Fit2MK@SPR
sprL1=Fit1Linf@SPR
sprL2=Fit2Linf@SPR

par(mfcol=c(1,1))

plot(2009:2019,spr1,ylim=c(0,1.5),xlab="Years",ylab="SPR estimates",pch=19)

lines(2009:2019, smoothEsts1$SPR, lwd = 1,col="black")
lines(2009:2019, smoothEsts1MK$SPR, lwd = 1,col=2)
lines(2009:2019, smoothEsts2MK$SPR, lwd = 1,col=3)
lines(2009:2019, smoothEsts1Linf$SPR, lwd = 1,col=5)
lines(2009:2019, smoothEsts2Linf$SPR, lwd = 1,col=6)

points(2009:2019, sprmk1,col=2,pch=19)
points(2009:2019, sprmk2,col=3,pch=19)
points(2009:2019, sprL1,col=5,pch=19)
points(2009:2019, sprL2,col=6,pch=19)

legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","0.75*Linf","1.25*Linf"),col=c("black", c(2,3,5,6)), pch=19, cex=0.8)

## Grafico F/M:
fm1=Fit1@FM
fmmk1=Fit1MK@FM
fmmk2=Fit2MK@FM
fmL1=Fit1Linf@FM
fmL2=Fit2Linf@FM

min(c(fm1,fmmk1,fmmk2,fmL1,fmL2))

plot(20009:2019,fm1,ylim=c(0,20),xlab="Years",ylab="F/M estimates",pch=19)

lines(2009:2019, smoothEsts1$FM, lwd = 1,col="black")
lines(2009:2019, smoothEsts1MK$FM, lwd = 1,col=2)
lines(2009:2019, smoothEsts2MK$FM, lwd = 1,col=3)
lines(2009:2019, smoothEsts1Linf$FM, lwd = 1,col=5)
lines(2009:2019, smoothEsts2Linf$FM, lwd = 1,col=6)
points(2009:2019, fmmk1,col=2,pch=19)
points(2009:2019, fmmk2,col=3,pch=19)
points(2009:2019, fmL1,col=5,pch=19)
points(2009:2019, fmL2,col=6,pch=19)

legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","0.75*Linf","1.25*Linf"),col=c("black", 2,3,5,6), pch=19, cex=0.8)
plotEsts(Fit1Linf)
plotEsts(Fit2Linf)
