library(kableExtra)
library(tidyverse)
require (rJava)
library(devtools)
## devtools::install_github('davidgohel/ReporteRsjars')
## devtools::install_github('davidgohel/ReporteRs')
library(LBSPR) 
library(reshape2)
library(ggplot2) 
library(tidyr)
library(ReporteRs) 
setwd("C:/Cristina/disco viejo/ELASMO/2019/IMPRESS/LBSPR")
source("https://raw.githubusercontent.com/ices-tools-dev/LBIndicator_shiny/master/utilities.R")
source("utilities_vpaz.R")
## Seleccionamos el directorio de trabajo
## Cargamos los ficheros (Tallas y pesos)
wal <- read.csv("rjn9awal.csv",sep=",")
head(wal)
freq <- read.csv("rjn9afreq.csv", stringsAsFactors = FALSE)
head(freq)
## El escenario de REFERENCIA EN el caso de la Raja naeuvus: Linf=75,Lmat=56.5 y M/k=1.5
lb_tableSH(freq, 2, "cm", linf=75.0, lmat=56.5, mk_ratio=1.5,wal)
## Probamos los escenarios 1) M/k infraestimando (M/k multiplicado por 0.75):
lb_tableSH(freq, 2, "cm", linf=75.0, lmat=56.5, mk_ratio=0.75*1.5,wal)
## Probamos los escenarios 2) M/k sobreestimando (M/k multiplicado por 1.25):
lb_tableSH(freq, 2, "cm", linf=75.0, lmat=56.5, mk_ratio=1.25*1.5,wal)
## Ahora modificando Linfinito
## Primero infraestimando (Linf Multiplicado por 0.75):
lb_tableSH(freq, 2, "cm", linf=0.75*75.0, lmat=56.5, mk_ratio=1.5,wal)
## sEGUNDO sobrestimando (Linf Multiplicado por 1.25):
lb_tableSH(freq, 2, "cm", linf=1.25*75.0, lmat=56.5, mk_ratio=1.5,wal)
## Tabla resumen de los resultados