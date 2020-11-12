

#####################################----> Norway Loster 8c FU25 Males

library(LBSPR) 
Hke1Pars <- new("LB_pars")
Hke1Pars@Linf <- 86
Hke1Pars@L50 <- 25
Hke1Pars@L95 <- 28.75
Hke1Pars@MK <- 1.9 
Hke1Pars@M <- 0.095 
Hke1Pars@L_units <- "mm"
HkeLenFreq1 <- new("LB_lengths", LB_pars=Hke1Pars, file="FU25_Males.csv", dataType="freq", header=TRUE)
HkeLenFreq1@L_units <- Hke1Pars@L_units

A=HkeLenFreq1@LData
B=is.na(HkeLenFreq1@LData)

for (i in 1:68){
  for (j in 1:38){
    if(B[i,j]==TRUE){
      A[i,j]=0}
  }
  
}

HkeLenFreq1@LData=A
Fit1 <- LBSPRfit(Hke1Pars, HkeLenFreq1, verbose = FALSE)
Hke1ParsMK <-Hke1Pars
Hke1ParsMK@MK <- 0.75*1.90


HkeLenFreq1MK <- new("LB_lengths", LB_pars=Hke1ParsMK, file="FU25_Males.csv", dataType="freq", header=TRUE)
HkeLenFreq1MK@L_units <- Hke1ParsMK@L_units


A=HkeLenFreq1MK@LData
B=is.na(HkeLenFreq1MK@LData)

for (i in 1:68){
  for (j in 1:38){
    if(B[i,j]==TRUE){
      A[i,j]=0}
  }
  
}

HkeLenFreq1MK@LData=A

Fit1MK <- LBSPRfit(Hke1ParsMK, HkeLenFreq1MK, verbose = FALSE)

Hke2ParsMK <-Hke1Pars
Hke2ParsMK@MK <- 1.25*1.90


HkeLenFreq2MK <- new("LB_lengths", LB_pars=Hke2ParsMK, file="FU25_Males.csv", dataType="freq", header=TRUE)
HkeLenFreq2MK@L_units <- Hke2ParsMK@L_units

A=HkeLenFreq2MK@LData
B=is.na(HkeLenFreq2MK@LData)

for (i in 1:68){
  for (j in 1:38){
    if(B[i,j]==TRUE){
      A[i,j]=0}
  }
  
}

HkeLenFreq2MK@LData=A


Fit2MK <- LBSPRfit(Hke2ParsMK, HkeLenFreq2MK, verbose = FALSE)

Hke3ParsMK <-Hke1Pars
Hke3ParsMK@MK <- 1.5

HkeLenFreq3MK <- new("LB_lengths", LB_pars=Hke3ParsMK, file="FU25_Males.csv", dataType="freq", header=TRUE)
HkeLenFreq3MK@L_units <- Hke3ParsMK@L_units


A=HkeLenFreq3MK@LData
B=is.na(HkeLenFreq3MK@LData)

for (i in 1:68){
  for (j in 1:38){
    if(B[i,j]==TRUE){
      A[i,j]=0}
  }
  
}

HkeLenFreq3MK@LData=A


Fit3MK <- LBSPRfit(Hke3ParsMK, HkeLenFreq3MK, verbose = FALSE)



Hke1ParsLinf <-Hke1Pars
Hke1ParsLinf@Linf <- 0.75*86

HkeLenFreq1Linf <- new("LB_lengths", LB_pars=Hke1ParsLinf, file="FU25_Males.csv", dataType="freq", header=TRUE)
HkeLenFreq1Linf@L_units <- Hke1ParsLinf@L_units

A=HkeLenFreq1Linf@LData
B=is.na(HkeLenFreq1Linf@LData)

for (i in 1:68){
  for (j in 1:38){
    if(B[i,j]==TRUE){
      A[i,j]=0}
  }
  
}

HkeLenFreq1Linf@LData=A


Fit1Linf <- LBSPRfit(Hke1ParsLinf, HkeLenFreq1Linf, verbose = FALSE)

Hke2ParsLinf <-Hke1Pars
Hke2ParsLinf@Linf <- 1.25*86

HkeLenFreq2Linf <- new("LB_lengths", LB_pars=Hke2ParsLinf, file="FU25_Males.csv", dataType="freq", header=TRUE)
HkeLenFreq2Linf@L_units <- Hke2ParsLinf@L_units

A=HkeLenFreq2Linf@LData
B=is.na(HkeLenFreq2Linf@LData)

for (i in 1:68){
  for (j in 1:38){
    if(B[i,j]==TRUE){
      A[i,j]=0}
  }
  
}

HkeLenFreq2Linf@LData=A



#HkeLenFreq1@LMids

HkeLenFreq2Linf@LMids=19:108
A=HkeLenFreq2Linf@LData
for (i in 1:22){
  A=rbind(A,HkeLenFreq2Linf@LData[68,])}

A[69:90,1:38]=rep(0,38)
#cbind(A,HkeLenFreq2Linf@LMids)

HkeLenFreq2Linf@LData=A
Hke2ParsLinf <-Hke1Pars
Hke2ParsLinf@Linf <- 1.25*86
Fit2Linf <- LBSPRfit(Hke2ParsLinf, HkeLenFreq2Linf, verbose = FALSE)


smoothEsts1 <- data.frame(Fit1@Ests)
smoothEsts1MK <- data.frame(Fit1MK@Ests)
smoothEsts2MK <- data.frame(Fit2MK@Ests)
smoothEsts3MK <- data.frame(Fit3MK@Ests)
smoothEsts1Linf <- data.frame(Fit1Linf@Ests)
smoothEsts2Linf <- data.frame(Fit2Linf@Ests)


spr1=Fit1@SPR
sprmk1=Fit1MK@SPR
sprmk2=Fit2MK@SPR
sprmk3=Fit3MK@SPR
sprL1=Fit1Linf@SPR
sprL2=Fit2Linf@SPR

min=min(c(spr1,sprmk1,sprmk2,sprmk3,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprmk3,sprL1,sprL2))

plot(1982:2019,spr1,ylim=c(min,max),xlab="Years",ylab="SPR estimates",pch=19,main="Norway Loster 8c FU25 Males")

lines(1982:2019, smoothEsts1$SPR, lwd = 1,col="black")
lines(1982:2019, smoothEsts1MK$SPR, lwd = 1,col=2)
lines(1982:2019, smoothEsts2MK$SPR, lwd = 1,col=3)
lines(1982:2019, smoothEsts3MK$SPR, lwd = 1,col=4)
lines(1982:2019, smoothEsts1Linf$SPR, lwd = 1,col=5)
lines(1982:2019, smoothEsts2Linf$SPR, lwd = 1,col=6)


points(1982:2019, sprmk1,col=2,pch=19)
points(1982:2019, sprmk2,col=3,pch=19)
points(1982:2019, sprmk3,col=4,pch=19)
points(1982:2019, sprL1,col=5,pch=19)
points(1982:2019, sprL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)


fm1=Fit1@FM
fmmk1=Fit1MK@FM
fmmk2=Fit2MK@FM
fmmk3=Fit3MK@FM
fmL1=Fit1Linf@FM
fmL2=Fit2Linf@FM

min=min(c(fm1,fmmk1,fmmk2,fmmk3,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmmk3,fmL1,fmL2))

plot(1982:2019,fm1,ylim=c(min,max),xlab="Years",ylab="F/M estimates",pch=19,main="Norway Loster 8c FU25 Males")

lines(1982:2019, smoothEsts1$FM, lwd = 1,col="black")
lines(1982:2019, smoothEsts1MK$FM, lwd = 1,col=2)
lines(1982:2019, smoothEsts2MK$FM, lwd = 1,col=3)
lines(1982:2019, smoothEsts3MK$FM, lwd = 1,col=4)
lines(1982:2019, smoothEsts1Linf$FM, lwd = 1,col=5)
lines(1982:2019, smoothEsts2Linf$FM, lwd = 1,col=6)


points(1982:2019, fmmk1,col=2,pch=19)
points(1982:2019, fmmk2,col=3,pch=19)
points(1982:2019, fmmk3,col=4,pch=19)
points(1982:2019, fmL1,col=5,pch=19)
points(1982:2019, fmL2,col=6,pch=19)

#legend("topleft", legend=c("REFERENCIA", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)

#####################################----< Norway Loster 8c FU25 Males


#####################################----> Norway Loster 8c FU25 Females

Hke1Pars <- new("LB_pars")
Hke1Pars@Linf <- 71
Hke1Pars@L50 <- 28
Hke1Pars@L95 <- 32.2
Hke1Pars@MK <- 1.9 
Hke1Pars@M <- 0.0475 
Hke1Pars@L_units <- "mm"

HkeLenFreq1 <- new("LB_lengths", LB_pars=Hke1Pars, file="FU25_Females.csv", dataType="freq", header=TRUE)
HkeLenFreq1@L_units <- Hke1Pars@L_units

Fit1 <- LBSPRfit(Hke1Pars, HkeLenFreq1, verbose = FALSE)

Hke1ParsMK <-Hke1Pars
Hke1ParsMK@MK <- 0.75*1.90


HkeLenFreq1MK <- new("LB_lengths", LB_pars=Hke1ParsMK, file="FU25_Females.csv", dataType="freq", header=TRUE)
HkeLenFreq1MK@L_units <- Hke1ParsMK@L_units


Fit1MK <- LBSPRfit(Hke1ParsMK, HkeLenFreq1MK, verbose = FALSE)

Hke2ParsMK <-Hke1Pars
Hke2ParsMK@MK <- 1.25*1.90


HkeLenFreq2MK <- new("LB_lengths", LB_pars=Hke2ParsMK, file="FU25_Females.csv", dataType="freq", header=TRUE)
HkeLenFreq2MK@L_units <- Hke2ParsMK@L_units


Fit2MK <- LBSPRfit(Hke2ParsMK, HkeLenFreq2MK, verbose = FALSE)


Hke3ParsMK <-Hke1Pars
Hke3ParsMK@MK <- 1.5

HkeLenFreq3MK <- new("LB_lengths", LB_pars=Hke3ParsMK, file="FU25_Females.csv", dataType="freq", header=TRUE)
HkeLenFreq3MK@L_units <- Hke3ParsMK@L_units

Fit3MK <- LBSPRfit(Hke3ParsMK, HkeLenFreq3MK, verbose = FALSE)

Hke1ParsLinf <-Hke1Pars
Hke1ParsLinf@Linf <- 0.75*71

HkeLenFreq1Linf <- new("LB_lengths", LB_pars=Hke1ParsLinf, file="FU25_Females.csv", dataType="freq", header=TRUE)
HkeLenFreq1Linf@L_units <- Hke1ParsLinf@L_units


Fit1Linf <- LBSPRfit(Hke1ParsLinf, HkeLenFreq1Linf, verbose = FALSE)

Hke2ParsLinf <-Hke1Pars
Hke2ParsLinf@Linf <- 1.25*71

HkeLenFreq2Linf <- new("LB_lengths", LB_pars=Hke2ParsLinf, file="FU25_Females.csv", dataType="freq", header=TRUE)
HkeLenFreq2Linf@L_units <- Hke2ParsLinf@L_units


#HkeLenFreq1@LMids

HkeLenFreq2Linf@LMids=15:89
A=HkeLenFreq2Linf@LData
for (i in 1:18){
  A=rbind(A,HkeLenFreq2Linf@LData[57,])}

A[58:75,1:38]=rep(0,38)
#tail(cbind(A,HkeLenFreq2Linf@LMids))

HkeLenFreq2Linf@LData=A
Hke2ParsLinf <-Hke1Pars
Hke2ParsLinf@Linf <- 1.25*71
Fit2Linf <- LBSPRfit(Hke2ParsLinf, HkeLenFreq2Linf, verbose = FALSE)

smoothEsts1 <- data.frame(Fit1@Ests)
smoothEsts1MK <- data.frame(Fit1MK@Ests)
smoothEsts2MK <- data.frame(Fit2MK@Ests)
smoothEsts3MK <- data.frame(Fit3MK@Ests)
smoothEsts1Linf <- data.frame(Fit1Linf@Ests)
smoothEsts2Linf <- data.frame(Fit2Linf@Ests)


spr1=Fit1@SPR
sprmk1=Fit1MK@SPR
sprmk2=Fit2MK@SPR
sprmk3=Fit3MK@SPR
sprL1=Fit1Linf@SPR
sprL2=Fit2Linf@SPR

min=min(c(spr1,sprmk1,sprmk2,sprmk3,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprmk3,sprL1,sprL2))

plot(1982:2019,spr1,ylim=c(min,max),xlab="Years",ylab="SPR estimates",pch=19,main="Norway Loster 8c FU25 Females")

lines(1982:2019, smoothEsts1$SPR, lwd = 1,col="black")
lines(1982:2019, smoothEsts1MK$SPR, lwd = 1,col=2)
lines(1982:2019, smoothEsts2MK$SPR, lwd = 1,col=3)
lines(1982:2019, smoothEsts3MK$SPR, lwd = 1,col=4)
lines(1982:2019, smoothEsts1Linf$SPR, lwd = 1,col=5)
lines(1982:2019, smoothEsts2Linf$SPR, lwd = 1,col=6)


points(1982:2019, sprmk1,col=2,pch=19)
points(1982:2019, sprmk2,col=3,pch=19)
points(1982:2019, sprmk3,col=4,pch=19)
points(1982:2019, sprL1,col=5,pch=19)
points(1982:2019, sprL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)

fm1=Fit1@FM
fmmk1=Fit1MK@FM
fmmk2=Fit2MK@FM
fmmk3=Fit3MK@FM
fmL1=Fit1Linf@FM
fmL2=Fit2Linf@FM

min=min(c(fm1,fmmk1,fmmk2,fmmk3,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmmk3,fmL1,fmL2))

plot(1982:2019,fm1,ylim=c(min,max),xlab="Years",ylab="F/M estimates",pch=19,main="Norway Loster 8c FU25 Females")

lines(1982:2019, smoothEsts1$FM, lwd = 1,col="black")
lines(1982:2019, smoothEsts1MK$FM, lwd = 1,col=2)
lines(1982:2019, smoothEsts2MK$FM, lwd = 1,col=3)
lines(1982:2019, smoothEsts3MK$FM, lwd = 1,col=4)
lines(1982:2019, smoothEsts1Linf$FM, lwd = 1,col=5)
lines(1982:2019, smoothEsts2Linf$FM, lwd = 1,col=6)


points(1982:2019, fmmk1,col=2,pch=19)
points(1982:2019, fmmk2,col=3,pch=19)
points(1982:2019, fmmk3,col=4,pch=19)
points(1982:2019, fmL1,col=5,pch=19)
points(1982:2019, fmL2,col=6,pch=19)

#legend("topleft", legend=c("REFERENCIA", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)

#####################################----< Norway Loster 8c FU25 Females


#####################################-----> Norway Loster 8c FU2627 Males

Nepmales1Pars <- new("LB_pars")
Nepmales1Pars@Linf <- 80
Nepmales1Pars@L50 <- 28 
Nepmales1Pars@L95 <- 32.2
Nepmales1Pars@MK <- 2 
Nepmales1Pars@M <- 0.3 
Nepmales1Pars@L_units <- "mm"

NepmalesLenFreq1 <- new("LB_lengths", LB_pars=Nepmales1Pars, file="FU2627_tallasMales_1988_2019_2.csv", dataType="freq", header=TRUE)
NepmalesLenFreq1@L_units <- Nepmales1Pars@L_units

Fit1 <- LBSPRfit(Nepmales1Pars, NepmalesLenFreq1, verbose = FALSE)

Nepmales1ParsMK <-Nepmales1Pars
Nepmales1ParsMK@MK <- 0.75*2


NepmalesLenFreq1MK <- new("LB_lengths", LB_pars=Nepmales1ParsMK, file="FU2627_tallasMales_1988_2019_2.csv", dataType="freq", header=TRUE)
NepmalesLenFreq1MK@L_units <- Nepmales1ParsMK@L_units


Fit1MK <- LBSPRfit(Nepmales1ParsMK, NepmalesLenFreq1MK,verbose=FALSE)

Nepmales2ParsMK <-Nepmales1Pars
Nepmales2ParsMK@MK <- 1.25*2


NepmalesLenFreq2MK <- new("LB_lengths", LB_pars=Nepmales2ParsMK, file="FU2627_tallasMales_1988_2019_2.csv", dataType="freq", header=TRUE)
NepmalesLenFreq2MK@L_units <- Nepmales2ParsMK@L_units


Fit2MK <- LBSPRfit(Nepmales2ParsMK, NepmalesLenFreq2MK,verbose = FALSE)

Nepmales1ParsLinf <-Nepmales1Pars
Nepmales1ParsLinf@Linf <- 0.75*80 

NepmalesLenFreq1Linf <- new("LB_lengths", LB_pars=Nepmales1ParsLinf, file="FU2627_tallasMales_1988_2019_2.csv", dataType="freq", header=TRUE)
NepmalesLenFreq1Linf@L_units <- Nepmales1ParsLinf@L_units


Fit1Linf <- LBSPRfit(Nepmales1ParsLinf, NepmalesLenFreq1Linf,verbose = FALSE)

Nepmales2ParsLinf <-Nepmales1Pars
Nepmales2ParsLinf@Linf <- 1.25*80

NepmalesLenFreq2Linf <- new("LB_lengths", LB_pars=Nepmales2ParsLinf, file="FU2627_tallasMales_1988_2019_2.csv", dataType="freq", header=TRUE)
NepmalesLenFreq2Linf@L_units <- Nepmales2ParsLinf@L_units


Fit2Linf <- LBSPRfit(Nepmales2ParsLinf, NepmalesLenFreq2Linf, verbose = FALSE)


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


min=min(c(spr1,sprmk1,sprmk2,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprL1,sprL2))

plot(1988:2019,spr1,ylim=c(min,max),xlab="Years",ylab="SPR estimates",pch=19,main="Norway Loster 8c FU2627 Males")

lines(1988:2019, smoothEsts1$SPR, lwd = 1,col="black")
lines(1988:2019, smoothEsts1MK$SPR, lwd = 1,col=2)
lines(1988:2019, smoothEsts2MK$SPR, lwd = 1,col=3)
lines(1988:2019, smoothEsts1Linf$SPR, lwd = 1,col=5)
lines(1988:2019, smoothEsts2Linf$SPR, lwd = 1,col=6)


points(1988:2019, sprmk1,col=2,pch=19)
points(1988:2019, sprmk2,col=3,pch=19)
points(1988:2019, sprL1,col=5,pch=19)
points(1988:2019, sprL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","0.75*Linf","1.25*Linf"),
#       col=c("black", c(2,3,5,6)), pch=19, cex=0.8)

fm1=Fit1@FM
fmmk1=Fit1MK@FM
fmmk2=Fit2MK@FM
fmL1=Fit1Linf@FM
fmL2=Fit2Linf@FM

min=min(c(fm1,fmmk1,fmmk2,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmL1,fmL2))

plot(1988:2019,fm1,ylim=c(min,max),xlab="Years",ylab="F/M estimates",pch=19,main="Norway Loster 8c FU2627 Males")

lines(1988:2019, smoothEsts1$FM, lwd = 1,col="black")
lines(1988:2019, smoothEsts1MK$FM, lwd = 1,col=2)
lines(1988:2019, smoothEsts2MK$FM, lwd = 1,col=3)
lines(1988:2019, smoothEsts1Linf$FM, lwd = 1,col=5)
lines(1988:2019, smoothEsts2Linf$FM, lwd = 1,col=6)


points(1988:2019, fmmk1,col=2,pch=19)
points(1988:2019, fmmk2,col=3,pch=19)
points(1988:2019, fmL1,col=5,pch=19)
points(1988:2019, fmL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","0.75*Linf","1.25*Linf"),
#       col=c("black", c(2,3,5,6)), pch=19, cex=0.8)

#####################################-----< Norway Loster 8c FU2627 Males

#####################################------> Norway Loster 8c FU2627 Females


Nepfemales1Pars <- new("LB_pars")
Nepfemales1Pars@Linf <- 65
Nepfemales1Pars@L50 <- 26 
Nepfemales1Pars@L95 <- 29.9
Nepfemales1Pars@MK <- 2.5 
Nepfemales1Pars@M <- 0.2 
Nepfemales1Pars@L_units <- "mm"

NepfemalesLenFreq1 <- new("LB_lengths", LB_pars=Nepfemales1Pars, file="FU2627_tallasFemales_1988_2019.csv", dataType="freq", header=TRUE)
NepfemalesLenFreq1@L_units <- Nepfemales1Pars@L_units

Fit1 <- LBSPRfit(Nepfemales1Pars, NepfemalesLenFreq1,verbose = FALSE)

Nepfemales1ParsMK <-Nepfemales1Pars
Nepfemales1ParsMK@MK <- 0.75*2.5


NepfemalesLenFreq1MK <- new("LB_lengths", LB_pars=Nepfemales1ParsMK, file="FU2627_tallasFemales_1988_2019.csv", dataType="freq", header=TRUE)
NepfemalesLenFreq1MK@L_units <- Nepfemales1ParsMK@L_units


Fit1MK <- LBSPRfit(Nepfemales1ParsMK, NepfemalesLenFreq1MK, verbose = FALSE)

Nepfemales2ParsMK <-Nepfemales1Pars
Nepfemales2ParsMK@MK <- 1.25*2.5


NepfemalesLenFreq2MK <- new("LB_lengths", LB_pars=Nepfemales2ParsMK, file="FU2627_tallasFemales_1988_2019.csv", dataType="freq", header=TRUE)
NepfemalesLenFreq2MK@L_units <- Nepfemales2ParsMK@L_units


Fit2MK <- LBSPRfit(Nepfemales2ParsMK, NepfemalesLenFreq2MK,verbose = FALSE)

Nepfemales3ParsMK <-Nepfemales1Pars
Nepfemales3ParsMK@MK <- 1.5

NepfemalesLenFreq3MK <- new("LB_lengths", LB_pars=Nepfemales3ParsMK, file="FU2627_tallasFemales_1988_2019.csv", dataType="freq", header=TRUE)
NepfemalesLenFreq3MK@L_units <- Nepfemales3ParsMK@L_units


Fit3MK <- LBSPRfit(Nepfemales3ParsMK, NepfemalesLenFreq3MK,verbose = FALSE)

Nepfemales1ParsLinf <-Nepfemales1Pars
Nepfemales1ParsLinf@Linf <- 0.75*65 

NepfemalesLenFreq1Linf <- new("LB_lengths", LB_pars=Nepfemales1ParsLinf, file="FU2627_tallasFemales_1988_2019.csv", dataType="freq", header=TRUE)
NepfemalesLenFreq1Linf@L_units <- Nepfemales1ParsLinf@L_units


Fit1Linf <- LBSPRfit(Nepfemales1ParsLinf, NepfemalesLenFreq1Linf,verbose = FALSE)

Nepfemales2ParsLinf <-Nepfemales1Pars
Nepfemales2ParsLinf@Linf <- 1.25*65

NepfemalesLenFreq2Linf <- new("LB_lengths", LB_pars=Nepfemales2ParsLinf, file="FU2627_tallasFemales_1988_2019.csv", dataType="freq", header=TRUE)
NepfemalesLenFreq2Linf@L_units <- Nepfemales2ParsLinf@L_units


Fit2Linf <- LBSPRfit(Nepfemales2ParsLinf, NepfemalesLenFreq2Linf,verbose = FALSE)


smoothEsts1 <- data.frame(Fit1@Ests)
smoothEsts1MK <- data.frame(Fit1MK@Ests)
smoothEsts2MK <- data.frame(Fit2MK@Ests)
smoothEsts3MK <- data.frame(Fit3MK@Ests)
smoothEsts1Linf <- data.frame(Fit1Linf@Ests)
smoothEsts2Linf <- data.frame(Fit2Linf@Ests)


spr1=Fit1@SPR
sprmk1=Fit1MK@SPR
sprmk2=Fit2MK@SPR
sprmk3=Fit3MK@SPR
sprL1=Fit1Linf@SPR
sprL2=Fit2Linf@SPR

min=min(c(spr1,sprmk1,sprmk2,sprmk3,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprmk3,sprL1,sprL2))

plot(1988:2019,spr1,ylim=c(min,max),xlab="Years",ylab="SPR estimates",pch=19,main="Norway Loster 8c FU2627 Females")

lines(1988:2019, smoothEsts1$SPR, lwd = 1,col="black")
lines(1988:2019, smoothEsts1MK$SPR, lwd = 1,col=2)
lines(1988:2019, smoothEsts2MK$SPR, lwd = 1,col=3)
lines(1988:2019, smoothEsts3MK$SPR, lwd = 1,col=4)
lines(1988:2019, smoothEsts1Linf$SPR, lwd = 1,col=5)
lines(1988:2019, smoothEsts2Linf$SPR, lwd = 1,col=6)


points(1988:2019, sprmk1,col=2,pch=19)
points(1988:2019, sprmk2,col=3,pch=19)
points(1988:2019, sprmk3,col=4,pch=19)
points(1988:2019, sprL1,col=5,pch=19)
points(1988:2019, sprL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)


fm1=Fit1@FM
fmmk1=Fit1MK@FM
fmmk2=Fit2MK@FM
fmmk3=Fit3MK@FM
fmL1=Fit1Linf@FM
fmL2=Fit2Linf@FM

min=min(c(fm1,fmmk1,fmmk2,fmmk3,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmmk3,fmL1,fmL2))
plot(1988:2019,fm1,ylim=c(min,max),xlab="Years",ylab="F/M estimates",pch=19,main="Norway Loster 8c FU2627 Females")

lines(1988:2019, smoothEsts1$FM, lwd = 1,col="black")
lines(1988:2019, smoothEsts1MK$FM, lwd = 1,col=2)
lines(1988:2019, smoothEsts2MK$FM, lwd = 1,col=3)
lines(1988:2019, smoothEsts3MK$FM, lwd = 1,col=4)
lines(1988:2019, smoothEsts1Linf$FM, lwd = 1,col=5)
lines(1988:2019, smoothEsts2Linf$FM, lwd = 1,col=6)


points(1988:2019, fmmk1,col=2,pch=19)
points(1988:2019, fmmk2,col=3,pch=19)
points(1988:2019, fmmk3,col=4,pch=19)
points(1988:2019, fmL1,col=5,pch=19)
points(1988:2019, fmL2,col=6,pch=19)

#legend("topleft", legend=c("REFERENCIA", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)

#####################################------< Norway Loster 8c FU2627 Females

#####################################------> Small Spotted Catshark


PinPars <- new("LB_pars")
PinPars@Linf <- 75
PinPars@L50 <- 54.2 
PinPars@L95 <- 58
PinPars@MK <- 1.5 
PinPars@M <- 0.3 
PinPars@L_units <- "cm"

PinLenFreq1 <- new("LB_lengths", LB_pars=PinPars, file="Pin8c9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq1@L_units <- PinPars@L_units

Fit1 <- LBSPRfit(PinPars, PinLenFreq1,verbose = FALSE)

# Primero infraestimando (${M/k}^{LIT}$ multiplicado por 0.75):
Pin1ParsMK <-PinPars
Pin1ParsMK@MK <- 0.75*1.5
PinLenFreq1MK <- new("LB_lengths", LB_pars=Pin1ParsMK, file="Pin8c9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq1MK@L_units <- Pin1ParsMK@L_units
Fit1MK <- LBSPRfit(Pin1ParsMK, PinLenFreq1MK,verbose = FALSE)

# Segundo sobrestimando (${M/k}^{LIT}$ multiplicado por 1.25):
Pin2ParsMK <-PinPars
Pin2ParsMK@MK <- 1.25*1.5
PinLenFreq2MK <- new("LB_lengths", LB_pars=Pin2ParsMK, file="Pin8c9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq2MK@L_units <- Pin2ParsMK@L_units
Fit2MK <- LBSPRfit(Pin2ParsMK, PinLenFreq2MK,verbose = FALSE)

# Primero infraestimando ($L_{inf}^{LIT}$ multiplicado por 0.75):
Pin1ParsLinf <-PinPars
Pin1ParsLinf@Linf <- 0.75*75 
PinLenFreq1Linf <- new("LB_lengths", LB_pars=Pin1ParsLinf, file="Pin8c9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq1Linf@L_units <- Pin1ParsLinf@L_units
Fit1Linf <- LBSPRfit(Pin1ParsLinf, PinLenFreq1Linf,verbose = FALSE)
# Segundo sobrestimando ($L_{inf}^{LIT}$ multiplicado por 1.25):
Pin2ParsLinf <-PinPars
Pin2ParsLinf@Linf <- 1.25*75
PinLenFreq2Linf <- new("LB_lengths", LB_pars=Pin2ParsLinf, file="Pin8c9afreq.csv", dataType="freq", header=TRUE)
PinLenFreq2Linf@L_units <- Pin2ParsLinf@L_units


#PinLenFreq1@LMids


PinLenFreq2Linf@LMids=seq(20.5,94.5,by=1)
A=PinLenFreq2Linf@LData
for(i in 1:20){
  A=rbind(A,(PinLenFreq2Linf@LData[55,]))}
A[55:75,1:6]=rep(0,6)
#tail(cbind(A,PinLenFreq2Linf@LMids))
PinLenFreq2Linf@LData=A
Pin2ParsLinf <-PinPars
Pin2ParsLinf@Linf <-1.25*75
Fit2Linf <- LBSPRfit(Pin2ParsLinf, PinLenFreq2Linf,verbose = FALSE)


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

min=min(c(spr1,sprmk1,sprmk2,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprL1,sprL2))

plot(2014:2019,spr1,ylim=c(min,max),xlab="Years",ylab="SPR estimates",pch=19)

lines(2014:2019, smoothEsts1$SPR, lwd = 1,col="black")
lines(2014:2019, smoothEsts1MK$SPR, lwd = 1,col=2)
lines(2014:2019, smoothEsts2MK$SPR, lwd = 1,col=3)
lines(2014:2019, smoothEsts1Linf$SPR, lwd = 1,col=5)
lines(2014:2019, smoothEsts2Linf$SPR, lwd = 1,col=6)

points(2014:2019, sprmk1,col=2,pch=19)
points(2014:2019, sprmk2,col=3,pch=19)
points(2014:2019, sprL1,col=5,pch=19)
points(2014:2019, sprL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","0.75*Linf","1.25*Linf"),col=c("black", c(2,3,5,6)), pch=19, cex=0.8)

fm1=Fit1@FM
fmmk1=Fit1MK@FM
fmmk2=Fit2MK@FM
fmL1=Fit1Linf@FM
fmL2=Fit2Linf@FM

min=min(c(fm1,fmmk1,fmmk2,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmL1,fmL2))

plot(2014:2019,fm1,ylim=c(min,max),xlab="Years",ylab="F/M estimates",pch=19)

lines(2014:2019, smoothEsts1$FM, lwd = 1,col="black")
lines(2014:2019, smoothEsts1MK$FM, lwd = 1,col=2)
lines(2014:2019, smoothEsts2MK$FM, lwd = 1,col=3)
lines(2014:2019, smoothEsts1Linf$FM, lwd = 1,col=5)
lines(2014:2019, smoothEsts2Linf$FM, lwd = 1,col=6)
points(2014:2019, fmmk1,col=2,pch=19)
points(2014:2019, fmmk2,col=3,pch=19)
points(2014:2019, fmL1,col=5,pch=19)
points(2014:2019, fmL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","0.75*Linf","1.25*Linf"),col=c("black", 2,3,5,6), pch=19, cex=0.8)
#
#####################################------< Small Spotted Catshark

#####################################------> Anchovy


MK<-1.44
LINF<-18.95
Hke1Pars <- new("LB_pars")
Hke1Pars@Linf <- 18.95
Hke1Pars@L50 <- 11.2 
Hke1Pars@L95 <- 13.3
Hke1Pars@MK <- 1.44 
Hke1Pars@M <- 1.3 
Hke1Pars@L_units <- "cm"

HkeLenFreq1 <- new("LB_lengths", LB_pars=Hke1Pars, file="lengthfreq_anchovy_fv.csv", dataType="freq", header=TRUE)
HkeLenFreq1@L_units <- Hke1Pars@L_units

Fit1 <- LBSPRfit(Hke1Pars, HkeLenFreq1,verbose =FALSE)

# Primero infraestimando (${M/k}^{LIT}$ multiplicado por 0.75):
Hke1ParsMK <-Hke1Pars
Hke1ParsMK@MK <- 0.75*MK


HkeLenFreq1MK <- new("LB_lengths", LB_pars=Hke1ParsMK, file="lengthfreq_anchovy_fv.csv", dataType="freq", header=TRUE)
HkeLenFreq1MK@L_units <- Hke1ParsMK@L_units


Fit1MK <- LBSPRfit(Hke1ParsMK, HkeLenFreq1MK,verbose = FALSE)

# Segundo sobrestimando (${M/k}^{LIT}$ multiplicado por 1.25):

Hke2ParsMK <-Hke1Pars
Hke2ParsMK@MK <- 1.25*MK


HkeLenFreq2MK <- new("LB_lengths", LB_pars=Hke2ParsMK, file="lengthfreq_anchovy_fv.csv", dataType="freq", header=TRUE)
HkeLenFreq2MK@L_units <- Hke2ParsMK@L_units


Fit2MK <- LBSPRfit(Hke2ParsMK, HkeLenFreq2MK,verbose = FALSE)

# Tercero usando el valor por defecto de 1.5.

Hke3ParsMK <-Hke1Pars
Hke3ParsMK@MK <- 1.5

HkeLenFreq3MK <- new("LB_lengths", LB_pars=Hke3ParsMK, file="lengthfreq_anchovy_fv.csv", dataType="freq", header=TRUE)
HkeLenFreq3MK@L_units <- Hke3ParsMK@L_units


Fit3MK <- LBSPRfit(Hke3ParsMK, HkeLenFreq3MK,verbose = FALSE)

# Primero infraestimando ($L_{inf}^{LIT}$ multiplicado por 0.75):

Hke1ParsLinf <-Hke1Pars
Hke1ParsLinf@Linf <- 0.75*LINF

HkeLenFreq1Linf <- new("LB_lengths", LB_pars=Hke1ParsLinf, file="lengthfreq_anchovy_fv.csv", dataType="freq", header=TRUE)
HkeLenFreq1Linf@L_units <- Hke1ParsLinf@L_units


Fit1Linf <- LBSPRfit(Hke1ParsLinf, HkeLenFreq1Linf,verbose = FALSE)

# Segundo sobrestimando ($L_{inf}^{LIT}$ multiplicado por 1.25) aumentando la length hasta 24 y llenando con ceros para que no salga el error más adelante :

Hke2ParsLinf <-Hke1Pars
Hke2ParsLinf@Linf <- 1.25*LINF

HkeLenFreq2Linf <- new("LB_lengths", LB_pars=Hke2ParsLinf, file="lengthfreq_anchovy_fv.csv", dataType="freq", header=TRUE)
HkeLenFreq2Linf@L_units <- Hke2ParsLinf@L_units
HkeLenFreq2Linf@LMids=seq(3.5,24,0.5)#1:130
rows2add<-length(HkeLenFreq2Linf@LMids)-dim(HkeLenFreq2Linf@LData)[1]
B<-array(0,dim=c(rows2add,dim(HkeLenFreq2Linf@LData)[2]))
A=rbind(HkeLenFreq2Linf@LData,B)
#A[130,1:9]=rep(0,9)
#tail(A)
HkeLenFreq2Linf@LData=A

Fit2Linf <- LBSPRfit(Hke2ParsLinf, HkeLenFreq2Linf,verbose = FALSE)



smoothEsts1 <- data.frame(Fit1@Ests)
smoothEsts1MK <- data.frame(Fit1MK@Ests)
smoothEsts2MK <- data.frame(Fit2MK@Ests)
smoothEsts3MK <- data.frame(Fit3MK@Ests)
smoothEsts1Linf <- data.frame(Fit1Linf@Ests)
smoothEsts2Linf <- data.frame(Fit2Linf@Ests)


spr1=Fit1@SPR
sprmk1=Fit1MK@SPR
sprmk2=Fit2MK@SPR
sprmk3=Fit3MK@SPR
sprL1=Fit1Linf@SPR
sprL2=Fit2Linf@SPR

min=min(c(spr1,sprmk1,sprmk2,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprL1,sprL2))

plot(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32],spr1,ylim=c(min,max),xlab="Years",ylab="SPR estimates",pch=19)

lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts1$SPR, lwd = 1,col="black")
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts1MK$SPR, lwd = 1,col=2)
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts2MK$SPR, lwd = 1,col=3)
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts3MK$SPR, lwd = 1,col=4)
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts1Linf$SPR, lwd = 1,col=5)
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts2Linf$SPR, lwd = 1,col=6)


points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], sprmk1,col=2,pch=19)
points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], sprmk2,col=3,pch=19)
points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], sprmk3,col=4,pch=19)
points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], sprL1,col=5,pch=19)
points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], sprL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)


fm1=Fit1@FM
fmmk1=Fit1MK@FM
fmmk2=Fit2MK@FM
fmmk3=Fit3MK@FM
fmL1=Fit1Linf@FM
fmL2=Fit2Linf@FM

min=min(c(fm1,fmmk1,fmmk2,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmL1,fmL2))

plot(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32],fm1,ylim=c(min,max),xlab="Years",ylab="F/M estimates",pch=19)

lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts1$FM, lwd = 1,col="black")
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts1MK$FM, lwd = 1,col=2)
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts2MK$FM, lwd = 1,col=3)
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts3MK$FM, lwd = 1,col=4)
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts1Linf$FM, lwd = 1,col=5)
lines(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], smoothEsts2Linf$FM, lwd = 1,col=6)


points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], fmmk1,col=2,pch=19)
points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], fmmk2,col=3,pch=19)
points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], fmmk3,col=4,pch=19)
points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], fmL1,col=5,pch=19)
points(HkeLenFreq2Linf@Years[1]:HkeLenFreq2Linf@Years[32], fmL2,col=6,pch=19)

#legend("topright", legend=c("REFERENCIA", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)
#####################################------< Anchovy

#####################################------> Pollack



Pol1 <- new("LB_pars")
Pol1@Linf <- 98.2 
Pol1@CVLinf <- 0.1 # by default
Pol1@L50 <- 42.3
Pol1@L95 <- 59 # estimated from figure (Alonso-Fernández etal, 2013)
Pol1@MK <- 0.3/0.182
Pol1@M <- 0.3 
Pol1@SL50 <- 47.5
Pol1@SL95 <- 69
Pol1@SPR <- 0.4
Pol1@Walpha <- 0.00001
Pol1@Wbeta <- 3.0044
Pol1@BinWidth <- 4
MySim <- LBSPRsim(Pol1)
Pol1@BinMax <- 130
Pol1@BinMin <- 0
Pol1@L_units <- "cm"

Len1 <- new("LB_lengths", LB_pars=Pol1, file=("pol89aSPR_MultYr4cm.csv"), dataType="freq",header=TRUE)

Fit1 <- LBSPRfit(Pol1, Len1,verbose = FALSE)

#Underestimate M/k^LIT, multiplying by the factor 0.75:

Pol0.75MK <- Pol1
Pol0.75MK@MK <- 0.75*Pol1@MK
Pol0.75LenMK <- new("LB_lengths",  LB_pars=Pol0.75MK, file="pol89aSPR_MultYr4cm.csv", dataType="freq", header=TRUE)
Pol0.75LenMK@L_units <- Pol0.75MK@L_units
Fit0.75MK <- LBSPRfit(Pol0.75MK, Pol0.75LenMK,verbose = FALSE)

#Overestimate M/k^LIT multiplying by the factor 1.25:

Pol1.25MK <- Pol1
Pol1.25MK@MK <- 1.25*Pol1@MK
Pol1.25LenMK <- new("LB_lengths", LB_pars=Pol1.25MK, file="pol89aSPR_MultYr4cm.csv", dataType="freq", header=TRUE)
Pol1.25LenMK@L_units <- Pol1.25MK@L_units
Fit1.25MK <- LBSPRfit(Pol1.25MK, Pol1.25LenMK,verbose = FALSE)

#Using the default value of M/k^LIT for teleosts, 1.5:
Pol1.5MK <- Pol1
Pol1.5MK@MK <- 1.5
Pol1.5LenMK <- new("LB_lengths", LB_pars=Pol1.5MK, file="pol89aSPR_MultYr4cm.csv", dataType="freq", header=TRUE)
Pol1.5LenMK@L_units <- Pol1.5MK@L_units
Fit1.5MK <- LBSPRfit(Pol1.5MK, Pol1.5LenMK,verbose = FALSE)

#Underestimate L^LIT_inf, multiplying by the factor 0.75:
Pol0.75Linf <- Pol1
Pol0.75Linf@Linf <- 0.75*Pol1@Linf
Pol0.75LenLinf <- new("LB_lengths", LB_pars=Pol0.75Linf, file="pol89aSPR_MultYr4cm.csv", dataType="freq", header=TRUE)
Pol0.75LenLinf@L_units <- Pol0.75Linf@L_units
Fit0.75Linf <- LBSPRfit(Pol0.75Linf, Pol0.75LenLinf,verbose = FALSE)

#Overestimate L^LIT_inf, multiplying by the factor 1.25:
Pol1.25Linf <- Pol1
Pol1.25Linf@Linf <- 1.25*Pol1@Linf
Pol1.25LenLinf <- new("LB_lengths", LB_pars=Pol1.25Linf, file="pol89aSPR_MultYr4cm.csv", dataType="freq", header=TRUE)
Pol1.25LenLinf@L_units <- Pol1.25Linf@L_units
Fit1.25Linf <- LBSPRfit(Pol1.25Linf, Pol1.25LenLinf,verbose = FALSE)



smoothEsts1 <- data.frame(Fit1@Ests)
smoothEsts1MK <- data.frame(Fit0.75MK@Ests)
smoothEsts2MK <- data.frame(Fit1.25MK@Ests)
smoothEsts3MK <- data.frame(Fit1.5MK@Ests)
smoothEsts1Linf <- data.frame(Fit0.75Linf@Ests)
smoothEsts2Linf <- data.frame(Fit1.25Linf@Ests)

spr1 <- Fit1@SPR
sprmk1 <- Fit0.75MK@SPR
sprmk2 <- Fit1.25MK@SPR
sprmk3 <- Fit1.5MK@SPR
sprL1 <- Fit0.75Linf@SPR
sprL2 <- Fit1.25Linf@SPR

min=min(c(spr1,sprmk1,sprmk2,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprL1,sprL2))

plot(c(2010:2019),spr1,ylim=c(min,max),xlab="Year",ylab="SPR estimate",pch=19)

lines(2010:2019, smoothEsts1$SPR, lwd = 1,col="black")
lines(2010:2019, smoothEsts1MK$SPR, lwd = 1, col=2)
lines(2010:2019, smoothEsts2MK$SPR, lwd = 1, col=3)
lines(2010:2019, smoothEsts3MK$SPR, lwd = 1, col=4)
lines(2010:2019, smoothEsts1Linf$SPR, lwd = 1, col=5)
lines(2010:2019, smoothEsts2Linf$SPR, lwd = 1 ,col=6)


points(2010:2019, sprmk1,col=2,pch=19)
points(2010:2019, sprmk2,col=3,pch=19)
points(2010:2019, sprmk3,col=4,pch=19)
points(2010:2019, sprL1,col=5,pch=19)
points(2010:2019, sprL2,col=6, pch=19)

#legend("topright", legend=c("Reference Case", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)


fm1=Fit1@FM
fmmk1=Fit0.75MK@FM
fmmk2=Fit1.25MK@FM
fmmk3=Fit1.5MK@FM
fmL1=Fit0.75Linf@FM
fmL2=Fit1.25Linf@FM

min=min(c(fm1,fmmk1,fmmk2,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmL1,fmL2))

plot(2010:2019,fm1,ylim=c(min,max),xlab="Year",ylab="F/M estimate",pch=19)

lines(2010:2019, smoothEsts1$FM, lwd = 1,col="black")
lines(2010:2019, smoothEsts1MK$FM, lwd = 1,col=2)
lines(2010:2019, smoothEsts2MK$FM, lwd = 1,col=3)
lines(2010:2019, smoothEsts3MK$FM, lwd = 1,col=4)
lines(2010:2019, smoothEsts1Linf$FM, lwd = 1, col=5)
lines(2010:2019, smoothEsts2Linf$FM, lwd = 1, col=6)


points(2010:2019, fmmk1,col=2,pch=19)
points(2010:2019, fmmk2,col=3,pch=19)
points(2010:2019, fmmk3,col=4,pch=19)
points(2010:2019, fmL1,col=5,pch=19)
points(2010:2019, fmL2,col=6,pch=19)
#legend("topright", legend=c("Reference Case", "0.75*M/k","1.25*M/k","M/k=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)

#####################################------< Pollack

#####################################------> Blackspot

sbr1Pars <- new("LB_pars")
sbr1Pars@Linf <- 62
sbr1Pars@L50 <- 33 
sbr1Pars@L95 <- 35
sbr1Pars@MK <- 1.42 
sbr1Pars@M <- 0.2 
sbr1Pars@L_units <- "cm"

sbrLenFreq1 <- new("LB_lengths", LB_pars=sbr1Pars, file="LFreqSBR.csv", dataType="freq", header=TRUE)
sbrLenFreq1@L_units <- sbr1Pars@L_units

Fit1 <- LBSPRfit(sbr1Pars, sbrLenFreq1,verbose = FALSE)


#Underestimate (M/k*0.75):

sbr1ParsMK <-sbr1Pars
sbr1ParsMK@MK <- 0.75*1.42

sbrLenFreq1MK <- new("LB_lengths", LB_pars=sbr1ParsMK, file="LFreqSBR.csv", dataType="freq", header=TRUE)
sbrLenFreq1MK@L_units <- sbr1ParsMK@L_units

Fit1MK <- LBSPRfit(sbr1ParsMK, sbrLenFreq1MK,verbose = FALSE)

# Overestimate (M/k*1.25):


sbr2ParsMK <-sbr1Pars
sbr2ParsMK@MK <- 1.25*1.42

sbrLenFreq2MK <- new("LB_lengths", LB_pars=sbr2ParsMK, file="LFreqSBR.csv", dataType="freq", header=TRUE)
sbrLenFreq2MK@L_units <- sbr2ParsMK@L_units

Fit2MK <- LBSPRfit(sbr2ParsMK, sbrLenFreq2MK,verbose = FALSE)

#Default value (M/k=1.5)

sbr3ParsMK <-sbr1Pars
sbr3ParsMK@MK <- 1.5

sbrLenFreq3MK <- new("LB_lengths", LB_pars=sbr3ParsMK, file="LFreqSBR.csv", dataType="freq", header=TRUE)
sbrLenFreq3MK@L_units <- sbr3ParsMK@L_units

Fit3MK <- LBSPRfit(sbr3ParsMK, sbrLenFreq3MK,verbose = FALSE)

#Underestimate (Linf*0.75)

sbr1ParsLinf <-sbr1Pars
sbr1ParsLinf@Linf <- 62*0.75

sbrLenFreq1Linf <- new("LB_lengths", LB_pars=sbr1ParsLinf, file="LFreqSBR.csv", dataType="freq", header=TRUE)
sbrLenFreq1Linf@L_units <- sbr1ParsLinf@L_units

Fit1Linf <- LBSPRfit(sbr1ParsLinf, sbrLenFreq1Linf,verbose = FALSE)

#Overestimate (Linf*1.25)

sbr2ParsLinf <-sbr1Pars
sbr2ParsLinf@Linf <- 62*1.25

sbrLenFreq2Linf <- new("LB_lengths", LB_pars=sbr2ParsLinf, file="LFreqSBR.csv", dataType="freq", header=TRUE)
sbrLenFreq2Linf@L_units <- sbr2ParsLinf@L_units

Fit2Linf <- LBSPRfit(sbr2ParsLinf, sbrLenFreq2Linf,verbose = FALSE)


smoothEsts1 <- data.frame(Fit1@Ests)
smoothEsts1MK <- data.frame(Fit1MK@Ests)
smoothEsts2MK <- data.frame(Fit2MK@Ests)
smoothEsts3MK <- data.frame(Fit3MK@Ests)
smoothEsts1Linf <- data.frame(Fit1Linf@Ests)
smoothEsts2Linf <- data.frame(Fit2Linf@Ests)

spr1=Fit1@SPR
sprmk1=Fit1MK@SPR
sprmk2=Fit2MK@SPR
sprmk3=Fit3MK@SPR
sprL1=Fit1Linf@SPR
sprL2=Fit2Linf@SPR

min=min(c(spr1,sprmk1,sprmk2,sprmk3,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprmk3,sprL1,sprL2))

plot(1997:2019,spr1,ylim=c(min,max),xlab="Years",ylab="SPR estimates",pch=19)

lines(1997:2019, smoothEsts1$SPR, lwd = 1,col="black")
lines(1997:2019, smoothEsts1MK$SPR, lwd = 1,col=2)
lines(1997:2019, smoothEsts2MK$SPR, lwd = 1,col=3)
lines(1997:2019, smoothEsts3MK$SPR, lwd = 1,col=4)
lines(1997:2019, smoothEsts1Linf$SPR, lwd = 1,col=5)
lines(1997:2019, smoothEsts2Linf$SPR, lwd = 1,col=6)

points(1997:2019, sprmk1,col=2,pch=19)
points(1997:2019, sprmk2,col=3,pch=19)
points(1997:2019, sprmk3,col=4,pch=19)
points(1997:2019, sprL1,col=5,pch=19)
points(1997:2019, sprL2,col=6,pch=19)

#legend("topright", legend=c("BASE", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)

fm1=Fit1@FM
fmmk1=Fit1MK@FM
fmmk2=Fit2MK@FM
fmmk3=Fit3MK@FM
fmL1=Fit1Linf@FM
fmL2=Fit2Linf@FM

min=min(c(fm1,fmmk1,fmmk2,fmmk3,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmmk3,fmL1,fmL2))

plot(1997:2019,fm1,ylim=c(min,max),xlab="Years",ylab="F/M estimates",pch=19)

lines(1997:2019, smoothEsts1$FM, lwd = 1,col="black")
lines(1997:2019, smoothEsts1MK$FM, lwd = 1,col=2)
lines(1997:2019, smoothEsts2MK$FM, lwd = 1,col=3)
lines(1997:2019, smoothEsts3MK$FM, lwd = 1,col=4)
lines(1997:2019, smoothEsts1Linf$FM, lwd = 1,col=5)
lines(1997:2019, smoothEsts2Linf$FM, lwd = 1,col=6)

points(1997:2019, fmmk1,col=2,pch=19)
points(1997:2019, fmmk2,col=3,pch=19)
points(1997:2019, fmmk3,col=4,pch=19)
points(1997:2019, fmL1,col=5,pch=19)
points(1997:2019, fmL2,col=6,pch=19)

#legend("topleft", legend=c("BASE", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf"),
#       col=c("black", 2:6), pch=19, cex=0.8)
#####################################------< Blackspot

#####################################------> Pouting


library(tidyverse)
library(lubridate)
library(kableExtra)
library(knitr)
MyPars <- new("LB_pars")
MyPars@Species <- "Trisopterus luscus"

# LHT
LHTs <- data.frame(t(c("Trisopterus luscus", 19.2, 1, 46.7, 0.21, 1.27, 9)))
colnames(LHTs) <- c("Sp", "L50", "Amat", "Linf", "K", "t0", "Amax")
LHTs$MKlit1 <-3.111105
LHTs$MKlit2 <- 1.761846
LHTs$MK <- 1.5

# 95% probability mature
# Alonso-Fernánde et al. 2008: Pmat = a+bxTL
a <- -25.77
b <- 1.34
L95 <- (log(0.95/0.05)-a)/b

MyPars@Linf <- as.numeric(as.character(LHTs$Linf))
MyPars@L50  <- as.numeric(as.character(LHTs$L50 ))
MyPars@L95  <- L95
MyPars@MK   <- as.numeric(as.character(LHTs$MKlit2)) 
MyPars@SPR <- 0.4
MyPars@L_units <- "cm"


MyLengths <- new("LB_lengths", LB_pars=MyPars, file="faneca.csv", 
                 dataType="raw", header = TRUE)
myFit <- LBSPRfit(MyPars, MyLengths,verbose = FALSE)

# M/K = 0.75 x M/K(LIT)
MyParsMKLow <- MyPars
MyParsMKLow@MK <- 0.75*MyPars@MK
MyLengthsMKLow <- new("LB_lengths", LB_pars=MyParsMKLow, file="faneca.csv", 
                 dataType="raw", header = TRUE)
MyLengthsMKLow@L_units <- MyParsMKLow@L_units
myFitMKLow <- LBSPRfit(MyParsMKLow, MyLengthsMKLow,verbose = FALSE)

# M/K = 1.25 x M/K(LIT)
MyParsMKSup <- MyPars
MyParsMKSup@MK <- 1.25*MyPars@MK
MyLengthsMKSup <- new("LB_lengths", LB_pars=MyParsMKSup, file="faneca.csv", 
                     dataType="raw", header = TRUE)
MyLengthsMKSup@L_units <- MyParsMKSup@L_units
myFitMKSup <- LBSPRfit(MyParsMKSup, MyLengthsMKSup,verbose = FALSE)

# M/K = 1.5
MyParsMK15 <- MyPars
MyParsMK15@MK <- 1.5
MyLengthsMK15 <- new("LB_lengths", LB_pars=MyParsMK15, file="faneca.csv", 
                     dataType="raw", header = TRUE)
MyLengthsMK15@L_units <- MyParsMK15@L_units
myFitMK15 <- LBSPRfit(MyParsMK15, MyLengthsMK15,verbose = FALSE)

# M/K = 3.1 (mortality based on longecity; Then et al. 2015)
MyParsMKnls <- MyPars
MyParsMKnls@MK <- 3.111105
MyLengthsMKnls <- new("LB_lengths", LB_pars=MyParsMKnls, file="faneca.csv", 
                     dataType="raw", header = TRUE)
MyLengthsMKnls@L_units <- MyParsMKnls@L_units
myFitMKnls <- LBSPRfit(MyParsMKnls, MyLengthsMKnls,verbose = FALSE)

# Linf = 0.75 x Linf(LIT)
MyParsLinfLow <- MyPars
MyParsLinfLow@Linf <- 0.75*MyPars@Linf
MyLengthsLinfLow <- new("LB_lengths", LB_pars=MyParsLinfLow, file="faneca.csv", 
                      dataType="raw", header = TRUE)
MyLengthsLinfLow@L_units <- MyParsLinfLow@L_units
myFitLinfLow <- LBSPRfit(MyParsLinfLow, MyLengthsLinfLow,verbose = FALSE)

# Linf = 1.25 x Linf(LIT)
MyParsLinfSup <- MyPars
MyParsLinfSup@Linf <- 1.25*MyPars@Linf
MyLengthsLinfSup <- new("LB_lengths", LB_pars=MyParsLinfSup, file="faneca.csv", 
                      dataType="raw",header = TRUE)
MyLengthsLinfSup@L_units <- MyParsLinfSup@L_units
myFitLinfSup <- LBSPRfit(MyParsLinfSup, MyLengthsLinfSup,verbose = FALSE)

smoothEsts1        <- data.frame(myFit@Ests)
smoothEsts1MK   <- data.frame(myFitMKLow@Ests)
smoothEsts2MK   <- data.frame(myFitMKSup@Ests)
smoothEsts3MK   <- data.frame(myFitMK15@Ests)
smoothEsts4MK   <- data.frame(myFitMKnls@Ests)
smoothEsts1Linf <- data.frame(myFitLinfLow@Ests)
smoothEsts2Linf <- data.frame(myFitLinfSup@Ests)


spr1     = myFit@SPR
sprmk1   = myFitMKLow@SPR
sprmk2   = myFitMKSup@SPR
sprmk3  = myFitMK15@SPR
sprmk4   = myFitMKnls@SPR
sprL1 = myFitLinfLow@SPR
sprL2 = myFitLinfSup@SPR

fm1   = myFit@FM
fmmk1   = myFitMKLow@FM
fmmk2   = myFitMKSup@FM
fmmk3   = myFitMK15@FM
fmmk4  = myFitMKnls@FM
fmL1 = myFitLinfLow@FM
fmL2= myFitLinfSup@FM

min=min(c(spr1,sprmk1,sprmk2,sprmk3, sprmk4,sprL1,sprL2))
max=max(c(spr1,sprmk1,sprmk2,sprmk3, sprmk4,sprL1,sprL2))

plot(1999:2018,spr1,ylim=c(min,max),xlab="Year",ylab="SPR estimate",pch=19)

lines(1999:2018, smoothEsts1$SPR, lwd = 1,col="black")
lines(1999:2018, smoothEsts1MK$SPR, lwd = 1, col=2)
lines(1999:2018, smoothEsts2MK$SPR, lwd = 1, col=3)
lines(1999:2018, smoothEsts3MK$SPR, lwd = 1, col=4)
lines(1999:2018, smoothEsts1Linf$SPR, lwd = 1, col=5)
lines(1999:2018, smoothEsts2Linf$SPR, lwd = 1 ,col=6)
lines(1999:2018, smoothEsts4MK$SPR, lwd = 1, col=7)

points(1999:2018, sprmk1,col=2,pch=19)
points(1999:2018, sprmk2,col=3,pch=19)
points(1999:2018, sprmk3,col=4,pch=19)
points(1999:2018, sprL1,col=5,pch=19)
points(1999:2018, sprL2,col=6, pch=19)
points(1999:2018, sprmk4,col=7,pch=19)

#legend("topright", legend=c("Reference Case", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf","MK=3.1"),
#       col=c("black", 2:7), pch=19, cex=0.8)

min=min(c(fm1,fmmk1,fmmk2,fmmk3, fmmk4,fmL1,fmL2))
max=max(c(fm1,fmmk1,fmmk2,fmmk3, fmmk4,fmL1,fmL2))

plot(1999:2018,fm1,ylim=c(min,max),xlab="Year",ylab="F/M estimate",pch=19)

lines(1999:2018, smoothEsts1$FM, lwd = 1,col="black")
lines(1999:2018, smoothEsts1MK$FM, lwd = 1,col=2)
lines(1999:2018, smoothEsts2MK$FM, lwd = 1,col=3)
lines(1999:2018, smoothEsts3MK$FM, lwd = 1,col=4)
lines(1999:2018, smoothEsts1Linf$FM, lwd = 1, col=5)
lines(1999:2018, smoothEsts2Linf$FM, lwd = 1, col=6)
lines(1999:2018, smoothEsts4MK$FM, lwd = 1,col=7)

points(1999:2018, fmmk1,col=2,pch=19)
points(1999:2018, fmmk2,col=3,pch=19)
points(1999:2018, fmmk3,col=4,pch=19)
points(1999:2018, fmL1,col=5,pch=19)
points(1999:2018, fmL2,col=6,pch=19)
points(1999:2018, fmmk4,col=7,pch=19)
#legend("topright", legend=c("Reference Case", "0.75*MK","1.25*MK","MK=1.5","0.75*Linf","1.25*Linf","MK=3.1"),
#       col=c("black", 2:7), pch=19, cex=0.8)

#####################################------< Pouting