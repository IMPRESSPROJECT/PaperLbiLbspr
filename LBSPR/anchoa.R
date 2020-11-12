#Anchoa

datos=read.csv(file="lengthfreq_anchovy_fv.csv")

datos_new=datos

for (i in 1:(dim(datos)[1])){
  for (j in 1:(dim(datos)[2])){
    datos_new[i,j]=0
  }
}

datos_new[1,]=datos[1,]

seq=seq(2,32,by=2)

for (i in seq){
  datos_new[i,]=datos[i,]+datos[i+1,]
}

datos_new[34,]=datos[34,]


ind=which(datos_new[,1]==0)

dat=datos_new[-ind,]
dat[2:17,1]=seq(4.5,19.5,by=1)

write.csv("anchovy.csv")