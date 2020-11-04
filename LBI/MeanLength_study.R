# Checking MeanLength



wal <- read.csv("Pol89awal.csv", stringsAsFactors = FALSE)
freq <- read.csv("Pol89afreq.csv", stringsAsFactors = FALSE)
head(freq)
head(wal)


bfreq=bin_mat(freq,1)
dim(bfreq)
dim(freq)

freq1=freq
freq1[,1]=freq[,1]-0.5

head(freq1)
tail(freq1)
bfreq1=bin_mat(freq1,1)
dim(bfreq1)
dim(freq1)

lb_table(freq, 4, "cm", 98.2, 42.3, 1.5,wal)



wal1=wal
a = 0.0000109; b = 3.0044
res=a*freq1[,1]^b
wal1[,1]=freq1[,1]
for (i in 2:10){
wal1[,i]=res}

lb_table(freq1, 4, "cm", 98.2, 42.3, 1.5,wal1)
bin_mat(wal1,1)

