# read_data.R

d1 <- read.csv('data/table1.txt',header = T,sep = '\t')
d2 <- read.csv('data/table2.txt',header = T,sep = '\t')

## make compatible
colnames(d1)[1]<-'ID'
d1$Tone.3<-NA
colnames(d1)[7]<-'Hz.1'
colnames(d1)[8]<-'Hz.2'
d1$Hz.3<-NA
d1<-dplyr::select(d1,-Ratio,-CD,-DV)
d1$Type<-'Dyad'

## make compatible
colnames(d2)[1]<-'ID'
colnames(d2)[5]<-'Hz.1'
colnames(d2)[6]<-'Hz.2'
colnames(d2)[7]<-'Hz.3'
d2$Type<-'Triad'

df<-rbind(d1,d2)
df$Nro<-1:nrow(df)

m1 <- read.csv('data/model1.txt',header = T,sep = ',')
m2 <- read.csv('data/model2.txt',header = T,sep = '\t')

## merge
mf <- rbind(m1,m2)
df$NRO<-mf$NRO
df$BHM<-mf$BHM
df$HPHM<-mf$HPHM
df$SIM<-mf$SIM

#### TRIMMING OPTIONS

# Triads
df$Familiar<-'No'
df$Familiar[df$Tone.3==13]<-'YesAlso'
df$Familiar[df$Tone.2==13]<-'YesAlso'
df$Familiar[df$Tone.2==2]<-'Yes'
df$Familiar[df$Tone.2==11]<-'Yes'
df$Familiar[df$Tone.3==2]<-'Yes'
df$Familiar[df$Tone.3==11]<-'Yes'

df$Familiar<-factor(df$Familiar)

rm(m1,m2,mf,d1,d2)
