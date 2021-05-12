# consistency.R (script)
  # T. Eerola 27/3/2019

library(psych)
library(reshape2)
table(m$Background)
m_ka <- dplyr::filter(m,Background=='Kalash')  
m_ko <- dplyr::filter(m,Background=='Khow')  
m_uk <- dplyr::filter(m,Background=='UK')  
m_ka$PARTICIPANTID<-factor(m_ka$PARTICIPANTID)
m_ko$PARTICIPANTID<-factor(m_ko$PARTICIPANTID)
m_uk$PARTICIPANTID<-factor(m_uk$PARTICIPANTID)
# length(table(m_ka$PARTICIPANTID))
# length(table(m_ko$PARTICIPANTID))
# length(table(m_uk$PARTICIPANTID))

#### KALASH ---------------

m_ka <- dplyr::filter(m_ka,PARTICIPANTID!='MKARH2907191130' & PARTICIPANTID!='MKAZS2407191400' & PARTICIPANTID!='MKANSAH0108190900' & PARTICIPANTID!='FKANSM0408191245')
m_ka$PARTICIPANTID<-factor(m_ka$PARTICIPANTID)
length(table(m_ka$PARTICIPANTID))

N <- length(unique(m_ka$PARTICIPANTID))
U <- unique(m_ka$PARTICIPANTID)  
tmp <- NULL
  for (k in 1:N) { # Stupid loop
      tmp0 <- data.frame(as.numeric(m_ka$Rating[m_ka$PARTICIPANTID==U[k]]))
      tmp <- c(tmp,tmp0)
    }
tmp<-data.frame(tmp)
colnames(tmp)<-paste('S',seq(1,N),sep='')
a <- suppressWarnings(suppressMessages(psych::alpha(tmp,check.keys = FALSE,warnings = FALSE)))

cat(paste('\nKalash - alpha: ',round(a$total$raw_alpha,3))) # 0.721, 0.736 after dropping S11 after dropping four 0.782

#S7, S11 S15, S19 are near 0, 

#### KHOW ---------------
N <- length(unique(m_ko$PARTICIPANTID))
U <- unique(m_ko$PARTICIPANTID)  
tmp <- NULL
for (k in 1:N) { # Stupid loop
  tmp0 <- data.frame(as.numeric(m_ko$Rating[m_ko$PARTICIPANTID==U[k]]))
  tmp <- c(tmp,tmp0)
}
tmp<-data.frame(tmp)
colnames(tmp)<-paste('S',seq(1,N),sep='')
a <- suppressWarnings(suppressMessages(psych::alpha(tmp,check.keys = FALSE,warnings = FALSE)))

cat(paste('\nKhow - alpha: ',round(a$total$raw_alpha,3))) # 0.812

#### UK ---------------
N <- length(unique(m_uk$PARTICIPANTID))
U <- unique(m_uk$PARTICIPANTID)  
tmp <- NULL
for (k in 1:N) { # Stupid loop
  tmp0 <- data.frame(as.numeric(m_uk$Rating[m_uk$PARTICIPANTID==U[k]]))
  tmp <- c(tmp,tmp0)
}
tmp<-data.frame(tmp)
colnames(tmp)<-paste('S',seq(1,N),sep='')
a <- suppressWarnings(suppressMessages(psych::alpha(tmp,check.keys = FALSE,warnings = FALSE)))

cat(paste('\nUK - alpha: ',round(a$total$raw_alpha,3))) # 0.959


rm(N,U,a,k,m_ka,m_ko,m_uk,tmp,tmp0)
  
  