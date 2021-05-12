# paired_comparisons.R
# start here with pre-loaded data

p$Type<-factor(p$Type)
p$Background<-factor(p$Background)
p$Instrument<-factor(p$Instrument)
p$Mode<-factor(p$Mode)
p$Gender<-factor(p$Gender)
p$PARTICIPANTID<-factor(p$PARTICIPANTID)

# re-order factors
p$Background<-factor(p$Background,levels = c('UK',"Kalash","Khow"))
p$value<-factor(p$value)
p$value<-factor(p$value,levels = c("Major","Minor","Augm.","Chrom."),labels = c("Major","Minor","Augm.","Chrom."))

table(p$Background,p$Gender)/length(unique(p$variable))

knitr::kable(table(p$value))

print(knitr::kable(table(p$Background,p$value),caption = 'Counts of the preferred chord choices across Culture.'))
knitr::kable(table(p$Mode1,p$value))

print(knitr::kable(table(p$Background,p$value,p$Instrument),caption = 'Counts of the preferred chord choices across Instrument.'))

knitr::kable(table(p$Mode1,p$value,p$Type))

knitr::kable(table(p$value,p$Background)) 
tab2<-prop.table(table(p$value, p$Background), margin=2)*100
knitr::kable(tab2,digits=2)

print(knitr::kable(table(p$value,p$Type),caption = 'Counts of the preferred chord choices across Type (Arpeggio or Chord).'))

knitr::kable(table(p$value,p$Instrument))

knitr::kable(table(p$Background,p$value))
knitr::kable(table(p$Background,p$value))
knitr::kable(table(p$Mode1,p$Background,p$value))



#### CREATE TABLE FOR SUPPORTING MATERIAL -----------------
#library(tidyr)
x<-table(p$Background,p$Instrument,p$value)
X <- as.data.frame(as.table(x)) 

X %>% 
  pivot_wider(names_from = c(Var1,Var2), values_from = c(Freq)) -> tmp2
print(knitr::kable(tmp2,caption = 'Counts across Culture and Instrument (Supporting Materials).'))

x<-table(p$Background,p$Type,p$value)
X <- as.data.frame(as.table(x)) 

X %>% 
  pivot_wider(names_from = c(Var1,Var2), values_from = c(Freq)) -> tmp2
print(knitr::kable(tmp2,caption = 'Counts across Culture and Type (Supporting Materials).'))


x<-table(p$Background,p$Gender,p$value)
X <- as.data.frame(as.table(x)) 

X %>% 
  pivot_wider(names_from = c(Var1,Var2), values_from = c(Freq)) -> tmp2
print(knitr::kable(tmp2,caption = 'Counts across Culture and Gender (Supporting Materials).'))

