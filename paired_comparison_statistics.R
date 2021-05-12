# paired_comparison_statistics.R

#### Effect of Type across Culture? --------------------

# Decision and Type with conditional independence of Culture 
mytable<-xtabs(~value+Type+Background, data=p)
print(mytable)
print(loglm(~(value+Type) * Background, data=mytable))

#No Three-Way Interaction
loglm(~value+Type+Background+value*Type+value*Background+Type*Background, mytable)

# Main effect
mytable<-xtabs(~value+Background, data=p)
mytable
print(loglm(~value+Background, data=mytable))


#### Effect of Timbre across Culture? -----------------

# Decision and Type with conditional independence of Culture 
mytable<-xtabs(~value+Instrument+Background, data=p)
print(mytable)
print(loglm(~(value+Instrument) * Background, data=mytable))

#No Three-Way Interaction
print(loglm(~value+Instrument+Background+value*Instrument+value*Background+Instrument*Background, mytable))

# Main effect
mytable<-xtabs(~value+Background, data=p)
print(ftable(mytable)) # print table (Table 2)
print(summary(mytable)) # chi-square test of independence

#### Difference between Kalash and Kho? ----------------------
m_tmp<-dplyr::filter(p,Background!='UK')
m_tmp$Background<-factor(m_tmp$Background)
mytable<-xtabs(~value + Background, data=m_tmp)
print(mytable)
print(loglm(~value+Background, data=mytable))


