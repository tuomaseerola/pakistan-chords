# figure2.R

#### HELPER FUNCTION ----------------------------------------
theme_fs <- function(fs=18){
  # Custom Font Size for ggplot2
  # TE 25.4.2017
  
  tt <- theme(axis.text = element_text(size=fs-1, colour=NULL)) + 
    theme(legend.text = element_text(size=fs, colour=NULL)) + 
    theme(legend.title = element_text(size=fs, colour=NULL)) + 
    theme(axis.title = element_text(size=fs, colour=NULL)) + 
    theme(legend.text = element_text(size=fs, colour=NULL))
  return <- tt
}

#### SUMMARISE ----------------------------------------
knitr::kable(table(p$Background,p$value))


#   |       | Major| Minor| Augm.| Chrom.|
#   |:------|-----:|-----:|-----:|------:|
#   |UK     |   399|   311|   225|     25|
#   |Kalash |    78|   167|   144|     91|
#   |Khow   |    53|   171|   128|    104|

d<-c(rep(0,6))

d[1] <- round(78 / (78+167)*100,1)
d[2] <- round(167 / (78+167)*100,1)

d[3] <- round(53 / (53+171)*100,1)
d[4] <- round(171 / (53+171)*100,1)

d[5] <- round(399 / (399+311)*100,1)
d[6] <- round(311 / (399+311)*100,1)


S1 <- m %>%
  dplyr::group_by(Background, Mode) %>%
  dplyr::summarise(n=n(),m=mean(Rating,na.rm = TRUE),sd=sd(Rating,na.rm = TRUE)) %>%
  dplyr::mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se) 

S2 <-dplyr::filter(S1,Mode=='Major' | Mode=='Minor')

#### Graphics options ---------------------------------------------

plsize <- 0.50 # errorbar line width
custom_theme_size <- theme_fs(14)
min_y<-1
max_y<-5
nb.cols <- 3
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(3, "Set1"))(nb.cols)
fos<-2.3

pd <- position_dodge(width = .5) # move them .05 to the left and right
S2$m<-S2$m-1
S2$LCI<-S2$LCI-1
S2$UCI<-S2$UCI-1

figure2 <- ggplot(S2,aes(x=Background,y=m,fill=Mode))+
  geom_bar(position=pd,stat="identity",width = 0.4,colour='black')+
  geom_errorbar(S2, mapping=aes(x=Background, ymin=LCI, ymax=UCI), width=0.2, size=plsize,position = pd,show.legend = FALSE,colour='gray30') + 
  scale_y_continuous(breaks = seq(0,4,by=1),limits=c(0,4), expand = c(0, 0),labels = 1:5)+
  scale_color_brewer(palette='Set1')+
  scale_fill_brewer(palette='Set1')+
  scale_shape_manual(values = c(21,23))+
  ylab('Mean valence (±95%CI)')+
  xlab('Chord')+
  theme_bw()+
  custom_theme_size+
  theme(strip.background=element_rect(fill="white",colour="white"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(strip.text = element_text(size=20))+
  theme(legend.position="top")+
  annotate("text",x = 1,y = 3.18,colour='gray50',label='p<.0001')+
  annotate("text",x = 2,y = 3.18,colour='gray50',label='p<.0001')+
  annotate("text",x = 3,y = 3.18,colour='gray50',label='p=.0024')+
  annotate("text",x = 1-.200,y = 3.5,label=paste(d[1],'%',sep = ''),colour=mycolors[1],size=4)+
  annotate("text",x = 1+.200,y = 3.5,label=paste(d[2],'%',sep = ''),colour=mycolors[2],size=4)+
  annotate("text",x = 2-.200,y = 3.5,label=paste(d[3],'%',sep = ''),colour=mycolors[1],size=4)+
  annotate("text",x = 2+.200,y = 3.5,label=paste(d[4],'%',sep = ''),colour=mycolors[2],size=4)+
  annotate("text",x = 3-.200,y = 3.5,label=paste(d[5],'%',sep = ''),colour=mycolors[1],size=4)+
  annotate("text",x = 3+.200,y = 3.5,label=paste(d[6],'%',sep = ''),colour=mycolors[2],size=4)+
  annotate("text",x = 1,y = 3.75,label='Paired Selection:',colour='black')+
  annotate("text",x = 2,y = 3.75,label='Paired Selection:',colour='black')+
  annotate("text",x = 3,y = 3.75,label='Paired Selection:',colour='black')
print(figure2)  

#ggsave(filename = 'Figure2.pdf',figure2,width = 6,height = 4)
#ggsave(filename = 'Figure2.tiff',figure2,device='tiff',width = 6,height = 4)


