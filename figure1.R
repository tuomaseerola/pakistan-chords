# figure1.R

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

S1 <- m %>%
  dplyr::group_by(Type, Mode, Background) %>%
  dplyr::summarise(n=n(),m=mean(Rating,na.rm = TRUE),sd=sd(Rating,na.rm = TRUE),.groups = ) %>%
  dplyr::mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se) 
S1

plsize <- 0.50 # errorbar line width
custom_theme_size <- theme_fs(18)
pd <- position_dodge(.6) # move them .05 to the left and right

min_y<-1
max_y<-5

ylev<-4.3

nb.cols <- 3
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(3, "Set2"))(nb.cols)
mycolors
fos<-2.3

g2 <- ggplot(S1,aes(x=Mode,y=m,fill=Background,shape=Type,colour=Background))+
  geom_point(position = pd,size=4,show.legend = TRUE)+
  geom_errorbar(S1, mapping=aes(x=Mode, ymin=LCI, ymax=UCI), width=0.2, size=plsize,position = pd,show.legend = FALSE) + 
  scale_y_continuous(breaks = seq(1,5,by=1),limits = c(min_y,max_y+0.1))+
  scale_color_brewer(palette='Set2')+
  scale_fill_brewer(palette='Set2')+
  scale_shape_manual(values = c(21,23))+
  ylab('Mean valence (±95%CI)')+
  xlab('Chord')+
  theme_bw()+
  custom_theme_size+
  theme(strip.background=element_rect(fill="white",colour="white"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(strip.text = element_text(size=20))+
  theme(legend.position="top")+
  annotate("segment",x=(1:4)-.25,xend = (1:4)-.15,y=ylev,yend=ylev,colour=mycolors[1])+
  annotate("segment",x=(1:4)-.25,xend = (1:4)-.25,y=ylev,yend=ylev-.075,colour=mycolors[1])+
  annotate("segment",x=(1:4)-.15,xend = (1:4)-.15,y=ylev,yend=ylev-.075,colour=mycolors[1])+
  annotate("segment",x=(1:4)-.05,xend = (1:4)+.05,y=ylev,yend=ylev,colour=mycolors[2])+
  annotate("segment",x=(1:4)-.05,xend = (1:4)-.05,y=ylev,yend=ylev-.075,colour=mycolors[2])+
  annotate("segment",x=(1:4)+.05,xend = (1:4)+.05,y=ylev,yend=ylev-.075,colour=mycolors[2])+
  annotate("segment",x=(1:4)+.25,xend = (1:4)+.15,y=ylev,yend=ylev,colour=mycolors[3])+
  annotate("segment",x=(1:4)+.25,xend = (1:4)+.25,y=ylev,yend=ylev-.075,colour=mycolors[3])+
  annotate("segment",x=(1:4)+.15,xend = (1:4)+.15,y=ylev,yend=ylev-.075,colour=mycolors[3])+
  
  annotate("segment",x=(1:4)-.20,xend = (1:4)+.00,y=ylev+.30,yend=ylev+.30,colour='gray40')+
  annotate("segment",x=(1:4)-.20,xend = (1:4)-.20,y=ylev+.30,yend=ylev+.30-.075,colour='gray40')+
  annotate("segment",x=(1:4)-.00,xend = (1:4)-.00,y=ylev+.30,yend=ylev+.30-.075,colour='gray40')+

  annotate("segment",x=(1:4)+.20,xend = (1:4)+.00,y=ylev+.50,yend=ylev+.50,colour='gray40')+
  annotate("segment",x=(1:4)+.20,xend = (1:4)+.20,y=ylev+.50,yend=ylev+.50-.075,colour='gray40')+
  annotate("segment",x=(1:4)-.00,xend = (1:4)-.00,y=ylev+.50,yend=ylev+.50-.075,colour='gray40')+

  annotate("segment",x=(1:4)-.20,xend = (1:4)+.20,y=ylev+.70,yend=ylev+.70,colour='gray40')+
  annotate("segment",x=(1:4)+.20,xend = (1:4)+.20,y=ylev+.70,yend=ylev+.70-.075,colour='gray40')+
  annotate("segment",x=(1:4)-.20,xend = (1:4)-.20,y=ylev+.70,yend=ylev+.70-.075,colour='gray40')+

  annotate("segment",x=1,xend = 2,y=ylev-3.0+.0,yend=ylev-3.0+.0,colour='gray40')+
  annotate("segment",x=1,xend = 1,y=ylev-3.0+.0,yend=ylev-3.0+.0+0.1,colour='gray40')+
  annotate("segment",x=2,xend = 2,y=ylev-3.0+.0,yend=ylev-3.0+.0+0.1,colour='gray40')+

  annotate("segment",x=1,xend = 3,y=ylev-3.0+.2,yend=ylev-3.0+.2,colour='gray40')+
  annotate("segment",x=1,xend = 1,y=ylev-3.0+.2,yend=ylev-3.0+.2+0.075,colour='gray40')+
  annotate("segment",x=3,xend = 3,y=ylev-3.0+.2,yend=ylev-3.0+.2+0.075,colour='gray40')+

  annotate("segment",x=1,xend = 4,y=ylev-3.0+.4,yend=ylev-3.0+.4,colour='gray40')+
  annotate("segment",x=1,xend = 1,y=ylev-3.0+.4,yend=ylev-3.0+.4+0.075,colour='gray40')+
  annotate("segment",x=4,xend = 4,y=ylev-3.0+.4,yend=ylev-3.0+.4+0.075,colour='gray40')+

  annotate("segment",x=2,xend = 3,y=ylev-3.0+.6,yend=ylev-3.0+.6,colour='gray40')+
  annotate("segment",x=2,xend = 2,y=ylev-3.0+.6,yend=ylev-3.0+.6+0.075,colour='gray40')+
  annotate("segment",x=3,xend = 3,y=ylev-3.0+.6,yend=ylev-3.0+.6+0.075,colour='gray40')+

  annotate("segment",x=3,xend = 4,y=ylev-3.0+.0,yend=ylev-3.0+.0,colour='gray40')+
  annotate("segment",x=3,xend = 3,y=ylev-3.0+.0,yend=ylev-3.0+.0+0.075,colour='gray40')+
  annotate("segment",x=4,xend = 4,y=ylev-3.0+.0,yend=ylev-3.0+.0+0.075,colour='gray40')+

  annotate("segment",x=2,xend = 4,y=ylev-3.0+.8,yend=ylev-3.0+.8,colour='gray40')+
  annotate("segment",x=2,xend = 2,y=ylev-3.0+.8,yend=ylev-3.0+.8+0.075,colour='gray40')+
  annotate("segment",x=4,xend = 4,y=ylev-3.0+.8,yend=ylev-3.0+.8+0.075,colour='gray40')+
  
  annotate("text",x=1.5,y=1.20+0.02,label='p=.0103',colour='black',size=fos)+
  annotate("text",x=2.5,y=1.82+0.02,label='p<.0001',colour='black',size=fos)+
  annotate("text",x=2.5,y=1.60+0.02,label='p<.0001',colour='black',size=fos)+
  annotate("text",x=3.5,y=1.20+0.02,label='p<.0001',colour='black',size=fos)+
  annotate("text",x=2.0,y=1.42+0.02,label='p=.0005',colour='black',size=fos)+
  annotate("text",x=3.0,y=2.02+0.02,label='p<.0001',colour='black',size=fos)+

  annotate("text",x=1.0,y=5.0+0.1,label='p=.0001',colour='black',size=fos)+
  annotate("text",x=1.0+0.1,y=5.0-0.1,label='p=.663',colour='black',size=fos)+
  annotate("text",x=1.0-0.1,y=5.0-0.3,label='p=.0001',colour='black',size=fos)+

  annotate("text",x=2.0,y=5.0+0.1,label='p=.940',colour='black',size=fos)+
  annotate("text",x=2.0+0.1,y=5.0-0.1,label='p=.984',colour='black',size=fos)+
  annotate("text",x=2.0-0.1,y=5.0-0.3,label='p=.852',colour='black',size=fos)+

  annotate("text",x=3.0,y=5.1,label='p=.262',colour='black',size=fos)+
  annotate("text",x=3.0+0.1,y=5.0-0.1,label='p=.987',colour='black',size=fos)+
  annotate("text",x=3.0-0.1,y=5.0-0.3,label='p=.180',colour='black',size=fos)+

  annotate("text",x=4.0,y=5.1,label='p=.551',colour='black',size=fos)+
  annotate("text",x=4.0+0.1,y=5.0-0.1,label='p=.910',colour='black',size=fos)+
  annotate("text",x=4.0-0.1,y=5.0-0.3,label='p=.272',colour='black',size=fos)+

  annotate("text",x=1.0+0.0,y=4.4,label='p=.043',colour='black',size=fos)+
  annotate("text",x=1.0-0.2,y=4.4,label='p=.064',colour='black',size=fos)+
  annotate("text",x=1.0+0.2,y=4.4,label='p=.038',colour='black',size=fos)+

  annotate("text",x=2.0+0.0,y=4.4,label='p=.122',colour='black',size=fos)+
  annotate("text",x=2.0-0.2,y=4.4,label='p=.007',colour='black',size=fos)+
  annotate("text",x=2.0+0.2,y=4.4,label='p=.087',colour='black',size=fos)+

  annotate("text",x=3.0+0.0,y=4.4,label='p=.043',colour='black',size=fos)+
  annotate("text",x=3.0-0.2,y=4.4,label='p<.0001',colour='black',size=fos)+
  annotate("text",x=3.0+0.2,y=4.4,label='p=.0003',colour='black',size=fos)+
  
  annotate("text",x=4.0+0.0,y=4.4,label='p=.122',colour='black',size=fos)+
  annotate("text",x=4.0-0.2,y=4.4,label='p<.0001',colour='black',size=fos)+
  annotate("text",x=4.0+0.2,y=4.4,label='p=.143',colour='black',size=fos)

print(g2)

#ggsave(filename = 'Figure1.pdf',g2,width = 9,height = 5)
#ggsave(filename = 'Figure1.tiff',g2,device='tiff',width = 9,height = 5)
