###############################################################################
#https://www.tns-sofres.com/dataviz?type=1&code_nom=macron
###############################################################################
## load libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)
library(RColorBrewer)
library(png)
library(extrafont)
library(tweenr)
library(ggflags)

library(ggimage)


###############################################################################
## functions

interpolate_df <- function(madf,kept_frames,fpy,nb_year,current_year) {
  filled_df <- madf[1,] %>% 
    keep_state(kept_frames) %>% 
    tween_state(madf[2,], ease = 'linear', nframes = fpy*nb_year)  %>% 
    keep_state(kept_frames)
  filled_df$.frame=filled_df$.frame+current_year*fpy*nb_year+2*kept_frames*current_year
  return(filled_df)
}


###############################################################################
## load data
macron_csv=read.csv("donnees/popularite_macron.csv",header=T,sep=",")

macron <- macron_csv %>% pivot_longer(c("confiance","defiance"), names_to = "opinion", values_to = "valeur")




### logo
logo_sad=c("ornament/macron_sad.png","red","defiance")
logo_hap=c("ornament/macron_happy.png","blue","confiance")
logo_all=rbind(logo_sad,logo_hap)
logo_all=as.data.frame(logo_all)
names(logo_all)=c("logo","couleur","opinion")
macron=merge(macron, logo_all, by="opinion")

## order
macron = macron[order(macron$temps),]

macron_all= NULL

uniq_year=unique(sort(macron$temps))
uniq_year <- uniq_year[-length(uniq_year)]

for(y in 1:length(uniq_year)) {
  uniq_year[y]
  current_year=macron[c(which(macron$temps ==uniq_year[y]), which(macron$temps ==(uniq_year[y+1]))),]
  for(com in unique(sort(current_year$opinion))) {
    current_p= current_year[which(current_year$opinion ==com),]
    print(current_p)
    interpol_curp=interpolate_df(current_p,1,80,1,(y-1))
    macron_all=rbind(macron_all,interpol_curp)
  }
}

macron_all$temps=as.integer(macron_all$temps)


macron_all=macron_all[which(macron_all$.phase == "transition"),]

macron_all$valeur=macron_all$valeur+0.01

staticplot <- ggplot(  macron_all,
                       aes(.frame, valeur,group=opinion )) +
  geom_line(aes(colour=as.factor(opinion)),size=2) +
  geom_image(aes(x=.frame,y=valeur, image = logo), size=0.1)+
  geom_text(aes(y=valeur,label = paste("    ",as.character(round(valeur,digits=2))), hjust=0), colour="gray10",size=12) +
  ylim(0,100)+
  scale_x_continuous(breaks=seq(from=1,to=length(sort(unique(macron_all$.frame))),length.out=6), labels=c("Juil'17","Jan'18","Juil'18","Janv'19","Juil'19", "Dec'19"))+
  scale_color_manual(values=c("blue","red"))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        legend.position="none",
        panel.grid.major.x = element_line( size=0.5, color="grey" ),
        axis.text.x = element_text( color="gray10", size=24),
        plot.margin = margin(2,7, 2,2, "cm")
  )


anim = staticplot +transition_reveal(.frame) +
  view_follow(fixed_x = TRUE,fixed_y=TRUE)



nombre_frames=as.integer(80*length(unique(sort(macron_all$dates))))
animate(anim, nframes=nombre_frames, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4


anim_save("macron.mp4", animation = for_mp4 )

