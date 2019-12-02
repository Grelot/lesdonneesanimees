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
    tween_state(madf[2,], ease = 'cubic-in-out', nframes = fpy*nb_year)  %>% 
    keep_state(kept_frames)
  filled_df$.frame=filled_df$.frame+current_year*fpy*nb_year+2*kept_frames*current_year
  return(filled_df)
}


###############################################################################
## load data
telecom=read.csv("donnees/fournisseur_telecom.csv",header=T,sep=",")
telecom$logo=as.character(paste("ornament/internet",as.character(telecom$logo),sep="/" ))


telecom_all= NULL

uniq_year=unique(sort(telecom$year))
uniq_year <- uniq_year[-length(uniq_year)]

for(y in 1:length(uniq_year)) {
  uniq_year[y]
  current_year=telecom[c(which(telecom$year ==uniq_year[y]), which(telecom$year ==(uniq_year[y]+1))),]
  for(com in unique(sort(current_year$fournisseur))) {
    current_p= current_year[which(current_year$fournisseur ==com),]
    print(current_p)
    interpol_curp=interpolate_df(current_p,5,280,1,(y-1))
    telecom_all=rbind(telecom_all,interpol_curp)
  }
}


telecom_all$abonnes=as.integer(telecom_all$abonnes)

telecom_all$year=as.integer(telecom_all$year)


###############################################################################
## visualization


staticplot <- ggplot(  telecom_all,
                       aes(.frame, abonnes, group = fournisseur )) +
  geom_line( size=1.4) +
  geom_image(aes(x=.frame,y=abonnes, image = logo), size=0.05)+
  geom_text(aes(y=abonnes,label = paste("    ",as.character(format(abonnes,big.mark=",",scientific = FALSE))), hjust=0), colour="gray10",size=11) +
  scale_x_continuous(breaks=as.integer(seq(1,length(unique(sort(telecom_all$.frame))),length.out = length(unique(sort(telecom_all$year))))),
                   labels=unique(sort(telecom_all$year)))+
  coord_cartesian(clip="off")+
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
  ease_aes('quadratic-in-out')+
  view_follow(fixed_x = TRUE)



nombre_frames=as.integer((280+2*5)*7)
animate(anim, nframes=nombre_frames, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4



anim_save("telecom_anim.mp4", animation = for_mp4 )

