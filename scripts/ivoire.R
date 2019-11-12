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

###############################################################################
## functions

interpolate_df <- function(madf,kept_frames,fpy,nb_year) {
   filled_df <- madf[1,] %>% 
  keep_state(kept_frames) %>% 
  tween_state(madf[2,], ease = 'cubic-in-out', nframes = fpy*nb_year)  %>% 
  keep_state(kept_frames)
  return(filled_df)
}



###############################################################################
## load data
ivoire=read.csv("donnees/ivoire.csv",header=T,sep=",")

saisi=ivoire$nombre*ivoire$poids

ivoires=cbind(ivoire,saisi)


ivoires_all= NULL


for(pays in unique(ivoires$pays)) {
	current_p= ivoires[which(ivoire$pays ==pays),]
	interpol_curp=interpolate_df(current_p,80,260,6)
	ivoires_all=rbind(ivoires_all,interpol_curp)
}


ivoires_all$saisi=as.integer(ivoires_all$saisi)

ivoires_all$year=as.integer(ivoires_all$year)

toplimit=10
tops_format <- ivoires_all %>%
  group_by(.frame) %>%
  mutate(rank = rank(-saisi,ties.method="first"),
         Value_rel = saisi/saisi[rank==1],
         Value_lbl = paste0(" ",saisi)) %>%
  group_by(pays) %>% 
  filter(rank <=toplimit) %>%
  ungroup()
tops_format$iso2=as.character(tops_format$iso2)
###############################################################################
## visualization
nb.cols <- length(unique(tops_format$pays))
africa_palette_color=c("#c35b48","#e5c027","#458962","#fa993e")
mycolors <- sample(colorRampPalette(africa_palette_color)(nb.cols))


staticplot = ggplot(tops_format, aes(-rank, group = pays, country=iso2,
                                     fill = as.factor(pays), color = as.factor(pays))) +
  geom_tile(aes(y = saisi/2,height = saisi, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = saisi, label = paste(pays, "    ")),colour="gray10", hjust = 1,size=12) +
  geom_text(aes(y=saisi,label = paste("   ",Value_lbl), hjust=0), colour="gray10",size=12) +
  geom_flag(aes(y=saisi),size=36)+
  geom_text(aes(x=-7,y=Inf,label = "Quantité d'ivoire \nsaisi sur une année", hjust=0.5,vjust=0.5), colour="black",fontface="bold",size=24) +
  geom_text(aes(x=-8.5,y=Inf,label = "(en kilogrammes)", hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=18) +

  geom_text(aes(x=-10,y=Inf,label = year, hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=60) +
  
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_continuous(labels = scales::comma,position="bottom") +
  scale_fill_manual(values=mycolors) +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_blank(),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey50", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey50"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="grey50"),
        plot.background=element_blank(),
        plot.margin = margin(2,12, 2, 6, "cm"))


anim = staticplot +transition_manual(.frame) +
    ease_aes('quadratic-in-out')+
    view_follow(fixed_x = TRUE)
 


nombre_frames=as.integer(dim(tops_format)[1]/toplimit)
animate(anim, nframes=nombre_frames, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4

anim_save("animation.mp4", animation = for_mp4 )






