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


library(countrycode)

library(ggimage)


###############################################################################
## functions

interpolate_df <- function(madf,kept_frames,fpy,nb_year,current_year) {
  filled_df <- madf[1,] %>% 
    keep_state(kept_frames) %>% 
    tween_state(madf[2,], ease = 'quadratic-in-out', nframes = fpy*nb_year)  %>% 
    keep_state(kept_frames)
  filled_df$.frame=filled_df$.frame
  return(filled_df)
}



###############################################################################
## load data

demog=read.csv("donnees/recencement/populations_communes.csv",header=T,sep=";")
names(demog)=c("com","reg","dep","name","2015", "2010","1999","1990","1982","1975","1968" )

###############################################################################
## order data

demog$name=as.character(demog$name)

fl <- demog %>% pivot_longer(c("2015", "2010","1999","1990","1982","1975","1968"), names_to = "year", values_to = "nombre") 


fl$year=as.integer(fl$year)
flo <- fl %>% arrange(com,reg,dep,name,year)




###############################################################################
## les villes les plus peuples de FRANCE
ma_selection = unique(sort(flo[which(flo$nombre > 40000),]$name))


###############################################################################
## interpol data

flc <- flo %>% filter(name %in% ma_selection)

flc_all=NULL
fpy=40
kept_fram=1
cur_fram=0


nb_year=c(7,7,8,9,11,5)
iy=0

uniq_year=unique(flc$year)

for(y in 2:length(uniq_year)) {
  iy=iy+1
  current_y=flc[c(which(flc$year ==uniq_year[y-1]), which(flc$year ==uniq_year[y])),]
  flc_p=NULL
  for(ville in unique(flc$name)) {
    current_p= current_y[which(current_y$name == ville),]
    interpol_curp=interpolate_df(current_p,kept_fram,fpy,nb_year[iy],uniq_year[y-1])
    interpol_curp$.frame=interpol_curp$.frame+cur_fram
    flc_p=rbind(flc_p,interpol_curp)
  }
  flc_all=rbind(flc_all,flc_p)
  print(y)
  cur_fram=cur_fram+max(flc_p$.frame)
}

flc_all$year=as.integer(flc_all$year)
flc_all$nombre=as.integer(flc_all$nombre)


toplimit=11
tops_format <- flc_all %>%
  group_by(.frame) %>%
  mutate(rank = rank(-nombre,ties.method="first"),
  	pos=nombre) %>%
  group_by(name) %>% 
  filter(rank <=toplimit) %>%
  ungroup()


## visualization

ress=read.csv("ornament/villes_fr/ville_logo.csv",header=T,sep=";")



nb.cols <- length(unique(tops_format$name))

mycolors <- as.character(ress$couleur)
names(mycolors) = as.character(ress$name)

logos <- data.frame(logo=as.character(paste("ornament/villes_fr",as.character(ress$blason),sep="/")),
                    name = as.character(ress$name))


tops_format_pic=inner_join(tops_format,logos, by="name")




staticplot = ggplot(tops_format_pic, aes(-rank, group = name, 
                                     fill = as.factor(name))) +
  geom_tile(aes(y = pos/2,height = pos, width = 0.9, colour = as.factor(name)), alpha = 0.8, color = NA) +
  geom_image(aes(x=-rank,y=pos, image = logo), size=0.067)+

  geom_text(aes(y = pos, label = paste(name, "    ")),colour="gray10", hjust = 1,size=12) +
  geom_text(aes(y=pos,label = paste("    ",format(nombre,big.mark=",",scientific=FALSE)), hjust=0), colour="gray10",size=12) +
  geom_text(aes(x=-8,y=Inf,label = "Les plus grandes", hjust=0.5,vjust=0.5), colour="gray10",fontface="bold",size=20) +
  geom_text(aes(x=-9,y=Inf,label = "villes de France", hjust=0.5,vjust=0.5), colour="gray10",fontface="bold",size=18) +
  geom_text(aes(x=-10.5,y=Inf,label = year, hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=60) +
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
        plot.margin = margin(2,10, 2, 6, "cm"))




anim = staticplot +transition_manual(.frame) +
    ease_aes('quadratic-in-out')+
    view_follow(fixed_x = TRUE)


nombre_frames=as.integer(dim(tops_format)[1]/toplimit)
animate(anim, nframes=nombre_frames, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4


anim_save("grande_ville_france.mp4", animation = for_mp4 )

