###############################################################################
## data source
#https://data.worldbank.org/indicator/SP.DYN.LE00.FE.IN?view=chart


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
female_life=read.csv("donnees/female_life/format.csv",header=T,sep=";")

female_life$iso3=tolower(countrycode(female_life$iso3, origin = 'iso3c', destination = 'iso2c'))



fl <- female_life %>% gather(year, esperance, X1960:X2017) %>% arrange(country,year)
fl$year= as.integer(str_replace(fl$year,'X',''))





countries <- fl %>%
filter(year == 2014) %>%
mutate(rank=rank(-esperance,ties.method="first")) %>%
filter(rank <=30) %>% .$country

flc <- fl %>% filter(country %in% countries)



flc_all=NULL
fpy=40
kept_fram=1
cur_fram=0

for(y in unique(flc$year)[-(length(unique(flc$year)))]) {
  current_y=flc[c(which(flc$year ==y), which(flc$year ==(y+1))),]
  flc_p=NULL
  for(pays in unique(flc$country)) {
    current_p= current_y[which(current_y$country ==pays),]
    interpol_curp=interpolate_df(current_p,kept_fram,fpy,1,(y-1))   
    flc_p=rbind(flc_p,interpol_curp)
  }
  flc_all=rbind(flc_all,flc_p)
  print(y)
  cur_fram=cur_fram+2*kept_fram+fpy
}

flc_all$year=as.integer(flc_all$year)


toplimit=13
tops_format <- flc_all %>%
  group_by(.frame) %>%
  mutate(rank = rank(-esperance,ties.method="first"),
  	pos=esperance-70) %>%
  group_by(country) %>% 
  filter(rank <=toplimit) %>%
  ungroup()

tops_format$iso3=as.character(tops_format$iso3)

tops_format=tops_format[which(tops_format$.phase == "transition"),]


## visualization
nb.cols <- length(unique(tops_format$country))
africa_palette_color=c("#c35b48","#e5c027","#458962","#fa993e")
flag_palette_color=c("#005da4","#ed1c24","#ffdd00")
baby_girl_palette_color=c("#ff69b4","#fcfdcd","#d6fbe4","#d1d1f9","#cff1fb","#ffdbdb","#FE4365","#F9CDAD","#C8C8A9","#957DAD","#E0BBE4","#F47C7C","#F7F48B","#A1DE93","#70A1D7")

mycolors <- sample(colorRampPalette(baby_girl_palette_color)(nb.cols))



staticplot = ggplot(tops_format, aes(-rank, group = country, country=iso3,
                                     fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = pos/2,height = pos, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = pos, label = paste(country, "   ")),colour="gray10", hjust = 1,size=12) +
  geom_text(aes(y=pos,label = paste("  ",round(esperance,digits=2)), hjust=0), colour="gray10",size=12) +
  geom_text(aes(x=-10,y=Inf,label = "Esperance de vie", hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=24) +
  geom_text(aes(x=-11,y=Inf,label = "pour les femmes", hjust=0.5,vjust=0.5), colour="#ff1493",fontface="bold",size=18) +
  geom_flag(aes(y=pos),size=24)+
  geom_text(aes(x=-12.5,y=Inf,label = year, hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=60) +

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
        plot.margin = margin(2,14, 2, 2, "cm"))



anim = staticplot +transition_manual(.frame) +
    ease_aes('quadratic-in-out')+
    view_follow(fixed_x = TRUE)
 


nombre_frames=as.integer(dim(tops_format)[1]/toplimit)
animate(anim, nframes=nombre_frames, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4

anim_save("life_esperance_femme.mp4", animation = for_mp4 )
