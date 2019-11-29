#https://static1.squarespace.com/static/5ae60dae4611a0353c4f5e8e/t/5b28e4b203ce64bc17771674/1529406658936/20141209_EPI-RF+Report+on+Demand+Side+Initiatives+FINAL.compressed.pdf

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
carbone=read.csv("donnees/co2_nations_format.csv",header=T,sep=",")
names(carbone)=c("nation","year","fuel","solid","liquid","gas","cement","flaring","capita","bunker")
carbone$fuel=carbone$fuel*1000


first_year=min(carbone$year)


countries <- carbone %>%
filter(year == 2014) %>%
mutate(rank=rank(-fuel,ties.method="first")) %>%
filter(rank <=40) %>% .$nation

carbonec <- carbone %>% filter(nation %in% countries)



carbo = NULL
for(y in seq(1800,2014)) {
  carboy <- carbonec %>% filter(year ==y) %>% select(nation, year, fuel)
  for(co in countries) {
    carboyco <- carboy %>% filter(nation ==co)
    if(dim(carboyco)[1] == 0) {   
      carboyco=data.frame(nation=co, year=y, fuel=0)
    }
    carbo = rbind(carbo,carboyco)
  }
}





toplimit=12
tops_format <- carbone_all %>%
  group_by(.frame) %>%
  mutate(rank = rank(-fuel,ties.method="first"),
         Value_rel = fuel/fuel[rank==1],
         Value_lbl = paste0(" ",fuel)) %>%
  group_by(nation) %>% 
  filter(rank <=toplimit) %>%
  ungroup()

###############################################################################
## visualization
nb.cols <- length(unique(tops_format$nation))
africa_palette_color=c("#c35b48","#e5c027","#458962","#fa993e")
mycolors <- sample(colorRampPalette(africa_palette_color)(nb.cols))



staticplot = ggplot(tops_format, aes(-rank, group = nation,
                                     fill = as.factor(nation), color = as.factor(nation))) +
  geom_tile(aes(y = fuel/2,height = fuel, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = fuel, label = paste(nation, "    ")),colour="gray10", hjust = 1,size=12) +
  geom_text(aes(y=fuel,label = paste("   ",Value_lbl), hjust=0), colour="gray10",size=12) +
  #geom_flag(aes(y=fuel),size=36)+
  geom_text(aes(x=-7,y=Inf,label = "rejet co2", hjust=0.5,vjust=0.5), colour="black",fontface="bold",size=24) +
  geom_text(aes(x=-8.5,y=Inf,label = "(en tonnes CO²)", hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=18) +

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
animate(anim, nframes=1000, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4
anim_save("animation.mp4", animation = for_mp4 )
