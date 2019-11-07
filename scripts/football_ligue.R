###############################################################################
## load libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)
library(RColorBrewer)
library(png)
library(extrafont)

ligue=read.csv("donnees/football_stats.csv",header=T,sep=";")



###############################################################################
## clean data

ldf=data.frame(team=ligue$Team,
              year=ligue$Season,
              nombre=ligue$Team.G)

lord <- ldf %>%
  group_by(team,year) %>% 
  mutate(nombre = sum(nombre)) %>%
  filter(row_number(team) == 1) %>% ungroup()


lords <- lord %>%
  group_by(team) %>%
  mutate(cumul = cumsum(nombre))

topTeam=data.frame(zero=1:20)
topCumul=data.frame(zero=1:20)
for(year in unique(lords$year)) {
  tYear=lords[which(lords$year==year),]
  tYearOrd=tYear[order(tYear$cumul,decreasing=TRUE),]
  topTeam=cbind(topTeam,tYearOrd[1:20,]$team)
  topCumul=cbind(topCumul,tYearOrd[1:20,]$cumul)
}
topTeam=topTeam[,-1]
#names(topPreusuel)=1900:2018
topCumul=topCumul[,-1]

###############################################################################
topPreusuel=topTeam
topNombre=topCumul



toplimit=16
#### number of frame for each year
fillsize=80

currentYear=1993
currentframe=1
tops=data.frame(frame=rep(currentframe,toplimit),prenom=topPreusuel[1:toplimit,1],
                nombre=topNombre[1:toplimit,1], year=rep(currentYear,toplimit))
for(k in 1:(fillsize/2)) {
  currentframe=currentframe+1
  currentTop=data.frame(frame=rep(currentframe,toplimit),prenom=topPreusuel[1:toplimit,1],
                        nombre=topNombre[1:toplimit,1], year=rep(currentYear,toplimit))
  tops=rbind(tops, currentTop)
}
for(i in 2:(dim(topNombre)[2]-1)) {
  currentYear=currentYear+1
  currentframe=currentframe+1
  currentTop=data.frame(frame=rep(currentframe,toplimit),prenom=topPreusuel[1:toplimit,i],nombre=topNombre[1:toplimit,i], year=rep(currentYear,toplimit))
  tops=rbind(tops,currentTop)
  ## fill tops
  fillSeqNombres=matrix(rep(0,toplimit*fillsize),nrow=toplimit,ncol=fillsize)
  j=1
  for(pren in currentTop$prenom) {
    currentNombre=topNombre[j,i]
    print(pren)  	
    if(pren %in% topPreusuel[,i+1]) {
      nextNombre=topNombre[which(pren == topPreusuel[,i+1]),i+1][1]
      print(nextNombre)
      print(currentNombre)
      fillSeqNombres[j,]=seq(currentNombre,nextNombre,length.out=fillsize)
    } else {
      print("ERRRRRRRRRRRRRR!!!!!!!!!!!!!")
      print(pren)
      fillSeqNombres[j,]=rep(currentNombre,fillsize)
    }
    j=j+1
  }
  print(currentYear)
  for(k in 1:fillsize) {
    currentframe=currentframe+1
    fillTop=data.frame(frame=rep(currentframe,toplimit),prenom=topPreusuel[1:toplimit,i],nombre=fillSeqNombres[,k], year=rep(currentYear,toplimit))
    tops=rbind(tops,fillTop)
  }
}
for(k in 1:fillsize) {
  currentframe=currentframe+1
  currentTop=data.frame(frame=rep(currentframe,toplimit),prenom=fillTop$prenom,nombre=fillTop$nombre, year=fillTop$year)
  tops=rbind(tops, currentTop)
}


tops$nombre=as.integer(tops$nombre)
tops$nombre=as.numeric(tops$nombre)



tops_format <- tops %>%
  group_by(frame) %>%
  mutate(rank = rank(-nombre,ties.method="random"),
         Value_rel = nombre/nombre[rank==1],
         Value_lbl = paste0(" ",nombre)) %>%
  group_by(prenom) %>% 
  filter(rank <=toplimit) %>%
  ungroup()


tops_format

unique(sort(tops_format$prenom)


###############################################################################
## visualization


ress=read.csv("ornament/club.csv",header=T,sep=",")



nb.cols <- length(unique(tops_format$prenom))
mycolors <- ress$colour
names(mycolors) = ress$team

staticplot = ggplot(tops_format, aes(-rank, group = prenom, 
                                     fill = as.factor(prenom), color = as.factor(prenom))) +
  geom_tile(aes(y = nombre/2,height = nombre, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = nombre, label = paste(prenom, " "),colour=prenom), hjust = 1,size=12) +
  scale_colour_manual(values=ress$textcolour )+
  geom_text(aes(y=nombre,label = Value_lbl, hjust=0), colour="gray10",size=12) +
  geom_text(aes(x=-11,y=Inf,label = "Nombre buts cumulés", hjust=0.5,vjust=0.5), colour="gray10",fontface="bold",size=24) +

  geom_text(aes(x=-12,y=Inf,label = "par club en Ligue 1", hjust=0.5,vjust=0.5), colour="gray10",fontface="bold",size=24) +
  
  geom_text(aes(x=-15,y=Inf,label = year, hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=72) +
  geom_text(aes(x=-15.5,y=Inf,label ="", hjust=0.5,vjust=0.5), colour="gray50",size=18) +
  
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
        plot.margin = margin(2,14, 2, 4, "cm"))


anim = staticplot +transition_manual(frame) +
  ease_aes('quadratic-in-out')+
  view_follow(fixed_x = TRUE)


nombre_frames=as.integer(dim(tops_format)[1]/12)
animate(anim, nframes=100, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4
anim_save("animation_foot_goals.mp4", animation = for_mp4 )







