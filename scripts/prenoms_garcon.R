###############################################################################
## load libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)
library(RColorBrewer)
library(png)
#install.packages('showtext', dependencies = TRUE)
#library(showtext)

###############################################################################
## load data
nat=read.csv("donnees/nat2018_format.csv",header=T,sep=";")
names(nat)=c("sex","preusuel","annais","nombre")
###############################################################################
## clean data
nat[nat=="XXXX"] <- NA
natcl=na.omit(nat)
natcll=natcl[-c(which(natcl$preusuel == "_prenoms_rares")),]
## prepare data
#### select homme
homme=natcll[which(natcll$sex !=2),]
#### order by birth day
femOrd=homme[order(homme$annais),]
#### top of names for each year
topPreusuel=data.frame(zero=1:100)
topNombre=data.frame(zero=1:100)
for(year in unique(femOrd$annais)) {
	femYear=femOrd[which(femOrd$annais==year),]
	femYearOrd=femYear[order(femYear$nombre,decreasing=TRUE),]
	#print(femYearOrd[1:100,]$preusuel)
	topPreusuel=cbind(topPreusuel,femYearOrd[1:100,]$preusuel)
	topNombre=cbind(topNombre,femYearOrd[1:100,]$nombre)
}
topPreusuel=topPreusuel[,-1]
#names(topPreusuel)=1900:2018
topNombre=topNombre[,-1]
#names(topNombre)=1900:2018


###############################################################################
## format data for gganimation
#### number of top elements
toplimit=12
#### number of frame for each year
fillsize=80

currentYear=1900
currentframe=1
tops=data.frame(frame=rep(currentframe,toplimit),prenom=topPreusuel[1:toplimit,1],nombre=topNombre[1:toplimit,1], year=rep(currentYear,toplimit))
for(k in 1:(fillsize/2)) {
  currentframe=currentframe+1
  currentTop=data.frame(frame=rep(currentframe,toplimit),prenom=topPreusuel[1:toplimit,1],nombre=topNombre[1:toplimit,1], year=rep(currentYear,toplimit))
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
  	#print(pren)  	
    if(pren %in% topPreusuel[,i+1]) {
      nextNombre=topNombre[which(pren == topPreusuel[,i+1]),i+1][1]
      #print(nextNombre)
      #print(currentNombre)
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


###############################################################################
## visualization
nb.cols <- length(unique(tops_format$prenom))
baby_boy_palette_color=c("#cccccc","#e1330e","#ffa500","#30d0f2","#ffca2b","#6ca0dc","#935f23","#00ffcd","#9b7653","#34b334")
mycolors <- sample(colorRampPalette(baby_boy_palette_color)(nb.cols))

staticplot = ggplot(tops_format, aes(-rank, group = prenom, 
                                     fill = as.factor(prenom), color = as.factor(prenom))) +
  geom_tile(aes(y = nombre/2,height = nombre, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = nombre, label = paste(prenom, " ")),colour="gray10", hjust = 1,size=12) +
  geom_text(aes(y=nombre,label = Value_lbl, hjust=0), colour="gray10",size=12) +
  geom_text(aes(x=-7,y=Inf,label = "Les plus populaires", hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=24) +
  geom_text(aes(x=-8,y=Inf,label = "prénoms de garçons", hjust=0.5,vjust=0.5), colour="#6ca0dc",fontface="bold",size=24) +

  geom_text(aes(x=-11,y=Inf,label = year, hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=72) +
  geom_text(aes(x=-12.5,y=Inf,label ="De 1900 à 2018", hjust=0.5,vjust=0.5), colour="gray50",size=18) +
  
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


staticplot

  anim = staticplot +transition_manual(frame, cumulative=FALSE) +
    ease_aes('quadratic-in-out')+
    view_follow(fixed_x = TRUE)
 

 nombre_frames=as.integer(dim(tops_format)[1]/12)
  animate(anim, nframes=nombre_frames, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4
  anim_save("animation_boy.mp4", animation = for_mp4 )
