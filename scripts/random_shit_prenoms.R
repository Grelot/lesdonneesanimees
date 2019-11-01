###############################################################################
## load libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)

theme_set(theme_bw())
###############################################################################
## load data
nat=read.csv("donnees/nat2018.csv",header=T,sep=";")

###############################################################################
## clean data
nat[nat=="XXXX"] <- NA
natcl=na.omit(nat)
natcll=natcl[-c(which(natcl$preusuel == "_PRENOMS_RARES")),]

###############################################################################
## prepare data
#### select femme
femme=natcll[which(natcll$sex !=1),]
#### order by birth day
femOrd=femme[order(femme$annais),]
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


currentYear=1900
currentframe=1
tops=data.frame(frame=rep(currentframe,10),prenom=topPreusuel[1:10,1],nombre=topNombre[1:10,1], year=rep(currentYear,10))
for(i in 2:(dim(topNombre)[2]-1)) {
  currentYear=currentYear+1
  currentframe=currentframe+1
  currentTop=data.frame(frame=rep(currentframe,10),prenom=topPreusuel[1:10,i],nombre=topNombre[1:10,i], year=rep(currentYear,10))
  tops=rbind(tops,currentTop)
  ## fill tops
  fillSeqNombres=matrix(rep(0,120),nrow=10,ncol=12)
  j=1
  for(pren in currentTop$prenom) {
  	currentNombre=topNombre[j,i]
  	#print(pren)  	
    if(pren %in% topPreusuel[,i+1]) {
      nextNombre=topNombre[which(pren == topPreusuel[,i+1]),i+1]
      #print(nextNombre)
      #print(currentNombre)
      fillSeqNombres[j,]=seq(currentNombre,nextNombre,length.out=12)
    } else {
  	  print("ERRRRRRRRRRRRRR!!!!!!!!!!!!!")
  	  print(pren)
  	  fillSeqNombres[j,]=rep(currentNombre,12)
    }
    j=j+1
  }
  print(currentYear)
  for(k in 1:12) {
  	currentframe=currentframe+1
  	fillTop=data.frame(frame=rep(currentframe,10),prenom=topPreusuel[1:10,i],nombre=fillSeqNombres[,k], year=rep(currentYear,10))
  	tops=rbind(tops,fillTop)
  }
}
for(k in 1:12) {
	currentframe=currentframe+1
	currentTop=data.frame(frame=rep(currentframe,10),prenom=topPreusuel[1:10,dim(topNombre)[2]],nombre=topNombre[1:10,dim(topNombre)[2]], year=rep(currentYear+1,10))
	tops=rbind(tops, currentTop)
}


tops$nombre=as.integer(tops$nombre)
tops$nombre=as.numeric(tops$nombre)





tops_format <- tops[1:132,] %>%
  group_by(frame) %>%
  mutate(rank = rank(-nombre),
         Value_rel = nombre/nombre[rank==1],
         Value_lbl = paste0(" ",nombre)) %>%
  group_by(prenom) %>% 
  filter(rank <=10) %>%
  ungroup()


tops_format


###############################################################################
## visualization


  staticplot = ggplot(tops_format, aes(rank, frame= year, group = prenom, 
                fill = as.factor(prenom), color = as.factor(prenom))) +
  geom_tile(aes(y = nombre/2,
                height = nombre,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(prenom, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=nombre,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
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
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
       plot.margin = margin(2,2, 2, 4, "cm"))


  anim = staticplot + transition_states(frame, transition_length = 4, state_length = 1) +
   ease_aes('quadratic-in-out')+
  view_follow(fixed_x = TRUE)  +
  labs(title = "Le top des prénoms de filles en France\n {closest_state}",  
       subtitle  =  "De 1900 à 2018",
       caption  = "")

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))


