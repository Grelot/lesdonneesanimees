###############################################################################
## load libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)
library(RColorBrewer)
library(png)
library(extrafont)

demog=read.csv("donnees/recencement/populations_communes.csv",header=T,sep=";")
names(demog)=c("com","reg","dep","name","y15", "y10","y99","y90","y82","y75","y68" )

###############################################################################
## clean data
annees=c(2015,2010,1999,1990,1982,1975,1968)

demog_df=data.frame(commune=demog$name,dpt=demog$dep,reg=demog$reg, nombre=rep(0,length(demog$reg)), year=rep(0,length(demog$reg)))


demog_dff=NULL
for(i in 1:dim(demog)[1]) {
	ligne=demog[i,]
	current_ligne=NULL
	for(y in 1:7) {
	  current_ref_year=annees[y]
	  current_y=data.frame(commune=as.character(ligne$name),dpt=as.numeric(ligne$dep),reg=as.numeric(ligne$reg),nombre=as.numeric(ligne[4+y]),year=current_ref_year)	  
	  #print(current_y)
	  current_ligne=rbind(current_ligne,current_y)	  
	}
	print(ligne$name)
	demog_dff=rbind(demog_dff, current_ligne)
}


write.table(demog_dff,"donnees/recencement/pop_com_year.csv",col.names=T,row.names=F,sep=";")