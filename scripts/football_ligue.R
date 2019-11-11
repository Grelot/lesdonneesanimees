###############################################################################
## load libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)
library(RColorBrewer)
library(png)
library(extrafont)

library(ggimage)
library(rsvg)

ligue=read.csv("donnees/football_stats.csv",header=T,sep=";")

###############################################################################
## clean data

ldf=data.frame(team=ligue$Team,
               date=ligue$Date,
               nombre=ligue$Team.G)

all_date=unique(sort(ldf$date))
all_team=unique(sort(ldf$team))

all_nb=matrix(rep(0,length(all_date)*length(all_team)),nrow=length(all_date),ncol=length(all_team))

all_date=all_date[order(as.Date(all_date,format="%m/%d/%Y"))]


for(i in 1:dim(ldf)[1]) {
  current_team=ldf[i,]$team
  current_date=ldf[i,]$date
  id_team=which(all_team == current_team)
  id_date=which(all_date == current_date)
  all_nb[id_date,id_team] = as.integer(ldf[i,]$nombre)
  
}

nbdf=as.data.frame(all_nb)
ldf= data.frame(team=c(),date=c(),nombre=c())
for(j in 1:dim(nbdf)[2]) {
  i=dim(nbdf)[1]
  current_ldf=data.frame(team=rep(all_team[j] ,i),date=all_date ,nombre=nbdf[,j])
  ldf=rbind(ldf, current_ldf)
}


lord <- ldf %>%
  group_by(team,date) %>% 
  mutate(nombre = sum(nombre)) %>%
  filter(row_number(team) == 1) %>% ungroup()

lords <- lord %>%
  group_by(team) %>%
  mutate(cumul = cumsum(nombre))

toplimit=16

lordd=lords[order(as.Date(lords$date,format="%m/%d/%Y")),]


tops_format <- lordd %>%
  group_by(date) %>%
  mutate(rank = rank(-cumul,ties.method="first"),
         Value_rel = cumul/cumul[rank==1],
         Value_lbl = paste0(" ",cumul)) %>%
  group_by(team) %>% 
  filter(rank <=toplimit) %>%
  ungroup()






###############################################################################
## visualization
ress=read.csv("ornament/club.csv",header=T,sep=",")



nb.cols <- length(unique(tops_format$team))

mycolors <- as.character(ress$colour)
names(mycolors) = as.character(ress$team)

myfontcolors <- as.character(ress$textcolour)
names(myfontcolors) = as.character(ress$team)

logos <- data.frame(logo=as.character(paste("ornament/fc",as.character(ress$logo),sep="/")),
                    team = as.character(ress$team))


tops_format_pic=inner_join(tops_format,logos, by="team")

fr_date= strptime(as.character(tops_format_pic$date), "%m/%d/%Y")
tops_format_pic$date = format(fr_date, "%d/%m/%Y")
frame=rep(1:length(unique(tops_format_pic$date)), each=toplimit)

tops_format_pic=cbind(tops_format_pic,frame)


## add palmares

year=format(as.Date(tops_format_pic$date, format="%d/%m/%Y"),"%Y")
yeardf=data.frame(year=as.integer(year))

palmares=read.csv("donnees/palmares_ligue1.csv",header=T,sep=",")

championdf=inner_join(yeardf,palmares,by="year")

champion_pic= inner_join(championdf, logos, by="team")
names(champion_pic)=c("year","champion","logo_champion")

tops_format_picc = cbind(tops_format_pic,champion_pic)

###############################################################################
## suppframe
tops_format_all=tops_format_pic[1:toplimit,]
myframe=1
for(i in 1:length(unique(tops_format_pic$frame))) {
  current_row=tops_format_pic[which(tops_format_pic$frame==i),]
  current_row=current_row[order(current_row$rank),]
  print(i)
  current_all=current_row
  current_all$frame = rep(myframe,toplimit)
  for(j in 1:4) {
    current_row$frame = rep(myframe,toplimit)
    
    current_all=rbind(current_all,current_row)
    myframe=myframe+1
  }
  tops_format_all=rbind(tops_format_all,current_all)
 
}
tops_format_all=tops_format_all[-c(1:16),]

###############################################################################
## plot

staticplot = ggplot(tops_format_picc, aes(-rank, group = team, 
                                         fill = as.factor(team)), colour = as.factor(team)) +
  geom_tile(aes(y = cumul/2,height = cumul, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = cumul, label = paste(team,"    "), colour=as.factor(team)),hjust = 1,size=12) +
  scale_colour_manual(values=myfontcolors )+
  geom_image(aes(x=-rank,y=cumul, image = logo), size=0.05)+
  geom_text(aes(y=cumul,label = paste("    ",Value_lbl), hjust=0), colour="gray10",size=12) +

  geom_image(aes(x=-10.5,y=Inf, image = logo_champion, hjust=0.5,vjust=0.5), size=0.1)+
  
  geom_text(aes(x=-12.5,y=Inf,label = "Buts marqués", hjust=0.5,vjust=0.5), colour="gray10",fontface="bold",size=18) +
  geom_text(aes(x=-13.5,y=Inf,label = "en Ligue 1", hjust=0.5,vjust=0.5), colour="gray10",fontface="bold",size=18) +
  geom_text(aes(x=-15,y=Inf,label = date, hjust=0.5,vjust=0.5), colour="gray50",fontface="bold",size=36) +
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
        plot.margin = margin(2,11, 2, 4, "cm"))



anim = staticplot +transition_states(frame, transition_length = 4, state_length = 1) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('cubic-in-out')



nombre_frames=as.integer(dim(tops_format_picc)[1]/toplimit)*2
animate(anim, nframes=nombre_frames, fps = 40,  width = 1920, height = 1080,renderer = ffmpeg_renderer()) -> for_mp4




  anim_save("animation_foot_goals.mp4", animation = for_mp4 )
  
