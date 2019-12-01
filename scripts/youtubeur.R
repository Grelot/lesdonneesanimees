## prepare data
## c'est bien galere !!
## d'abord il faut aller sur
##http://web.archive.org/web/20131220072434/http://socialblade.com/youtube/top/country/FR/mostsubscribed
## puis copier coller a la main chaque tableau de chaque annee
## pour chaque annee txt raw file
test=read.table("donneesyoutubeur.txt",header=F)
tt=matrix(test$V1,ncol=6,byrow=T)