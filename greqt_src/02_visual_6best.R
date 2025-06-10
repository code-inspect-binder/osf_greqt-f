#Load all the required functions
library(scales)
source("functions1.R")
source("functions2.R")
source("functions_plot.R")

#Visualize 6 most interesting complete viualizations for the most interesting characteristics
load("results.Rdata")


tiff("Figure1.tif",width=17,height=18.5,units="cm",res=300,compression="lzw")

mr<-6
ml<-mr

#Draw basic plots for all characteristics
layout(matrix(c(1:12),ncol=2),heights=c(5,3,5,3,5,4))

gap<-0.2

plotFull(list.res$Attractiveness,tifit=F,layit=F,legends=c(F,F,F),xlab="",mar1=c(0,ml,2+gap,1+gap),mar2=c(1+gap,ml,0,1+gap),ylab1="Average difference\nbetween\nfather and partner",ylab2="Year-on-year\nchange in\n(absent - presnet)")
plotFull(list.res$Body_weight,tifit=F,layit=F,legends=c(F,F,F),xlab="",mar1=c(0,ml,2+gap,1+gap),mar2=c(1+gap,ml,0,1+gap),ylab1="Average difference\nbetween\nfather and partner",ylab2="Year-on-year\nchange in\n(absent - presnet)")
plotFull(list.res$Extraversion,tifit=F,layit=F,legends=c(F,F,F),mar1=c(0,ml,2+gap,1+gap),mar2=c(3,ml,0,1+gap),ylab1="Average difference\nbetween\nfather and partner",ylab2="Year-on-year\nchange in\n(absent - presnet)")

#Second column with legends
plotFull(list.res$Masculinity,tifit=F,layit=F,legends=c(F,F,T),xlab="",ylab1="",ylab2="",mar1=c(0,1+gap,2+gap,mr),mar2=c(1+gap,1+gap,0,mr))

colP="#008888"
colA="#FFAA00"
colB="#00AA00"

legend("topright",c("Non-biol.\npresent\n","Non-biol.\nabsent\n","Biological\npresent\n"),col=c(colP,colA,colB),pch=16,box.col=NA,cex=0.9,inset=c(-0.27,-1.8),xpd=NA,bg=NA)


plotFull(list.res$Eye_colour,tifit=F,layit=F,legends=c(F,F,T),xlab="",ylab1="",ylab2="",mar1=c(0,1+gap,2+gap,mr),mar2=c(1+gap,1+gap,0,mr))

percol <- colorRampPalette(c("#888888","#FFFFFF","#FF1188"))
rseq<-seq(par("usr")[4]+(par("usr")[4]-par("usr")[3])*0.15,par("usr")[4]+(par("usr")[4]-par("usr")[3])*1.65,l=101)
lseq<-seq(0,1,l=101)
rect(15.5,rseq[1:100],17,rseq[2:101],xpd=T,col=alpha(percol(101),abs(lseq-0.5)*2),border=NA,xpd=NA)
text(rep(16.25,3),seq(par("usr")[4]+(par("usr")[4]-par("usr")[3])*0.15,par("usr")[4]+(par("usr")[4]-par("usr")[3])*1.65,l=15)[c(2,8,14)],c(0,0.5,1),cex=0.8,xpd=NA)


plotFull(list.res$Openness,tifit=F,layit=F,legends=c(F,F,T),ylab1="",ylab2="",mar1=c(0,1+gap,2+gap,mr),mar2=c(3,1+gap,0,mr))

dev.off()
