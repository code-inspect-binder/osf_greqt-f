library(scales)
source("functions1.R")
load("results.Rdata")

#Extract measures of interest to objects
p.compare<-sapply(list.res,function(x){x$p.compare})
DELTA<-sapply(list.res,function(x){x$DELTA})

Cohens.dAP<-sapply(list.res,function(x){x$Cohens.d$Cohens.dAP})
Cohens.dAB<-sapply(list.res,function(x){x$Cohens.d$Cohens.dAB})
Cohens.dPB<-sapply(list.res,function(x){x$Cohens.d$Cohens.dPB})

#Standard error divided by the respective mean (relative SE)
SE.AP<-sapply(list.res,function(x){sqrt(x$SE[1,]^2+x$SE[2,]^2)/colMeans(x$obs)})
SE.AB<-sapply(list.res,function(x){sqrt(x$SE[2,]^2+x$SEB^2)/colMeans(rbind(x$obs[2,],x$muB))})
SE.PB<-sapply(list.res,function(x){sqrt(x$SE[1,]^2+x$SEB^2)/colMeans(rbind(x$obs[1,],x$muB))})

char<-c(
  #Soc-dem characteristics
  "Residence_size",
  "Policy",
  "Education",
  "Wealth",
  
  #Physical characetristics
  "Attractiveness",
  "Masculinity",
  "Body_height",
  "Body_weight",
  "Eye_colour",
  "Hair_colour",
  "Facial_masculinity",
  "Muscularity",
  "BMI",
  "Beardedness",
  "Hirsuteness",
  
  #Personality
  "Extraversion",
  "Agreeableness",
  "Conscientiousness",
  "Emotional_stability",
  "Openness",
  "Dominance"
)


#rows defining categories
rctas<-list(1:4,5:15,16:21)

summar.res<-lapply(rctas,function(v){list(
  p.compare=inv_logit(rowMeans(logit(p.compare[,v]))),
  DELTA=rowMeans(DELTA[,v]),
  Cohens.d=list(
    Cohens.dAP=rowMeans(Cohens.dAP[,v]),
    Cohens.dAB=rowMeans(Cohens.dAB[,v]),
    Cohens.dPB=rowMeans(Cohens.dPB[,v])
  ))})

SE.AP<-cbind(SE.AP,sapply(rctas,function(v){rowMeans(SE.AP[,v])}))
SE.AB<-cbind(SE.AB,sapply(rctas,function(v){rowMeans(SE.AB[,v])}))
SE.PB<-cbind(SE.PB,sapply(rctas,function(v){rowMeans(SE.PB[,v])}))


list.res.all<-c(list.res,summar.res)
length(list.res.all)

char.all<-c(char,"Demographic (average)","Physical (average)","Personality (average)")

char.col<-c("#228822","#663388","#006688")
groupn<-c(4,11,6,3)

8/(6+4)

0.8*0.9

of1<-0.8
ofs<-c(rep(of1*c(0:3),groupn))
yd<-c(-0.3,0.3)
gap<-0.4

cols<-c(rep(char.col,groupn[1:3]),char.col)

ytop<-(length(ofs):1)*(yd[2]-yd[1]+gap)-ofs
ymid<-(length(ofs):1)*(yd[2]-yd[1]+gap)-(yd[2]-yd[1])/2-ofs
ybot<-(length(ofs):1)*(yd[2]-yd[1]+gap)-(yd[2]-yd[1])-ofs



percol <- colorRampPalette(c("#FFFFFF","#808080"))
perscale<-percol(1000) 
perseq<-seq(0,max(c(SE.AP,SE.AB,SE.PB)),l=1000)


bgcol1="#D0D0D0"

tiff("FigureS2_big.tif",width=16,height=16,units="cm",res=600,compression="lzw")

#Here starts the plotting

layout(matrix(1:3,nrow=1),widths=c(1.98,1,1))

#first plot with everything
par(mar=c(3.5,12,3.5,3.5),mgp=c(2,0.8,0))
plot(NULL,xlim=c(0,15),ylim=c(min(ybot)-gap/2,max(ytop)+gap/2),xaxs="i",yaxs="i",xlab="Age",ylab="",yaxt="n",bty="n")
rect(-1000,-1000,1000,1000,col=0)

text(18,par("usr")[3]+par("usr")[4]/2,expression(paste("Cohen's d, non-biological absent − present")),srt=270,xpd=T,font=2)

for(i in 1:length(list.res.all)){
  result<-list.res.all[[i]]
  
  percols<-perscale[sapply(SE.AP[,i],function(x){which.min(abs(x-perseq))})]
  rect(-1000,ytop[i]+gap/2,1000,ybot[i]-gap/2,col=0,border=NA)
  rect(0:14,ytop[i]+gap/2,1:15,ybot[i]-gap/2,col=percols,border=NA)
  
  axis(2,at=c(ymid[i]),labels = gsub("_"," ",char.all[i]),las=2,col=cols[i],col.ticks=NA,col.axis=cols[i],font.axis=2)
  
  axis(4,at=c(ybot[i]),labels = c(""),las=2,col.ticks =alpha(1,0.4),col=alpha(1,0.4))
  axis(4,at=c(ytop[i]),labels = c(""),las=2,col.ticks =alpha(1,0.4),col=alpha(1,0.4))
  
  points(16,ytop[i],pch=21,xpd=NA,cex=0.8,col="#008800",bg="#55FF55")
  points(16,ybot[i],pch=21,xpd=NA,cex=0.8,col="#888800",bg="#FFFF55")
  
  abline(h=c(ybot[i],ymid[i],ytop[i]),lty=c(1,2,1),col=alpha(c("#000000","#000000","#000000"),c(0.4,1,0.4)))
}

#Plot the delta curves on top of it all
for(i in 1:length(list.res.all)){
  lines(0:16-0.5,ad(list.res.all[[i]]$Cohens.d$Cohens.dAP+ymid[i]),col=cols[i])
}

top<-par("usr")[4]
ran<-par("usr")[4]-par("usr")[3]

rseq<-seq(par("usr")[1],par("usr")[2],l=101)
lseq<-seq(0,1,l=101)
rect(c(-2,15),top+ran*0.02,c(0,17),top+ran*0.05,col=percol(2),xpd=T,border=NA)
rect(rseq[1:100],top+ran*0.02,rseq[2:101],top+ran*0.05,xpd=T,col=alpha(percol(101),abs(lseq-0.5)*2),border=NA)
text(seq(0,15,l=5),top+ran*0.035,round(seq(0,max(c(SE.AP,SE.AB,SE.PB)),l=5),2),cex=0.8,xpd=T)
text(-1,top+ran*0.07,"Estimate uncertainty",pos=4,xpd=T,font=2)

rctas<-c(rctas,list(max(unlist(rctas))+c(1:3)))

rect(0,ytop[sapply(rctas,function(x){x[1]})]+gap/2,15,ybot[sapply(rctas,function(x){x[length(x)]})]-gap/2)




#Plot comparing Biological father with absent nonbiological
par(mar=c(3.5,0.2,3.5,3.5),mgp=c(2,0.8,0))

plot(NULL,xlim=c(0,15),ylim=c(min(ybot)-gap/2,max(ytop)+gap/2),xaxs="i",yaxs="i",xlab="Age",ylab="",yaxt="n",bty="n")
rect(-1000,-1000,1000,1000,col=0)

text(18,par("usr")[3]+par("usr")[4]/2,expression(paste("Cohen's d, non-biological absent − biological")),srt=270,xpd=T,font=2)

for(i in 1:length(list.res.all)){
  result<-list.res.all[[i]]
  
  percols<-perscale[sapply(SE.AB[,i],function(x){which.min(abs(x-perseq))})]
  rect(-1000,ytop[i]+gap/2,1000,ybot[i]-gap/2,col=0,border=NA)
  rect(0:14,ytop[i]+gap/2,1:15,ybot[i]-gap/2,col=percols,border=NA)
  
  axis(4,at=c(ybot[i]),labels = c(""),las=2,col.ticks =alpha(1,0.4),col=alpha(1,0.4))
  axis(4,at=c(ytop[i]),labels = c(""),las=2,col.ticks =alpha(1,0.4),col=alpha(1,0.4))
  
  points(16,ytop[i],pch=21,xpd=NA,cex=0.8,col="#008800",bg="#55FF55")
  points(16,ybot[i],pch=21,xpd=NA,cex=0.8,col="#888800",bg="#FFFF55")
  
  abline(h=c(ybot[i],ymid[i],ytop[i]),lty=c(1,2,1),col=alpha(c("#000000","#000000","#000000"),c(0.4,1,0.4)))
}

#Plot the delta curves on top of it all
for(i in 1:length(list.res.all)){
  lines(0:16-0.5,ad(list.res.all[[i]]$Cohens.d$Cohens.dAB+ymid[i]),col=cols[i])
}

rect(0,ytop[sapply(rctas,function(x){x[1]})]+gap/2,15,ybot[sapply(rctas,function(x){x[length(x)]})]-gap/2)


#Plot comparing Biological father with present nonbiological
par(mar=c(3.5,0.2,3.5,3.5),mgp=c(2,0.8,0))

plot(NULL,xlim=c(0,15),ylim=c(min(ybot)-gap/2,max(ytop)+gap/2),xaxs="i",yaxs="i",xlab="Age",ylab="",yaxt="n",bty="n")
rect(-1000,-1000,1000,1000,col=0)

text(18,par("usr")[3]+par("usr")[4]/2,expression(paste("Cohen's d, non-biological present − biological")),srt=270,xpd=T,font=2)

for(i in 1:length(list.res.all)){
  result<-list.res.all[[i]]
  
  percols<-perscale[sapply(SE.PB[,i],function(x){which.min(abs(x-perseq))})]
  rect(-1000,ytop[i]+gap/2,1000,ybot[i]-gap/2,col=0,border=NA)
  rect(0:14,ytop[i]+gap/2,1:15,ybot[i]-gap/2,col=percols,border=NA)
  
  axis(4,at=c(ybot[i]),labels = c(""),las=2,col.ticks =alpha(1,0.4),col=alpha(1,0.4))
  axis(4,at=c(ytop[i]),labels = c(""),las=2,col.ticks =alpha(1,0.4),col=alpha(1,0.4))
  
  points(16,ytop[i],pch=21,xpd=NA,cex=0.8,col="#008800",bg="#55FF55")
  points(16,ybot[i],pch=21,xpd=NA,cex=0.8,col="#888800",bg="#FFFF55")
  
  abline(h=c(ybot[i],ymid[i],ytop[i]),lty=c(1,2,1),col=alpha(c("#000000","#000000","#000000"),c(0.4,1,0.4)))
}

#Plot the delta curves on top of it all
for(i in 1:length(list.res.all)){
  lines(0:16-0.5,ad(list.res.all[[i]]$Cohens.d$Cohens.dPB+ymid[i]),col=cols[i])
}

rect(0,ytop[sapply(rctas,function(x){x[1]})]+gap/2,15,ybot[sapply(rctas,function(x){x[length(x)]})]-gap/2)

legend("bottomright",c("+0.3","−0.3"),pch=21,col=c("#008800","#888800"),pt.bg=c("#55FF55","#FFFF55"),cex=1,xpd=NA,inset=c(-0.40,-0.08),bty="n")


dev.off()






