#Load all the required functions
library(scales)
source("functions1.R")
source("functions2.R")
source("functions_plot.R")

load("results.Rdata")


colP="#008888"
colA="#FFAA00"

colB="#00AA00"
colCH="#FF1188"
colgrid="#CCCCCC"
bgcol1="#F0F0F0"
boxcol="#888888"
h0col1="#1111CC"
h0col2="#FFFFFF"
multip=1.0
mright<-7
mar1=c(0,5,2,mright)
mgp1=c(1.6,0.5,0)
ylab1="Average difference\nbetween\nfather and partner"

mar2=c(0,5,0,mright)
mgp2=c(1.6,0.5,0)

xlab="Age"
ylab2="Year-on-year\nchange in\n(absent - presnet)"

result<-list.res$Body_height




polycol <- colorRampPalette(c(h0col1,h0col2))
corcol<-polycol(5) 

# Caculate the compatibility intervals of H0 (no abrubt year-on-year changes that would point to sensitive period)
# 66, 80, 90 and 95% H0 compatibility intervals are calulated
shade95<-apply(result$rand.diff,2,quantile,probs=c(0.025,0.975))
shade90<-apply(result$rand.diff,2,quantile,probs=c(0.05,0.95))
shade80<-apply(result$rand.diff,2,quantile,probs=c(0.1,0.9))
shade66<-apply(result$rand.diff,2,quantile,probs=c(1/3,2/3))


# Calculating the x and y limits and ranges for the plot
linesA<-c(result$obs[1,]-result$SE[1,]*multip,rev(result$obs[1,]+result$SE[1,]*multip),result$obs[2,]-result$SE[2,]*multip,rev(result$obs[2,]+result$SE[2,]*multip))
rangeA<-max(linesA)-min(linesA)
limA<-c(min(linesA)-rangeA*0.1,max(linesA)+rangeA*0.2)

linesB<-c(shade95,result$changesO)
rangeB<-signif(max(linesB),1)-signif(min(linesB),1)
limB<-c(signif(min(linesB),1),signif(max(linesB),1))


#Start the actual plotting

tiff("FigureS4.tif",width=10*1.2*2,height=16*1.2,units="cm",res=300,compression="lzw")


#Set the layout
layout(matrix(c(1:10),ncol=2),heights=c(5,3,10,5,2.5),widths=c(1.1,1))

par(mar=mar1,mgp=mgp1)

plot(NULL,type="n",xlim=c(0,15),ylim=rev(limA),ylab=ylab1,xaxt="n",yaxt="n",xaxs="i",fg=boxcol)

title("Body height (empirical)",adj=0)
axis(2,at=signif(seq(min(linesA),max(linesA),l=5),1),fg=boxcol)

# Background
rect(-1000,-1000,1000,1000,col=bgcol1)

# Likelihood of sensitive period

percol <- colorRampPalette(c("#888888","#FFFFFF","#FF1188"))
perscale<-percol(1000) 
perseq<-seq(0.001,0.999,l=1000)

percols<-perscale[sapply(result$p.compare,function(x){which.min(abs(x-perseq))})]


# SE compatibility corridors from calculated means and bootsrapped standard errors (with different multip value these might be changed to for example 95% compatibility intervals)
# First overlay the background with white corridors
polygon(c(0:16,rev(0:16))-0.5,c(ad(result$obs[1,]-result$SE[1,]*multip),rev(ad(result$obs[1,]+result$SE[1,]*multip))),col="white",border=NA)
polygon(c(0:16,rev(0:16))-0.5,c(ad(result$obs[2,]-result$SE[2,]*multip),rev(ad(result$obs[2,]+result$SE[2,]*multip))),col="white",border=NA)

# Then overlay these with semitransparent colourful corridors
polygon(c(0:16,rev(0:16))-0.5,c(ad(result$obs[1,]-result$SE[1,]*multip),rev(ad(result$obs[1,]+result$SE[1,]*multip))),col=paste(colP,"33",sep=""),border=NA)
polygon(c(0:16,rev(0:16))-0.5,c(ad(result$obs[2,]-result$SE[2,]*multip),rev(ad(result$obs[2,]+result$SE[2,]*multip))),col=paste(colA,"33",sep=""),border=NA)


#Plot the mean and SD for the biological father
abline(h=mean(result$diffb,na.rm=T),lwd=1,col=colB,lty=1)

#Colours indicating significance
sigcol <- colorRampPalette(c("#FF1188","#FFFFFF","#FF1188"))
colscale<-sigcol(1000) 
scaleseq<-seq(0.001,0.999,l=1000)

cols<-colscale[sapply(result$p1,function(x){which.min(abs(x-scaleseq))})]

# Plot the change values
segments(1:14,par("usr")[3],1:14,par("usr")[4],col=colgrid,xpd=T)
#segments(1:14,par("usr")[3],1:14,par("usr")[4],col=alpha(cols,abs(result$p1-0.5)*2),xpd=T)

# Refresh the plot box
box(fg=boxcol)

# Plot the observed difference lines on top of all these supporting graphics
lines(0:16-0.5,ad(result$obs[1,]),col=colP)
lines(0:16-0.5,ad(result$obs[2,]),col=colA)

# Add a legend indicating a colour code of non-biological father currently present and non-biological father currently absent

# legend("topleft",c("Non-biol. present","Non-biol. absent","Biological present"),col=c(colP,colA,colB),pch=16,box.col=boxcol,cex=0.9)

# Add explanatory text to indicate similarity extremes (the y axis indicating the difference is reversed such that more similar fathers and partners are on the top of the plotting region)
shadowtext(15/2,min(linesA),"Father and partner\nsimilar",cex=0.9,col=1,r=0.13,bg=bgcol1)
shadowtext(15/2,max(linesA)-rangeA/20,"Father and partner\ndissimilar",cex=0.9,col=1,r=0.13,bg=bgcol1)


# Continue with the second part of the plot
# Modify plotting paramteres
par(mar=mar2,mgp=mgp2)

# Set the new area
plot(NULL,type="n",xlim=c(0,15),ylim=limB,xaxs="i",xlab="",ylab=ylab2,fg=boxcol,yaxt="n",xaxt="n",xpd=NA)

# The background corresponds to the <95% compatibility interval
rect(-1000,-1000,1000,1000,border=NA,col=corcol[5])

# Create customized y axis of the difference
axis(2,at=signif(c(min(linesB),0,max(linesB)),1),fg=boxcol)

# Draw all corridors corresponding to subsequent levels of compatibility with H0
polygon(c(0:15,rev(0:15)),c(ad(shade95[1,]),rev(ad(shade95[2,]))),col=corcol[4],border=NA)
polygon(c(0:15,rev(0:15)),c(ad(shade90[1,]),rev(ad(shade90[2,]))),col=corcol[3],border=NA)
polygon(c(0:15,rev(0:15)),c(ad(shade80[1,]),rev(ad(shade80[2,]))),col=corcol[2],border=NA)
polygon(c(0:15,rev(0:15)),c(ad(shade66[1,]),rev(ad(shade66[2,]))),col=corcol[1],border=NA)

# Indicate a 0 level of difference between adjecent years
abline(h=0,col=1,lty=2)

# Plot the age grid 
abline(v=1:14,col=colgrid)


  for(i in 1:4){
    axis(4,at=seq(0,min(linesB),l=4)[i],labels = paste(c(66,80,90,95),"%",sep="")[i],col.axis=corcol[1:4][i],las=1,tick=F,cex.axis=0.9)
  }
  
  for(i in 2:4){
    axis(4,at=seq(0,max(linesB),l=4)[i],labels = paste(c(66,80,90,95),"%",sep="")[i],col.axis=corcol[1:4][i],las=1,tick=F,cex.axis=0.9)
  }


# Refresh the box 
box(fg=boxcol)


# Overlay the basic grid with colourful grid corresponding to the limits of sensitive period rectangles. These colourful lines should start in the points indicating the change values.

cols<-colscale[sapply(result$p1,function(x){which.min(abs(x-scaleseq))})]

# Plot the change values
points(1:14,result$changesO,col=0,pch=16)
points(1:14,result$changesO,col=alpha(cols,abs(result$p1-0.5)*2),pch=16)

segments(1:14,par("usr")[3]-(par("usr")[4]-par("usr")[3])*0.05,1:14,result$changesO,col=0,xpd=NA)
segments(1:14,par("usr")[3]-(par("usr")[4]-par("usr")[3])*0.05,1:14,result$changesO,col=alpha(cols,abs(result$p1-0.5)*2),xpd=NA)

# Plot the legend for the compatibility intervals
shadowtext(15/2,par("usr")[3]+(par("usr")[4]-par("usr")[3])/10,"H0 compatibility intervals",cex=1,r=0.13,bg="white",col=corcol[1])


#intervals
par(mgp=c(2.5,0.5,0))
plot(NULL,type="n",xlim=c(0,15),ylim=c(121,-5),xaxs="i",xlab="",ylab="Intervals",fg=boxcol,xaxt="n",yaxt="n",xpd=NA)

bgcol2<-"#FFFFD0"

rect(-1000,-1000,1000,1000,col=bgcol2)

# Plot the change values
segments(1:14,par("usr")[3],1:14,par("usr")[4],col=0,xpd=T)
segments(1:14,par("usr")[3],1:14,par("usr")[4],col=alpha(cols,abs(result$p1-0.5)*2),xpd=T)

onset<-c(0.5,result$p1,0.5)

text(-2,-4.25,expression('p'['ony']),xpd=NA,cex=1.2)

cmb<-combn(1:length(onset),2)

prob.between<-lapply(1:ncol(cmb),
                     function(i){
                       onset[cmb[,i]]})

prob.interval<-sapply(prob.between,function(x){x[1]*(1-x[2])})


#Color of the product of two p[on] values
prodcol <- colorRampPalette(c("#888888","#FFFFFF","#FF1188"))
prodcolscale<-prodcol(1000) 
prodseq<-seq(0.001,0.999,l=1000)

prodcols<-prodcolscale[sapply(prob.interval,function(x){which.min(abs(x-prodseq))})]

for(i in 1:ncol(cmb)){
  rect(c(0:15)[cmb[1,i]],i-0.5,c(0:15)[cmb[2,i]],i+0.5,lwd=2,col=prodcols[i],border=NA)
}

box(fg=boxcol)

shadowtext(0:15,rep(c(-6,-2.5),times=8),round(onset,2),cex=0.9,bg=bgcol2,r=0.13,col=c(0,cols,0),xpd=NA)



l1<-16
wl<-1

gap<-1
txof<-0.5

#Legend for the likelihood
rseq<-seq(par("usr")[3],par("usr")[4],l=101)
lseq<-seq(0,1,l=101)
rect(l1,rseq[1:100],l1+wl,rseq[2:101],xpd=NA,col=prodcol(101),border=NA)

text(l1-txof,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2,expression(paste("p"["ony"],"(start) × (1 -","p"["ony"],"(end))")),xpd=NA,srt=90)


rect(l1+wl+gap,rseq[1:100],l1+2*wl+gap,rseq[2:101],xpd=NA,col=prodcol(101),border=NA)

topl<-par("usr")[3]+(par("usr")[4]-par("usr")[3])*max(prob.interval)
botl<-par("usr")[3]+(par("usr")[4]-par("usr")[3])*min(prob.interval)

lbgc<-"#202020"
segments(l1,topl,l1+2*wl+gap,topl,xpd=2,col=lbgc,lty=2)
segments(l1,botl,l1+2*wl+gap,botl,xpd=2,col=lbgc,lty=2)

shadowtext(l1+1.5*wl+gap,topl,round(max(prob.interval)/sum(prob.interval),3),cex=0.9,bg=prodcol(2)[2],r=0.13,col=lbgc,xpd=NA)

shadowtext(l1+1.5*wl+gap,botl,round(min(prob.interval)/sum(prob.interval),3),cex=0.9,bg=prodcol(2)[1],r=0.13,col=lbgc,xpd=NA)

text(l1+1.5*wl+gap,c(par("usr")[3:4]),c("min","max"),pos=c(1,3),cex=1,xpd=NA,offset=0.6)

text(rep(l1+wl/2,3),seq(par("usr")[3],par("usr")[4],l=21)[c(2,11,20)],c(0,0.5,1),cex=0.8,xpd=T)

text(l1+wl+gap-txof,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2,"Standardized to sum up to 1",xpd=NA,srt=90)




#define years as numbers between ticks
years<-1:(length(result$p1)+1)+0.5

intervals<-lapply(years,
                  function(y){
                    (prob.interval/sum(prob.interval))[which(cmb[1,]<y&cmb[2,]>y)]})

profile<-sapply(intervals,function(i){1-prod(1-i)})



par(mar=c(0,5,0,mright),mgp=c(2.5,0.5,0))
plot(NULL,type="n",xlim=c(0,15),ylim=c(0,1),xaxs="i",xlab="",ylab="1-prod(1-standardized interval probability)",fg=boxcol,xpd=NA,xaxt="n",col.lab=2)

rect(-1000,-1000,1000,1000,col=bgcol1)

box(fg=boxcol)


#abline(v=1:14,col=boxcol)

# for(i in 1:100){
#   lines(0:14+0.5,result$p.sper.rand[,i],col="#80808010")
# }


percol <- colorRampPalette(c("#888888","#FFFFFF","#FF1188"))
perscale<-percol(1000) 
perseq<-seq(0.001,0.999,l=1000)
percols<-perscale[sapply(result$p.compare,function(x){which.min(abs(x-perseq))})]

bw<-1
boxcol<-"#809090"

for(i in 1:15){
  q90<-quantile(result$p.sper.rand[i,],c(0.05,0.95))
  q100<-quantile(result$p.sper.rand[i,],c(0.005,0.995))
  rect(i-0.5-bw/2,q100[1],i-0.5+bw/2,q100[2],border=paste(boxcol,"80",sep=""),col=0)  
  rect(i-0.5-bw/2,q90[1],i-0.5+bw/2,q90[2],border=boxcol,col=NA)
  #segments(i-0.5-bw/2,result$p.sper[i],i-0.5+bw/2,result$p.sper[i],col=alpha(percols[i],abs(result$p.compare[i]-0.5)*2),lwd=1)  
}


arrows(0:14+0.5,profile+0.02,0:14+0.5,par("usr")[4]+0.085+(par("usr")[4]-par("usr")[3])*0.01467*c(rev(cumsum(1:15))[2:15],0),xpd=NA,length=0.05,code=1,col=0)
arrows(0:14+0.5,profile+0.02,0:14+0.5,par("usr")[4]+0.085+(par("usr")[4]-par("usr")[3])*0.01467*c(rev(cumsum(1:15))[2:15],0),xpd=NA,length=0.05,code=1,col="#FF000080")

arrows(0:14+0.5,profile,0:14+0.5,par("usr")[3],xpd=NA,length=0.1,code=0,col=0)
arrows(0:14+0.5,profile,0:14+0.5,par("usr")[3],xpd=NA,length=0.1,code=0,col=alpha(percols,abs(result$p.compare-0.5)*2))

lines(0:14+0.5,probInt(rep(0.5,14)),col=3)
lines(0:14+0.5,result$p.sper.rand[,3],col="#8080FF")
lines(0:14+0.5,result$p.sper,col=2)



par(mar=c(4,5,0,mright),mgp=mgp1)
plot(NULL,type="n",xlim=c(0,15),ylim=c(0,1),xaxs="i",xlab=xlab,ylab="",fg=boxcol,yaxt="n",xpd=NA)

rect(0:14,-1,1:15,2,col=alpha(percols,abs(result$p.compare-0.5)*2),border=NA)

shadowtext(0,0.5,"Sensitive period likelihood",cex=1,pos=4,r=0.13,col=colCH,bg="white")


#Legend for the likelihood
rseq<-seq(par("usr")[3],par("usr")[4]*5,l=101)
lseq<-seq(0,1,l=101)
rect(l1,rseq[1:100],l1+wl,rseq[2:101],xpd=NA,col=alpha(percol(101),abs(lseq-0.5)*2),border=NA)

text(rep(l1+wl/2,3),seq(par("usr")[3],par("usr")[4]*5,l=21)[c(2,11,20)],c(0,0.5,1),cex=0.8,xpd=NA)

text(l1-txof,par("usr")[3]+par("usr")[4]*5/2,"Sensitive period likelihood",xpd=NA,srt=90)









###########
#Second plot - random set
###########

result<-list.res$Body_height

runs<-dim(result$rand.diff)[1]
exrun<-3

mright<-7
mleft<-2
mar1=c(0,mleft,2,mright)
mgp1=c(1.6,0.5,0)
ylab1=""

mar2=c(0,mleft,0,mright)
mgp2=c(1.6,0.5,0)

xlab="Age"
ylab2=""


#Set the layout
par(mar=mar1,mgp=mgp1)

plot(NULL,type="n",xlim=c(0,15),ylim=rev(limA),ylab=ylab1,xaxt="n",yaxt="n",xaxs="i",fg=boxcol)

title("Body height (one permutation out of 10000)",adj=0)
axis(2,at=signif(seq(min(linesA),max(linesA),l=5),1),fg=boxcol)

# Background
rect(-1000,-1000,1000,1000,col=bgcol1)

# Likelihood of sensitive period

percol <- colorRampPalette(c("#1188FF","#FFFFFF","#FF1188"))
perscale<-percol(1000) 
perseq<-seq(0.001,0.999,l=1000)

p.compare<-sapply(1:15,function(i){sum(result$p.sper.rand[i,exrun]>c(result$p.sper[i],result$p.sper.rand[i,-c(exrun)]))/runs})

percols<-perscale[sapply(p.compare,function(x){which.min(abs(x-perseq))})]


# SE compatibility corridors from calculated means and bootsrapped standard errors (with different multip value these might be changed to for example 95% compatibility intervals)
# First overlay the background with white corridors
polygon(c(0:16,rev(0:16))-0.5,c(ad(result$exp[,,exrun][1,]-result$SE[1,]*multip),rev(ad(result$exp[,,exrun][1,]+result$SE[1,]*multip))),col="white",border=NA)
polygon(c(0:16,rev(0:16))-0.5,c(ad(result$exp[,,exrun][2,]-result$SE[2,]*multip),rev(ad(result$exp[,,exrun][2,]+result$SE[2,]*multip))),col="white",border=NA)

# Then overlay these with semitransparent colourful corridors
polygon(c(0:16,rev(0:16))-0.5,c(ad(result$exp[,,exrun][1,]-result$SE[1,]*multip),rev(ad(result$exp[,,exrun][1,]+result$SE[1,]*multip))),col=paste(colP,"33",sep=""),border=NA)
polygon(c(0:16,rev(0:16))-0.5,c(ad(result$exp[,,exrun][2,]-result$SE[2,]*multip),rev(ad(result$exp[,,exrun][2,]+result$SE[2,]*multip))),col=paste(colA,"33",sep=""),border=NA)


#Plot the mean and SD for the biological father
abline(h=mean(result$diffb,na.rm=T),lwd=1,col=colB,lty=1)

#Colours indicating significance
sigcol <- colorRampPalette(c("#FF1188","#FFFFFF","#FF1188"))
colscale<-sigcol(1000) 
scaleseq<-seq(0.001,0.999,l=1000)

cols<-colscale[sapply(result$p1.rand[,exrun],function(x){which.min(abs(x-scaleseq))})]

# Plot the change values
segments(1:14,par("usr")[3],1:14,par("usr")[4],col=colgrid,xpd=T)
#segments(1:14,par("usr")[3],1:14,par("usr")[4],col=alpha(cols,abs(result$p1-0.5)*2),xpd=T)

# Refresh the plot box
box(fg=boxcol)

# Plot the observed difference lines on top of all these supporting graphics
lines(0:16-0.5,ad(result$exp[,,exrun][1,]),col=colP)
lines(0:16-0.5,ad(result$exp[,,exrun][2,]),col=colA)

# Add a legend indicating a colour code of non-biological father currently present and non-biological father currently absent

legend("topright",c("Non-biol. present","Non-biol. absent","Biological present"),col=c(colP,colA,colB),pch=16,box.col=boxcol,cex=0.9)

# Add explanatory text to indicate similarity extremes (the y axis indicating the difference is reversed such that more similar fathers and partners are on the top of the plotting region)
shadowtext(15/2,min(linesA),"Father and partner\nsimilar",cex=0.9,col=1,r=0.13,bg=bgcol1)
shadowtext(15/2,max(linesA)-rangeA/20,"Father and partner\ndissimilar",cex=0.9,col=1,r=0.13,bg=bgcol1)


# Continue with the second part of the plot
# Modify plotting paramteres
par(mar=mar2,mgp=mgp2)

# Set the new area
plot(NULL,type="n",xlim=c(0,15),ylim=limB,xaxs="i",xlab="",ylab=ylab2,fg=boxcol,yaxt="n",xaxt="n",xpd=NA)

# The background corresponds to the <95% compatibility interval
rect(-1000,-1000,1000,1000,border=NA,col=corcol[5])

# Create customized y axis of the difference
axis(2,at=signif(c(min(linesB),0,max(linesB)),1),fg=boxcol)

# Draw all corridors corresponding to subsequent levels of compatibility with H0
polygon(c(0:15,rev(0:15)),c(ad(shade95[1,]),rev(ad(shade95[2,]))),col=corcol[4],border=NA)
polygon(c(0:15,rev(0:15)),c(ad(shade90[1,]),rev(ad(shade90[2,]))),col=corcol[3],border=NA)
polygon(c(0:15,rev(0:15)),c(ad(shade80[1,]),rev(ad(shade80[2,]))),col=corcol[2],border=NA)
polygon(c(0:15,rev(0:15)),c(ad(shade66[1,]),rev(ad(shade66[2,]))),col=corcol[1],border=NA)

# Indicate a 0 level of difference between adjecent years
abline(h=0,col=1,lty=2)

# Plot the age grid 
abline(v=1:14,col=colgrid)


for(i in 1:4){
  axis(4,at=seq(0,min(linesB),l=4)[i],labels = paste(c(66,80,90,95),"%",sep="")[i],col.axis=corcol[1:4][i],las=1,tick=F,cex.axis=0.9)
}

for(i in 2:4){
  axis(4,at=seq(0,max(linesB),l=4)[i],labels = paste(c(66,80,90,95),"%",sep="")[i],col.axis=corcol[1:4][i],las=1,tick=F,cex.axis=0.9)
}


# Refresh the box 
box(fg=boxcol)


# Overlay the basic grid with colourful grid corresponding to the limits of sensitive period rectangles. These colourful lines should start in the points indicating the change values.

cols<-colscale[sapply(result$p1.rand[,exrun],function(x){which.min(abs(x-scaleseq))})]

# Plot the change values
points(1:14,result$rand.diff[exrun,],col=0,pch=16)
points(1:14,result$rand.diff[exrun,],col=alpha(cols,abs(result$p1-0.5)*2),pch=16)

segments(1:14,par("usr")[3]-(par("usr")[4]-par("usr")[3])*0.05,1:14,result$rand.diff[exrun,],col=0,xpd=NA)
segments(1:14,par("usr")[3]-(par("usr")[4]-par("usr")[3])*0.05,1:14,result$rand.diff[exrun,],col=alpha(cols,abs(result$p1.rand[,exrun]-0.5)*2),xpd=NA)

# Plot the legend for the compatibility intervals
shadowtext(15/2,par("usr")[3]+(par("usr")[4]-par("usr")[3])/10,"H0 compatibility intervals",cex=1,r=0.13,bg="white",col=corcol[1])


#intervals
par(mgp=c(2.5,0.5,0))
plot(NULL,type="n",xlim=c(0,15),ylim=c(121,-5),xaxs="i",xlab="",ylab="",fg=boxcol,xaxt="n",yaxt="n",xpd=NA)

rect(-1000,-1000,1000,1000,col=bgcol2)

# Plot the change values
segments(1:14,par("usr")[3],1:14,par("usr")[4],col=0,xpd=T)
segments(1:14,par("usr")[3],1:14,par("usr")[4],col=alpha(cols,abs(result$p1.rand[,exrun]-0.5)*2),xpd=T)

onset<-c(0.5,result$p1.rand[,exrun],0.5)

#text(-2,-4.25,expression('p'['ony']),xpd=NA,cex=1.2)

cmb<-combn(1:length(onset),2)

prob.between<-lapply(1:ncol(cmb),
                     function(i){
                       onset[cmb[,i]]})

prob.interval<-sapply(prob.between,function(x){x[1]*(1-x[2])})


#Color of the product of two p[on] values
prodcol <- colorRampPalette(c("#888888","#FFFFFF","#FF1188"))
prodcolscale<-prodcol(1000) 
prodseq<-seq(0.001,0.999,l=1000)

prodcols<-prodcolscale[sapply(prob.interval,function(x){which.min(abs(x-prodseq))})]

for(i in 1:ncol(cmb)){
  rect(c(0:15)[cmb[1,i]],i-0.5,c(0:15)[cmb[2,i]],i+0.5,lwd=2,col=prodcols[i],border=NA)
}

box(fg=boxcol)

shadowtext(0:15,rep(c(-6,-2.5),times=8),round(onset,2),cex=0.9,bg=bgcol2,r=0.13,col=c(0,cols,0),xpd=NA)


l1<-16
wl<-1

gap<-1
txof<-0.5

#Legend for the likelihood
rseq<-seq(par("usr")[3],par("usr")[4],l=101)
lseq<-seq(0,1,l=101)
rect(l1,rseq[1:100],l1+wl,rseq[2:101],xpd=NA,col=prodcol(101),border=NA)

text(l1-txof,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2,expression(paste("p"["ony"],"(start) × (1 -","p"["ony"],"(end))")),xpd=NA,srt=90)


rect(l1+wl+gap,rseq[1:100],l1+2*wl+gap,rseq[2:101],xpd=NA,col=prodcol(101),border=NA)

topl<-par("usr")[3]+(par("usr")[4]-par("usr")[3])*max(prob.interval)
botl<-par("usr")[3]+(par("usr")[4]-par("usr")[3])*min(prob.interval)

lbgc<-"#202020"
segments(l1,topl,l1+2*wl+gap,topl,xpd=2,col=lbgc,lty=2)
segments(l1,botl,l1+2*wl+gap,botl,xpd=2,col=lbgc,lty=2)

shadowtext(l1+1.5*wl+gap,topl,round(max(prob.interval)/sum(prob.interval),3),cex=0.9,bg=prodcol(2)[2],r=0.13,col=lbgc,xpd=NA)

shadowtext(l1+1.5*wl+gap,botl,round(min(prob.interval)/sum(prob.interval),3),cex=0.9,bg=prodcol(2)[1],r=0.13,col=lbgc,xpd=NA)

text(l1+1.5*wl+gap,c(par("usr")[3:4]),c("min","max"),pos=c(1,3),cex=1,xpd=NA,offset=0.6)

text(rep(l1+wl/2,3),seq(par("usr")[3],par("usr")[4],l=21)[c(2,11,20)],c(0,0.5,1),cex=0.8,xpd=T)

text(l1+wl+gap-txof,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2,"Standardized to sum up to 1",xpd=NA,srt=90)




#define years as numbers between ticks
years<-1:(length(result$p1)+1)+0.5

intervals<-lapply(years,
                  function(y){
                    (prob.interval/sum(prob.interval))[which(cmb[1,]<y&cmb[2,]>y)]})

profile<-sapply(intervals,function(i){1-prod(1-i)})



par(mar=c(0,mleft,0,mright),mgp=c(2.5,0.5,0))
plot(NULL,type="n",xlim=c(0,15),ylim=c(0,1),xaxs="i",xlab="",ylab="",fg=boxcol,xpd=NA,xaxt="n",col.lab=2)

rect(-1000,-1000,1000,1000,col=bgcol1)

box(fg=boxcol)


#abline(v=1:14,col=boxcol)

# for(i in 1:100){
#   lines(0:14+0.5,result$p.sper.rand[,i],col="#80808010")
# }


percol <- colorRampPalette(c("#888888","#FFFFFF","#FF1188"))
perscale<-percol(1000) 
perseq<-seq(0.001,0.999,l=1000)
percols<-perscale[sapply(p.compare,function(x){which.min(abs(x-perseq))})]

bw<-1
boxcol<-"#809090"

for(i in 1:15){
  q90<-quantile(result$p.sper.rand[i,],c(0.05,0.95))
  q100<-quantile(result$p.sper.rand[i,],c(0.005,0.995))
  rect(i-0.5-bw/2,q100[1],i-0.5+bw/2,q100[2],border=paste(boxcol,"80",sep=""),col=0)  
  rect(i-0.5-bw/2,q90[1],i-0.5+bw/2,q90[2],border=boxcol,col=NA)
  #segments(i-0.5-bw/2,result$p.sper[i],i-0.5+bw/2,result$p.sper[i],col=alpha(percols[i],abs(result$p.compare[i]-0.5)*2),lwd=1)  
}


arrows(0:14+0.5,profile+0.02,0:14+0.5,par("usr")[4]+0.085+(par("usr")[4]-par("usr")[3])*0.01467*c(rev(cumsum(1:15))[2:15],0),xpd=NA,length=0.05,code=1,col=0)
arrows(0:14+0.5,profile+0.02,0:14+0.5,par("usr")[4]+0.085+(par("usr")[4]-par("usr")[3])*0.01467*c(rev(cumsum(1:15))[2:15],0),xpd=NA,length=0.05,code=1,col="#0000FF80")

arrows(0:14+0.5,profile,0:14+0.5,par("usr")[3],xpd=NA,length=0.1,code=0,col=0)
arrows(0:14+0.5,profile,0:14+0.5,par("usr")[3],xpd=NA,length=0.1,code=0,col=alpha(percols,abs(result$p.compare-0.5)*2))

lines(0:14+0.5,probInt(rep(0.5,14)),col=3)
lines(0:14+0.5,result$p.sper.rand[,exrun],col="#8080FF")
#lines(0:14+0.5,result$p.sper,col=2)



par(mar=c(4,mleft,0,mright),mgp=mgp1)
plot(NULL,type="n",xlim=c(0,15),ylim=c(0,1),xaxs="i",xlab=xlab,ylab="",fg=boxcol,yaxt="n",xpd=NA)

rect(0:14,-1,1:15,2,col=alpha(percols,abs(p.compare-0.5)*2),border=NA)

shadowtext(0,0.5,"Sensitive period likelihood",cex=1,pos=4,r=0.13,col=colCH,bg="white")


#Legend for the likelihood
rseq<-seq(par("usr")[3],par("usr")[4]*5,l=101)
lseq<-seq(0,1,l=101)
rect(l1,rseq[1:100],l1+wl,rseq[2:101],xpd=NA,col=alpha(percol(101),abs(lseq-0.5)*2),border=NA)

text(rep(l1+wl/2,3),seq(par("usr")[3],par("usr")[4]*5,l=21)[c(2,11,20)],c(0,0.5,1),cex=0.8,xpd=NA)

text(l1-txof,par("usr")[3]+par("usr")[4]*5/2,"Sensitive period likelihood",xpd=NA,srt=90)


dev.off()


