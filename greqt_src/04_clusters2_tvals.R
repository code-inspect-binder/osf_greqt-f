
colnames(DELTA)<-gsub("_"," ",colnames(p.compare))

dmat<-dist(t(DELTA), method = "euclidean")
dend1 <- as.dendrogram(hclust(dmat, method = "ward.D2" ))


#rows defining categories
rctas<-list(1:4,5:15,16:21)

#Define colour for each category
char.col<-c("#228822","#663388","#006688")
char.col2<-c("#D7FFD7","#D7AFFF","#38D7FF")


#Color code characetrs in the order of the dendrogram

ord.col<-char.col[rep(c(1:3),sapply(rctas,length))][order.dendrogram(dend1)]
ord.col2<-char.col2[rep(c(1:3),sapply(rctas,length))][order.dendrogram(dend1)]

labels_colors(dend1)<-ord.col

#reorder the list of results to match the alignment of the dendrogram
list.res.reo<-list.res[order.dendrogram(dend1)]

layd<-c(10,1)

tiff("Figure_hclust2.tif",width=sum(layd),height=11.5,units="cm",res=600,compression="lzw")

layout(matrix(1:2,nrow=1),widths=layd)
par(mar=c(3.5,1,1.5,14),mgp=c(2,0.8,0))
plot(dend1, cex = 0.8, horiz=T,xlab="Height")

#Get the x scaling parameter determinated by the dendrogrm height and multiply other graphics positions by that
xsc<-(par("usr")[1]-par("usr")[2])/33

w<-2.7*xsc
l<--54*xsc

colbg<-rep(c("#E0E0E0","#FAFAFA"),times=20)

#Setting top lim within each bar
#yscale*3=0.5
wanttop<-4
yscale<-0.5/wanttop

wantlim<-2

drawlim<-yscale*wantlim

for(i in 1:length(list.res.reo)){
  result<-list.res.reo[[i]]
  rect(l,i-0.5,l-15*w,i+0.5,col=colbg[i],border=NA,lwd=2,xpd=T)
  rect(l,i-0.5,l-15*w,i+0.5,col=alpha(ord.col2[i],0.1),border=NA,lwd=2,xpd=T)
  
  segments(l,c(i+drawlim,i,i-drawlim),l-c(16,15,16)*w,c(i+drawlim,i,i-drawlim),col=alpha(c("#000000","#000000","#000000"),c(0.4,1,0.4)),xpd=T,lty=c(1,2,1))
  
  points(l-16*w,i+drawlim,pch=21,xpd=NA,cex=0.7,col="#008800",bg="#55FF55")
  points(l-16*w,i-drawlim,pch=21,xpd=NA,cex=0.7,col="#888800",bg="#FFFF55")
  #text(l-15.5*w,i+drawlim,"2",pos=4,xpd=NA,cex=0.8)
  #text(l-15.5*w,i-drawlim,"-2",pos=4,xpd=T)
}

for(i in 1:length(list.res.reo)){
  result<-list.res.reo[[i]]
  lines(l-(1:15-0.5)*w,result$DELTA*yscale+i,col=1,xpd=NA)
}


axis(1,at=l-seq(0,15,by=5)*w,labels=seq(0,15,by=5),xpd=NA)
rect(l,0.5,l-15*w,length(list.res.reo)+0.5,xpd=NA)

mtext("Age",1,at=l-w*15/2,line=2)

text(l-19*w,par("usr")[3]+par("usr")[4]/2,expression(paste("Relative similarity (t value)")),srt=270,xpd=NA,font=2)

legend("bottomright",c("+2","−2"),pch=21,col=c("#008800","#888800"),pt.bg=c("#55FF55","#FFFF55"),cex=0.8,xpd=NA,inset=c(-3.42,-0.05),bty="n")

dev.off()
