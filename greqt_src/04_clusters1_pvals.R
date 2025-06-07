library(dendextend)


percol <- colorRampPalette(c("#888888","#FFFFFF","#FF1188"))
perscale<-percol(1000) 
perseq<-seq(0.001,0.999,l=1000)

colnames(p.compare)<-gsub("_"," ",colnames(p.compare))

dmat<-dist(t(logit(p.compare)), method = "euclidean")
dend1 <- as.dendrogram(hclust(dmat, method = "ward.D2" ))

#rows defining categories
rctas<-list(1:4,5:15,16:21)

#Define colour for each category
char.col<-c("#228822","#663388","#006688")

#Color code characetrs in the order of the dendrogram
labels_colors(dend1)<-char.col[rep(c(1:3),sapply(rctas,length))][order.dendrogram(dend1)]

#reorder the list of results to match the alignment of the dendrogram
list.res.reo<-list.res[order.dendrogram(dend1)]



tiff("Figure_hclust1.tif",width=10,height=11.5,units="cm",res=600,compression="lzw")

par(mar=c(3.5,1,1.5,14),mgp=c(2,0.8,0))
plot(dend1, cex = 0.8, horiz=T,xlab="Height")

#Get the x scaling parameter determinated by the dendrogrm height and multiply other graphics positions by that
xsc<-(par("usr")[1]-par("usr")[2])/33

w<-2.7*xsc
l<--54*xsc

for(i in 1:length(list.res.reo)){
  result<-list.res.reo[[i]]
  percols<-perscale[sapply(result$p.compare,function(x){which.min(abs(x-perseq))})]
  rect(l-0:14*w,i-0.5,l-1:15*w,i+0.5,col=alpha(percols,abs(result$p.compare-0.5)*2),border=NA,lwd=2,xpd=T)
  #rect(l-0:14*w,i-0.5,l-1:15*w,i+0.5,col=NA,border=ifelse(adjusted[,i]<=0.1,"#00FF00",NA),lwd=2,xpd=T)
}
for(i in 1:length(list.res.reo)){
  result<-list.res.reo[[i]]
  rect(l-0:14*w,i-0.5,l-1:15*w,i+0.5,col=NA,border=ifelse(result$p.compare>0.95,"#00FF00",ifelse(result$p.compare>0.90,"#FFFF00",NA)),lwd=1.8,xpd=T)
}

axis(1,at=l-seq(0,15,by=5)*w,labels=seq(0,15,by=5),xpd=NA)
rect(l,0.5,l-15*w,length(list.res.reo)+0.5,xpd=T)

mtext("Age",1,at=l-w*15/2,line=2)

#legend
top<-par("usr")[4]
ran<-par("usr")[4]-par("usr")[3]

rseq<-seq(0,-15*w,l=101)
lseq<-seq(0,1,l=101)

rect(c(l+0.5*w,l-15*w),top+ran*-0.02,c(l,l-15.5*w),top+ran*0.02,col=percol(2),xpd=T,border=NA)
rect(l+rseq[1:100],top+ran*-0.02,l+rseq[2:101],top+ran*0.02,xpd=T,col=alpha(percol(101),abs(lseq-0.5)*2),border=NA)

text(seq(l,l-15*w,l=5),top+ran*0.0,seq(0,1,l=5),cex=0.8,xpd=T)
text(l+20*xsc,top+ran*0.05,"Sensitive period likelihood",pos=4,xpd=T,font=2,cex=0.8)

dev.off()

