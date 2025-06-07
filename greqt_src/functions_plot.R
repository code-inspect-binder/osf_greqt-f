plotFull<-function(result,
                   colP="#008888",   #Non-biological father present color
                   colA="#FFAA00",   #Non-biological father absent color
                   
                   colB="#00AA00",   #Color of the line indicating teh similarity between bilogical father and the partner
                   
                   colCH="#FF1188",  #Change and outlined sensitive period color
                   
                   colgrid="#CCCCCC", #Color of the basic age grid
                   
                   bgcol1="#F0F0F0",  #Background of the top part of the visualization
                   boxcol="#888888",  #Color of the boxes, axes, etc.
                   h0col1="#1111CC",
                   h0col2="#FFFFFF",  #Colors of H0 compatibility intervals - color gradient defined by its extremes
                   multip=1.0,       #Multiplication of bootstrapped SE corridors, 1 correspond to simple SE corridors
                   tifit=T, #whether tiff bitmap should be generated right away
                   layit=T, #whether layout should be specified within the function that plots everything
                   ratio=c(5,3), #window division (only relevant if layit=T)
                   mar1=c(0,4,2,3),
                   mgp1=c(1.6,0.5,0),
                   ylab1="Average difference between\nfather and partner",
                   
                   mar2=c(3,4,0,3),
                   mgp2=c(1.6,0.5,0),
                   
                   xlab="Age",
                   ylab2="Year-on-year change\nin (absent - presnet)",
                   legends=c(T,T,T)
){
  
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
  
  if(tifit==T){
    tiff(paste("Fig",formatC(which(char==gsub(" ","_",result$name)),width=2,flag=0),"_",result$name,".tif",sep=""),width=15,height=13,units="cm",res=600,compression="lzw")
  }
  
  #Set the layout
  
  if(layit==T){
    layout(matrix(c(1,2)),heights=ratio)
  }
  
  par(mar=mar1,mgp=mgp1)
  
  plot(NULL,type="n",xlim=c(0,15),ylim=rev(limA),ylab=ylab1,xaxt="n",yaxt="n",xaxs="i",fg=boxcol)
  
  title(result$name,adj=0)
  axis(2,at=signif(seq(min(linesA),max(linesA),l=5),1),fg=boxcol)
  
  # Background
  rect(-1000,-1000,1000,max(linesA)+rangeA*0.1,col=bgcol1)
  
  # Likelihood of sensitive period
  
  
  percol <- colorRampPalette(c("#888888","#FFFFFF","#FF1188"))
  perscale<-percol(1000) 
  perseq<-seq(0.001,0.999,l=1000)
  
  percols<-perscale[sapply(result$p.compare,function(x){which.min(abs(x-perseq))})]
  
  rect(0:14,max(linesA)+rangeA*0.1,1:15,1000,col=alpha(percols,abs(result$p.compare-0.5)*2),border=NA)
  
  
  #Legend for the likelihood
  if(legends[2]==T){
    rseq<-seq(par("usr")[3],par("usr")[4],l=101)
    lseq<-seq(0,1,l=101)
    rect(15.5,rseq[1:100],16.5,rseq[2:101],xpd=T,col=alpha(percol(101),abs(lseq-0.5)*2),border=NA)
    
    text(rep(16,3),seq(par("usr")[3],par("usr")[4],l=21)[c(2,11,20)],c(0,0.5,1),cex=0.8,xpd=T)
  }
  
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
  

  # Add a line sepparating the Likely sensitive period region from the rest of the graph
  lines(c(-5,20),rep(max(linesA)+rangeA*0.1,2),col=boxcol)
  
  # Plot the change values
  segments(1:14,par("usr")[3],1:14,par("usr")[4],col=0,xpd=T)
  segments(1:14,par("usr")[3],1:14,par("usr")[4],col=alpha(cols,abs(result$p1-0.5)*2),xpd=T)
  
  # Refresh the plot box
  box(fg=boxcol)
  
  # Plot the observed difference lines on top of all these supporting graphics
  lines(0:16-0.5,ad(result$obs[1,]),col=colP)
  lines(0:16-0.5,ad(result$obs[2,]),col=colA)
  

  
  # Add a legend indicating a colour code of non-biological father currently present and non-biological father currently absent
  if(legends[1]==T){
    legend("topleft",c("Non-biol. present","Non-biol. absent","Biological present"),col=c(colP,colA,colB),pch=16,box.col=boxcol,cex=0.9)
  }
  
  # Add explanatory text to indicate similarity extremes (the y axis indicating the difference is reversed such that more similar fathers and partners are on the top of the plotting region)
  shadowtext(15/2,min(linesA),"Father and partner\nsimilar",cex=0.9,col=1,r=0.13,bg=bgcol1)
  shadowtext(15/2,max(linesA)-rangeA/20,"Father and partner\ndissimilar",cex=0.9,col=1,r=0.13,bg=bgcol1)
  
  # Add text with white outline indicating the plotting region with the indication of likely sensitve period.
  shadowtext(0,max(linesA)+rangeA*0.17,"Sensitive period likelihood",cex=1,pos=4,r=0.13,col=colCH,bg="white")
  
  
  # Continue with the second part of the plot
  # Modify plotting paramteres
  par(mar=mar2,mgp=mgp2)
  
  # Set the new area
  plot(NULL,type="n",xlim=c(0,15),ylim=limB,xaxs="i",xlab=xlab,ylab=ylab2,fg=boxcol,yaxt="n",xpd=NA)
  
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

  if(legends[3]==T){
    for(i in 1:4){
      axis(4,at=seq(0,min(linesB),l=4)[i],labels = paste(c(66,80,90,95),"%",sep="")[i],col.axis=corcol[1:4][i],las=1,tick=F,cex.axis=0.9)
    }
    
    for(i in 2:4){
      axis(4,at=seq(0,max(linesB),l=4)[i],labels = paste(c(66,80,90,95),"%",sep="")[i],col.axis=corcol[1:4][i],las=1,tick=F,cex.axis=0.9)
    }
  }
  
  # Refresh the box 
  box(fg=boxcol)
  
  
  # Overlay the basic grid with colourful grid corresponding to the limits of sensitive period rectangles. These colourful lines should start in the points indicating the change values.
  
  cols<-colscale[sapply(result$p1,function(x){which.min(abs(x-scaleseq))})]
  
  # Plot the change values
  points(1:14,result$changesO,col=0,pch=16)
  points(1:14,result$changesO,col=alpha(cols,abs(result$p1-0.5)*2),pch=16)
  
  segments(1:14,result$changesO,1:14,par("usr")[4]+(par("usr")[4]-par("usr")[3])*0.05,col=0,xpd=NA)
  segments(1:14,result$changesO,1:14,par("usr")[4]+(par("usr")[4]-par("usr")[3])*0.05,col=alpha(cols,abs(result$p1-0.5)*2),xpd=NA)
  
  # Plot the legend for the compatibility intervals
  shadowtext(15/2,par("usr")[3]+(par("usr")[4]-par("usr")[3])/10,"H0 compatibility intervals",cex=1,r=0.13,bg="white",col=corcol[1])
  
  
  # Save the image
  if(tifit==T){
    dev.off()
  }
}

