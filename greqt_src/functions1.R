# Function that creates a TRUE/FALSE matrix of non-biological father availibility from two numbers that are recorded in the data - FROM and TILL. The function does this by evaluating the presence of the caretaker using < and >  for each interval in question.

giveage<-function(data){
  
  FROM<-data$Grew_up_from_nonbiol
  TILL<-data$Grew_up_till_nonbiol
  
  ages<-data.frame(x=rep(NA,nrow(data)))
  
  for(i in 1:15){
    j<-i-1
    text<-paste("grew",j,i,"<-TILL>",j,"&FROM<",i,sep="")
    eval(parse(text=text))
    text<-paste("ages<-cbind(ages,grew",j,i,")",sep="")
    eval(parse(text=text))
  }
  
  ages<-ages[,-1]
  
  return(ages)
}

#Function that calculates differences between non-biological fathers and partners in both groups (Non-biological father currently present, non-biological father currently absent) for each year in the analysis.

deltas<-function(diff,ages){
  deltaT<-NA
  deltaF<-NA
  
  for(i in 1:15){
    deltaT[i]<-mean(diff[ages[i]==T],na.rm=T)
    deltaF[i]<-mean(diff[ages[i]==F],na.rm=T)
  }
  
  return(rbind(deltaT,deltaF))
}

#similarly calculated standard deviations  
SDdeltas<-function(diff,ages){
  sdT<-NA
  sdF<-NA
  
  for(i in 1:15){
    sdT[i]<-sd(diff[ages[i]==T],na.rm=T)
    sdF[i]<-sd(diff[ages[i]==F],na.rm=T)
  }
  
  return(rbind(sdT,sdF))
}


#Function that takes a vector of p vlaues (proportion of smaller year-on-year changes)
probInt<-function(measured){
  onset<-c(0.5,measured,0.5)
  
  cmb<-combn(1:length(onset),2)
  
  prob.between<-lapply(1:ncol(cmb),
                       function(i){
                         onset[cmb[,i]]})
  
  prob.interval<-sapply(prob.between,function(x){x[1]*(1-x[2])})
  
  #define years as numbers between ticks
  years<-1:(length(measured)+1)+0.5
  
  intervals<-lapply(years,
                    function(y){
                      (prob.interval/sum(prob.interval))[which(cmb[1,]<y&cmb[2,]>y)]})
  
  profile<-sapply(intervals,function(i){1-prod(1-i)})
  
  return(profile)
}

#Function that repeats last and first item of a vector - usefull if we want to draw plots and CI all the way to the border of the plotting region.

ad<-function(v){
  return(c(v[1],v,v[length(v)]))
}


# Function that plots a text with an outline. it is equivalent to TeachingDemos shadowtext function described at: https://stackoverflow.com/questions/29303480/text-labels-with-outline-in-r 

shadowtext <- function(x, y=NULL, labels, col='white', bg='black', theta= seq(pi/4, 2*pi, length.out=40), r=0.1, ... ) {
  
  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')
  
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  text(xy$x, xy$y, labels, col=col, ... ) }



#Functions coonducting logit and inverse logit transformation (p to log-odds and back)
logit<-function(x){log(x/(1-x))}
inv_logit<-function(x){exp(x)/(1+exp(x))}


#Function that returns matrix cells nicely
findMe<-function(mat,what){
  idx<-which(mat==what, arr.ind = T)
  res<-matrix(mat[idx])
  if(nrow(idx)>1){
    res<-as.list(res)
    for(i in 1:length(res)){
      res[[i]]<-as.matrix(res[[i]])
      rownames(res[[i]])<-rownames(mat)[idx[i,1]]
      colnames(res[[i]])<-colnames(mat)[idx[i,2]]
    }
  }else{
    rownames(res)<-rownames(mat)[idx[1]]
    colnames(res)<-colnames(mat)[idx[2]]
  }
  return(res)
}

