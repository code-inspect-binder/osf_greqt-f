doit<-function(char="Body_height",runs=1000,continuous=T){
  
  titline<-gsub("_"," ",char)
  
  #Get rid of the extra rows without this characteristics known for partners and non-biological fathers. We need to create the command first and a character string to include the previously stated charactersitic of interest
  text<-paste("dbiol<-data[!is.na(data$Partner_",char,")&!is.na(data$Biol_",char,")&data$Grew_up_from_biol==0&data$Grew_up_till_biol>15,]",sep="")
  eval(parse(text=text))
  
  text<-paste("data<-data[!is.na(data$Partner_",char,")&!is.na(data$Nonbiol_",char,"),]",sep="")
  eval(parse(text=text))
  
  #Check the sample size
  n<-nrow(data)
  nbiol<-length(dbiol$code[!is.na(dbiol$code)])
  
  codes<-data$code
  bcodes<-dbiol$code
  
  age.sum<-summary(data$Resp_age)
  age.sd<-sd(data$Resp_age)
  
  # Create a TRUE/FALSE matrix of Non-biological fathers availibility during the life of each individual. Function giveage() from the functions1.R script will do this for you.
  ages<-giveage(data)
  colSums(ages)
  
  # Extract the vectors of analysed characteristics of Partner and Non-biological father from the dataset. 
  text<-paste("trait_p<-data$Partner_",char,sep="")
  eval(parse(text=text))
  
  text<-paste("trait_f<-data$Nonbiol_",char,sep="")
  eval(parse(text=text))
  
  text<-paste("trait_pb<-dbiol$Partner_",char,sep="")
  eval(parse(text=text))
  
  text<-paste("trait_b<-dbiol$Biol_",char,sep="")
  eval(parse(text=text))
  
  # Calculate the differecnes between non-biological parent and partner for each individual
  if(continuous==T){
    diff<-abs(trait_f-trait_p)
    diffb<-abs(trait_b-trait_pb)
  }else{
    diff<-ifelse(trait_f==trait_p,0,1)
    diffb<-ifelse(trait_b==trait_pb,0,1)
  }
  
  
  # Create the objects to be filled with outcomes of permutation tests
  
  # rand.diff should contain a distribution of expected changes in simmilarity/dissimiliarity in a year-on-year perspective. There are 14 year-on-year changes in parent-partner simillarity in total. 
  rand.diff<-matrix(NA,nrow=runs,ncol=14)
  
  # rand.dll contains the data abou parent-partner similarity in both groups depending on current non-biological father availibility in the randomized runs.
  rand.dll<-array(NA,dim=c(2,15,runs))
  
  # Fill these objects with the permutation tests
  for(i in 1:runs){
    dll<-deltas(diff=sample(diff),ages=ages)
    diffPA<-dll[2,]-dll[1,]
    rand.diff[i,]<-diffPA[2:length(diffPA)]-diffPA[1:(length(diffPA)-1)]
    rand.dll[,,i]<-dll
  }
  
  # Now calculate the observed equivalents of these measures
  # Observed Differences between non-biological fathers and partners for each Age
  dllO<-deltas(diff=diff,ages=ages)
  
  # Changes in simmilarity/dissimilarity in adjecent years
  diffPA<-dllO[2,]-dllO[1,]
  changesO<-diffPA[2:length(diffPA)]-diffPA[1:(length(diffPA)-1)]
  
  # Calculate the p values from the comparison of bootstrapped differences in adjecent years and the observed differences
  p1<-NA
  p2<-NA
  p1.rand<-matrix(NA,nrow=14,ncol=runs)
  pvals<-NA
  
  for(i in 1:14){
    p1[i]<-sum(rand.diff[,i]<changesO[i])/runs   #probability that the observed change is larger  than expected
    p2[i]<-sum(rand.diff[,i]>changesO[i])/runs   #probability that the observed change is smaller than expected
    pvals[i]<-min(c(p1[i],p2[i]))*2
    p1.rand[i,]<-sapply(1:runs,function(r){sum(c(changesO[i],rand.diff[-c(r),i])<rand.diff[r,i])/runs})
  }
  
  p.sper<-probInt(p1)
  p.sper.rand<-sapply(1:runs,function(i){probInt(p1.rand[,i])})
  
  p.comapre<-sapply(1:15,function(i){sum(p.sper[i]>p.sper.rand[i,])/runs})
  
  
  # Calculate the bootsrapped expecetd mean and standard error from the extracted samples.
  est.m<-apply(rand.dll,c(1,2),mean)
  est.sd<-apply(rand.dll,c(1,2),sd)
  
  #Check the sample size for each group and year
  n_age<-rbind(colSums(ages,na.rm=T),colSums(!ages,na.rm=T))
  row.names(n_age)<-c("fatherT","fatherF")
  
  #calculate the distinctiveness
  #DELTA<-(2*(dllO[2,]-dllO[1,]))/(est.sd[2,]+est.sd[1,])
  DELTA<-(dllO[2,]-dllO[1,])/sqrt(est.sd[2,]^2+est.sd[1,]^2)
  
  # Cohen's d
  #First create the equivalent arrangement for the stimation of differences if the father is biologicla and present
  muB<-rep(mean(diffb,na.rm=T),15)
  sdB<-rep(sd(diffb,na.rm=T),15)
  nB<-rep(length(diffb[!is.na(diffb)]),15)
  
  # Observed standard deviation of the differences (if father is non-biological)
  sdO<-SDdeltas(diff=diff,ages=ages)
  
  #Pooled standard deviations
  sAP<-sqrt(((n_age[2,]-1)*sdO[2,]^2+(n_age[1,]-1)*sdO[1,]^2)/(n_age[2,]+n_age[1,]))
  sAB<-sqrt(((n_age[2,]-1)*sdO[2,]^2+(nB-1)*sdB^2)/(n_age[2,]+nB))
  sPB<-sqrt(((n_age[1,]-1)*sdO[1,]^2+(nB-1)*sdB^2)/(n_age[1,]+nB))
  
    
  #Cohen's d
  Cohens.dAP<-(dllO[2,]-dllO[1,])/sAP
  Cohens.dAB<-(dllO[2,]-muB)/sAB
  Cohens.dPB<-(dllO[1,]-muB)/sPB
  
  
  # Prepare results to return for future use
  res<-list(
    name=titline,
    n=n,codes=codes,age=c(age.sum,sd=age.sd),
    nbiol=nbiol,bcodes=bcodes,
    n.group=n_age,
    ages=ages,
    obs=dllO,
    exp=rand.dll,
    SE=est.sd,
    DELTA=DELTA,
    sdO=sdO,
    Cohens.d=list(Cohens.dAP=Cohens.dAP,Cohens.dAB=Cohens.dAB,Cohens.dPB=Cohens.dPB),
    changesO=changesO,
    rand.diff=rand.diff,
    diffb=diffb,
    muB=muB,sdB=sdB,nB=nB,SEB=sdB/sqrt(nB),
    p1=p1,p1.rand=p1.rand,pvals=pvals,
    p.sper=p.sper,p.sper.rand=p.sper.rand,
    p.compare=p.comapre
  )
  
  return(res)
}






