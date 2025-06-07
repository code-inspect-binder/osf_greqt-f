library(scales)
source("functions1.R")
#Load the data
data<-read.table("data.txt",header=T,sep="\t",stringsAsFactors = F)

load("results.Rdata")


char<-c(
  #Soc-dem characteristics
  "Residence_size",
  "Policy",
  "Education",
  "Wealth",
  
  #Physical
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
  
  #Psychological
  "Extraversion",
  "Agreeableness",
  "Conscientiousness",
  "Emotional_stability",
  "Openness",
  "Dominance"
)


#Extract measures of interest to objects
p.compare<-sapply(list.res,function(x){x$p.compare})
row.names(p.compare)<-paste(0:14,"-",1:15)
length(p.compare)

#out of
dim(p.compare)[2]

#traits
sum(colSums(p.compare>0.95)>0)

#In total
sum(p.compare>0.95)
#sug windows were observed
sum(p.compare>0.90)
#showed statistcical significance p<0.1

p.compare[10:13,]
sum(p.compare[10:13,]>0.95)
sum(p.compare[10:13,]>0.90)


length(p.compare[10:13,])*0.05
length(p.compare[10:13,])*0.1


#Probability of obtaining such or more extreme result
p005<-sum(dbinom(x=sum(p.compare[10:13,]>0.95):length(p.compare[10:13,]),size=length(p.compare[10:13,]),prob=0.05))

p01<-sum(dbinom(x=sum(p.compare[10:13,]>0.90):length(p.compare[10:13,]),size=length(p.compare[10:13,]),prob=0.1))

p005

#There are 12 options of how to define 4 consecutive years
1-(1-p005)^12
1-(1-p01)^12


length(p.compare[10:13,])

#Relative similarity (t value)
DELTA<-sapply(list.res,function(x){x$DELTA})
row.names(DELTA)<-paste(0:14,"-",1:15)
DELTA


#Cohens
Cohens.dAP<-sapply(list.res,function(x){x$Cohens.d$Cohens.dAP})
Cohens.dAB<-sapply(list.res,function(x){x$Cohens.d$Cohens.dAB})
Cohens.dPB<-sapply(list.res,function(x){x$Cohens.d$Cohens.dPB})

#Standard error divided by the respective mean (relative SE)
SE.AP<-sapply(list.res,function(x){sqrt(x$SE[1,]^2+x$SE[2,]^2)/colMeans(x$obs)})
SE.AB<-sapply(list.res,function(x){sqrt(x$SE[2,]^2+x$SEB^2)/colMeans(rbind(x$obs[2,],x$muB))})
SE.PB<-sapply(list.res,function(x){sqrt(x$SE[1,]^2+x$SEB^2)/colMeans(rbind(x$obs[1,],x$muB))})


min(Cohens.dAP)
max(Cohens.dAP)

#Cohen’s d of the difference in similarity between groups with absent and present non-biological fathers calculated for each characteristic and year assume values between 
findMe(Cohens.dAP,min(Cohens.dAP))
findMe(Cohens.dAP,max(Cohens.dAP))

#Visualization of standard errors
image(SE.AP)
text(rep(seq(0,1,l=15),times=21),rep(seq(0,1,l=21),each=15),round(SE.AP,2))

#Corrected - i remove values that are beyond some relative uncertainty thershold
Cohens.dAPc<-Cohens.dAP
Cohens.dAPc[SE.AP>0.15]<-0

findMe(Cohens.dAPc,min(Cohens.dAPc))
findMe(Cohens.dAPc,max(Cohens.dAPc))

quantile(Cohens.dAPc,c(0.25,0.75))


#Cohen's d AB
findMe(Cohens.dAB,min(Cohens.dAB))
findMe(Cohens.dAB,max(Cohens.dAB))

Cohens.dABc<-Cohens.dAB
Cohens.dABc[SE.AB>0.15]<-0

findMe(Cohens.dABc,min(Cohens.dABc))
findMe(Cohens.dABc,max(Cohens.dABc))

quantile(Cohens.dAB,c(0.25,0.75))
quantile(Cohens.dABc,c(0.25,0.75))


#Cohen's d PB
findMe(Cohens.dPB,min(Cohens.dPB))
findMe(Cohens.dPB,max(Cohens.dPB))

Cohens.dPBc<-Cohens.dPB
Cohens.dPBc[SE.PB>0.15]<-0

findMe(Cohens.dPBc,min(Cohens.dPBc))
findMe(Cohens.dPBc,max(Cohens.dPBc))

quantile(Cohens.dPB,c(0.25,0.75))
quantile(Cohens.dPBc,c(0.25,0.75))


quantile(Cohens.dPB[10:13,],c(0.25,0.75))
quantile(Cohens.dPBc[10:13,],c(0.25,0.75))


#Attractiveness
Cohens.dAB[,"Attractiveness"]
max(Cohens.dAB[,"Attractiveness"])
min(Cohens.dAB[,"Attractiveness"])

Cohens.dPB[,"Attractiveness"]
SE.PB[,"Attractiveness"]
max(Cohens.dPB[,"Attractiveness"])
min(Cohens.dPB[,"Attractiveness"])

#Only certain
Cohens.dPB[,"Attractiveness"][SE.PB[,"Attractiveness"]<0.15]
min(Cohens.dPB[,"Attractiveness"][SE.PB[,"Attractiveness"]<0.15])
max(Cohens.dPB[,"Attractiveness"][SE.PB[,"Attractiveness"]<0.15])



#Get and print tables
codes<-lapply(list.res,function(x){x$codes})
byone<-summary(as.factor(unlist(codes)),maxsum=2000)
unpart<-unique(unlist(codes))
length(unpart)


dens<-density(data$Resp_age[match(unpart,data$code)])

plot(dens,xlim=c(10,70))

age<-data$Resp_age[match(unpart,data$code)]
length(age)

sum(age<=45)


summary(data$Resp_age[match(unpart,data$code)])
sd(data$Resp_age[match(unpart,data$code)])

summary(as.factor(byone))

#Nonbiol
ns<-sapply(list.res,function(x){x$n})
findMe(as.matrix(ns),min(ns))
findMe(as.matrix(ns),max(ns))

mean(ns)
sd(ns)


#biol
bcodes<-lapply(list.res,function(x){x$bcodes})
bcodes<-unlist(bcodes)
bcodes<-bcodes[!is.na(bcodes)]
bbyone<-summary(as.factor(bcodes),maxsum=2000)
bunpart<-unique(unlist(bcodes))
length(bunpart)


bage<-data$Resp_age[match(bunpart,data$code)]
bdens<-density(bage)
plot(bdens,xlim=c(10,70))

summary(data$Resp_age[match(bunpart,data$code)])
round(sd(data$Resp_age[match(bunpart,data$code)],na.rm=T),2)

summary(as.factor(bbyone))


bns<-sapply(list.res,function(x){length(x$bcodes[!is.na(x$bcodes)])})
findMe(as.matrix(bns),min(bns))
findMe(as.matrix(bns),max(bns))

round(mean(bns),2)
round(sd(bns),2)

names(bns)<-char

Btab<-cbind(gsub("_"," ",char),bns)

Btab

write.table(Btab,"Tab2sampleSizeBio.txt",sep="\t",row.names = F)


#Sample size table
str(list.res[[1]])

N<-lapply(list.res,function(x){x$n.group})
Ntab<-as.data.frame(do.call(rbind,N))

chlab<-c(rbind(gsub("_"," ",char),rep("",length(char))))
flab<-rep(c("present","absent"),times=length(char))

Ntab<-cbind(chlab,flab,Ntab)

names(Ntab)<-c("Characteristic","Non-biological father",paste(0:14,1:15,sep=" - "))

write.table(Ntab,"Tab1sampleSize.txt",sep="\t",row.names = F)


#Npresent/absent averages
Npresent<-t(sapply(list.res,function(x){x$n.group[1,]}))

plot(apply(Npresent,2,mean))

round(apply(Npresent,2,mean),2)
round(apply(Npresent,2,sd),2)

Nabsent<-t(sapply(list.res,function(x){x$n.group[2,]}))

round(apply(Nabsent,2,mean),2)
round(apply(Nabsent,2,sd),2)


#Other tables from S7

str(list.res[[1]])

obs<-lapply(list.res,function(x){x$obs})
obstab<-as.data.frame(do.call(rbind,obs))

obstab<-cbind(chlab,flab,round(obstab,2))

names(obstab)<-c("Characteristic","Non-biological father",paste(0:14,1:15,sep=" - "))

write.table(obstab,"Tab3AverageDifference.txt",sep="\t",row.names = F)








