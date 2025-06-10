#Load all the required functions
library(scales)
library(corrplot)
source("functions1.R")
source("functions2.R")
source("functions_plot.R")


#Load the data
data<-read.table("data.txt",header=T,sep="\t",stringsAsFactors = F)

#Load the sensitive period results
load("results.Rdata")

#Extract measures of interest to objects
p.compare<-sapply(list.res,function(x){x$p.compare})
DELTA<-sapply(list.res,function(x){x$DELTA})

#Convert to log-odds
LO.compare<-logit(p.compare)

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
  
  #Personality
  "Extraversion",
  "Agreeableness",
  "Conscientiousness",
  "Emotional_stability",
  "Openness",
  "Dominance"
)


NBF<-data[,match(paste("Nonbiol",char,sep="_"),names(data))]
names(NBF)<-gsub("_"," ",char)

PAR<-data[,match(paste("Partner",char,sep="_"),names(data))]
names(PAR)<-gsub("_"," ",char)


LO.compare<-as.data.frame(LO.compare)
DELTA<-as.data.frame(DELTA)

names(LO.compare)<-gsub("_"," ",char)
names(DELTA)<-gsub("_"," ",char)


cortabNB<-cor(NBF,use = "pairwise.complete.obs")
cortabPA<-cor(PAR,use = "pairwise.complete.obs")

cortabLO<-cor(LO.compare,use = "pairwise.complete.obs")
cortabD<-cor(DELTA,use = "pairwise.complete.obs")


#Check some extremes
#Nonbio fathers
cNB<-cortabNB[upper.tri(cortabNB,diag=F)]

findMe(as.matrix(cortabNB),min(cNB))
findMe(as.matrix(cortabNB),max(cNB))

quantile(abs(cNB),c(0.25,0.75))

#Partners
cPA<-cortabPA[upper.tri(cortabPA,diag=F)]

findMe(as.matrix(cortabPA),min(cPA))
findMe(as.matrix(cortabPA),max(cPA))

quantile(abs(cPA),c(0.25,0.75))

#LOG-ODDS
cLO<-cortabLO[upper.tri(cortabLO,diag=F)]

findMe(as.matrix(cortabLO),min(cLO))
findMe(as.matrix(cortabLO),max(cLO))

quantile(abs(cLO),c(0.25,0.75))

#Delta
cD<-cortabD[upper.tri(cortabD,diag=F)]

findMe(as.matrix(cortabD),min(cD))
findMe(as.matrix(cortabD),max(cD))

cortabNB
cortabPA

quantile(abs(cLO),c(0.25,0.75))




#Draw correlation plots
char.col<-c("#228822","#663388","#006688")
groupn<-c(4,11,6,3)

left<--8
top<-25

corrplot.mixed(cortabNB, upper = "ellipse",tl.pos="lt",tl.col=rep(char.col,groupn[1:3]),number.cex = .7)
text(left,top,"Non-biological\nfathers",cex=1.2,pos=4)

nc<-0.5

w<-16

tiff("Figure_S3_corr1.tif",width=w,height=2*w,units="cm",res=600,compression="lzw")

par(mfrow=c(2,1))

corrplot.mixed(cortabNB, upper = "ellipse",tl.pos="lt",number.cex = nc,tl.col=rep(char.col,groupn[1:3]))
text(left,top,"Non-biological\nfathers",cex=1.2,pos=4)

corrplot.mixed(cortabPA, upper = "ellipse",tl.pos="lt",number.cex = nc,tl.col=rep(char.col,groupn[1:3]))
text(left,top,"Partners",cex=1.2,pos=4)

dev.off()

tiff("Figure_S4_corr2.tif",width=w,height=2*w,units="cm",res=600,compression="lzw")
par(mfrow=c(2,1))

corrplot.mixed(cortabLO, upper = "ellipse",tl.pos="lt",number.cex = nc,tl.col=rep(char.col,groupn[1:3]))
text(left,top,"Sensitive\nperiod\nlog-odds",cex=1.2,pos=4)

corrplot.mixed(cortabD, upper = "ellipse",tl.pos="lt",number.cex = nc,tl.col=rep(char.col,groupn[1:3]))
text(left,top,"Relative\nsimilarity\nt value",cex=1.2,pos=4)

dev.off()


