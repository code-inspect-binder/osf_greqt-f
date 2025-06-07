#Tart from the raw data and prepare a nice file with proper names etc

#Important function that returns n characetrs from the right end of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#load the data
data<-read.table("rawdata.txt",header=T,sep="\t",stringsAsFactors = F)

#Give code to each participant
data$code<-formatC(1:nrow(data),width=4,flag=0)

#fix some names of personality measures to suit the rest of the algorithm
fixnames<-names(data)[substr(names(data),1,4)=="Part"&substr(names(data),1,7)!="Partner"]
names(data)[substr(names(data),1,4)=="Part"&substr(names(data),1,7)!="Partner"]<-gsub("Part","Partner",fixnames)

names(data)[substr(names(data),1,7)=="Partner"]

#Change a few names for easy labeling 
#first define a function

changeName<-function(data,string,replace){
  fixnames<-names(data)[substrRight(names(data),nchar(string))==string]
  print(fixnames)
  
  names(data)[substrRight(names(data),nchar(string))==string]<-paste(substr(fixnames,1,nchar(fixnames)-nchar(string)),replace,sep="")
  
  return(data)
}

labels.final<-c(
  "Residence_size",
  "Education",
  "Religion",
  "Policy",
  "Wealth",
  "Profession",
  "Body_height",
  "Body_weight",
  "Attractiveness",
  "Masculinity",
  "Eye_colour",
  "Eye_colour_lightXdark",
  "Hair_colour",
  "Facial_masculinity",
  "Beardedness",
  "Muscularity",
  "BMI",
  "Relative_height",
  "Hirsuteness",
  "Extraversion",
  "Agreeableness",
  "Conscientiousness",
  "Emotional_stability",
  "Openness",
  "Dominance"
)

names(data)

data<-changeName(data,"residence","Residence_size")
data<-changeName(data,"education","Education")
data<-changeName(data,"religion","Religion")
data<-changeName(data,"policy","Policy")
data<-changeName(data,"wealth","Wealth")
data<-changeName(data,"profession","Profession")
data<-changeName(data,"height","Body_height")
data<-changeName(data,"weight","Body_weight")
data<-changeName(data,"attractiveness","Attractiveness")
data<-changeName(data,"masculinity","Masculinity")
data<-changeName(data,"eye_colour","Eye_colour")
data<-changeName(data,"eye_colour_dark_light","Eye_colour_lightXdark")
data<-changeName(data,"hair_colour","Hair_colour")
data<-changeName(data,"hair_colour_natural","Hair_colour_natural")
data<-changeName(data,"facial_Masculinity","Facial_masculinity")
data<-changeName(data,"beard","Beardedness")
data<-changeName(data,"muscularity","Muscularity")
data<-changeName(data,"BMI","BMI")
data<-changeName(data,"relative_Body_height","Relative_height")
data<-changeName(data,"body_hair","Hirsuteness")
data<-changeName(data,"Extraversion","Extraversion")
data<-changeName(data,"Agreebleness","Agreeableness")
data<-changeName(data,"Conscientiousness","Conscientiousness")
data<-changeName(data,"Emotional_stability","Emotional_stability")
data<-changeName(data,"Oppeness","Openness")
data<-changeName(data,"IPIP","Dominance")


#Check, if it worked
names(data)

#Change the code fro constant 4digit-length string
data<-data[-1,]
data$code<-formatC(1:nrow(data),width=4,flag=0)


#Reverse code some data columns base on how the data was coded in the collection
revcode<-function(x,max){
  return(max-x+1)
}

#residence size
summary(as.numeric(data$Biol_Residence_size))
summary(as.numeric(data$Nonbiol_Residence_size))
summary(as.numeric(data$Partner_Residence_size))

cbind(data$Biol_Residence_size,revcode(as.numeric(data$Biol_Residence_size),8))

data$Biol_Residence_size<-revcode(as.numeric(data$Biol_Residence_size),8)
data$Nonbiol_Residence_size<-revcode(as.numeric(data$Nonbiol_Residence_size),8)
data$Partner_Residence_size<-revcode(as.numeric(data$Partner_Residence_size),8)

#Wealth
summary(as.numeric(data$Biol_Wealth))
summary(as.numeric(data$Nonbiol_Wealth))
summary(as.numeric(data$Partner_Wealth))

data$Biol_Wealth<-revcode(as.numeric(data$Biol_Wealth),5)
data$Nonbiol_Wealth<-revcode(as.numeric(data$Nonbiol_Wealth),5)
data$Partner_Wealth<-revcode(as.numeric(data$Partner_Wealth),5)

#Eye Colour (eventually we treat it as a numeric variable instead of categorical for easier comparison with other analysis, 1 is grey, 2 blue, 3 green, 4 brown, 5 black, we therfore need to reverse-code it as well, to keep the dark eyes as high values)
summary(as.factor(data$Biol_Eye_colour))
summary(as.factor(data$Nonbiol_Eye_colour))
summary(as.factor(data$Partner_Eye_colour))

data$Biol_Eye_colour<-revcode(as.numeric(data$Biol_Eye_colour),5)
data$Nonbiol_Eye_colour<-revcode(as.numeric(data$Nonbiol_Eye_colour),5)
data$Partner_Eye_colour<-revcode(as.numeric(data$Partner_Eye_colour),5)


#Similarly with hair colour - for insufficient conversion to the darkness scale, we omitted bald people and people with grey hair. We connected red and ginger hair into a signle category. Now the numbers are as folowing: 1-blonde, 2-red/ginger, 3-light brown, 4-dark brown, 5-black

summary(as.factor(data$Biol_Hair_colour))
summary(as.factor(data$Nonbiol_Hair_colour))
summary(as.factor(data$Partner_Hair_colour))

data$Biol_Hair_colour<-as.factor(data$Biol_Hair_colour)
levels(data$Biol_Hair_colour)<-c("5","4","3","2","2","1",NA,NA)
data$Biol_Hair_colour<-as.character(data$Biol_Hair_colour)

data$Nonbiol_Hair_colour<-as.factor(data$Nonbiol_Hair_colour)
levels(data$Nonbiol_Hair_colour)<-c("5","4","3","2","1",NA,NA)
data$Nonbiol_Hair_colour<-as.character(data$Nonbiol_Hair_colour)

data$Partner_Hair_colour<-as.factor(data$Partner_Hair_colour)
levels(data$Partner_Hair_colour)<-c("5","4","3","2","2","1",NA,NA)
data$Biol_Partner_Hair_colour<-as.character(data$Partner_Hair_colour)



#Facial masculinity
summary(as.numeric(data$Biol_Facial_masculinity))
summary(as.numeric(data$Nonbiol_Facial_masculinity))
summary(as.numeric(data$Partner_Facial_masculinity))

data$Biol_Facial_masculinity<-revcode(as.numeric(data$Biol_Facial_masculinity),7)
data$Nonbiol_Facial_masculinity<-revcode(as.numeric(data$Nonbiol_Facial_masculinity),7)
data$Partner_Facial_masculinity<-revcode(as.numeric(data$Partner_Facial_masculinity),7)

nrow(data)

plot(density(as.numeric(data$Resp_age),na.rm=T))

d<-data[!is.na(data$Nonbiol_Residence_size),]
nrow(d)

data<-data[data$Resp_age<=45,]
d<-data[!is.na(data$Nonbiol_Residence_size),]
nrow(d)


#Write the table
write.table(data,"data.txt",sep="\t",row.names=F)
