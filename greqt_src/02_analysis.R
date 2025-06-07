#Load all the required functions
library(scales)
source("functions1.R")
source("functions2.R")
source("functions_plot.R")

#Load the data
data<-read.table("data.txt",header=T,sep="\t",stringsAsFactors = F)

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

#Indicate which variable should not be treated as continuous (eye and hair colour), eventually we treated all variables as continuous
#cont<-ifelse(char=="Eye_colour"|char=="Hair_colour",F,T) 

set.seed(42)
list.res<-lapply(1:length(char),function(i){doit(char=char[i],runs=10000,continuous=T)})
names(list.res)<-char

#Save the result and load if you run the permutation test on a different computer than the visualizations
save(list.res,file="results.Rdata")
load("results.Rdata")

#Draw basic plots for all characteristics
lapply(list.res,plotFull)

