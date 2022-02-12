# Code to generate seedling regeration graphics
# J Comiskey 01/23/2017
# v1.01

currentdir <- getwd()
setwd(currentdir)
setwd("D:/FVM/2016/_Regen")
library(ggplot2)
library(reshape)
library(plyr) # used to sort by a field using arrange
source("SE.r")
source("JC_functions.r")
source("Theme.r")  
source("seedlingcalc.r")
windowsFonts(nps = windowsFont('Frutiger LT Std 55 Roman'))  # set font to use in the graphics to be Frutiger     

# Create graph of seedling regeneration score 
# Park can be: 
#   MIDN to include all parks, or 
#   park code to plot that parks by community type
# Community can be 
#   "Y" to plot community type by 
#   park when combined with a park code
#   all parks when combined with MIDNsapMIDN
#   
#   


park = "MIDN"
# park="APCO"
community = "N"
year="MIDNFVM"
data_type="VS"
label="yes"

# Prepare all seedling data for analysis by extracting seedlings and saplings, calculating scores
# 
# ******
# Just neet to run this portion to update seedling data if FVM database has changed
# ******
#   
# seedlingdata <- Seedling_Scores(year)
# save(seedlingdata,file="seedlingdata.rdata")
# 
# Else, load file stored from previous save
load(file="seedlingdata.rdata")
clusters <- read.csv("clusters.csv",header=TRUE)  # Import data to work with
# 
# Scores for all parks or community types; 
# Include VS, Deer, BN or any combination as defined by data_type
# 
y1<-Seedling_Scores_Graph(park, community , data_type);y1

# to add "Good", "Caution", "Significant Concern" labels to graph
if (label == "yes"){
  if (community == "N")
    y1<-y1+annotate("text", x = 19.5, y = 12, label = "", colour = "black",size = 3, family="F") +
      annotate("text", x = 18.5, y = 1, label = "Significant \nConcern", colour = "black",size = 3, ) +
      annotate("text", x = 18.3, y = 5, label = "Caution", colour = "black",size = 3) +
      annotate("text", x = 18.3, y = 10, label = "Good", colour = "black",size = 3)
    y1
  }
  
if (community == "Y"){
    y1<-y1+annotate("text", x = 11, y = 12, label = "", colour = "black",size = 3, family="F") +
      annotate("text", x = 9.3, y = 1, label = "Significant \nConcern", colour = "black",size = 3, ) +
      annotate("text", x = 9.3, y = 5, label = "Caution", colour = "black",size = 3) +
      annotate("text", x = 9.3, y = 10, label = "Good", colour = "black",size = 3)
    y1
  }



# The folllowing is used to restrict the data set to selected parks 
# This is used to show a sbuset of the parks, for example when looking at deer exclosure or other issues
y1<-Seedling_Scores_Graph(park, community , data_type);y1
parks<- c("BOWA","APCO", "MIDN")
parks<- c("GETT", "VAFO","APCO", "MIDN")
parks <- c("FRSP_CHWILD", "FRSP_FRED", "FRSP_SPOT", "PETE_EAST", "RICH","MIDN")
#
y1$data <- y1$data[y1$data$Unit_ID %in% parks,];y1



# The following section produces graphics by genus
# 
# 
data_type<- c("Deer", "VS")
data_type<- c("VS")
data_type<- c("Deer")
# data_type<- c("BN")
park="APCO"
# 
# Scores by genus
# 
y1<-Seedling_Score_Genus(park);y1



# Deer exclosure plot analysis
seedlingdataplot <- read.csv("seedlingdataplot_deer.csv",header=TRUE) 
# seedlingdataplot$x <- paste(seedlingdataplot$Unit_ID,"_", seedlingdataplot$Pair, "_",seedlingdataplot$Sample_Year,sep = "")
seedlingdataplot$x <- paste(seedlingdataplot$Pairplot,"_", seedlingdataplot$Sample_Year,sep = "")
park="FRSP"
#
y1<-graph(park);y1# Create graph
graph <- function(park) {
  data <- seedlingdataplot[seedlingdataplot$Park == park,] 
  y1<-ggplot(data, aes(x=x, y=Score, fill=Fenced))+ labs(x = NULL, y = "Score") +
    labs(x = NULL, y = (bquote('Regeneration Score ('*~ m^-2~')')))+
    geom_bar(stat="identity", width=.9,position=position_dodge())+ 
    geom_bar(stat="identity", colour="black", show.legend=FALSE, width=.9,position=position_dodge())+ # map again to add boundary to bar but not caption (http://wiki.stdout.org/rcookbook/Graphs/Legends%20%28ggplot2%29/)
    geom_hline(aes(yintercept=2), colour="red", linetype="dashed", size=.75)+
    geom_hline(aes(yintercept=8), colour="green", linetype="dashed", size=.75)+
    scale_fill_manual(values=c("Black", "#9EB882", "#386319","#B5C688"))+
    ggtitle(park)
  y1<- (y1+ MIDNTheme(base_family="nps") + theme(legend.position = "right", legend.title=element_blank())) ;y1  

 }

# The folllowing section was used to add vertical bars to specific graphs 
# 
# For RICH 
y1+geom_vline(xintercept = 5.5, size=2)+geom_vline(xintercept = 9.5, size=2)
# for FRSP 
y1+geom_vline(xintercept = 5.5, size=2)+geom_vline(xintercept = 10.5, size=2)+geom_vline(xintercept = 14.5, size=2)
# For PETE  
y1+geom_vline(xintercept = 4.5, size=2)+geom_vline(xintercept = 7.5, size=2)
