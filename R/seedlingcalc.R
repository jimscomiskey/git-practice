# Script to create seedling calc to be used by other scripts for dynamics analysis
# from MIDN Forest Vegetation Monitoring 
# James Comiskey
# ver.1.0 2/17/2012
# Updated by Aaron Weed 2/2016
#
# UPdated by James Comiskey 11/05/2016
# Change made as these are now in MIDNSeedlingfunc.r:
#     Removed portion dealining with dates
#     NAs reaplaced by zeros
#     Cycles and COLO specific cycles


# Extracts data from database (or saved file if altered), and calculates regeneration scores for seedlingsa
# and saplings. 
# 
Seedling_Scores <- function(year) {
  source("MIDNseedlingfunc.R")
  source("MIDNsapfunc.R")
  
  clusters <- read.csv("clusters.csv",header=TRUE)  # Import data to work with
  clusternames <- read.csv("cluster_names.csv",header=TRUE)
  #clusters <- merge(clusters,clusternames, by="clusters",in.x=TRUE)
  sppregen <- read.csv("spp_regen.csv",header=TRUE)  # Import data to work with
  sppregen<-subset(sppregen, regen !="N")
  
  # Extract Seedling data from database 
  seedlingdata<-MIDNseedlingdata(year)
  # save(seedlingdata,file="seedlingdata.rda")
  #load(file="seedlingdata.rda")
  
  # Calcualte seedling scores
  seedlingdata<-Seedlingcalc(seedlingdata)
  
  # aggregate seedling data by:
  # "Sample_Year", "Panel", "Unit_ID", "Plot_Number", "Genus", "Latin_Name", "Census", "Cover","Score"
  seedlingdata <- Seedling_aggregation(seedlingdata)
  head(seedlingdata)
  
  
  # 
  #######
  ####### Include sapling data in the analysis
  #######
  ###### Create saplings data ----
  saplings<-MIDNsapdata(year)# import saplings
  # save(saplings,file="saplings.rda")
  #load(file="saplings.rda")
  
  
  # Clean up the saplings so as not to include any dead or excluded
  status<-c("AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
  #
  saplings<-saplings[saplings$Status_ID %in% status,]
  saplings$Status_ID<-droplevels(saplings$Status_ID)
  levels(saplings$Status_ID)
  
  
  ## Create saplings score field 
  # saplings are measured in 3 x 3m radius microplots (84.823 m2)
  # adjust to score per m2
  # 
  saplings$Score<-  50/(pi*3^2*3)
  
  
  # Merge the sapling data with the seedlingdata
  #
  # Create same table structure as seedlindata
  nms <- names(seedlingdata)                # Identify columns in seedlindata 
  Missing <- setdiff(nms,names(saplings))   # Find names of columns not in the saplings data
  saplings[Missing] <-NA                    # Add them, filled with '0's
  temp<-saplings[nms]                       # create temp frame with only the sapling variables present in seedlingdata
  names(temp);names(seedlingdata)
  seedlingdata<-rbind(seedlingdata,temp)
  
  
  # select species that will be used in calc of regen
  seedlingdata<-seedlingdata[seedlingdata$Latin_Name %in% sppregen$spp,]
  names(seedlingdata)
  
  ## Change around some genus names
  #levels(as.factor(seedlingdata$Genus))
  seedlingdata$Genus[seedlingdata$Genus =="Quercus (Red group)"  ] = "Quercus"
  seedlingdata$Genus[seedlingdata$Genus =="Quercus (White group)"  ] = "Quercus"
  droplevels(as.factor(seedlingdata$Genus))

  seedlingdata
}

# Creates graph of seedling data by park or community type based on defined variables
# 
Seedling_Scores_Graph <- function(park, community,data_type) {
  if (exists("y1")){
    rm(y1)}
  
  
  # Restrict data to data type
  seedlingdata<-seedlingdata[seedlingdata$Loc_Type %in% data_type,]
  
  
  #Create table of scores per plot
  seedlingdataplot<-with(seedlingdata,(aggregate(seedlingdata$Score, by=list(Sample_Year, Unit_ID, Plot_Number, Census), FUN=sum))) 
  colnames(seedlingdataplot)<-c("Sample_Year","Unit_ID","Plot_Number","Census", "Score")
  head(seedlingdataplot)
  
  
  ############## START THE PLOTS ############
  ## PLot average regen per park and netowrk in each census
  if (park == "MIDN" & community =="No"){
    # calcualte network mean and park mean scores for different years.
    seedlingdata.parks <- summarySE(seedlingdataplot, measurevar="Score", groupvars=c("Census","Unit_ID"))
    seedlingdata.network <- summarySE(seedlingdataplot, measurevar="Score", groupvars=c("Census"))
    seedlingdata.network$Unit_ID <-"MIDN"
    # merge both tables
    seedlingdata.all <- rbind(seedlingdata.parks,seedlingdata.network); head(seedlingdata.all) 
    # endusure that MIDN appears first in graph
    seedlingdata.all$Unit_ID <- relevel(seedlingdata.all$Unit_ID, "MIDN")   
    # Create graph
    y1<-ggplot(seedlingdata.all, aes(x=Unit_ID, y=Score, fill=Census))+ 
        labs(x = NULL, y = (bquote('Regeneration Score ('*~ m^-2~')')))+
        geom_bar(stat="identity", width=.9,position=position_dodge())+ 
        geom_bar(stat="identity", colour="black", show.legend=FALSE, width=.9,position=position_dodge())+ # map again to add boundary to bar but not caption (http://wiki.stdout.org/rcookbook/Graphs/Legends%20%28ggplot2%29/)
        geom_hline(aes(yintercept=2), colour="red", linetype="dashed", size=.75)+
        geom_hline(aes(yintercept=8), colour="green", linetype="dashed", size=.75)+
        # annotate("text", x = 19.5, y = 12, label = "", colour = "black",size = 3, family="F") +
        # annotate("text", x = 18.5, y = 1, label = "Significant \nConcern", colour = "black",size = 3, ) +
        # annotate("text", x = 18.3, y = 5, label = "Caution", colour = "black",size = 3) +
        # annotate("text", x = 18.3, y = 10, label = "Good", colour = "black",size = 3) +
      scale_fill_manual(values=c("Black", "#9EB882", "#386319","#B5C688"),
                         name="Year")+  #breaks=c("Mortality Network", "", "",""), els=c("A", "B", 
        geom_errorbar(aes(ymin=Score, ymax=Score+se),
                      width=.6,                    # Width of the error bars
                      position=position_dodge(.9)) ;y1                
       y1<- (y1+ MIDNTheme(base_family="nps") + theme(legend.position = "right", legend.title=element_blank())) ;y1  
  
  }          
  
  
  #################################
  # Regen by forest community type by park or across network
  #
  ###############################
  #
  # By park
  #
  if (community == "Yes"){
    seedlingdataplot <- merge(seedlingdataplot,clusters, by="Plot_Number",in.x=TRUE)
    head(seedlingdataplot)
    #Create summary for network
    temp <- summarySE(seedlingdataplot, measurevar="Score", groupvars=c("Census", "names", "order")) 
    temp$Unit_ID="MIDN"    ; temp
    #Create blank frame for park
    temp2<-temp
    temp2[,c(4,5,6,7,8)] <-0  
    
    # Subsets data to either the network or a park
    if (park != "All"){
      temp2$Unit_ID<-park    ;temp2
    }
    
    # Create summary for park
    temp1 <- summarySE(seedlingdataplot, measurevar="Score", groupvars=c("Census", "names", "order", "Unit_ID"))  ; temp1
    if (park != "All"){
      temp1 <- temp1[temp1$Unit_ID == park,] # subset selected park
      }
    # merge blank frame and park data to create zeros
    names(temp);names(temp2)
    temp1<-rbind(temp1,temp2)
    temp2<-with(temp1,(aggregate(temp1[,c("N","Score","sd","se", "ci")], by=list(Census, names, order,Unit_ID), FUN=sum)))  ;head(temp2)
    names(temp2)[1:4] <- c("Census","names", "order", "Unit_ID")
    # merge park and network data
    seedlingdata.all<- rbind(temp,temp2); head(seedlingdata.all)
    seedlingdata.all$Unit_ID<-as.factor(seedlingdata.all$Unit_ID)
    if (park != "All"){
      seedlingdata.all$Unit_ID <- relevel(seedlingdata.all$Unit_ID, park)
    }else{
      seedlingdata.all$Unit_ID <- relevel(seedlingdata.all$Unit_ID, "MIDN") 
    }
    seedlingdata.all
    }
  
  if (park != "All") {
    a="Low deer \ndensity"
    b="High deer \ndensity"
  }else{a="";b=""}
  
  #
  
  if (park!="MIDN" & community == "No"){
    text = paste("\n   When plotting by park, please select 'Yes'\n",
                  "             for community type. \n",
                 "\n",
                 "       Parks summaries can only by displayed by community.")
    y1<-ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = text) + 
      MIDNTheme_blankplot()
    
  }
  
  if (park!="MIDN" & community == "Yes"){
    # Create graph
    y1<- ggplot(seedlingdata.all, aes(x=names, y=Score,fill=Census))+ labs(x = NULL, y = "Score") +
        geom_bar(stat="identity", width=.9,position=position_dodge()) +
        geom_bar(stat="identity", colour="black", show.legend=FALSE, width=.9,position=position_dodge())+ # map again to add boundary to bar but not caption (http://wiki.stdout.org/rcookbook/Graphs/Legends%20%28ggplot2%29/)
        geom_hline(aes(yintercept=2), colour="red", linetype="dashed", size=.75)+
        geom_hline(aes(yintercept=8), colour="green", linetype="dashed", size=.75)+
        # annotate("text", x = 10, y = 12, label = "", colour = "black",size = 3) +
        # annotate("text", x = 9.5, y = 2, label = a, colour = "black",size = 3) +
        # annotate("text", x = 9.5, y = 8, label = b, colour = "black",size = 3) +
        scale_fill_manual(values=c("Black", "#9EB882", "#386319","#B5C688"),name="Census") +  
        geom_errorbar(aes(ymin=Score, ymax=Score+se),
                      width=.6,                    # Width of the error bars
                      position=position_dodge(.9))            
        if (park!="All"){
          # y1<- y1+geom_text(aes(x=9.5, y=2, label = "Low deer \ndensity"),colour = "black",size=3)+
          #       geom_text(aes(x=9.5, y=8, label = "High deer \ndensity"),colour = "black",size=3)
          y1<-(y1+facet_grid(Unit_ID~., scales="free", space="fixed")+ MIDNTheme() + theme(legend.position = "right", legend.title=element_blank()))  
        }else{ # for all parks as a grid
          y1<-(y1 + facet_wrap( ~Unit_ID, ncol=3, scales = "free_y")+ MIDNTheme() + theme(legend.position = "right", legend.title=element_blank()))  
        }
          y1
  }    
    
  if (park == "MIDN" & community =="Yes"){
    seedlingdata.all<-subset(seedlingdata.all,Unit_ID=="MIDN") 
    seedlingdata.all$names<-reorder(seedlingdata.all$names, seedlingdata.all$order)          
    y1<- ggplot(seedlingdata.all, aes(x=names, y=Score,fill=Census))+ labs(x = NULL, y = "Score") +
        geom_bar(stat="identity", width=.9,position=position_dodge()) +
        geom_bar(stat="identity", colour="black", show.legend=FALSE, width=.9,position=position_dodge())+ # map again to add boundary to bar but not caption (http://wiki.stdout.org/rcookbook/Graphs/Legends%20%28ggplot2%29/)
        geom_hline(aes(yintercept=2), colour="red", linetype="dashed", size=.75)+
        geom_hline(aes(yintercept=8), colour="green", linetype="dashed", size=.75)+
        # annotate("text", x = 10, y = 12, label = "", colour = "black",size = 3) +
        # annotate("text", x = 9.5, y = 2, label = "Low deer \ndensity", colour = "black",size = 3) +
        # annotate("text", x = 9.5, y = 8, label = "High deer \ndensity", colour = "black",size = 3) +
        scale_fill_manual(values=c("Black", "#9EB882", "#386319","#B5C688"),
                          name="Year")+  #breaks=c("Mortality Network", "", "",""), els=c("A", "B", 
        geom_errorbar(aes(ymin=Score, ymax=Score+se),
                      width=.6,                    # Width of the error bars
                      position=position_dodge(.9))            
        y1<-(y1+ MIDNTheme() + theme(legend.position = "right", legend.title=element_blank())) 
        y1
        }
  
  y1
  
}


# Creates graph of seedling data by genus for a predifined park
# 
Seedling_Score_Genus <- function(park) {
  # Create plots by genus
  #Create table of scores per plot
  seedlingdataplot<-with(seedlingdata,(aggregate(seedlingdata$Score, by=list(Sample_Year, Panel, Unit_ID, Plot_Number, Genus, Census), FUN=sum))) 
  colnames(seedlingdataplot)<-c("Sample_Year","Panel","Unit_ID","Plot_Number", "Genus","Census", "Score")
  head(seedlingdataplot)
  
  # Genera to plot
  genera<-c("Acer","Carya","Fagus", "Fraxinus", "Liriodendron", "Quercus")
  seedlingdata<-seedlingdata[seedlingdata$Genus %in% genera,]
  seedlingdata$Genus <- droplevels(seedlingdata$Genus)
  levels(seedlingdata$Genus)
  
  seedlingdata_genus <- summarySE(seedlingdata, measurevar="Score", groupvars=c("Sample_Year", "Census","Panel", "Unit_ID", "Genus"))
  x<-subset(seedlingdata_genus, Unit_ID == park)
  
  
  # *************************
  # The following is needed to ensure that all parks have six plots ilustrrating the six genera even 
  # in cases when one of the genera are not present in the park.
  # 
  # 
  # with an existing data.frame called "x".
  #
  # create a one-row matrix the same length as x
  temprow <- matrix(c(rep.int(NA,length(x))),nrow=1,ncol=length(x))
  
  # make it a data.frame and give cols the same names as data
  newrow <- data.frame(temprow)
  colnames(newrow) <- colnames(x)
  
  #Create list of columns minus genus
  a<- colnames(x);a
  b= subset(a, !(a %in% "Genus"));b
  
  # Create table of genera 
  x1=expand.grid(genera)
  colnames(x1) <- "Genus";x1
  # add extra columns (excluding genus)
  x1[,b] <- NA;x1
  x1$Sample_Year = 2016
  # x$Sample_Year[x$Sample_Year == NA] <- 2016
  # add to the original data
  x <- rbind(x,x1)
  # **********************************
  
  x$Panel<-as.factor(x$Panel)
   
  y1<-ggplot(x, aes(x=Sample_Year, y=Score, fill= Panel)) + labs(y = "Avg. regeneration score + SE", x= "") +
    geom_bar(stat="identity", show.legend=TRUE, width=.9)+
    #scale_y_continuous(limits = c(0, mean+se)) +
    geom_errorbar(aes(ymin=Score, ymax=Score+se),
                  width=.6)+
    scale_fill_manual(breaks = c ("1", "2", "3","4"), values=c("#3E71C2","cornflowerblue", "cadetblue3","skyblue"),name="Panel")  
  y1 
  y1<-(y1+facet_wrap(~Genus, nrow=2) +  
         theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12 * 0.8))+
         theme(axis.text.x = element_text(angle = 90, face= "bold", vjust=0,size = 12 * 0.8)) +
         theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
         geom_hline(aes(yintercept=2), colour="black", linetype="dashed", size=.75)+
         geom_hline(aes(yintercept=8), colour="red", linetype="dashed", size=.75)+
         theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
         theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
         theme(panel.background =  element_rect(fill="white", colour="black")) +
         theme(panel.grid.major = element_line(colour = "grey90"))+
         ggtitle(paste("Regeneration scores of canopy tree genera ", park)) +
         theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
         theme(strip.background= element_rect(size=10, color="lightblue")))
  y1
}
