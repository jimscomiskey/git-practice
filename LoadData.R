# Source file for loading required data sets

# load(file="./Data/seedlingdata.rdata")
clusters <- read.csv("./Data/clusters.csv",header=TRUE)  # Import data to work with
seedlingdataplot <- read.csv("./Data/seedlingdataplot_deer.csv",header=TRUE) 
seedlingdataplot$x <- paste(seedlingdataplot$Pairplot,"_", seedlingdataplot$Sample_Year,sep = "")

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
