#The server.R script contains the instructions that your computer needs to build your app

library(shiny)
library(leaflet)
library(lattice)
library(rgdal)
library(shinyjs)
library(jsonlite,pos=100)
library(httr)
library(dplyr)
library(DT)
library(ggplot2)
library(RColorBrewer)

library(reshape)
library(plyr) # used to sort by a field using arrange

# currentdir <- getwd()
# setwd(currentdir)
setwd("D:/FVM/Regen_viz/R")
source("SE.r")
source("JC_functions.r")
source("Theme.r")  
source("seedlingcalc.r")
windowsFonts(nps = windowsFont('Frutiger LT Std 55 Roman'))  # set font to use in the graphics to be Frutiger     
setwd("D:/FVM/Regen_viz")

load(file="./Data/seedlingdata.rdata")
clusters <- read.csv("./Data/clusters.csv",header=TRUE)  # Import data to work with



park = "MIDN"
park="APCO"
park="BOWA"
park="GETT"
park="RICH"
community = "Yes"
year="FVM2018"
data_type="VS"
label="Yes"
y1<-Seedling_Scores_Graph(park, community , data_type);y1

rm(y1)

##### Begin Server Function ####

shinyServer(function(input,output){
  
   output$plot <- renderPlot({
    
    # Base plot 
    # if(input$park == "MIDN"){
        y1<-Seedling_Scores_Graph(input$park, input$community , data_type)
    # }
  
    
        
     # to add "Good", "Caution", "Significant Concern" labels to graph
     if (input$label == "Yes"){
       if (input$community == "No"){
         y1<-y1+annotate("text", x = 19.5, y = 12, label = "", colour = "black",size = 3, family="F") +
           annotate("text", x = 18.5, y = 1, label = "Significant \nConcern", colour = "black",size = 5) +
           annotate("text", x = 18.3, y = 5, label = "Caution", colour = "black",size = 5) +
           annotate("text", x = 18.3, y = 10, label = "Good", colour = "black",size = 5)
     }
     
     if (input$community == "Yes"){
       y1<-y1+annotate("text", x = 11, y = 12, label = "", colour = "black",size = 5, family="F") +
         annotate("text", x = 9.3, y = 1, label = "Significant \nConcern", colour = "black",size = 5) +
         annotate("text", x = 9.3, y = 5, label = "Caution", colour = "black",size = 5) +
         annotate("text", x = 9.3, y = 10, label = "Good", colour = "black",size = 5)
      }
     }
     
     
    if(input$genus == "Yes"){
      y1<-Seedling_Score_Genus(input$park)
    }
     
     
     
       print(y1) 
     
    }, height = 800, width = "auto")
 
   
   output$plot2 <- renderPlot({
     
     y1<-graph(input$park2)
     
     # For RICH 
     if (input$park2 == "RICH"){
       y1<-y1+geom_vline(xintercept = 6.5, size=2)+geom_vline(xintercept = 12.5, size=2)
     }
     
     # for FRSP
     if (input$park2 == "FRSP"){
       y1<-y1+geom_vline(xintercept = 5.5, size=2)+geom_vline(xintercept = 10.5, size=2)+geom_vline(xintercept = 15.5, size=2)
     }
    # For PETE
     if (input$park2 == "PETE"){
       y1<-y1+geom_vline(xintercept = 4.5, size=2)+geom_vline(xintercept = 8.5, size=2)
     }
     
     print(y1)
       
     
   } , height = 800, width = "auto")
   
  
 
}) ## end shiny serverfunc    



