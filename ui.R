
# The user-interface (ui) script controls the layout and appearance of your app. 

library(shiny)
library(leaflet)
library(shinyjs)
library(DT)
library(tidyverse)
# library(Jim)

# currentdir <- getwd()
# setwd(currentdir)
setwd("D:/FVM/Regen_viz/R")
source("SE.r")
source("JC_functions.r")
source("Theme.r")  
source("seedlingcalc.r")
windowsFonts(nps = windowsFont('Frutiger LT Std 55 Roman'))  # set font to use in the graphics to be Frutiger     
setwd("D:/FVM/Regen_viz")

# source("./R/seedlingdata/seedlingcalc.r")
source("LoadData.R")

# ParkList<-c("Appomattox Courthouse NHP","Booker T. Washington", "Eisenhower", "Fredericksburg Spotsylvania MP", "Gettsyburg","Hopewell Furnace" ,"Petersburg Battlefield MP","Richmond Battlefield MP")
# ParkList<-c("MIDN","APCO", "BOWA","GETT")
CommunityList<-c("Yes","No")
year="MIDNFVM"
DataTypeList=c("VS","deer")
LabelList=c("Yes","No")

ParkList<-unique(seedlingdata$Unit_ID)
ParkList<-droplevels(ParkList)
ParkList <- c("MIDN",levels(ParkList))
ParkListDeer <- c("FRSP", "PETE","RICH")

GenusList<-c("Yes","No")

shinyUI(navbarPage(title=HTML("<div> <a href='https://science.nature.nps.gov/im/units/midn/'> <img src='ah_small_black.gif',
          alt='Forest Regeneration Visualizer'> </a> Forest Regeneration Visualizer</div>"),position = "static-top", inverse=TRUE, collapsible = FALSE, fluid=TRUE, 
           windowTitle = "MIDN Forest Regeneration Visualizer", id="MainNavBar",
           
######################################### Regeneration Panel ####################################################################
           
tabPanel(title="Forest Regeneration Data",
         #style="padding: 0",
                    useShinyjs(),
         div(class="outer",
             #tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
              #puts up icon on tab
            #, tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
         ),
         
fluidPage(
  sidebarPanel(
    h4("Plot summary stats from each sampled site within a park."),
    br(),
    tags$div(title="Choose the park you want to work with",selectInput(inputId='park', 
             label='Select Park', choices= ParkList, selected = "MIDN")),
   tags$div(title="Do you want to select community?",selectInput(inputId='community',
             label='Select community', choices= CommunityList, selected = "Yes")),
   # tags$div(title="Choose the metric you want to work with",selectInput(inputId='z', label='Select metric to plot', choices=VarList, selected = "State Multi-metric")),
   tags$div(title="Do you want to label the graph?",selectInput(inputId='label', 
             label='Select label', choices= LabelList, selected = "No")),
   tags$div(title="Select by Genus",selectInput(inputId='genus', 
             label='Genus', choices= GenusList, selected = "No")),
   
    br(),
    p(""),
    br(),
    img(src = "photo1.jpg", height = 240, width = 240),
    br(),
   br(),  br(), 
    p("For further information about this sampling protocol, visit the ", 
    a("MIDN protocol page.", href= "https://www.nps.gov/im/midn/forest-vegetation-coastal-plain-piedmont-parks.htm")),
    br()
    ),

    mainPanel(plotOutput("plot"))
  )
),  ## end of regen page

######################################## Deer Exclosure Panel ##########################################################

tabPanel(title="Deer Exclosure", 
                    
                    useShinyjs(),

shinyUI(fluidPage(
  sidebarPanel(
    h4("Deer exclosure comparison data."),
    tags$div(title="Choose the park", selectInput(inputId='park2', label='Select Park', choices= ParkListDeer, selected = "FRSP")),
    br(),
    p("For further information about this sampling protocol, visit the ", 
      a("MIDN protocol page.", href= "https://www.nps.gov/im/midn/forest-vegetation-coastal-plain-piedmont-parks.htm")),
    br()
  ),
  
  mainPanel(plotOutput("plot2"))
    )
   )
  )

)#end navbarPage()
)