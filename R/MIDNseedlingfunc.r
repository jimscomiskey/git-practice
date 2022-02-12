## This function connects to the FVM MS Access back end database (FVM)
# Following DB connection steps from http://sandymuspratt.blogspot.com/2013/01/getting-access-data-into-r.html
## The code then queries and outputs seedling data per quadrat, nested within plots
# Produces Quadrat level  tree seedling height class, browse per species

# similar to the table created by Jim: seedling.csv

##created by Aaron S. Weed Version 1; 7/1/2015

#install.packages("RODBC")
MIDNseedlingdata<-function(year){
  library(RODBC)
  con <- odbcConnect(year)# Connect to database that I named under USER DSN in ODBC Data Source Admin
  # examples
  sqlTables(con, tableType = "TABLE")$TABLE_NAME # Access data table names
  sqlColumns(con, "tbl_Disturbances")$COLUMN_NAME#Access column names of specifc table
  
  ###################### Import data and lookup tables used for the query
  # import dataframes of each tables within the DB
  loc <- sqlFetch(con, "tbl_Locations");names(loc)
  park <- sqlFetch(con, "tlu_Park_Names");names(park)
  
  event <- sqlFetch(con, "tbl_Events"); names(event)
  seed<-sqlFetch(con,"tbl_Quadrat_Seedling_Data");names(seed)
  
  plants<- sqlFetch(con, "tlu_Plants"); names(plants)
  
  odbcClose(con)
  ##########################
  library(plyr)
  # assign taxonomy to saplings
  intersect(names(seed),names(plants))
  temp<-join(seed, plants, by="TSN");names(temp)
  
  # append park data to location data
  loc2<-join(park,loc, by="Unit_ID")
  
  # append event data to location data
  intersect(names(loc2),names(event))
  temp1<-join(loc2,event, by="Location_ID")
  
  # append event data t oquantitative seedling data
  intersect(names(temp1),names(temp))
  temp2<-join(temp1,temp, by="Event_ID")
  names(temp2)
  
  ### Select data relevant to MIDN, remove QAQC plots, VS plots only
  temp3<-temp2[temp2$Network %in% "MIDN",]
  
  # remove QAQC plot (VALUES =1)
  noerrors<-"0"
  temp4<-temp3[temp3$Event_QAQC %in% noerrors,]
  
  #
  # The following section modified so that it includes all data
  # 
  # Access data from vital signs monitoring (VALUES = VS) 
  levels(temp4$Loc_Type)
  #loctype= data_type
  temp5<-temp4 #[temp4$Loc_Type %in% loctype,]
  names(temp5)
  
  #remove any NA records in TSN
  temp5<-temp5[!temp5$Latin_Name == "No species recorded",]
  names(temp5)
  
  final_seed<-temp5[c("Start_Date","Panel","Unit_ID", "Unit_Code","Subunit_Code" , "Plot_Number","Quadrat",
                      "TSN", "Genus","Latin_Name", "Common", "Indicator_MIDN" ,"Seedlings_Less_5cm",
                      "Seedlings_5_15cm","Seedlings_15_30cm", "Seedlings_30_100cm" ,"Seedlings_100_150cm","Seedlings_Above_150cm" ,"Cover" ,"Browsed",
                      "Deer_Browse_Indicator","Loc_Type")]
  final_seed
  
  
  ####Create vars for JPs package
  #year variable and variable indicating number of times resampled ('cycle')
  final_seed$Start_Date<-gsub("00:00:00","",final_seed$Start_Date) #Remove time
  final_seed$Start_Date<-as.Date(final_seed$Start_Date, format= "%Y-%m-%d") #convert to date
  final_seed$Sample_Year<-as.factor(format(final_seed$Start_Date,"%Y")) #Create year variable
  
  s1<-seq(2007,2010, by=1);s2<-seq(2011,2014, by=1);s3<-seq(2015,2018, by=1)## generate seqs of years to index dataframe
  final_seed$Cycle[final_seed$Sample_Year %in% s1]=1;final_seed$Cycle[final_seed$Sample_Year %in% s2]=2;final_seed$Cycle[final_seed$Sample_Year %in% s3]=3
  seedling<-final_seed
  rm(final_seed)
  
  # force COLO parks to first census
  exparks<-c("COLO_JAMES","COLO_YORK")
  # *****************
  # JC 10/15/2016
  # COLO 2011-2014 = cycle 1, 2015-2018 = Cycle 2
  # ****************
  seedling$Cycle[(seedling$Unit_ID %in% exparks & seedling$Sample_Year == '2011') | 
                   (seedling$Unit_ID %in% exparks & seedling$Sample_Year == '2012') |
                   (seedling$Unit_ID %in% exparks & seedling$Sample_Year == '2013') |
                   (seedling$Unit_ID %in% exparks & seedling$Sample_Year == '2014')] = 1
  seedling$Cycle[(seedling$Unit_ID %in% exparks & seedling$Sample_Year == '2015') | 
                   (seedling$Unit_ID %in% exparks & seedling$Sample_Year == '2016') |
                   (seedling$Unit_ID %in% exparks & seedling$Sample_Year == '2017') |
                   (seedling$Unit_ID %in% exparks & seedling$Sample_Year == '2018')] = 2
  
  seedling$Cycle<-as.factor(seedling$Cycle)
  
  # JC 11/04/2016
  # The following sections were inserted from regne.r
  # 
  # Reaplce nas to zero
  seedling$Seedlings_Less_5cm[is.na(seedling$Seedlings_Less_5cm)]<- 0
  seedling$Seedlings_5_15cm[is.na(seedling$Seedlings_5_15cm)]<- 0
  seedling$Seedlings_15_30cm[is.na(seedling$Seedlings_15_30cm)]<- 0
  seedling$Seedlings_30_100cm[is.na(seedling$Seedlings_30_100cm)]<- 0
  seedling$Seedlings_100_150cm[is.na(seedling$Seedlings_100_150cm)]<- 0
  seedling$Seedlings_100_150cm[is.na(seedling$Seedlings_100_150cm)]<-0
  seedling$Seedlings_Above_150cm[is.na(seedling$Seedlings_Above_150cm)]<-0
  
  
  # *************
  # JC 10/15/2016
  # Something similar needs to be done for the other NCRN parks (GEWA, THST, SAHI) as these started in 2008 so they
  # are out of sequence with the remainder
  # *************
  
  # ***********************************
  # unique(seedling[,c("Unit_ID","year","cycle")])
  # 11/04/2016 JC 
  # ***************
  # no longer needed 
  # seedling$cycle<-as.factor(seedling$cycle)
  #****************
  seedling$Unit_ID<-droplevels(seedling$Unit_ID);levels(seedling$Unit_ID)# check that all parks are included
  
  ##### Calc regeneration score of canopy seedlings by Plot----
  ## convert years to cycles
  seedlingdata<-seedling
  # JC Changed this to be Census
  #seedlingdata$year<-as.character(seedlingdata$year)
  seedlingdata$Census <- ""
  seedlingdata$Census[seedlingdata$Cycle==1] = "Census 1"
  seedlingdata$Census[seedlingdata$Cycle==2] = "Census 2"
  seedlingdata$Census[seedlingdata$Cycle==3] = "Census 3"
  
  seedlingdata
  
}


# Function to calculate the seedling scores and convert caver category to mid-point percent
# Comiskey 11/06/2016
# 
Seedlingcalc <- function(seedlingdata) {
  ## Published Scores are based on sampling in 2-m cirular plots (area= 12.56) so score from a MIDN quad needs to be scaled
  seedlingdata$Score <- (seedlingdata$Seedlings_5_15cm)+(seedlingdata$Seedlings_15_30cm)+ (seedlingdata$Seedlings_30_100cm*2) + (seedlingdata$Seedlings_100_150cm*20)+(seedlingdata$Seedlings_Above_150cm*50)
  # Original error in Aaron Weed calculations was as follows and included in presentatio to technical advisory committee in 2016
  # seedlingdata$Score <- (seedlingdata$Seedlings_5_15cm)+(seedlingdata$Seedlings_15_30cm*2)+ (seedlingdata$Seedlings_30_100cm*20) + (seedlingdata$Seedlings_100_150cm*50)+(seedlingdata$Seedlings_Above_150cm*50)
  
  # *********************
  #   JC 10/15/2016
  # Need to convert cover values to actual percentages in order to sum and average
  # **********************
  # change the cover value to the mid-point percent
  seedlingdata$Cover[seedlingdata$Cover==1]=0.5
  seedlingdata$Cover[seedlingdata$Cover==2]=1.5
  seedlingdata$Cover[seedlingdata$Cover==3]=3.5
  seedlingdata$Cover[seedlingdata$Cover==4]=7.5
  seedlingdata$Cover[seedlingdata$Cover==5]=17.5
  seedlingdata$Cover[seedlingdata$Cover==6]=37.5
  seedlingdata$Cover[seedlingdata$Cover==7]=62.5
  seedlingdata$Cover[seedlingdata$Cover==8]=85
  seedlingdata$Cover[seedlingdata$Cover==9]=97.5
  
  seedlingdata
}




Seedling_aggregation <- function(seedlingdata) {
  # Summarize scores for species by plot 
  temp<-with(seedlingdata,(aggregate(seedlingdata[,c("Cover","Score")], by=list(Sample_Year, Panel, Unit_ID, Plot_Number, Loc_Type, Genus, Latin_Name, Census), FUN=sum)))
  colnames(temp)<-c("Sample_Year", "Panel", "Unit_ID", "Plot_Number", "Loc_Type", "Genus", "Latin_Name", "Census", "Cover","Score")
  head(temp) 
  temp$Score<-temp$Score/12 ; temp$Cover<-temp$Cover/12
  seedlingdata<-temp
  
  #Create DBH class categories
  seedlingdata$Cover.value<-seedlingdata$Cover
  seedlingdata$Cover[seedlingdata$Cover.value==0]="0%" 
  seedlingdata$Cover[seedlingdata$Cover.value>=0 & seedlingdata$Cover.value<1]="<1%" 
  seedlingdata$Cover[seedlingdata$Cover.value>=1 & seedlingdata$Cover.value<2]="1-2%"
  seedlingdata$Cover[seedlingdata$Cover.value>=2 & seedlingdata$Cover.value<5]="2-5%" 
  seedlingdata$Cover[seedlingdata$Cover.value>=5 & seedlingdata$Cover.value<10]="5-10%" 
  seedlingdata$Cover[seedlingdata$Cover.value>=10 & seedlingdata$Cover.value<25]="10-25%" 
  seedlingdata$Cover[seedlingdata$Cover.value>=25 & seedlingdata$Cover.value<50]="25-50%" 
  seedlingdata$Cover[seedlingdata$Cover.value>=50 & seedlingdata$Cover.value<75]="50-75%" 
  seedlingdata$Cover[seedlingdata$Cover.value>=75 & seedlingdata$Cover.value<95]="75-95%" 
  seedlingdata$Cover[seedlingdata$Cover.value>95]="95-100%"
  seedlingdata$Cover<-as.factor(seedlingdata$Cover)
  seedlingdata
  
}

