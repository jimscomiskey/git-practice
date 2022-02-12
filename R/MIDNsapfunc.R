## This code connects to the FVM MS Access back end database (FVM)
# Following DB connection steps from http://sandymuspratt.blogspot.com/2013/01/getting-access-data-into-r.html
## The code then queries microplot-level measurements of saplings within MIDN. The output is Similar to Jim C's 
#'saplings.csv'reporting sapling DBH; the only difference being that I removed QAQC plots
##created by Aaron S. Weed Version 2/23/2015


#install.packages("RODBC")
MIDNsapdata<-function(year){
  library(RODBC)
  con <- odbcConnect(year)# Connect to database that I named under USER DSN in ODBC Data Source Admin
  # examples
  sqlTables(con, tableType = "TABLE")$TABLE_NAME # Access data table names
  sqlColumns(con, "tbl_Disturbances")$COLUMN_NAME#Access column names of specifc table
  
  ###################### Import data and lookup tables used for the query
  #tbl_Tree_Data, tbl_Trees, tlu_Crown_Classes, tbl_Events, tbl_Locations, tlu_Plants
  
  # import dataframes of each tables within the DB
  loc <- sqlFetch(con, "tbl_Locations");names(loc)
  event <- sqlFetch(con, "tbl_Events"); names(event)
  treedata<-sqlFetch(con, "tbl_Tree_Data");names(treedata)
  trees<-sqlFetch(con, "tbl_Trees");names(trees)
  park <- sqlFetch(con, "tlu_Park_Names");names(park)
  plants<- sqlFetch(con, "tlu_Plants"); names(plants)
  cc<-sqlFetch(con, "tlu_Crown_Classes"); names(cc)
  saps<- sqlFetch(con, "tbl_Microplot_Sapling_Data_MIDN"); names(saps)
  microplot<-sqlFetch(con, "tbl_Microplot_Characterization_Data"); names(saps)
  
  odbcClose(con)
  ######################################## Join various dataframes be realtions to create queries
  library(plyr)
  # assign taxonomy to saplings
  intersect(names(trees),names(plants))
  temp<-join(trees, plants, by="TSN");names(temp)
  
  # append location data to tree data
  
  loc2<-join(park,loc, by="Unit_ID")
  temp1<-join(loc2,temp, by="Location_ID")
  names(temp1)
  
  # assign microplot location to saplings data
  intersect(names(saps),names(microplot))
  temp2<-join(saps,microplot, by="Microplot_Characterization_Data_ID");names(temp2)
  
  # assign event data to sapling data
  intersect(names(temp2),names(event))
  temp3<-join(temp2, event, by="Event_ID");names(temp3)
  
  # join plant character to measurements 
  intersect(names(temp1),names(temp3))
  temp4<-join(temp3,temp1, by="Tree_ID")
  names(temp4)
  
  ############################# Index dataframe to generate final query ########
  # remove QAQC plot (VALUES =1)
  noerrors<-"0"
  temp5<-temp4[temp4$Event_QAQC %in% noerrors,]
  
  # Access data from vital signs monitoring (VALUES = VS) 
  levels(temp5$Loc_Type)
  #loctype= data_type
  temp6<-temp5#[temp5$Loc_Type %in% loctype,]
  names(temp6)
  
  
  ######################
  # TEMPORARY
  # ADD Dev_Status
  ########################
  temp6$Dev_Status<-""
  
  ## subset data frame and export to csv
  
  final_saplings<-temp6[c("Start_Date","Panel","Unit_ID", "Unit_Code","Subunit_Code" , "Plot_Number","X_Coord","Y_Coord","Latitude","Longitude" ,"Elevation",
                          "Coord_Units","Coord_System", "UTM_Zone" ,"Aspect","Physiographic_Class","Microplot_Name","TSN", "Tree_Number_MIDN","Genus","Latin_Name", "Common",
                          "DBH","Status_ID", "Dev_Status","Event_QAQC", "Event_ID","Indicator_MIDN","Deer_Browse_Indicator","Loc_Type" )]
  
  final_saplings$Dev_Status[final_saplings$DBH < 10 ]="Sapling"
  final_saplings$Dev_Status[final_saplings$DBH >= 10 ]="Tree"
  
  #remove any NA records in TSN
  final_saplings<-final_saplings[!is.na(final_saplings$TSN),]
  
  ####Create vars for JPs package
  #Sample_Year variable and variable indicating number of times resampled ('cycle')
  final_saplings$Start_Date<-gsub("00:00:00","",final_saplings$Start_Date) #Remove time
  final_saplings$Start_Date<-as.Date(final_saplings$Start_Date, format= "%Y-%m-%d") #convert to date
  final_saplings$Sample_Year<-as.factor(format(final_saplings$Start_Date,"%Y")) #Create Sample_Year variable
  
  s1<-seq(2007,2010, by=1);s2<-seq(2011,2014, by=1);s3<-seq(2015,2018, by=1)## generate seqs of Sample_Years to index dataframe
  final_saplings$Cycle[final_saplings$Sample_Year %in% s1]=1;final_saplings$Cycle[final_saplings$Sample_Year %in% s2]=2;final_saplings$Cycle[final_saplings$Sample_Year %in% s3]=3
  
  # force COLO parks to first census
  exparks<-c("COLO_JAMES","COLO_YORK")
  # *****************
  # JC 10/15/2016
  # COLO 2011-2014 = cycle 1, 2015-2018 = Cycle 2
  # ****************
  final_saplings$Cycle[(final_saplings$Unit_ID %in% exparks & final_saplings$Sample_Year == '2011') | 
                         (final_saplings$Unit_ID %in% exparks & final_saplings$Sample_Year == '2012') |
                         (final_saplings$Unit_ID %in% exparks & final_saplings$Sample_Year == '2013') |
                         (final_saplings$Unit_ID %in% exparks & final_saplings$Sample_Year == '2014')] = 1
  final_saplings$Cycle[(final_saplings$Unit_ID %in% exparks & final_saplings$Sample_Year == '2015') | 
                         (final_saplings$Unit_ID %in% exparks & final_saplings$Sample_Year == '2016') |
                         (final_saplings$Unit_ID %in% exparks & final_saplings$Sample_Year == '2017') |
                         (final_saplings$Unit_ID %in% exparks & final_saplings$Sample_Year == '2018')] = 2
  
  ## Create census
  final_saplings$Census[final_saplings$Cycle==1] = "Census 1"
  final_saplings$Census[final_saplings$Cycle==2] = "Census 2"
  final_saplings$Census[final_saplings$Cycle==3] = "Census 3"
  
  # Clean up sapling data based on DBH 
  final_saplings<-final_saplings[final_saplings$DBH >1,]
  final_saplings<-final_saplings[final_saplings$DBH <10,]
  
  
  final_saplings$Cycle<-as.factor(final_saplings$Cycle)
  final_saplings
  #write.table(final_saplings,"saplingdata.csv", sep=",", row.names=F)
}
