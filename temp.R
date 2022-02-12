year="FVM2018"
seedlingdata <- Seedling_Scores(year)
save(seedlingdata,file="seedlingdata2.rdata")

head(seedlingdataplot)
headers(seedlingdataplot)

deerplots <- c(6327, 121, 6322, 134, 6383, 135, 6329, 146, 6328, 200, 6330, 220, 6324, 230, 6323, 238, 6384, 301, 6382, 308)
data<-seedlingdataplot[seedlingdataplot$Plot_Number %in% deerplots,]
