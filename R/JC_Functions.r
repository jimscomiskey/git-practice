# A series of convenience Functions developed by James Comiskey
#
# shortcut function to export data
# James Comiskey v.2 3/16/2012
#
writecsv<- function(file, tablename) {
  if (exists("tablename")){
    tablename=deparse(substitute(file))
    }
  name=paste(tablename,".csv",sep="")
  write.table(file,name, sep=",", col.names=TRUE, row.names=FALSE, quote=TRUE)
  }

# function to count data
# James Comiskey v. 3/16/2012
#
count <- function(x) {
  length(na.omit(x))
}

# function to determine col names and numbers
# 3/16/2012
# This is used to simplify writing code by using col numbers 
# rather than names, but means you need some sort of reference
# to know what the numbers are refering to. 
# For example, for the data frame "netdata"
# netdata columns are: 1 unit ,  2 N ,  3 growth ,  4 sd ,  5 se ,  6 ci
#
headers <- function(data){
   fields=rbind(1:ncol(data),colnames(data))
   text=paste(fields[1,1],fields[2,1])
   for (x in 2:ncol(fields)){
      text=paste (text,", ",fields[1,x],fields[2,x],sep=" ")
      }
      tablename=deparse(substitute(data))
      text = paste ("#", tablename, "columns are:", text)
      writeClipboard(text)
      rm(list = c("fields","text","x","tablename"))
    }

# 6/30/2014
# use this in place of source to allow for code to be inserted
# that allows you to exit a script, return to script that called it,
# and continue running the code.
# Code inserted at 'break point' as follows:
# parent.frame(3)$exit.source(7)  # exit script
#
my.source <- function(file, ...) {
	source. <- function(exit.source) source(file, ...)
	callCC(source.)
}




# Functions to compare two tables
# diff <- negate_match_df(a1,a2)
# diff

match_df <- function (x, y, on = NULL) 
{
  if (is.null(on)) {
    on <- intersect(names(x), names(y))
    message("Matching on: ", paste(on, collapse = ", "))
  }
  keys <- join.keys(x, y, on)
  x[keys$x %in% keys$y, , drop = FALSE]
}


library(plyr)
negate_match_df <- function (x, y, on = NULL) 
{
  if (is.null(on)) {
    on <- intersect(names(x), names(y))
    message("Matching on: ", paste(on, collapse = ", "))
  }
  keys <- join.keys(x, y, on)
  x[!(keys$x %in% keys$y), , drop = FALSE]
}



