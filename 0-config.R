


#load required packages
library(tidyverse)
library(here)
library(haven)

#set data directories

dropboxDir <- NULL
#Andrew
if(dir.exists("C:/Users/andre/Dropbox/WASH-IPD-data/")){ 
  dropboxDir <- "C:/Users/andre/Dropbox/WASH-IPD-data/"
}
#Ruwan
if(dir.exists("")){ 
  dropboxDir <- ""
}
