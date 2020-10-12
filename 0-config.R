


#load required packages
library(tidyverse)
library(here)
library(haven)
library(lubridate)
library(janitor)
library(washb)

#set data directories

dropboxDir <- NULL
#Andrew
if(dir.exists("C:/Users/andre/Dropbox/IPD WASH/")){ 
  dropboxDir <- "C:/Users/andre/Dropbox/IPD WASH/"
}
#Ruwan
if(dir.exists("C:/Users/ruwan/Dropbox/IPD WASH/")){ 
  dropboxDir <- "C:/Users/ruwan/Dropbox/IPD WASH/"
}



makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  tibble(name = names(labels),
         label = labels)
}
