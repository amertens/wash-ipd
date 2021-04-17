


#load required packages
library(tidyverse)
library(here)
library(haven)
library(lubridate)
library(janitor)
library(washb)
library(modelr)
library(caret)
library(rcartocolor)

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


source(here("src/0-analysis functions.R"))


theme_ki <- function() {
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

#hbgdki pallets
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
tableau11 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

theme_set(theme_ki())



scaleFUN <- function(x) sprintf("%.2f", x)



#---------------
# Outcome groupings
#---------------

#pathogens:
any_pathogens = c("E. coli virulence gene",  "Pathogenic E. coli", "Giardia",  "C. difficile",
                  "Shigella",  "Entamoeba histolytica",  "V. cholerae", "Yersinia",       
                  "Norovirus",             "Any STH", "Ascaris", "any pathogen-improved", "any pathogen-unimproved",
                  "Adenovirus","Trichuris",  "Rotavirus", "Astrovirus", "Cryptosporidium", "Salmonella")   

any_virus = c("Norovirus",  "Adenovirus", "Rotavirus", "Astrovirus")   
any_bacteria = c("E. coli virulence gene", "Pathogenic E. coli", "Yersinia",  "V. cholerae", "Shigella",  "C. difficile",  "Salmonella")   
#any_helminth = c("Any STH", "Ascaris", "Trichuris")   
any_protozoa = c("Giardia", "Cryptosporidium", "Entamoeba histolytica")   





#MST's:
general_MST = c("GenBac3","BacUni")

animal_MST = c( "Animal (BacCow)",   
                "Ruminant",              "Avian",
                "Avian (Helicobacter)")

human_MST = c("Human (HumM2)",  "Human (Bacteroides)",   "Human (M. smithii)")

any_MST = c(general_MST, animal_MST, human_MST)



target_levels = unique(c(
  "Any pathogen", "Any general MST",       "Any human MST",        "Any animal MST",  
  "Any bacteria",                       
  "Any virus",     "Any protozoa",  "Any STH", 
  any_MST, any_pathogens))
