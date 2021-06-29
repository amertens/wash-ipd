


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
library(metafor)
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


source(here("src/0-analysis functions.R"))


theme_ki <- function(){
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      axis.text.x=element_text(size=7),
      axis.text.y=element_text(size=7),
      legend.text=element_text(size=7),
      axis.title = element_text(size = 10),
      strip.text.x = element_text(size=9, face = "bold"),
      strip.text.y = element_text(size=9, angle = 270, face = "bold"),          
      plot.title = element_text(hjust = 0.5, face = "plain", size=9)
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

any_virus = c("Norovirus",  "Adenovirus", "Rotavirus", "Astrovirus", "Sapovirus",  "Pan enterovirus")   
any_bacteria = c("E. coli virulence gene", "Pathogenic E. coli", 
                 "Yersinia",  "V. cholerae", "Shigella",  "C. difficile",  
                 "Salmonella", "Campylobacter")   
any_helminth = c("Any STH", "Ascaris", "Trichuris")   
any_protozoa = c("Giardia", "Cryptosporidium", "Entamoeba histolytica")   

any_pathogens = c(any_virus, any_bacteria, any_helminth, any_protozoa)

   
zoonotic_pathogens = c("EC_zoo","Campylobacter", "Salmonella", "Cryptosporidium", "Yersinia", "Giardia", "Ascaris",  "C. difficile")


#MST's:
general_MST = c("General (GenBac3)","General (BacUni)")

animal_MST = c( "Animal (BacCow)",   
                "Animal (BacR)",              "Avian (GFD)",
                "Avian (Helicobacter)", "Animal (BacCan)" )

human_MST = c("Human (HumM2)",  "Human (Bacteroides)",   "Human (M. smithii)")

any_MST = c(general_MST, animal_MST, human_MST)



target_levels = unique(c(
  "Any pathogen", "Any MST", "Any general MST",       "Any human MST",        "Any animal MST",  
  "Any bacteria",                       
  "Any virus",     "Any protozoa",  "Any STH", 
  any_MST, any_pathogens))












clean_res <- function(d, target_lev=target_levels){
  
  target_lev <- gsub("Any STH ","Any Helminth",target_lev)
  d$target_f <- gsub("Any STH ","Any Helminth",d$target)  
  target_lev <- gsub("Any ","Any\n",target_lev)
  d$target_f <- gsub("Any ","Any\n",d$target)
  target_lev <- gsub("Entamoeba histolytica","Entamoeba\nhistolytica",target_lev)
  d$target_f <- gsub("Entamoeba histolytica","Entamoeba\nhistolytica",d$target_f)
  target_lev <- gsub("Pathogenic E. coli","Pathogenic\nE. coli",target_lev)
  d$target_f <- gsub("Pathogenic E. coli","Pathogenic\nE. coli",d$target_f)
  
  d$target_f <- factor(d$target_f, levels = c(target_lev, unique(d$target_f)[!(unique(d$target_f) %in% target_lev)]) ) 
  
  d <- d %>% mutate(
    sample_type =case_when(
      sample == "any sample type" ~ "Any sample\ntype",
      sample == "SW" ~ "Water",
      sample == "W" ~ "Water",
      sample == "CH" ~ "Hands",
      sample == "MH" ~ "Hands",
      sample == "FlyKitch" ~ "Flies",
      sample == "FlyLat" ~ "Flies",
      sample == "LS" ~ "Soil",
      sample == "S" ~ "Soil"
    ),
    sample_type = factor(sample_type, levels=c("Any sample\ntype", "Water", "Hands","Soil", "Flies")),
    sample_cat =case_when(
      sparse == "yes" ~ "Sparse data",
      sample == "any sample type" & sparse != "yes" ~ "Any sample",
      sample == "SW"  & sparse != "yes"~ "Source water",
      sample == "W"  & sparse != "yes"~ "Stored water",
      sample == "CH"  & sparse != "yes"~ "Child hands",
      sample == "MH"  & sparse != "yes"~ "Mother's hands",
      sample == "FlyKitch"  & sparse != "yes"~ "Flies in kitchen",
      sample == "FlyLat"  & sparse != "yes"~ "Flies in latrine",
      sample == "LS"  & sparse != "yes"~ "Latrine soil",
      sample == "S"  & sparse != "yes"~ "House soil"
    ), 
    sample_cat = factor(sample_cat, 
                        levels=c("Any sample","Source water","Stored water",
                                 "Child hands", "Mother's hands", "Latrine soil",
                                 "House soil", "Flies in kitchen",  "Flies in latrine", "Sparse data"))
  )
  
  #get raw OR and cell count labels
  if(!is.null(d$a)){
    d <- d %>% mutate(OR=(a*d)/(b*c), cell_lab=paste0(a,"/",a+b,":",c,"/",c+d))
  }
  
  return(d)
}



#strict_left_join function - will throw error instead of increasing dataframe size
strict_left_join <- function(x, y, by = NULL, ...){
  by <- common_by(by, x, y)
  if(any(duplicated(y[by$y]))) {
    stop("Duplicate values in foreign key")
  } else left_join(x, y, by = by, ...)
}
