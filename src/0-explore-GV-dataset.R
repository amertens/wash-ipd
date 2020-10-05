

# importing packages and setting directories
source(here::here("0-config.R"))

load(paste0(dropboxDir,"Data/Gram Vikas/gramvikas.rda"))
d <- gramvikas
colnames(d)
head(d)


findVar <- function(var){
  colnames(d)[grepl(var,colnames(d))]
}


# #Drop duplicate variables
# dim(d)
# d <- d[,!grepl("\\.y$", colnames(d))]
# dim(d)
# colnames(d) <- gsub("\\.x$","",colnames(d))


#Explore aim 1 variables
table(d$hh.rnd)
table(d$mh.rnd)

#all colony countr the same
table(d$vc.o1)
table(d$vc.o11)
table(d$vc.o12)
table(d$vc.o13)

table(d$vc.o139)
table(d$vc.o1391)
table(d$vc.o1392)
table(d$vc.o1393)



#Subset to aim 1 variables and save dataset
df <- d %>% select(
  
  #intervention
  ic,
  
  #HH ID's
  hh.vid, hh.rnd, hh.hid, hh.mid3, hh.st, #mh.num.adults,
    

    #env. sample identifiers
    ev.id, ev.id1,  ev.id1.1,  ev.id2, ev.id3,  ev.id4,
    ev.01, ev.01b, ev.03,    ev.04,       ev.05,    
    ev.smp.id,ev.id.rnd,
  
    #cholera variables
  vc.kiit,
  vc.o1,
  vc.o139,
  vc.tcbs,
  vc_tcno,
    
  #Shigella variables
    sh_bcno,
    sh_bnote,
    sh_dil1,
    sh_dil2,
    sh_dil3,
    
    sh.dys,
    sh.flx,
    
    sh.sacnt1,
    sh.sacnt2,
    sh.sacnt3,
    sh_shcnt1,
    sh_shcnt2,
    sh_shcnt3) %>%
  rename(
    tr=ic
  )

# rename(tr=studyArm_binary,
#        pos=detect,
#        hhwealth=povNormal,
#        Nhh=Hhsize,
#        nrooms=hhrooms,
#        momedu=carerEDU,
#        walls=hh_walls,
#        floor=hhCement,
#        elec=compElec
# )




#V cholera presence
summary(df$vc.kiit)

#seperate strains
summary(df$vc.o1)
summary(df$vc.o139)

#Variables match
table(df$vc.kiit)
table(df$vc.o1==1 | df$vc.o1391==1)

#What is the difference between these 2 positive variables?
table(df$vc.kiit)
table(df$vc.tcbs)


#V cholera count
table(df$vc_tcno)

#What are the individual colony counts? Does the above make sense


#Shigella presence


#Shigella count



table(df$sh_bcno)
table(df$sh_bnote)

table(df$sh_dil1)
table(df$sh_dil2)
table(df$sh_dil3)


table(df$sh.dys)
table(df$sh.flx)

table(df$sh.sacnt1)


#Subset to env. observations
df <- df %>% filter(!is.na() | !is.na(vc.kiit))


#are these env. versus stool samples?

# sh.sacnt1,
# sh.sacnt2,
# sh.sacnt3,
# sh_shcnt1,
# sh_shcnt2,
# sh_shcnt3

# lab <- read.csv(paste0(dropboxDir,"Data glimpse/Gram Vikas/Codebook_exclusions.csv"))
# lab <- lab %>% filter(Include=="y"|Include=="Y")
# lab$Variable[lab$Notes=="COARSEN"]
# 
# #Examine date/age variables
# summary(d$ec.fd) #Not in dataset
# summary(d$he.d) #Not in dataset
# summary(d$dob) #Not in dataset
# summary(d$hh.age) #Already coarsened to year
# summary(d$hh.dob) #Not in dataset
# summary(d$sh.fd) #Not in dataset
# summary(d$st.sd) #Not in dataset
# summary(d$hh.et) #Not in dataset
# summary(d$hh.st) #Not in dataset
# summary(d$tod)  #Not in dataset
# 
# colnames(d)[grepl("dob",colnames(d))]
# 
# 
# sum(colnames(d) %in% lab$Variable)
# sum(!(colnames(d) %in% lab$Variable))
# sum(lab$Variable %in% colnames(d))
# sum(!(lab$Variable %in% colnames(d)))
# 
# colnames(d)[!(colnames(d) %in% lab$Variable)]
# 
# 
# head(d)
# head(lab)
# 
# df <- full_join(d, lab)


#
saveRDS(df, file=paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds"))
