

# importing packages and setting directories
source(here::here("0-config.R"))

load(paste0(dropboxDir,"Data/Gram Vikas/gramvikas.rda"))
d <- gramvikas
colnames(d)
head(d)


findVar <- function(var){
  colnames(d)[grepl(var,colnames(d))]
}




#Subset to aim 1 variables and save dataset
df <- d %>% select(
  
  #intervention
  ic,
  
  #HH ID's
  hh.vid, hh.rnd, hh.hid, hh.mid3, hh.st, #mh.num.adults,
    
  #child growth
  haz, waz, whz,

    #env. sample identifiers
    ev.id, ev.id1, 
  #ev.id1.1,  #blank
  ev.id2, ev.id3,  ev.id4,
    ev.01, 
  ev.01b, #house is selected for : 1 = Water samples, 2 = Hand rinse samples, 3 = Stool samples, 4 = NA
  ev.03,    ev.04,       ev.05,    
    ev.smp.id,ev.id.rnd,
  
    #cholera variables
  vc.kiit,
  vc.o1,
  vc.o139,
  vc.tcbs,
  vc_tcno,
    
  #Shigella variables
  sh.kiit ,
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
    sh_shcnt3,
  
  #Covariates
  hoh.edu4
  
  ) %>%
  rename(
    round=hh.rnd, 
    momedu=hoh.edu4
  ) %>%
  #filter(!is.na(sh.kiit) | !is.na(vc.kiit)) %>%
  mutate(
    tr=case_when(
      ic==0 ~ "Control",
      ic==1 ~ "Intervention"
    )
  )
write.csv(df, paste0(dropboxDir,"Data/Gram Vikas/gramvikas_env_temp.csv"))
#-----------------------------------------------------------------------------------------------------------
# Check numbers with Reese dissertation
#-----------------------------------------------------------------------------------------------------------

# Source water (n=1583) and drinking water (n=2044) were assayed for E. coli, Shigella spp., and V. cholerae, 
# and children's hands (n=976)  for E. coli and Shigella spp.
dim(df)

#We collected samples of the source water and drinking water for each household four times, once in each study round,
#and child hand rinse samples two times, in rounds 2 and 4. If a household randomly selected for sampling was absent, 
#field workers collected samples from the nearest enrolled household to the right.

table(df$round, df$vc.kiit)
table(df$round, df$sh.kiit)

df <- df %>% filter(round==4)
head(df)

table(df$ev.01b)
# 1 = Water samples
# 2 = Hand rinse samples
# 3 = Stool samples
# 4 = NA

table(df$tr, df$sh.kiit)
table(df$tr, df$vc.kiit)

head(df)


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
