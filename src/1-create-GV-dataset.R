

# importing packages and setting directories
source(here::here("0-config.R"))

load(paste0(dropboxDir,"Data/Gram Vikas/gramvikas.rda"))
d <- gramvikas
colnames(d)
head(d)


findVar <- function(var){
  colnames(d)[grepl(var,colnames(d))]
}


#Drop duplicate variables
dim(d)
d <- d[,!grepl("\\.y$", colnames(d))]
dim(d)
colnames(d) <- gsub("\\.x$","",colnames(d))


#Explore aim 1 variables
table(d$hh.rnd)
table(d$mh.rnd)

#Subset to aim 1 variables and save dataset
df <- d %>% select(hh.vid, hh.rnd, hh.hid, hh.mid3, mh.hh.st, mh.num.adults,
    # vc.kiit,
    # vc.o1,
    # vc.o1391,
    # vc.tcbs,
    # vc.tcno,
    
    sh.bcno,
    sh.bnote,
    sh.dil1,
    sh.dil2,
    sh.dil3,
    
    sh.dys,
    sh.flx,
    
    sh.sacnt1,
    sh.sacnt2,
    sh.sacnt3,
    sh.shcnt1,
    sh.shcnt2,
    sh.shcnt3)




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
