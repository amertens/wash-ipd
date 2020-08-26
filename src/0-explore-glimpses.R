

# importing packages and setting directories
source(here::here("0-config.R"))

load(paste0(dropboxDir,"Data glimpse/Gram Vikas/gv_glimpse.rda"))
d <- gv_glimpse
colnames(d)
head(d)

#Drop duplicate variables
dim(d)
d <- d[,!grepl("\\.y$", colnames(d))]
dim(d)
colnames(d) <- gsub("\\.y$","",colnames(d))

df <- as.data.frame(glimpse(d))

lab <- read.csv(paste0(dropboxDir,"Data glimpse/Gram Vikas/Codebook_exclusions.csv"))
lab <- lab %>% filter(Include=="y"|Include=="Y")
lab$Variable[lab$Notes=="COARSEN"]

#Examine date/age variables
summary(d$ec.fd) #Not in dataset
summary(d$he.d) #Not in dataset
summary(d$dob) #Not in dataset
summary(d$hh.age) #Already coarsened to year
summary(d$hh.dob) #Not in dataset
summary(d$sh.fd) #Not in dataset
summary(d$st.sd) #Not in dataset
summary(d$hh.et) #Not in dataset
summary(d$hh.st) #Not in dataset
summary(d$tod)  #Not in dataset

colnames(d)[grepl("dob",colnames(d))]


sum(colnames(d) %in% lab$Variable)
sum(!(colnames(d) %in% lab$Variable))
sum(lab$Variable %in% colnames(d))
sum(!(lab$Variable %in% colnames(d)))

colnames(d)[!(colnames(d) %in% lab$Variable)]
lab$Variable[!(lab$Variable %in% colnames(d))]


head(d)
head(lab)

df <- full_join(d, lab)





missing.in.codebook <- colnames(d)[!(colnames(d) %in% lab$Variable)]
missing.in.data <- lab$Variable[!(lab$Variable %in% colnames(d))]

#Save names of missingness variables
write.csv(missing.in.data, file = "C:/Users/andre/Dropbox/IPD WASH/Data glimpse/Gram Vikas/missing_vars_data_GV.csv")
write.csv(missing.in.codebook, file = "C:/Users/andre/Dropbox/IPD WASH/Data glimpse/Gram Vikas/missing_vars_codebook_GV.csv")

