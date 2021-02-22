

# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))


#Checking direct lab datasets

d<-read.csv(paste0(dropboxDir,"Data/Gram Vikas/lab.dw_03212018.csv"))
d2<-read.csv(paste0(dropboxDir,"Data/Gram Vikas/lab.sw_03212018.csv"))
dim(d)
dim(d2)

colnames(d)
table(d$ev_01b)
table(d$vc_o139)
table(d$vc_o1)
table(d$vc_kiit)
table(d$vc_o1, d$vc_kiit, d$vc_o139_pos)

d$vc_kiit <- as.numeric(d$vc_kiit)
d2$vc_kiit <- as.numeric(d2$vc_kiit)


#pathogenic pos:
table(d$vc_kiit_pos)
table(d2$vc_kiit_pos)


table(d$sh_kiit_pos)
table(d2$sh_kiit_pos)


#presumptive positive:
table(d$vc_tcbs.1 )
table(d2$vc_tcbs.1 )

table(d$sh_pos.1 )
table(d2$sh_pos.1 )


#Rename and subset variables
 dw <- d %>% 
   mutate(type="W",
     vc.pos=as.numeric(vc_kiit_pos), sh.pos=as.numeric(sh_kiit_pos),
     vc.pres.pos=as.numeric(vc_tcbs.1), sh.pres.pos=as.numeric(sh_pos.1)) %>%
   rename(
     round=hh_rnd, 
     momedu=hoh_edu4,
   ) %>%
   subset(., select = c(
     hh_vid, round, hh_hid, hh_mid, hh_st, ic, type, vc.pos, sh.pos, vc.pres.pos, sh.pres.pos, momedu, haz, whz, dia7
   ))
head(dw)

sw <- d2 %>% 
  mutate(type="SW",
    vc.pos=as.numeric(vc_kiit_pos), sh.pos=as.numeric(sh_kiit_pos),
    vc.pres.pos=as.numeric(vc_tcbs.1), sh.pres.pos=as.numeric(sh_pos.1)) %>%
  rename(
    round=hh_rnd, 
    momedu=hoh_edu4
  ) %>%
  subset(., select = c(
    hh_vid, round, hh_hid, hh_mid, hh_st, ic, type, vc.pos, sh.pos, vc.pres.pos, sh.pres.pos, momedu, haz, whz, dia7
  ))
head(sw)

dim(sw)
dim(sw %>% distinct(hh_vid, round, hh_hid, hh_mid, hh_st))

dim(dw)
dim(dw %>% distinct(hh_vid, round, hh_hid, hh_mid, hh_st))

#Merge
df <- bind_rows(sw, dw)
head(df)
table(df$type, df$sh.pos)
table(df$type, df$vc.pos)

df <- df %>%
    mutate(
      tr=case_when(
        ic==0 ~ "Control",
        ic==1 ~ "Intervention"
      )) %>%
  subset(., select = -c(ic))


#Transform to long, and only keep confirmed positives:
df <- df %>% 
  gather(vc.pos:sh.pres.pos , key = target, value = pos ) %>%
  filter(!is.na(pos), target %in% c("sh.pos","vc.pos")) %>%
  mutate(dataid=hh_vid, logquant=NA,
         target = case_when(
           target=="sh.pos" ~"shigella",
           target=="vc.pos" ~"vibrio_cholera"
         )) %>%
  rename(clusterid=hh_vid )

head(df)

# for(i in colnames(d)[grepl("sh_",colnames(d))]){
#   cat(i,":\n")
#   print(table(d[[i]]))
# }

saveRDS(df, file=paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds"))



#-----------------------------------------------------------------------------------------------------------
# Check numbers with Reese dissertation
#-----------------------------------------------------------------------------------------------------------

# type water (n=1583) and drinking water (n=2044) were assayed for E. coli, Shigella spp., and V. cholerae, 
# and children's hands (n=976)  for E. coli and Shigella spp.
dim(df)

#We collected samples of the type water and drinking water for each household four times, once in each study round,
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



