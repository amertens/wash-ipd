





# importing packages and setting directories
rm(list=ls())
source(here::here("0-config.R"))


#Checking direct lab datasets

d<-read.csv(paste0(dropboxDir,"Data/Gram Vikas/lab.dw_03212018.csv"))
d2<-read.csv(paste0(dropboxDir,"Data/Gram Vikas/lab.sw_03212018.csv"))
dim(d)
dim(d2)

colnames(d)
length(unique(d$ev_id))
length(unique(d$ev_id1))
length(unique(d$ev_id2))
length(unique(d$ev_id3))
length(unique(d$ev_id4))

length(unique(d$ev_id_rnd))
length(unique(d$ev_smp_id))

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





#----------------------------------------------------
# rename covariates
#----------------------------------------------------

# 1.	Child birth order/parity 

# 2.	Asset-based wealth index 
#   557 - hh.asset - does any member of your household own the following items?
#   558 - 581 - hh.asset_x

# 3.	Number of individuals and children in household
    # Mnum4 - [Number of HH members, ppl >=60 yrs]
    # Mnum5 - [Number of HH members, men 18-59 yrs]
    # Mnum6 - [Number of HH members, women 18-59 yrs]
    # Mnum7 - [Number of HH members, child 5-17 yrs]
    # Numcu5 - [Number of HH members, child 5 yrs]

# 4.	Household food security 
#   Hh.hhs1 - In the past 30 days, was there ever no food to eat of any kind in your house because of lack of resources to get food?
#   Hh.hhs1a - How often did this happen in the past 30 days?
#   Hh.hhs3 - In the past 30 days, did you or any household members go a whole day and night without eating anything at all because there was not enough food?
#   Hh.hhs3a - How often did this happen in the past 30 days?

# 5.	Household electrification and construction, including wall/roof material 
# 6.	Parental age 
# 7.	Parental education 
# 8.	Parental employment 
# a.	Indicator for works in agriculture 
# 9.	Land ownership 
Ws = Wvars = c("hhwealth", "Nhh","momedu",
               "hfiacat", "nrooms","walls", "floor","elec")    




#   Household electrification and construction, including wall/roof material 
# hh.asset_12
# Hh.asset_12
# Hh.fuel - what type of fuel does your household mainly use for cooking?
#   Hh.light - What is the principal source of lighting for your household?
#   hc.A1a	- Has your household ever constructed any of the following: Household latrine
# hc.A1b	- Has your household ever constructed any of the following: Specified household wash room
# hc.A1c	- Has your household ever constructed any of the following: Household water connection(s) that links to a VILLAGE water distribution system or water tank
# For your most recently constructed household WASH ROOM, when did construction begin?
#   How long did it take to construct the most recently constructed household WASH ROOM?
#   Was the most recently constructed household WASH ROOM constructed to completion?
#   hc.C7 - Did your household hire a MASON or other SKILLED LABOURER to make repairs or improvements to the household latrine, wash room, or water connections AFTER the facilities were constructed?
#   hc.C1
# hc.C10
# hc.C10a
# hc.C10b
# hc.C1a
# hc.C1b
# hc.C1c
# hc.D10a
# hc.D11a
# hc.D13a
# hc.D13b
# hc.D14a
# hc.D17a
# hc.D17b
# hc.D1a
# hc.D20a
# hc.D21a
# hc.D23a
# hc.D25a
# hc.D2a
# hc.D2ai
# hc.D2aii
# hc.D2b
# hc.D2bi
# hc.D2bii
# hc.D3a
# hc.D3ai
# hc.D3aii
# hc.D3b
# hc.D3bi
# hc.D3bii
# hc.D4a
# hc.D4ai
# hc.D4aii
# hc.D4b
# hc.D4bi
# hc.D4bii
# hc.D5a
# hc.D5ai
# hc.D5aii
# hc.D5b
# hc.D5bi
# hc.D5bii
# hc.D6a
# hc.D6ai
# hc.D6aii
# hc.D6b
# hc.D6bi
# hc.D6bii
# Parental age 
# VA.3 - respondent's age (in whole years)
# Parental education 
# 699 - hoh.edu.any
# 700 - hoh.edu.prim
# 701 - hoh.edu1
# 702 - hoh.edu2
# 703 - hoh.edu3
# 704 - hoh.edu4
# 1123 - wom.edu.any
# wom.edu.prim
# Wom.edu1
# Wom.edu2
# Wom.edu3
# Wom.edu4
# Parental employment (and Indicator for works in agriculture)
# 6 - A.6 - Respondent's occupation
# 7 - A.6a - respondent's current employment status
# 632 - hh.hohjob - occupation of the head of the household
# Land ownership 
# 481 - hc.H2 - does your household privately own this latrine, or do you share it with other households? 
#   550 - hh.agric1 - does any member of this household own or rent any land that can be used for agriculture?
#   551 - hh.agric10 - [Acres agricultural land owned, >10 = 10]
# 552 - hh.agric2 - how many acres of agricultural land do members of this household own?
#   554 - hh.agricown - own any agricultural land, binary
# 665 - hh.own - do you or someone living in this household own this dwelling?
#   666 - hh.ownership - own house, binary
# Animal ownership 
# 12 - any.livest.lg - does the household have any large livestock (oxen, cattle)
# 13 - any.livest.sm - does the household have any small livestock (goats, pigs, sheep)
# 14 - any.poult - does the household have chickens, ducks, turkeys, or pigeons
# 478 - hc.G6 - how do you bring water home from the source?
#   600 - hh.corral1 - if you or anyone in this household owns livestock, where are the animals corralled?
#   745 - ls.any - any livestock owned
# 746 - ls.lg - large livestock owned
# 747 - ls.poult - poultry owned
# Season 
# 996 - time.rain - rainy season, binary
# Urban vs. rural 
# Child sex 
# 681 - hh.sex - Is [hh.name] male or female?
#   1010 - respondent's sex
# Child age 
# 91 - dbmoc - Child age in months, categorical
# 601 - hh.dbchk - DOB
# 1026 - VA.3 - respondent's age
# Reported geophagia (soil eating)
# 611 - hh.drt7 - Did you observe [hh.name] putting soil, mud, clay, or sand directly in their mouth in the past 7 days?
#   612 - hh.drtrs - What was your response?
#   613-616 - hh.drtrs_1, hh.drtrs_2, hh.drtrs_3, hh.drtrs_66
# 617 - hh.drtvol - How much soil, mud, clay or sand did [hh.name] put in their mouth at a time?
#   





#Rename and subset variables
 dw <- d %>% 
   mutate(sample="W",
     vc.pos=as.numeric(vc_kiit_pos), sh.pos=as.numeric(sh_kiit_pos),
     vc.pres.pos=as.numeric(vc_tcbs.1), sh.pres.pos=as.numeric(sh_pos.1)) %>%
   rename(
     round=hh_rnd, 
     momedu=hoh_edu4,
     sampleid=ev_smp_id
   ) %>%
   subset(., select = c(
     sampleid, hh_vid, round, hh_hid, hh_mid, hh_st, ic, sample, vc.pos, sh.pos, vc.pres.pos, sh.pres.pos, momedu, haz, whz, dia7
   ))
head(dw)

sw <- d2 %>% 
  mutate(sample="SW",
    vc.pos=as.numeric(vc_kiit_pos), sh.pos=as.numeric(sh_kiit_pos),
    vc.pres.pos=as.numeric(vc_tcbs.1), sh.pres.pos=as.numeric(sh_pos.1)) %>%
  rename(
    round=hh_rnd, 
    momedu=hoh_edu4,
    sampleid=ev_smp_id
  ) %>%
  subset(., select = c(
    sampleid, hh_vid, round, hh_hid, hh_mid, hh_st, ic, sample, vc.pos, sh.pos, vc.pres.pos, sh.pres.pos, momedu, haz, whz, dia7
  ))
head(sw)

dim(sw)
dim(sw %>% distinct(hh_vid, round, hh_hid, hh_mid, hh_st))

dim(dw)
dim(dw %>% distinct(hh_vid, round, hh_hid, hh_mid, hh_st))

#Merge
df <- bind_rows(sw, dw)
head(df)
table(df$sample, df$sh.pos)
table(df$sample, df$vc.pos)

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

# sample water (n=1583) and drinking water (n=2044) were assayed for E. coli, Shigella spp., and V. cholerae, 
# and children's hands (n=976)  for E. coli and Shigella spp.
dim(df)

#We collected samples of the sample water and drinking water for each household four times, once in each study round,
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



