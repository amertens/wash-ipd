





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


#Child sex/age
table(d$hh_sex)
table(d$dbmo)


# 1.	Child birth order/parity 

# 2.	Asset-based wealth index 
#   557 - hh.asset - does any member of your household own the following items?
#   558 - 581 - hh.asset_x



summary(as.numeric(d$wealth ))
table(d$wealth_ind )
summary(as.numeric(d$wealth_st ))



# 3.	Number of individuals and children in household
    # Mnum4 - [Number of HH members, ppl >=60 yrs]
    # Mnum5 - [Number of HH members, men 18-59 yrs]
    # Mnum6 - [Number of HH members, women 18-59 yrs]
    # Mnum7 - [Number of HH members, child 5-17 yrs]
    # Numcu5 - [Number of HH members, child 5 yrs]
table(d$mnum4)
table(d$mnum4+d$mnum5+d$mnum6+d$mnum7+d$numcu5)


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
table(d$hh_asset_12)
d <- d %>% mutate(elec=as.numeric(hh_asset_12))
d2 <- d2 %>% mutate(elec=as.numeric(hh_asset_12))

# Parental age 
# VA.3 - respondent's age (in whole years)
# Parental education 
table(d$hoh_edu4)




# Parental employment (and Indicator for works in agriculture)
# 6 - A.6 - Respondent's occupation
  # 1 = Professional
  # 2 = Sales worker/service worker
  # 4 = Production worker
  # 5 = Agricultural worker
  # 6 = Not employed
table(d$hh_hohjob)
d <- d %>% mutate(dadagri=factor(hh_hohjob))
d2 <- d2 %>% mutate(dadagri=factor(hh_hohjob))

# Land ownership 
d <- d %>% mutate(landacre=factor(hh_agricown))
d2 <- d2 %>% mutate(landacre=factor(hh_agricown))


# Animal ownership 
# 12 - any.livest.lg - does the household have any large livestock (oxen, cattle)
# 13 - any.livest.sm - does the household have any small livestock (goats, pigs, sheep)
# 14 - any.poult - does the household have chickens, ducks, turkeys, or pigeons
table(d$ls_any)
table(d2$ls_any)

d <- d %>% mutate(animals=as.numeric(ls_any))
d2 <- d2 %>% mutate(animals=as.numeric(ls_any))


# 478 - hc.G6 - how do you bring water home from the source?
#   600 - hh.corral1 - if you or anyone in this household owns livestock, where are the animals corralled?
#   745 - ls.any - any livestock owned
# 746 - ls.lg - large livestock owned
# 747 - ls.poult - poultry owned
# Season 
# 996 - time.rain - rainy season, binary
# Urban vs. rural 

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
     vc.pres.pos=as.numeric(vc_tcbs.1), sh.pres.pos=as.numeric(sh_pos.1),
     sex=as.numeric(hh_sex),
     age=as.numeric(dbmo),
     env_date=ymd(hh_st)) %>%
   rename(
     round=hh_rnd, 
     momedu=hoh_edu4,
     sampleid=ev_smp_id
   ) %>%
   subset(., select = c(
     env_date, sampleid, hh_vid, round, hh_hid, hh_mid, hh_st, ic, sample, vc.pos, sh.pos, vc.pres.pos, sh.pres.pos, momedu, haz, whz, sex, age, dia7, wealth_st,
     mnum4,mnum5,mnum6,mnum7,numcu5, elec,  dadagri, landacre, animals
   ))
head(dw)

sw <- d2 %>% 
  mutate(sample="SW",
    vc.pos=as.numeric(vc_kiit_pos), sh.pos=as.numeric(sh_kiit_pos),
    vc.pres.pos=as.numeric(vc_tcbs.1), sh.pres.pos=as.numeric(sh_pos.1),
    sex=as.numeric(hh_sex),
    age=as.numeric(dbmo),
    env_date=ymd(hh_st)) %>%
  rename(
    round=hh_rnd, 
    momedu=hoh_edu4,
    sampleid=ev_smp_id
  ) %>%
  subset(., select = c(
    env_date, sampleid, hh_vid, round, hh_hid, hh_mid,  hh_st, ic, sample, vc.pos, sh.pos, vc.pres.pos, sh.pres.pos, momedu, haz, whz,  sex, age,dia7, wealth_st,
    mnum4,mnum5,mnum6,mnum7,numcu5, elec, dadagri, landacre, animals
  ))
head(sw)

dim(sw)
dim(sw %>% distinct(hh_vid, round, hh_hid, hh_mid, env_date))

dim(dw)
dim(dw %>% distinct(hh_vid, round, hh_hid, hh_mid, env_date))

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
      ),
      hhwealth=as.numeric(wealth_st),
      Nhh=mnum4+mnum5+mnum6+mnum7+numcu5,
      haz=as.numeric(haz),
      whz=as.numeric(whz)) %>%
  rename(clusterid=hh_vid) %>%
  subset(., select = -c(ic, wealth_st, mnum4,mnum5,mnum6,mnum7,numcu5))



#Keep one obs per household


#Transform to long, and only keep confirmed positives:
df <- df %>% 
  gather(vc.pos:sh.pres.pos , key = target, value = pos ) %>%
  filter(!is.na(pos), target %in% c("sh.pos","vc.pos")) %>%
  mutate(logquant=NA,
         target = case_when(
           target=="sh.pos" ~"shigella",
           target=="vc.pos" ~"vibrio_cholera"
         )) 

head(df)

# for(i in colnames(d)[grepl("sh_",colnames(d))]){
#   cat(i,":\n")
#   print(table(d[[i]]))
# }

table(is.na(df$env_date))
saveRDS(df, file=paste0(dropboxDir,"Data/Gram Vikas/GV_env_cleaned.rds"))



#-----------------------------------------------------------------------------------------------------------
# Check numbers with Reese dissertation
#-----------------------------------------------------------------------------------------------------------

# sample water (n=1583) and drinking water (n=2044) were assayed for E. coli, Shigella spp., and V. cholerae,
# and children's hands (n=976)  for E. coli and Shigella spp.
table(df$sample , df$pos, df$round, df$target)
table(df$sample , df$pos, df$round, df$target)

#We collected samples of the sample water and drinking water for each household four times, once in each study round,
#and child hand rinse samples two times, in rounds 2 and 4. If a household randomly selected for sampling was absent,
#field workers collected samples from the nearest enrolled household to the right.

