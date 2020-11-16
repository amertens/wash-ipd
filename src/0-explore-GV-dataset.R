

# import packages and setting directories
source(here::here("0-config.R"))

# load in data
load(here("data/gramvikas.rda"))

d <- gramvikas
head(d)
colnames(d)

# search for covariates 
findVar <- function(var){
  colnames(d)[grepl(var,colnames(d))]
}

# Covariate list:
# Child birth order/parity 
# Asset-based wealth index 
# Number of individuals and children in the household
# Household food security 
# Household electrification and construction, including wall/roof material 
# Parental age 
# Parental education 
# Parental employment (and Indicator for works in agriculture)
# Land ownership 
# Animal ownership 
# Season 
# Urban vs. rural 
# Child sex 
# Child age 
# Reported geophagia (soil eating)

########################################

#Find head of household occupation (from codebook)
findVar("hh.hohjob") #function to see if there is a match for codebook variable
#If there is no match, try a partial match like so:
findVar("job")

#tabulate covariate
table(d$hh.hohjob)

#If you find a match with a listed adjustment covariate that's a factor, replace the numeric
# code with the right levels from the codebook (and give more informative names.
#For example, for "hh.hohjob", these are the levels:
# 1 = Professional
# 2 = Sales worker/service worker
# 4 = Production worker
# 5 = Agricultural worker
# 6 = Not employed"

d <- d %>% mutate(
  occupation = case_when(
    hh.hohjob=="1" ~ "Professional",
    hh.hohjob=="2" ~ "Service worker",
    hh.hohjob=="4" ~ "Production worker",
    hh.hohjob=="5" ~ "Agricultural worker",
    hh.hohjob=="6" ~ "Unemployed"
  ))


#Note that variables from the "CE" or "blood" form from the codebook aren't included in the data, so ignore those when trying to find covariates


# UNKNOWN
# Child birth order/parity 
# Urban vs. rural 

# KNOWN
# 85 - Asset-based wealth index 
# Number of individuals and children in the household
# Household food security 
# Household electrification and construction, including wall/roof material 
# Parental age 
# Parental education 
# 36 - Parental employment (and Indicator for works in agriculture)
# Land ownership 
# Animal ownership 
# Season 
# Child sex 
# Child age 
# Reported geophagia (soil eating)

#Find asset-based wealth index (from codebook)
findVar("hh.asset") #function to see if there is a match for codebook variable

#tabulate covariate
table(d$hh.asset)

# replace the numeric code with the right levels from the codebook
# 1 = Refrigerator
# 2 = Scooter or motorcycle
# 3 = Landline telephone
# 4 = Mobile phone
# 5 = Electric fan
# 6 = Pressure cooker
# 7 = Chair
# 8 = Table
# 9 = Sewing machine
# 10 = Mattress
# 11 = Television
# 12 = Electricity


d <- d %>% mutate(
  assets = case_when(
    hh.asset=="1" ~ "Refrigerator",
    hh.asset=="2" ~ "Scooter or motorcycle",
    hh.asset=="3" ~ "Landline telephone",
    hh.asset=="4" ~ "Mobile phone",
    hh.asset=="5" ~ "Electric fan", 
    hh.asset=="6" ~ "Pressure cooker", 
    hh.asset=="7" ~ "Chair", 
    hh.asset=="8" ~ "Table", 
    hh.asset=="9" ~ "Sewing machine", 
    hh.asset=="10" ~ "Mattress", 
    hh.asset=="11" ~ "Television", 
    hh.asset=="12" ~ "Electricity"
  ))



#Find number of individuals and children in household
vars = as.list(findVar("mnum"))

#tabulate covariate
for (var in vars) {
  table(d$var)
}

# mnum4 - [Number of HH members, ppl >=60 yrs]
# mnum5 - [Number of HH members, men 18-59 yrs]
# mnum6 - [Number of HH members, women 18-59 yrs]
# mnum7 - [Number of HH members, child 5-17 yrs]
# numcu5 - [Number of HH members, child 5 yrs]
d = d %>% 
  rename(
    people_over60 = mnum4,
    men_18_59 = mnum5,
    women_18_59 = mnum6,
    child_5_17 = mnum7,
    child_5 = numcu5
  ) %>%
  mutate(
    total_hh_members = people_over60 + men_18_59 + women_18_59 + child_5_17 + child_5,
    total_children = child_5_17 + child_5
    )




#Find parental age
vars = findVar("va.3")

#tabulate covariate

d = d %>% 
  rename(
    respondent age = va.3)



