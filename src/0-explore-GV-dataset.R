

# importing packages and setting directories
source(here::here("0-config.R"))

load(here("data/gramvikas.rda"))
d <- gramvikas
head(d)
colnames(d)


findVar <- function(var){
  colnames(d)[grepl(var,colnames(d))]
}






# Covariate list:
#   Child birth order/parity 
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







#Find head of household occupation (from codebook)
findVar("hh.hohjob") #function to see if there is a match for codebook variable
#If there is no match, try a partial match like so:
findVar("job")

#tabulate covariate
table(d$hh.hohjob)

#If you find a match with a listed adjustment covariate that's a factor, replace the numeric code 
#with the right levels from the codebook (and give more informative names.
#For example, for "hh.hohjob", these are the levels:
# "1 = Professional
# 2 = Sales worker/service worker
# 4 = Production worker
# 5 = Agricultural worker
# 6 = Not employed"

d <- d %>% mutate(
  occupation = case_when(
    hh.hohjob=="1" ~ "Professional",
    hh.hohjob=="2" ~ "service worker",
    hh.hohjob=="4" ~ "Production worker",
    hh.hohjob=="5" ~ "Agricultural worker",
    hh.hohjob=="6" ~ "Unemployed"
  ))



#Note that variables from the "CE" or "blood" form from the codebook aren't included in the data, so ignore those when trying to find covariates
