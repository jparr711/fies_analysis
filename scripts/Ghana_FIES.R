################ GHANA  ######################
library(haven)
library(tidyverse)
library(readxl)
library(vtable)
library(RM.weights)
library(survey)
# READ BASELINE

####### 1. IMPORT DATA ###################

# OR
setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Ghana/STATA/Analytic")
gh_hh_baseline_import <- read_dta("FTF_P2-ZOI_2020_Ghana-Survey_household_data_analytic.dta")

# READ MIDLINE
gh_hh_midline_import <- read_dta("FTF_P2-ZOI_2023_Ghana-Survey_household_data_analytic.dta")


# varlist
# vtable(gh_hh_midline_import)
# vtable(gh_hh_baseline_import)



######## 2. Data validation ################# 
# describe FIES vars

gh_hh_midline_import %>%
  dplyr::select(v301:v308) %>%
  psych::describe()
# View first 10 rows
gh_hh_midline_import %>%
  dplyr::distinct(hhea, hhnum, .keep_all = T) %>%
  dplyr::select(hhea, hhnum, v301:v308)


# v300d - count consent to FIES section
gh_hh_midline_import %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::count(v300d)


# view incomplete cases
gh_hh_midline_import %>%
  dplyr::select(v300d, v301:v308) %>%
  dplyr::filter(!complete.cases(.))
# 2 cases with all items missing, all respondent not at home. remove

fies_prop_ml <- 
  gh_hh_midline_import %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() %>%
  prop.table(., 1) * 100

# 
# proportion responding yes to each item
round(fies_prop_ml, 2)
# check refusals on v306


# examine correlation between items
# gh_hh_midline_import %>%
#   dplyr::select(v301:v308) %>%
#   psych::cor.plot()



# BASELINE
# see first 10 rows
gh_hh_baseline_import %>%
  dplyr::distinct(hhea, hhnum, .keep_all = T) %>%
  dplyr::select(hhea, hhnum, v301:v308)

# desc stats
gh_hh_baseline_import %>%
  dplyr::select(v301:v308) %>%
  psych::describe()

# incomplete cases
gh_hh_baseline_import %>%
  dplyr::select(v300e, v301:v308) %>%
  dplyr::filter(!complete.cases(.)) %>%
  as.data.frame()
# 29 rows with incomplete cases
# 2 HHs refused consnt to module. otherwise NA - refers to HHs who did not participate in interview as a whole


# many are interviews that were not completed
gh_hh_baseline_import %>%
  dplyr::count(ahresult)

gh_hh_baseline_import %>%
  dplyr::count(ahresult, v300e)


# look at FIES items who have completed interviews and NA result for v300d

gh_hh_baseline_import %>%
  dplyr::filter(is.na(v300d)) %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::select(hhea, hhnum, ahresult, v300d, v301:v308)
# NONE

#


# Incomplete cases - all have NAs for all FIES questions and . 
# Includes 14 completed interviews, the rest are incomplete for 
# various reasons (as shown in previous table)


gh_hh_baseline_import %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::select(v301:v308) %>%
  psych::describe()


# proportions - unweighted
fies_prop_bl <- 
  gh_hh_baseline_import %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() %>%
  prop.table(., 1) * 100

round(fies_prop_bl, 2)
round(fies_prop_ml, 2)

# view refused
gh_hh_baseline_import %>%
  #   dplyr::filter(is.na(v300e)) %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() 

# total N on each item  
gh_hh_baseline_import %>%
  #   dplyr::filter(is.na(v300e)) %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() %>%
  t() %>%
  colSums()

#### 3. Data prep #################################### ##############
# function for calculating sampling errors/MOE
source("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Nepal/R/FIES/moe_complex survey design.R")
#create a new function to compute population standard deviation
var.p <- function(x) var(x) * (length(x)-1) / length(x)
sd.p <- function(x) sqrt(var.p(x))

# gh_hh_midline_import %>%
#   dplyr::left_join()

gh_hh_baseline <- gh_hh_baseline_import

# Filter consent cases
gh_hh_baseline <- 
gh_hh_baseline %>%
#  dplyr::filter(ahresult == 1) %>%
  dplyr::filter(v300e == 1)
# SHOULD WE REMOVE PARTIAL COMPLETED INTERVIEWS?
# 
# Join shock_sev variable
gh_hh_bl_ml_shock_sev <- read_dta("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Ghana/STATA/Autopopulation/Resilience/Analytic/FTF_P2-ZOI_Survey_Ghana_BL_ML_resilience.dta")


gh_hh_midline  <- gh_hh_midline_import

# Filter consent cases
gh_hh_baseline <- 
  gh_hh_baseline %>%
#   dplyr::filter(ahresult == 1) %>%
  dplyr::filter(v300e == 1)

#  BASELINE RECODING
#  imputation
 gh_hh_baseline %>%
   dplyr::filter(v301 == 7) %>%
   dplyr::select(hhea, hhnum, stratum, v301:v308)
 # 0 HH
 
 
 gh_hh_baseline %>%
   dplyr::filter(v302 == 7) %>%
   dplyr::select(hhea, hhnum, stratum, v301:v308)
 # 1 HH
 
 # Surrounding values equal to yes. impute
 gh_hh_baseline <-
   gh_hh_baseline %>%
   dplyr::mutate(v302 = ifelse(hhea == 112 & hhnum == 1, 1, v302)) 
 
 # 
 gh_hh_baseline %>%
   dplyr::filter(v303 == 7) %>%
   dplyr::select(hhea, hhnum, stratum, v301:v308)
 # None
 gh_hh_baseline %>%
   dplyr::filter(v304 == 7) %>%
   dplyr::select(hhea, hhnum, stratum, v301:v308)
 # None
 gh_hh_baseline %>%
   dplyr::filter(v305 == 7) %>%
   dplyr::select(hhea, hhnum, stratum, v301:v308)
 # 1 HH
 # All other responses equal to NO. Impute to NO
 
 gh_hh_baseline <-
   gh_hh_baseline %>%
   dplyr::mutate(v305 = ifelse(hhea == 112 & hhnum == 11, 2, v305)) 
 
 
 gh_hh_baseline %>%
   dplyr::filter(v306 == 7) %>%
   dplyr::select(hhea, hhnum, stratum, v301:v308)
 
 
 # 4HH. 2 HHs with surrounding items equal to NO. 2 HHs with inconsistent neighbor values. 
 # Impute only HHs with
 # surrounding values equal to NO.
 
 gh_hh_baseline <-
   gh_hh_baseline %>%
   dplyr::mutate(v306 = ifelse(hhea == 111 & hhnum == 19, 2, v306),
                 v306 = ifelse(hhea == 182 & hhnum == 3, 2, v306)) 
 
 
 gh_hh_baseline %>%
   dplyr::filter(v307 == 7) %>%
   dplyr::select(hhea, hhnum, stratum, v301:v308)
 # 1 HH. Also with v306 equal to 7. Leave as is
 gh_hh_baseline %>%
   dplyr::filter(v308 == 7) %>%
   dplyr::select(hhea, hhnum, stratum, v301:v308)
 # 2 HHs. Unclear patterns for both. v307 equal to 1 with NOs in each (v305, v306) 
 # Leave
 
 gh_hh_baseline %>%
   dplyr::select(v301:v308) %>%
   pivot_longer(cols = v301:v308) %>%
   mutate(value = as_factor(value)) %>%
   dplyr::group_by(name) %>%
   count(value) %>%
   mutate(pct = n/sum(n)) %>%
   dplyr::select(name, value, pct) %>%
   pivot_wider(values_from = c(pct))
 
fies_prop_bl

# recode FIES vars - NO response from 2 to 0
gh_hh_baseline <- 
  gh_hh_baseline %>% mutate_at(dplyr::vars(v301:v308),
                               dplyr::funs(case_when(
                                 . == 1 ~ 1,
                                 . == 2 ~ 0,
                                 . == 7 ~ NA_real_)
                               ))

gh_hh_baseline %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name) %>%
  count(value) %>%
  mutate(pct = n/sum(n)) %>%
  dplyr::select(name, value, pct) %>%
  pivot_wider(values_from = c(pct))


# rename vars


gh_hh_baseline <- 
  gh_hh_baseline %>%
  dplyr::rename(
    "WORRIED" = "v301", 
    "HEALTHY" = "v302", 
    "FEWFOOD" = "v303", 
    "SKIPPED" = "v304", 
    "ATELESS" = "v305", 
    "RUNOUT"  = "v306", 
    "HUNGRY"  = "v307", 
    "WHLDAY"  = "v308")

table(is.na(gh_hh_baseline$genhhtype_dj))

table(gh_hh_baseline$genhhtype_dj)

gh_hh_baseline$genhhtype_dj <- factor(gh_hh_baseline$genhhtype_dj, levels = c(1,2,3,4), labels = c("De jure male and female adults","De jure female, no male", 
                                                                                                   "De jure male, no female", "De jure children only")) 



# cluster and HH id
gh_hh_baseline$hhea <- gh_hh_baseline$hhea
# hhnum
gh_hh_baseline$hhnum <- gh_hh_baseline$hhnum
# psu
gh_hh_baseline$psu <- gh_hh_baseline$a03d
# NEED TO RECODE - 
gh_hh_baseline$psu <- factor(gh_hh_baseline$psu, levels=c(12,14,15,16), labels=c("Northern", "North East", "Upper East", "Upper West"))

# Add region - same as psu 
gh_hh_baseline %>%
  dplyr::count(a03c)

gh_hh_baseline$region <- gh_hh_baseline$psu

gh_hh_baseline %>%
  dplyr::count(region)

# create combined weight and HH size
gh_hh_baseline$wt <- gh_hh_baseline$wgt_hh
gh_hh_baseline$wt_mem <- gh_hh_baseline$wt * gh_hh_baseline$hhsize_dj

# Urban/rural - not in ML dataset?
gh_hh_baseline$ahtype <- factor(gh_hh_baseline$ahtype, levels=c(1,2), labels=c("Urban","Rural"))

# strata
gh_hh_baseline$strata <- gh_hh_baseline$a03c
# survey round- midline == 2
gh_hh_baseline$survey <- 1

gh_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  summarise_all(~ sum(is.na(.)))


# wealth quintile

# shock severity

gh_hh_baseline <- 
  gh_hh_baseline %>%
  dplyr::select(-contains("awi")) %>%
  left_join(gh_hh_bl_ml_shock_sev %>%
              dplyr::filter(survey == "BL") %>%
              dplyr::select(hhea, hhnum, shock_sev, awiquint_rev))

table(gh_hh_baseline$awiquint_rev)
table(is.na(gh_hh_baseline$awiquint_rev))
# gh_hh_baseline$awiquint_rev <- forcats::fct_rev(factor(as.character(gh_hh_baseline$awiquint)))
gh_hh_baseline$awiquint_rev <- factor(gh_hh_baseline$awiquint_rev, levels = c(5,4,3,2,1), labels = c("Poorest","Second", "Middle", "Fourth", "Wealthiest"))
table(gh_hh_baseline$awiquint_rev)



gh_hh_baseline$shock_sev <- factor(gh_hh_baseline$shock_sev, levels = c(1,2,3,4), labels = c("None", "Low", "Medium", "High"))


# edu level
# gh_hh_baseline$edulevel_hh_dj <- factor(gh_hh_baseline$edulevel_hh2_dj, levels = c(1,2,3,4,5,6), labels = c("No education","Primary One 1-3", "Primary One 4-6", 
#                                                                                                            "Primary 2 1-3", "Secondary 1-4",
#                                                                                                            "Higher"))
#
#
#
#
#
#

# No HHID?
gh_hh_baseline <- 
  gh_hh_baseline %>%
  mutate(hhid = row_number())

head(gh_hh_baseline$hhid)

# select vars to keep, drop vars not needed for analysis

gh_hh_baseline <- 
  gh_hh_baseline %>%
  dplyr::select(survey, hhid, hhea, hhnum, psu, region, 
                wt, hhsize_dj, wt_mem, strata, genhhtype_dj, ahtype, # edulevel_hh_dj, 
                awiquint_rev, shock_sev, 
                WORRIED:WHLDAY)



# MIDLINE
# imputation
gh_hh_midline %>%
  dplyr::filter(v301 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# 1 HH

# Impute - Yes responses on more severe items v303, v305, etc.
gh_hh_midline <-
  gh_hh_midline %>%
  dplyr::mutate(v301 = ifelse(hhea == 45 & hhnum == 6, 1, v301)) 


gh_hh_midline %>%
  dplyr::filter(v302 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# 1 HH

# Same HH. impute
gh_hh_midline <-
  gh_hh_midline %>%
  dplyr::mutate(v302 = ifelse(hhea == 45 & hhnum == 6, 1, v302)) 


# 
gh_hh_midline %>%
  dplyr::filter(v303 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# NOne
gh_hh_midline %>%
  dplyr::filter(v304 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# None
gh_hh_midline %>%
  dplyr::filter(v305 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# 1 HH
# No responses on surrounding cases. Impute to NO

gh_hh_midline <-
  gh_hh_midline %>%
  dplyr::mutate(v305 = ifelse(hhea == 41 & hhnum == 61, 2, v305)) 


gh_hh_midline %>%
  dplyr::filter(v306 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# 1HH. Same HH

gh_hh_midline <-
  gh_hh_midline %>%
  dplyr::mutate(v306 = ifelse(hhea == 41 & hhnum == 61, 2, v306)) 


gh_hh_midline %>%
  dplyr::filter(v307 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# None
gh_hh_midline %>%
  dplyr::filter(v308 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# 1HH 

# v307 is equal to No. v304 as well. Impute to no.
gh_hh_midline <-
  gh_hh_midline %>%
  dplyr::mutate(v308 = ifelse(hhea == 89 & hhnum == 65, 2, v308)) 



table(gh_hh_midline$v300d)

# recode FIES vars - NO response from 2 to 0
gh_hh_midline <- 
  gh_hh_midline %>% mutate_at(dplyr::vars(v301:v308),
                              dplyr::funs(case_when(
                                . == 1 ~ 1,
                                . == 2 ~ 0,
                                . == 7 ~ NA_real_)
                              ))
# rename vars
gh_hh_midline <- 
  gh_hh_midline %>%
  dplyr::rename(
    "WORRIED" = "v301", 
    "HEALTHY" = "v302", 
    "FEWFOOD" = "v303", 
    "SKIPPED" = "v304", 
    "ATELESS" = "v305", 
    "RUNOUT"  = "v306", 
    "HUNGRY"  = "v307", 
    "WHLDAY"  = "v308")

# sampling weights + size of hh
gh_hh_midline$wt <- gh_hh_midline$wgt_hh


# cluster and HH id
gh_hh_midline$hhea <- gh_hh_midline$hhea
# hhnum
gh_hh_midline$hhnum <- gh_hh_midline$hhnum
# psu
gh_hh_midline$psu <- gh_hh_midline$c05

gh_hh_midline$psu <- factor(gh_hh_midline$psu, levels=c(12,14,15,16), labels=c("Northern", "North East", "Upper East", "Upper West"))

# Add region - same as psu 
gh_hh_midline %>%
  dplyr::count(c05)

gh_hh_midline$region <- gh_hh_midline$psu

gh_hh_midline %>%
  dplyr::count(region)


# create combined weight and HH size
gh_hh_midline$wt_mem <- gh_hh_midline$wt * gh_hh_midline$hhsize_dj


# strata
gh_hh_midline$strata <- gh_hh_midline$stratum
# survey round- midline == 2
gh_hh_midline$survey <- 2

# add background variables as factors, with levels and labels
# HH type
gh_hh_midline$genhhtype_dj <- gh_hh_midline$genhhtype_dj
# NOT IN DATASET
gh_hh_midline$genhhtype_dj <- factor(gh_hh_midline$genhhtype_dj, levels = c(1,2,3,4), labels = c("De jure male and female adults","De jure female, no male", 
                                                                                                 "De jure male, no female", "De jure children only")) 


# urban - rural not in dataset?
gh_hh_midline$ahtype <- factor(gh_hh_midline$c22, levels=c(1,2), labels=c("Urban","Rural"))
# highest HH education level
# NOT IN DATASET
# gh_hh_midline$edulevel_hh_dj <- factor(gh_hh_midline$edulevel_hh2_dj, levels = c(1,2,3,4,5,6), labels = c("No education","Primary One 1-3", "Primary One 4-6", 
#                                                                                                          "Primary 2 1-3", "Secondary 1-4",
#                                                                                                          "Higher"))
# wealth quintile & shock_sev

gh_hh_midline <- 
  gh_hh_midline %>%
  left_join(gh_hh_bl_ml_shock_sev %>% 
              dplyr::filter(survey == "ML") %>%
              dplyr::select(hhea, hhnum, shock_sev, awiquint_rev))

table(gh_hh_midline$awiquint_rev)

gh_hh_midline$awiquint_rev <- factor(gh_hh_midline$awiquint_rev, levels = c(5,4,3,2,1), labels = c("Poorest","Second", "Middle", "Fourth", "Wealthiest"))

table(gh_hh_midline$awiquint_rev)
table(gh_hh_baseline$awiquint_rev)

gh_hh_midline$awiquint_rev
table(gh_hh_midline$awiquint_rev)

# shock severity
# 
gh_hh_midline$shock_sev <- factor(gh_hh_midline$shock_sev, levels = c(1,2,3,4), labels = c("None", "Low", "Medium", "High"))
table(gh_hh_midline$shock_sev)
# No HHID?
gh_hh_midline <- 
  gh_hh_midline %>%
  mutate(hhid = row_number())

# select vars - NOT READY
gh_hh_midline <- 
  gh_hh_midline %>%
  dplyr::select(survey, hhid, hhea, hhnum, wt, 
                psu, region, strata, ahtype,
                hhsize_dj, 
                wt_mem, genhhtype_dj, # edulevel_hh_dj, 
                awiquint_rev, shock_sev, 
                WORRIED:WHLDAY)

### FINAL EXAMINATION OF MISSING DATA #########
# examine missing vals
gh_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  filter_at(dplyr::vars(WORRIED:WHLDAY), dplyr::any_vars(is.na(.))) %>%
  as.data.frame()


gh_hh_midline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  filter_at(dplyr::vars(WORRIED:WHLDAY), dplyr::any_vars(is.na(.))) %>%
  as.data.frame()

# 2 HHs with all missing in ML. 4 HHs with missing items in BL, unclear pattern.

# remove HHs with all FIES items that are missing
gh_hh_baseline <-
  gh_hh_baseline %>%
  filter_at(dplyr::vars(WORRIED:WHLDAY), dplyr::all_vars(!is.na(.))) %>%
  as.data.frame()


gh_hh_midline <-
  gh_hh_midline %>%
  filter_at(dplyr::vars(WORRIED:WHLDAY), dplyr::all_vars(!is.na(.))) %>%
  as.data.frame()

# check pros
gh_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  pivot_longer(cols = WORRIED:WHLDAY) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name) %>%
  count(value) %>%
  mutate(pct = n/sum(n)) %>%
  dplyr::select(name, value, pct) %>%
  pivot_wider(values_from = c(pct))

gh_hh_midline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  pivot_longer(cols = WORRIED:WHLDAY) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name) %>%
  count(value) %>%
  mutate(pct = n/sum(n)) %>%
  dplyr::select(name, value, pct) %>%
  pivot_wider(values_from = pct)

fies_prop_ml

gh_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  pivot_longer(cols = WORRIED:WHLDAY) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name) %>%
  count(value) %>%
  mutate(pct = n/sum(n)) %>%
  dplyr::select(name, value, pct) %>%
  pivot_wider(values_from = pct)

fies_prop_bl

# CREATE COMBINED from BL and ML
gh_hh_combined <- bind_rows(gh_hh_baseline, gh_hh_midline)




##################### 4. Rasch Modeling ####################
# generate two matrices with FIES questions only: total, baseline and midline

# BL
FIES_baseline <-
  gh_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY)
# ML
FIES_midline <-
  gh_hh_midline %>%
  dplyr::select(WORRIED:WHLDAY)



# create row score var RS that sums number of yes responses across all items
gh_hh_baseline$RS = rowSums(FIES_baseline)
gh_hh_midline$RS = rowSums(FIES_midline)


# Step 3a - run RASCH model 
# NOTE: rasch models are invariant to weights. We can still use them however they are not necessary.

# baseline
library(RM.weights)
res_bl = RM.w(as.matrix(FIES_baseline))
# midline
res_ml = RM.w(as.matrix(FIES_midline))

# DIAGNOSTICS
# Check statistical validity of the measurement model
# Step 3b. check item infit -
# Infit means inlier-sensitive or information-weighted fit. This is more sensitive to the pattern of responses to items
# targeted on the person, and vice-versa. For example, infit reports overfit for Guttman patterns, 
# underfit for alternative curricula or idiosyncratic clinical groups. These patterns can be hard to diagnose and remedy.

# NOTE: if values are 1.3 or above, remove item and rerun rasch model
Infit_table = cbind("Baseline" = res_bl$infit,
                    "Midline" = res_ml$infit)

round(Infit_table, 2)

# Step 3c - Check reliability statistic and SD - 
# ideally, reliability statistic should be .7 or greater and SD should be over  1.5
# Rasch reliability (flat statistic) is an estimate of how much the variability is explained by the Rasch model.
# Similar to an R2 in the context of a regression. The closer to 1, the more that the model is fitting the data.

# Usually low item reliability is because the person sample size is too small to establish a reproducible item difficulty hierarchy.
# if reliability is lower than suggested threshold, examine NA values and remove them. this can adversely impact precision and reliability.

# if neither of these conditions are met, it might require examining respondent OUTFIT values
# to remove households that have outlier response patterns (i.e. respondents who respond "YES" to items out of line with the severity of the items)

# once these households are removed, the rasch model can be rerun and the reliability statistc and SD can be reassesed.

Check_table = rbind(cbind("Baseline" = res_bl$reliab.fl, 
                          "Midline"  = res_ml$reliab.fl),
                    cbind("Baseline" = sd.p(res_bl$b),
                          "Midline"  = sd.p(res_ml$b)))
rownames(Check_table) = c("Reliability", "SD")

round(Check_table, 2)


# Step 3d - analysis of residuals run PCA on residual matrix. 
# this tells us what the variance not explained by the model is due to the principal components.
# should look like white noise (no structure) and should not be dominated by any one principal component
# compute variance explained by residuals to see if items are balanced.

# if any item score dominates such that the screeplot looks like a hockey stick or L shape.

pca_res_variance <- cbind("Baseline" = prcomp(res_bl$mat.res)$sd^2, # baseline
                          "Midline" = prcomp(res_ml$mat.res)$sd^2) %>% # midline 
  tibble::as_tibble() %>%
  dplyr::mutate(Item = as.factor(as.character(row_number()))) %>%
  tidyr::pivot_longer(cols = c("Baseline", "Midline"),
                      names_to = "Survey", 
                      values_to = "Variance")
# visualize results
library(ggthemes)
ggplot(pca_res_variance, aes(x = Item, y= Variance)) +
  facet_grid(Survey ~ .) +
  geom_col() +
  #  geom_line()  +
  #  coord_flip() +
  theme_fivethirtyeight() +
  ggtitle("Screeplot of Variance") +
  theme(axis.title = element_text(size = 10)) +
  xlab("Item") +
  ylab("Variance Explained")


# Step 3e - Checking stability of the scale across rounds
{
  x = res_bl$b/sd.p(res_bl$b)
  y_ml = res_ml$b/sd.p(res_ml$b)
  plot(x,y_ml, col = 1, ylim = c(-2.5,2),ylab="",xlab="")
  points(x, y_ml, col = 2)
  text(x+0.04,x-0.25,colnames(FIES_baseline),cex=0.6,pos=2,srt=90)
  abline(0,1,lty = 2)
  legend("topleft",c("baseline","midline"),pch = 1, col = c(1,2), cex = 0.75)
  title(main = "Comparing scales across surveys")
  }

(res_bl$b/sd.p(res_bl$b)) -  (res_ml$b/sd.p(res_ml$b))

# Step 3f - Equating across rounds (baseline and midline).
# the baseline and midline need to be put on a common scale in order
# to ensure comparison.
# identify items in common - one where scores are similar. all scores are similar in GHANA
common = colnames(FIES_baseline)

m.bl = mean(res_bl$b[common]) 
m.ml = mean(res_ml$b[common])
# sd of item scores
s.bl = sd.p(res_bl$b[common])
s.ml = sd.p(res_ml$b[common])

# Adjust midline scale to the baseline metric
adj_ml = res_ml
adj_ml$b = (res_ml$b - m.ml)/s.ml*s.bl + m.bl
adj_ml$a = (res_ml$a - m.ml)/s.ml*s.bl + m.bl
adj_ml$se.a = res_ml$se.a/s.ml*s.bl

# visualize results to see difference in values
{
  x = res_bl$b/sd.p(res_bl$b)
  y_bl = res_bl$b/sd.p(res_bl$b)
  y_ml = res_ml$b/sd.p(res_ml$b)
  y_gh_adj = adj_ml$b/sd.p(adj_ml$b)
  plot(x,y_bl, col = 1, ylim = c(-2.5,2),ylab="",xlab="")
  points(x, y_ml, col = 2)
  points(x,y_gh_adj, col = 3)
  text(x+0.04,x-0.25,colnames(FIES_baseline),cex=0.6,pos=2,srt=90)
  abline(0,1,lty = 2)
  legend("topleft",c("baseline","midline","midline adjusted"),pch = 1, col = c(1,2,3), cex = 0.75)
  title(main = "Comparing scales across surveys - after adjustment")
}


{
  x = res_bl$a
  plot(x,res_ml$a,col = 1,xlab="Baseline RS severity", ylab = "Midline RS severity")
  points(x,adj_ml$a, pch = 2, col = 2)
  abline(0,1)
  legend("topleft",c("Before adjustment","After adjustment"),pch=c(1,2),col=c(1,2))
  title(main="Adjusting the severity levels associated to raw scores")
}


# create matrix with (weighted) distributions of Raw Scores for Total, Baseline, and Midline
RS_table = t(cbind(
  "Baseline" = aggregate(gh_hh_baseline$wt, list(gh_hh_baseline$RS), FUN=sum, na.rm=TRUE)$x /sum(gh_hh_baseline$wt[!is.na(gh_hh_baseline$RS)]),
  "Midline" = aggregate(gh_hh_midline$wt, list(gh_hh_midline$RS), FUN=sum, na.rm=TRUE)$x /sum(gh_hh_midline$wt[!is.na(gh_hh_midline$RS)])
))

RS_table %>%
  as.data.frame() %>%
  rownames_to_column("Survey") %>%
  pivot_longer(V1:V9) %>%
  dplyr::mutate(name = str_replace(name, "V", "Raw score ")) %>%
  dplyr::mutate(name = str_replace_all(name, c("1"="0","2"="1","3"="2","4"="3","5"="4","6"="5","7"="6","8"="7","9"="8"))) %>% 
  ggplot(., aes(x = name, y = value, color = Survey, fill = Survey)) +
  geom_col(stat = "count", position = position_dodge()) +
  xlab("Raw Scores") +
  ylab("Proportion") +
  coord_flip() +
  scale_x_discrete(limits=rev)


### Step 4 - COMPUTE PREVALENCE RATES ####

# step 4a - map global scale onto local (combined) scale
# define LOCAL scale
loc_st <- res_bl$b
## use the baseline scale (not the combined) as reference.
# This is because you already adjusted the midline to the baseline metrics, so 
# it is natural to think of the baseline as the reference
# Then, you only need to map the thresholds from the global refernce scale, onto
# the baseline


# global scale item values - these are based on the FAO's estimates from
# a series of surveys done from 2014-2016.
# more can be found on the website 
glob_st = c("WORRIED"= -1.2230564, "HEALTHY"= -0.847121, "FEWFOODS"= -1.1056616,
            "SKIPPED" = 0.3509848, "ATELESS" = -0.3117999, "RUNOUT" = 0.5065051, 
            "HUNGRY" = 0.7546138, "WHLDAY" = 1.8755353)

# comparing items in both scales
round(loc_st - glob_st, 2)
round(loc_st/sd.p(loc_st) - glob_st/sd.p(glob_st), 2)
plot(x = loc_st, y = glob_st)
text(x = loc_st, y = glob_st, cex = 0.6, pos = 4)
abline(0,1,lty = 2)

loc_st

glob_st
abs(glob_st - loc_st)
# standardized version of both scales
plot(x = loc_st/sd.p(loc_st), y = glob_st/sd.p(glob_st))
text(x = loc_st/sd.p(loc_st), y = glob_st/sd.p(glob_st),names(glob_st),cex=0.6,pos=4)
abline(0,1,lty = 2)

round(loc_st/sd.p(loc_st) - glob_st/sd.p(glob_st), 2)
# exclude Items 8 (WHLDAY), 7 (HUNGRY) from the equating set - scores are significantly different 

# produce mean and sd for each scale among common items used in equating
# columns 1-6
glob_st.m <- mean(glob_st)
glob_st.s <- sd.p(glob_st)
m.bl = mean(res_bl$b) 
s.bl = sd.p(res_bl$b)


# mapping the thresholds from the global scale onto the local (baseline) scale

glob_st_adj = (glob_st - glob_st.m)/glob_st.s * s.bl  + m.bl

glob_st_adj

newthres = glob_st_adj[c(5,8)]
# GLOBAL ESTIMATION

# proceed by computing the combined prevalence as weighted average 
# of the baseline and midline
# GLOBAL ESTIMATION
# moderate+severe FI
glo_probs_bl_mod_sev = 1-pnorm(newthres[1], mean = res_bl$a, sd = res_bl$se.a)

glo_probs_gh_mod_sev = 1-pnorm(newthres[1], mean = adj_ml$a, sd = adj_ml$se.a)
glo_probs_bl_mod_sev[1] = glo_probs_gh_mod_sev[1] = 0

glo_prev_bl_mod_sev = glo_probs_bl_mod_sev %*% RS_table[1,]

glo_prev_gh_adj_mod_sev = glo_probs_gh_mod_sev %*% RS_table[2,]

# check results
# baseline food insecurity - moderate+severe
glo_prev_bl_mod_sev
# midline food insecurity - moderate+severe
glo_prev_gh_adj_mod_sev


# severe FI
glo_probs_bl_sev = 1-pnorm(newthres[2], mean = res_bl$a, sd = res_bl$se.a)
glo_probs_gh_sev = 1-pnorm(newthres[2], mean = adj_ml$a, sd = adj_ml$se.a)
glo_probs_bl_sev[1] = glo_probs_gh_sev[1] =0
glo_prev_bl_sev = glo_probs_bl_sev %*% RS_table[1,]
glo_prev_gh_adj_sev = glo_probs_gh_sev %*% RS_table[2,]

# check results
# baseline food insecurity - severe
glo_prev_bl_sev
# baseline food insecurity - severe
glo_prev_gh_adj_sev
# glo_prev_gh_not_adj_sev

# create moderate only = mod+sev - sev
glo_prev_bl_mod <- glo_prev_bl_mod_sev - glo_prev_bl_sev
glo_prev_gh_adj_mod <- glo_prev_gh_adj_mod_sev - glo_prev_gh_adj_sev

glo_probs_bl_mod <- glo_probs_bl_mod_sev - glo_probs_bl_sev
glo_probs_gh_mod <- glo_probs_gh_mod_sev - glo_probs_gh_sev

# put into common object
glo_prev_bl <- c(glo_prev_bl_mod_sev, glo_prev_bl_mod, glo_prev_bl_sev)
glo_prev_ml <- c(glo_prev_gh_adj_mod_sev, glo_prev_gh_adj_mod, glo_prev_gh_adj_sev)

glo_prev_bl
glo_prev_ml


### Step 5 - assigning probability ad computing aggregate prevalence ####
# read in MOE script
library(survey)

#Attaching probabilities to each case/HH
gh_hh_baseline$prob_mod_sev = NULL
gh_hh_midline$prob_mod_sev = NULL
gh_hh_baseline$prob_mod = NULL
gh_hh_midline$prob_mod = NULL
gh_hh_baseline$prob_sev = NULL
gh_hh_midline$prob_sev = NULL


for (rs in 0:8) {
  gh_hh_baseline$prob_mod[gh_hh_baseline$RS == rs] = glo_probs_bl_mod[rs+1]
  gh_hh_midline$prob_mod[gh_hh_midline$RS == rs] =   glo_probs_gh_mod[rs+1]
  gh_hh_baseline$prob_mod_sev[gh_hh_baseline$RS == rs] = glo_probs_bl_mod_sev[rs+1]
  gh_hh_midline$prob_mod_sev[gh_hh_midline$RS == rs] =   glo_probs_gh_mod_sev[rs+1]
  gh_hh_baseline$prob_sev[gh_hh_baseline$RS == rs] = glo_probs_bl_sev[rs+1]
  gh_hh_midline$prob_sev[gh_hh_midline$RS == rs] =   glo_probs_gh_sev[rs+1]
}

table(gh_hh_baseline$prob_mod,gh_hh_baseline$RS,useNA = "ifany")

table(gh_hh_baseline$prob_mod_sev,gh_hh_baseline$RS,useNA = "ifany")

table(gh_hh_baseline$prob_sev,gh_hh_baseline$RS,useNA = "ifany")


table(gh_hh_midline$prob_mod,gh_hh_midline$RS,useNA = "ifany")

table(gh_hh_midline$prob_mod_sev,gh_hh_midline$RS,useNA = "ifany")

table(gh_hh_midline$prob_sev,gh_hh_midline$RS,useNA = "ifany")



#### Step 6 - Calculate aggregate prevalence levels and put in df #####
AGG_df <- data.frame(NA, nrow = 2, ncol = 17)

modsev_moe_bl_95 <- moe(gh_hh_baseline$prob_mod_sev,gh_hh_baseline$RS,gh_hh_baseline$wt * 10^6, conf.level = .95,
                        psu = gh_hh_baseline$psu, strata = gh_hh_baseline$strata)$moe * 100
mod_moe_bl_95 <- moe(gh_hh_baseline$prob_mod,gh_hh_baseline$RS,gh_hh_baseline$wt * 10^6, conf.level = .95,
                     psu = gh_hh_baseline$psu, strata = gh_hh_baseline$strata)$moe * 100
sev_moe_bl_95 <- moe(gh_hh_baseline$prob_sev,gh_hh_baseline$RS,gh_hh_baseline$wt * 10^6, conf.level = .95,
                     psu = gh_hh_baseline$psu, strata = gh_hh_baseline$strata)$moe * 100

moe_bl_95 <- c(modsev_moe_bl_95, mod_moe_bl_95, sev_moe_bl_95)

modsev_moe_gh_95 <- moe(gh_hh_midline$prob_mod_sev,gh_hh_midline$RS,gh_hh_midline$wt * 10^6, conf.level = .95,
                        psu = gh_hh_midline$psu, strata = gh_hh_midline$strata)$moe * 100
mod_moe_gh_95 <- moe(gh_hh_midline$prob_mod,gh_hh_midline$RS,gh_hh_midline$wt * 10^6, conf.level = .95,
                     psu = gh_hh_midline$psu, strata = gh_hh_midline$strata)$moe * 100
sev_moe_gh_95 <- moe(gh_hh_midline$prob_sev,gh_hh_midline$RS,gh_hh_midline$wt * 10^6, conf.level = .95,
                     psu = gh_hh_midline$psu, strata = gh_hh_midline$strata)$moe * 100

moe_gh_95 <- c(modsev_moe_gh_95, mod_moe_gh_95, sev_moe_gh_95)


AGG_df[1, c(1,2, 3)] <- glo_prev_bl * 100
AGG_df[1, c(4)] <- nrow(res_bl$XX)
AGG_df[1, c(5)] <- round(sum(gh_hh_baseline$wt * 10^6), 2)
AGG_df[1, c(6, 7, 8)] <- moe_bl_95
AGG_df[1, c(9, 10, 11)] <- (glo_prev_bl * 100) - moe_bl_95 
AGG_df[1, c(12, 13, 14)] <- (glo_prev_bl * 100) + moe_bl_95
AGG_df[1, c(15, 16, 17)] <- c(NA, NA, NA)

AGG_df[2, c(1,2, 3)] <- glo_prev_ml * 100
AGG_df[2, c(4)] <- nrow(res_ml$XX)
AGG_df[2, c(5)] <- round(sum(gh_hh_midline$wt * 10^6), 2)
AGG_df[2, c(6, 7, 8)] <- moe_gh_95
AGG_df[2, c(9, 10, 11)] <- (glo_prev_ml * 100) - moe_gh_95 
AGG_df[2, c(12, 13, 14)] <- (glo_prev_ml * 100) + moe_gh_95
AGG_df[2, c(15, 16, 17)] <- ifelse(((glo_prev_ml * 100) - (glo_prev_bl * 100)) > moe_gh_95, "T", "F")


colnames(AGG_df) = c("Moderate+Severe_Food_Insecurity", "Moderate_Food_Insecurity", "Severe_Food_Insecurity",
                     "N","WN", 
                     "MSFI_MoE", "MFI_MoE","SFI_MoE",
                     "MSFI_CI_Low", "MFI_CI_Low", "SFI_CI_Low",
                     "MSFI_CI_High", "MFI_CI_High", "SFI_CI_High",
                     "MSFI_Sig", "MFI_Sig", "SFI_Sig")
rownames(AGG_df) = c("Baseline","Midline")

AGG_df


#### 7. Disaggregate Analysis - ############ 
# Computing prevalence and MoEs by groups
# join BL and ML into common dataset
gh_hh_combined <- bind_rows(gh_hh_baseline, gh_hh_midline)

group1 = gh_hh_combined$survey
group1 <- factor(group1, levels = c(1,2), labels = c("Baseline","Midline")) 
# URBAN/RURAL
group2 = gh_hh_combined$ahtype
# REGION
group3 = gh_hh_combined$region
# GEN HH TYPE
group4 = gh_hh_combined$genhhtype_dj
# SHOCK SEV
group5 <- gh_hh_combined$shock_sev
# AWIQUINT
group6 <- gh_hh_combined$awiquint_rev
# EDUCATION
# group7 <- as.character(gh_hh_combined$edulevel_hh_dj)

group_list <- list(group2, group3, group4, group5, group6)

groups <- c(unique(as.character(group2)), unique(as.character(group3)), 
            unique(as.character(group4)), unique(as.character(group5)), 
            unique(as.character(group6)))

# insert disaggregate values into empty dataframe
# moderate
mod_fi <- data.frame()

for (i in 1:length(group_list)) {
  for (round in unique(group1)) {
    for (dis in unique(groups)) {
      if(!(dis %in% group_list[[i]])) next 
      rd = round
      disag = dis
      fltr = which(group1 == round &  group_list[[i]] == dis)
      prob_mods = gh_hh_combined$prob_mod[fltr]
      wt = gh_hh_combined$wt[fltr]*10^6
      rs = gh_hh_combined$RS[fltr]
      psu = gh_hh_combined$psu[fltr] 
      strata = gh_hh_combined$strata[fltr]
      output_1 = rd 
      output_2 = disag
      output_3 = length(fltr) 
      output_4 = sum(wt) 
      output_5 = wtd.mean(prob_mods,wt) * 100
      output_6 = moe(prob_mods,rs,wt,psu=psu,strata=strata, conf.level = .95)$moe * 100
      output_7 = moe(prob_mods,rs,wt,psu=psu,strata=strata,  conf.level = .95)$deff
      tot_output = c(output_1, output_2, output_3, round(output_4, 1), round(output_5, 1), 
                     round(output_6, 1), round(output_7, 1))
      mod_fi <- rbind(mod_fi, tot_output)
    }
  }
  colnames(mod_fi) = c("Survey_Round", "Disaggregate", "N","WN","FI","MoE", "DEFF")
}

mod_fi

# severe food insecurity
sev_fi <- data.frame()

for (i in 1:length(group_list)) {
  for (round in unique(group1)) {
    for (dis in unique(groups)) {
      if(!(dis %in% group_list[[i]])) next 
      rd = round
      disag = dis
      fltr = which(group1 == round &  group_list[[i]] == dis)
      prob_mods = gh_hh_combined$prob_sev[fltr]
      wt = gh_hh_combined$wt[fltr]*10^6
      rs = gh_hh_combined$RS[fltr]
      psu = gh_hh_combined$psu[fltr] 
      strata = gh_hh_combined$strata[fltr]
      output_1 = rd 
      output_2 = disag
      output_3 = length(fltr) 
      output_4 = sum(wt) 
      output_5 = wtd.mean(prob_mods,wt) * 100
      output_6 = moe(prob_mods,rs,wt,psu=psu,strata=strata, conf.level = .95)$moe * 100
      output_7 = moe(prob_mods,rs,wt,psu=psu,strata=strata,  conf.level = .95)$deff
      tot_output = c(output_1, output_2, output_3, round(output_4, 1), round(output_5, 1), 
                     round(output_6, 1), round(output_7, 1))
      sev_fi <- rbind(sev_fi, tot_output)
    }
  }
  colnames(sev_fi) = c("Survey_Round", "Disaggregate", "N","WN","FI","MoE", "DEFF")
}

sev_fi

# moderate and severe food insecurity
mod_sev_fi <- data.frame()

for (i in 1:length(group_list)) {
  for (round in unique(group1)) {
    for (dis in unique(groups)) {
      if(!(dis %in% group_list[[i]])) next 
      rd = round
      disag = dis
      fltr = which(group1 == round &  group_list[[i]] == dis)
      prob_mods = gh_hh_combined$prob_mod_sev[fltr]
      wt = gh_hh_combined$wt[fltr]*10^6
      rs = gh_hh_combined$RS[fltr]
      psu = gh_hh_combined$psu[fltr] 
      strata = gh_hh_combined$strata[fltr]
      output_1 = rd 
      output_2 = disag
      output_3 = length(fltr) 
      output_4 = sum(wt) 
      output_5 = wtd.mean(prob_mods,wt) * 100
      output_6 = moe(prob_mods,rs,wt,psu=psu,strata=strata, conf.level = .95)$moe * 100
      output_7 = moe(prob_mods,rs,wt,psu=psu,strata=strata,  conf.level = .95)$deff
      tot_output = c(output_1, output_2, output_3, round(output_4, 1), round(output_5, 1), 
                     round(output_6, 1), round(output_7, 1))
      mod_sev_fi <- rbind(mod_sev_fi, tot_output)
    }
  }
  colnames(mod_sev_fi) = c("Survey_Round", "Disaggregate", "N","WN","FI","MoE", "DEFF")
}

mod_sev_fi

mod_sev_fi <- 
  mod_sev_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_Low = format(as.numeric(CI_Low), nsmall = 1, trim = T),
                CI_High = round((FI + MoE), 1),
                CI_High = format(as.numeric(CI_High), nsmall = 1, trim = T),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  dplyr::filter(N > 0) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(mod_sev_fi)


mod_fi <- 
  mod_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_Low = format(as.numeric(CI_Low), nsmall = 1, trim = T),
                CI_High = round((FI + MoE), 1),
                CI_High = format(as.numeric(CI_High), nsmall = 1, trim = T),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  dplyr::filter(N > 0) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

mod_fi



sev_fi <- 
  sev_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_Low = format(as.numeric(CI_Low), nsmall = 1, trim = T),
                CI_High = round((FI + MoE), 1),
                CI_High = format(as.numeric(CI_High), nsmall = 1, trim = T),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  dplyr::filter(N > 0) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(sev_fi)


##### RESHAPE DATA AND STORE RESULTS #########
# rename columns for joining together

colnames(sev_fi)[-1] <- paste0("Severe_", colnames(sev_fi)[-1])

colnames(sev_fi)

colnames(mod_fi)[-1] <- paste0("Moderate_", colnames(mod_fi)[-1])

colnames(mod_fi)

table_5_1_2 <- 
  mod_fi %>% 
  left_join(sev_fi, by = "Disaggregate")

table_5_1_1 <- mod_sev_fi

# reorder rows?
table_5_1_1

table_5_1_1 <-
  table_5_1_1 %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "Urban", "Rural", "None", "Low", "Medium", "High",
                                "Northern", "North East", "Upper East", "Upper West")))

table_5_1_2 <-
  table_5_1_2 %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "Urban", "Rural", "None", "Low", "Medium", "High",
                                "Northern", "North East", "Upper East", "Upper West"))) %>%
  dplyr::select(Disaggregate, ends_with("_Baseline"), ends_with("Midline"), ends_with("_Diff"), ends_with("_Sig"))

AGG_df_511_join <-
  AGG_df %>% 
  dplyr::select(starts_with(c("Moderate+", "MSFI")), N) %>%
  rownames_to_column("Period") %>%
  pivot_wider(values_from = -Period, names_from = Period) %>%
  #  names(.)
  `colnames<-`(c("FI_Baseline", "FI_Midline", 
                 "MoE_Baseline", "MoE_Midline",
                 "CI_Low_Baseline", "CI_Low_Midline",
                 "CI_High_Baseline", "CI_High_Midline",
                 "Sig_Baseline", "Sig_Midline",
                 "N_Baseline", "N_Midline" 
  )) %>% 
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE),
         CI_Baseline = paste0("(", round(CI_Low_Baseline, 1) , ", ", round(CI_High_Baseline, 1), ")"),
         CI_Midline = paste0("(", round(CI_Low_Midline, 1), ", ", round(CI_High_Midline, 1), ")"),
         Disaggregate = "All Households") %>%
  dplyr::select(-Sig_Baseline, -Sig_Midline, -starts_with(c("CI_Low_", "CI_High")))

AGG_df_512_join <-
  AGG_df %>% 
  dplyr::select(-starts_with(c("Moderate+", "MSFI")), -WN) %>%
  rownames_to_column("Period") %>%
  pivot_wider(values_from = -Period, names_from = Period)

names(AGG_df_512_join) <- 
  AGG_df_512_join %>% names() %>% 
  str_replace_all('Food_Insecurity', "FI") %>% 
  str_replace_all('MFI', "Moderate") %>%
  str_replace_all('SFI', "Severe")

AGG_df_512_join <- 
  AGG_df_512_join %>% 
  mutate(Moderate_Diff = Moderate_FI_Midline - Moderate_FI_Baseline,
         Severe_Diff = Severe_FI_Midline - Severe_FI_Baseline,
         Severe_N_Baseline = N_Baseline,
         Moderate_N_Baseline = N_Baseline,
         Severe_N_Midline = N_Midline,
         Moderate_N_Midline = N_Midline,
         
         Moderate_Sig = ifelse(abs(Moderate_Diff) > abs(Moderate_MoE_Midline), TRUE, FALSE),
         Severe_Sig = ifelse(abs(Severe_Diff) > abs(Severe_MoE_Midline), TRUE, FALSE),
         
         Moderate_CI_Baseline = paste0("(", round(Moderate_CI_Low_Baseline, 1) , ", ", round(Moderate_CI_High_Baseline, 1), ")"),
         Moderate_CI_Midline = paste0("(",  round(Moderate_CI_Low_Midline, 1), ", ",   round(Moderate_CI_High_Midline, 1), ")"),
         
         Severe_CI_Baseline = paste0("(", round(Severe_CI_Low_Baseline, 1) , ", ", round(Severe_CI_High_Baseline, 1), ")"),
         Severe_CI_Midline = paste0("(",  round(Severe_CI_Low_Midline, 1), ", ",   round(Severe_CI_High_Midline, 1), ")"),
         Disaggregate = "All Households") %>%
  dplyr::select(-N_Baseline, -N_Midline, 
                -contains(c("CI_Low_", "CI_High")))


# NOTES - Revise education level to include all levels, finalize binding of aggregate estimate for 5.1.2

table_5_1_1 <- bind_rows(AGG_df_511_join, table_5_1_1[1:19,])
table_5_1_2 <- bind_rows(AGG_df_512_join, table_5_1_2[1:19,])

table_5_1_1 <- 
  table_5_1_1 %>%
  select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, FI_Midline, CI_Midline, N_Midline, Diff, Sig)


table_5_1_2 <- 
  table_5_1_2 %>%
  select(Disaggregate, 
         Moderate_FI_Baseline, Moderate_CI_Baseline, 
         Severe_FI_Baseline,   Severe_CI_Baseline,   Severe_N_Baseline, 
         
         Moderate_FI_Midline, Moderate_CI_Midline, Moderate_Sig, 
         Severe_FI_Midline,   Severe_CI_Midline,   Severe_Sig,
         Severe_N_Midline)

table_5_1_1$FI_Baseline <- round(as.numeric(table_5_1_1$FI_Baseline), 1)
table_5_1_1$FI_Baseline <- format(as.numeric(table_5_1_1$FI_Baseline), nsmall = 1, trim = T)
table_5_1_1$FI_Midline <- round(as.numeric(table_5_1_1$FI_Midline), 1)
table_5_1_1$FI_Midline <- format(as.numeric(table_5_1_1$FI_Midline), nsmall = 1, trim = T)



table_5_1_2$Moderate_FI_Baseline <- round(as.numeric(table_5_1_2$Moderate_FI_Baseline), 1)
table_5_1_2$Moderate_FI_Baseline <- format(as.numeric(table_5_1_2$Moderate_FI_Baseline), nsmall = 1, trim = T)
table_5_1_2$Moderate_FI_Midline <- round(as.numeric(table_5_1_2$Moderate_FI_Midline), 1)
table_5_1_2$Moderate_FI_Midline <- format(as.numeric(table_5_1_2$Moderate_FI_Midline), nsmall = 1, trim = T)

table_5_1_2$Severe_FI_Baseline <- round(as.numeric(table_5_1_2$Severe_FI_Baseline), 1)
table_5_1_2$Severe_FI_Baseline <- format(as.numeric(table_5_1_2$Severe_FI_Baseline), nsmall = 1, trim = T)
table_5_1_2$Severe_FI_Midline <- round(as.numeric(table_5_1_2$Severe_FI_Midline), 1)
table_5_1_2$Severe_FI_Midline <- format(as.numeric(table_5_1_2$Severe_FI_Midline), nsmall = 1, trim = T)



setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Ghana/R/Ghana_FIES")
write_csv(table_5_1_1, "ghana-table_5_1_1_v1-1182024.csv")
write_csv(table_5_1_2, "ghana-table_5_1_2_v1-1182024.csv")

setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Ghana/STATA/Analytic")
write_dta(gh_hh_baseline, "FTF P2-ZOI Survey Ghana 2020 Baseline - FIES.dta")
write_dta(gh_hh_midline, "FTF P2-ZOI Survey Ghana 2023 Midline - FIES.dta")

moe(gh_hh_midline$prob_mod_sev,
    gh_hh_midline$RS,
    gh_hh_midline$wt * 10^6, 
    conf.level = .95,
    psu = gh_hh_midline$hhea, 
    strata = gh_hh_midline$strata)

moe(gh_hh_midline$prob_mod,
    gh_hh_midline$RS,
    gh_hh_midline$wt * 10^6, 
    conf.level = .95,
    psu = gh_hh_midline$hhea, 
    strata = gh_hh_midline$strata)

moe(gh_hh_midline$prob_sev,
    rs = gh_hh_midline$RS,
    wt = gh_hh_midline$wt * 10^6, 
    conf.level = .95,
    psu = gh_hh_midline$hhea, 
    strata = gh_hh_midline$strata)


moe(gh_hh_midline$prob_mod_sev[gh_hh_midline$genhhtype_dj == "De jure male and female adults"],
    gh_hh_midline$RS[gh_hh_midline$genhhtype_dj == "De jure male and female adults"],
    gh_hh_midline$wt[gh_hh_midline$genhhtype_dj == "De jure male and female adults"] * 10^6, 
    conf.level = .95,
    psu = gh_hh_midline$hhea[gh_hh_midline$genhhtype_dj == "De jure male and female adults"], 
    strata = gh_hh_midline$strata[gh_hh_midline$genhhtype_dj == "De jure male and female adults"])


levels(gh_hh_midline$genhhtype_dj)


moe(gh_hh_midline$prob_mod_sev[gh_hh_midline$genhhtype_dj == "De jure female, no male"],
    gh_hh_midline$RS[gh_hh_midline$genhhtype_dj == "De jure female, no male"],
    gh_hh_midline$wt[gh_hh_midline$genhhtype_dj == "De jure female, no male"] * 10^6, 
    conf.level = .95,
    psu = gh_hh_midline$hhea[gh_hh_midline$genhhtype_dj == "De jure female, no male"], 
    strata = gh_hh_midline$strata[gh_hh_midline$genhhtype_dj == "De jure female, no male"])


moe(gh_hh_midline$prob_mod_sev[gh_hh_midline$genhhtype_dj == "De jure male, no female"],
    gh_hh_midline$RS[gh_hh_midline$genhhtype_dj == "De jure male, no female"],
    gh_hh_midline$wt[gh_hh_midline$genhhtype_dj == "De jure male, no female"] * 10^6, 
    conf.level = .95,
    psu = gh_hh_midline$hhea[gh_hh_midline$genhhtype_dj == "De jure male, no female"], 
    strata = gh_hh_midline$strata[gh_hh_midline$genhhtype_dj == "De jure male, no female"])

moe(gh_hh_midline$prob_mod_sev[gh_hh_midline$genhhtype_dj == "De jure male, no female"],
    gh_hh_midline$RS[gh_hh_midline$genhhtype_dj == "De jure male, no female"],
    gh_hh_midline$wt[gh_hh_midline$genhhtype_dj == "De jure male, no female"] * 10^6, 
    conf.level = .95,
    psu = gh_hh_midline$psu[gh_hh_midline$genhhtype_dj == "De jure male, no female"], 
    strata = gh_hh_midline$strata[gh_hh_midline$genhhtype_dj == "De jure male, no female"])$se_s
