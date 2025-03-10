################## KENYA #######################################################
library(haven)
library(tidyverse)
library(readxl)
library(RM.weights)
library(eRm)
library(ggplot2)
library(survey)
library(Hmisc)
library(ggthemes)
library(vtable)
# READ BASELINE


####### 1. IMPORT DATA ###################
# C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\Backup\Baseline\Processed Data
setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Kenya/Backup/Baseline/Processed Data")
# OR
# setwd("C:/Users/N116937/OneDrive - ICF/Documents/Data Analysis/Projects/FIES")
ke_hh_baseline_import <- read_dta("Kenya2019_hhold_analytic.dta")

# Remove FTFZONE == 1 from BL. These regions do not compose BL
ke_hh_baseline_import <- 
  ke_hh_baseline_import %>%
  dplyr::filter(FTFZONE != 1) 


# READ MIDLINE
setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Kenya/KenyaDA/2. Midline/03. Processed_Data/Analytic Data")
ke_hh_midline_import <- read_dta("FTF_P2-ZOI_2023_Kenya-Survey_household_data_analytic.dta")


######## 2. Data validation for DP ################# 


# MIDLINE
# describe FIES vars
ke_hh_midline_import %>%
  dplyr::select(v301:v308) %>%
  psych::describe()
# View first 10 rows
ke_hh_midline_import %>%
  dplyr::distinct(hhea, hhnum, .keep_all = T) %>%
  dplyr::select(hhea, hhnum, v301:v308)


# v300e - count consent to FIES section

ke_hh_midline_import %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::count(v300e)

# view incomplete cases
ke_hh_midline_import %>%
  dplyr::select(v300e, v301:v308) %>%
  dplyr::filter(!complete.cases(.))
# 1 case with all items missing, respondent not available after callbacks. remove

fies_prop_ml <- 
  ke_hh_midline_import %>%
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

ke_hh_midline_import %>%
  dplyr::filter(v306 == 7) %>%
  dplyr::select(hhea, hhnum, stratum, v301:v308)
# only one var with REFUSED - 306. responded NO to all others. Recode to NO.

# BASELINE
# see first 10 rows
ke_hh_baseline_import %>%
  dplyr::distinct(hhea, hhnum, .keep_all = T) %>%
  dplyr::select(hhea, hhnum, v301:v308)

# desc stats
ke_hh_baseline_import %>%
  dplyr::select(v301:v308) %>%
  psych::describe()

# incomplete cases
ke_hh_baseline_import %>%
  count(ahconsent)

table(ke_hh_baseline_import$v366)

# No v300d in updated dataset.Using v366
#   V300D         n
#  1  1 [yes]   3584
#  2  2 [no]       7
#  3 NA           99

# > table(ke_hh_baseline_import$v366)
# 
# 1    2    3    4    6    8   10 
# 3574    7    1   71    1    1    4 


# did the respondents who didn't consent have responses on FIES items?
ke_hh_baseline_import %>%
  filter(v366 == 4) %>%
  dplyr::select(v366, v301:v308) %>%
  dplyr::filter(!complete.cases(.)) %>%
  as.data.frame()

ke_hh_baseline_import %>%
  dplyr::group_by(ahconsent) %>%
  dplyr::count(v366)

  
ke_hh_baseline_import %>%
  dplyr::select(v366, v301:v308) %>%
  dplyr::filter(!complete.cases(.)) %>%
  nrow()

ke_hh_baseline_import %>%
  dplyr::select(v366, v301:v308) %>%
  dplyr::filter(!complete.cases(.)) %>%
  head(50L) %>% 
  as.data.frame()

# 150 rows with incomplete cases


# 7 HHs with v300d codes equal to 2. All FIES items coded to NA. these declined participating and can be removed.
# HHs with V300D equal to 1.

# distribution of v300d/v366
ke_hh_baseline_import %>%
  dplyr::filter(v366 == 1) %>%
  dplyr::select(v366, v301:v308) %>%
  dplyr::filter(!complete.cases(.)) %>%
  as.data.frame()
# 43 HHs who consented and have at least 1 FIES item coded as NA/missing.
ke_hh_baseline_import %>%
  dplyr::filter(v366 == 1) %>%
  dplyr::select(v301:v308) %>%
  dplyr::filter(!complete.cases(.)) %>%
  dplyr::mutate(numberNAs = rowSums(is.na(.))) %>%
  dplyr::count(numberNAs)

# Count of HHs by total number of items missing
# numberNAs  n
#      1    30
#      2     3
#      3     3
#      4     1
#      6     1
#      7     4
#      8     2

# v366 filter
# A tibble: 6 × 2
# numberNAs     n
# <dbl> <int>
# 1         1    28
# 2         2     3
# 3         3     1
# 4         4     1
# 5         7     1
# 6         8     9
# 
#




# proportions - unweighted
fies_prop_bl <- 
  ke_hh_baseline_import %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() %>%
  prop.table(., 1) * 100

round(fies_prop_bl, 2)
round(fies_prop_ml, 2)

# view refused
ke_hh_baseline_import %>%
  #   dplyr::filter(is.na(v300e)) %>%
  dplyr::filter(v366 == 1) %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() 

# total N on each item  
ke_hh_baseline_import %>%
  dplyr::filter(v366 == 1) %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
#  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name) %>%
  count(value) %>% 
  dplyr::filter(is.na(value))

# A tibble: 8 × 3
# Groups:   name [8]
# name  value         n
# 1 V301  NA(r)         8
# 2 V302     NA        13
# 3 V303  NA(r)        16
# 4 V304  NA(d)         8
# 5 V305  NA(d)        15
# 6 V306  NA(d)        14
# 7 V307     NA         9
# 8 V308     NA        16

# using v366
# Groups:   name [8]
# name  value         n
# <chr> <dbl+lbl> <int>
#   1 v301     NA        14
# 2 v302     NA        15
# 3 v303  NA(r)        18
# 4 v304  NA(d)        11
# 5 v305  NA(d)        18
# 6 v306  NA(d)        15
# 7 v307     NA        11
# 8 v308     NA        18

#### 3. Data prep ####
# function for calculating sampling errors/MOE
source("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Kenya/KenyaDA/2. Midline/X5. Jamie/R/Kenya_FIES/moe_complex survey design.R")
#create a new function to compute population standard deviation
var.p <- function(x) var(x) * (length(x)-1) / length(x)
sd.p <- function(x) sqrt(var.p(x))

# ke_hh_midline_import %>%
#   dplyr::left_join()
### 3a. Import additional data ####
ke_hh_baseline <- ke_hh_baseline_import
setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Kenya/KenyaDA/1. Baseline/Processed Data/Analytic Data")
ke_hh_bl_shock_sev <- read_dta("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Kenya/KenyaDA/2. Midline/X5. Jamie/Analytic/FTF_P2-ZOI_Survey_Kenya_2019_household_data_analytic_resilience.dta")


ke_hh_midline  <- ke_hh_midline_import


ke_hh_ml_shock_sev <- read_dta("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Kenya/KenyaDA/2. Midline/X5. Jamie/Analytic/FTF_P2-ZOI_Survey_Kenya_2023_household_data_analytic_resilience.dta")

ke_hh_ml_wealth <- read_dta("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Kenya/KenyaDA/2. Midline/03. Processed_Data/Analytic Data/FTF_P2-ZOI_2023_Kenya_Survey_wealthindex_AWI.dta")

#### 3b. BASELINE RECODING ####

# RENAME VARS IN BASELINE - to lowercase
# ke_hh_baseline <- 
#   ke_hh_baseline %>%
#   rename_at(dplyr::vars(HHNUM, hhea, REGION, V300D, v301:v308), dplyr::funs(str_to_lower(.)))

# rename FIES vars
ke_hh_baseline <- 
  ke_hh_baseline %>%
  dplyr::rename(
    "WORRIED" = "v301", 
    "HEALTHY" = "v302", 
    "FEWFOOD" = "v303", 
    "SKIPPED" = "v304", 
    "ATELESS" = "v305", 
    "RUNOUT"  = "v306", 
    "HUNGRY"  = "v307", 
    "WHLDAY"  = "v308")

# recode FIES vars - NO response from 2 to 0
ke_hh_baseline <- 
  ke_hh_baseline %>% mutate_at(dplyr::vars(WORRIED:WHLDAY),
                               dplyr::funs(case_when(
                                 . == 1 ~ 1,
                                 . == 2 ~ 0)
                               ))


table(is.na(ke_hh_baseline$genhhtype_dj))

table(ke_hh_baseline$genhhtype_dj)

ke_hh_baseline$genhhtype_dj

ke_hh_baseline$genhhtype_dj <- factor(ke_hh_baseline$genhhtype_dj, levels = c(1,2,3,4), labels = c("Male and Female adults","Female adults only", 
                                                                                                   "Male adults only", "Children only")) 


table(ke_hh_baseline$genhhtype_dj)

# cluster and HH id
ke_hh_baseline$hhea <- ke_hh_baseline$hhea
# hhnum
ke_hh_baseline$hhnum <- ke_hh_baseline$hhnum


# REGION

ke_hh_baseline$region <- factor(ke_hh_baseline$region, 
                                levels = c(1, 3, 2), 
                                labels = c("1. High Rainfall", "2. Northern Arid Land", "3. Semi Arid Land"))
ke_hh_baseline$region  

# psu
ke_hh_baseline$psu <- ke_hh_baseline$region

# High Rainfall 1 (HR1) Semi-Arid 2 (SA2) Northern Kenya (NAL)  
  
# create combined weight and HH size
ke_hh_baseline$wt <- ke_hh_baseline$wgt_hh
ke_hh_baseline$wt_mem <- ke_hh_baseline$wt * ke_hh_baseline$hhsize_dj

ke_hh_baseline$ahtype <- factor(ke_hh_baseline$ahtype, levels=c(1,2), labels=c("Urban","Rural"))

# strata
ke_hh_baseline$strata <- ke_hh_baseline$stratum
ke_hh_baseline$strata <- haven::as_factor(ke_hh_baseline$strata)


# wealth quintile - already in proper order
table(ke_hh_baseline$awiquint)
table(ke_hh_baseline$awiquint_rev)


ke_hh_baseline$awiquint_rev <- factor(ke_hh_baseline$awiquint, levels = c(1,2,3,4,5), labels = c("Poorest","Second", "Middle", "Fourth", "Wealthiest"))

# shock severity

ke_hh_baseline <- 
  ke_hh_baseline %>%
  left_join(ke_hh_bl_shock_sev %>% dplyr::select(hhea, hhnum, shock_sev))
            
ke_hh_baseline$shock_sev <- factor(ke_hh_baseline$shock_sev, levels = c(1,2,3,4), labels = c("None", "Low", "Medium", "High"))
table(ke_hh_baseline$shock_sev)

# edu level


# ke_hh_baseline$edulevel_hh_dj <- haven::as_factor(ke_hh_baseline$)
#
#
#
#
#
#

# No HHID?
ke_hh_baseline$hhid <- as.numeric(row.names(ke_hh_baseline))

head(ke_hh_baseline$hhid)

# survey round- 
ke_hh_baseline$survey <- 1
# select vars to keep, drop vars not needed for analysis

ke_hh_baseline <- 
  ke_hh_baseline %>%
  dplyr::select(survey, hhid, hhea, hhnum, hhsize_dj, v366, ahconsent, psu, region, wt, hhsize_dj, wt_mem, strata, genhhtype_dj, ahtype, 
            #    edulevel_hh_dj, 
                awiquint_rev, shock_sev,
                WORRIED:WHLDAY)



### 3c. Baseline - MISSING DATA #########
# examine missing vals
# distribution of v300d

# drop rows without consent - v300d == 1 
ke_hh_baseline %>%
  dplyr::count(v366)

#   V300D         n
#  1  1 [yes]   3584
#  2  2 [no]       7
#  3 NA           99

# A tibble: 8 × 2
# v366                                                                 n
# <dbl+lbl>                                                          <int>
# 1     1 [completed]                                                 3574
# 2     2 [NO HOUSEHOLD MEMBER AT HOME]                                  7
# 3     3 [ENTIRE HOUSEHOLD ABSENT FO EXTENDED PERIOD OF TIME]           1
# 4     4 [POSTPONED/UNAVAILABLE]                                       71
# 5     6 [DWELLING VACANT]                                              1
# 6     8 [DWELLING DESTROYED]                                           1
# 7    10 [HOUSEHOLD MEMBER TOO ILL TO RESPOND/COGNITIVELY IMPAIRED]     4
# 8 NA(r)                                                               25

ke_hh_baseline %>%
  dplyr::filter(is.na(v366)) %>%
  dplyr::select(v366, WORRIED:WHLDAY) %>%
  as.data.frame()

# all FIES items are NA where v300d is NA. drop

ke_hh_baseline <- 
  ke_hh_baseline %>%
  dplyr::filter(v366 == 1)



# create var counting number of NAs
ke_hh_baseline <- 
  ke_hh_baseline %>%
  rowwise() %>%
  mutate(numberNA=sum(is.na(c_across(WORRIED:WHLDAY))))

ke_hh_baseline %>%
  dplyr::count(numberNA)

# Count of HHs by total number of items missing
# numberNA  n
#      1    30
#      2     3
#      3     3
#      4     1
#      6     1
#      7     4
#      8     2

# REVISED:
# numberNA     n
#<int> <int>
# 1        0  3531
# 2        1    28
# 3        2     3
# 4        3     1
# 5        4     1
# 6        7     1
# 7        8     9

ke_hh_baseline %>%
  dplyr::filter(v366 == 1) %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  ungroup() %>%
  dplyr::filter(!complete.cases(.)) %>%
  summarise(across(everything(), ~ sum(is.na(.x))))

# A tibble: 8 × 3
# Groups:   name [8]
# name  value         n
# 1 V301  NA(r)         8
# 2 V302     NA        13
# 3 V303  NA(r)        16
# 4 V304  NA(d)         8
# 5 V305  NA(d)        15
# 6 V306  NA(d)        14
# 7 V307     NA         9
# 8 V308     NA        16

# Updated
# A tibble: 1 × 8
# WORRIED HEALTHY FEWFOOD SKIPPED ATELESS RUNOUT HUNGRY WHLDAY
# <int>   <int>   <int>   <int>   <int>  <int>  <int>  <int>
#  14      15      18      11      18     15     11     18

# produce complete list of HHs with at least 1 missing item 
ke_hh_baseline %>% 
  dplyr::filter(hhsize_dj  != 0) %>%
  dplyr::select(WORRIED:WHLDAY, numberNA) %>%
  ungroup() %>%
  dplyr::filter(!complete.cases(.)) %>%
  as.data.frame()

# drop where numberNA is equal to 6-8
ke_hh_baseline <- 
  ke_hh_baseline %>% 
  dplyr::filter(numberNA < 6)

ke_hh_fiesmissingitems <- 
  ke_hh_baseline %>% 
  dplyr::select(hhea, hhnum, WORRIED:WHLDAY, numberNA) %>%
  ungroup() %>%
  dplyr::filter(!complete.cases(.)) %>%
  dplyr::arrange(desc(numberNA)) %>%
  as.data.frame()

write.csv(ke_hh_fiesmissingitems, "ke_hh_fiesmissingitems.csv")



ke_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  filter_at(dplyr::vars(WORRIED:WHLDAY), dplyr::any_vars(is.na(.))) %>%
  as.data.frame()

# individual items 
ke_hh_baseline <-
  ke_hh_baseline %>%
  dplyr::mutate(FEWFOOD = case_when(hhea == 401010301010 & hhnum == 18 ~ 0,
                                   TRUE ~ as.numeric(FEWFOOD)),
                SKIPPED = case_when(hhea == 401010301010 & hhnum == 18 ~ 0,
                                    TRUE ~ as.numeric(SKIPPED)),
                ATELESS = case_when(hhea == 401010301010 & hhnum == 18 ~ 0,
                                    TRUE ~ as.numeric(ATELESS)),
                RUNOUT = case_when(hhea == 401010301010 & hhnum == 18 ~ 0,
                                    TRUE ~ as.numeric(RUNOUT))) %>%
  
  dplyr::mutate(WORRIED = case_when(hhea == 701040101007 & hhnum == 26 ~ 1,
                                    TRUE ~ as.numeric(WORRIED)),
                FEWFOOD = case_when(hhea == 701040101007 & hhnum == 26 ~ 1,
                                    TRUE ~ as.numeric(FEWFOOD)),
                RUNOUT = case_when(hhea == 701040101007 & hhnum == 26 ~ 1,
                                    TRUE ~ as.numeric(RUNOUT))) %>%
  
  dplyr::mutate(FEWFOOD = case_when(hhea == 701010101038 & hhnum == 103 ~ 1,
                                    TRUE ~ as.numeric(FEWFOOD)),
                ATELESS = case_when(hhea == 701010101038 & hhnum == 103 ~ 1,
                                    TRUE ~ as.numeric(ATELESS))) %>%
  
  dplyr::mutate(WORRIED = case_when(hhea == 702060101002 & hhnum == 73 ~ 1,
                                    TRUE ~ as.numeric(WORRIED)),
                FEWFOOD = case_when(hhea == 702060101002 & hhnum == 73 ~ 1,
                                    TRUE ~ as.numeric(FEWFOOD))) %>%
  
  dplyr::mutate(ATELESS = case_when(hhea == 404010101006 & hhnum == 1 ~ 0,
                                    TRUE ~ as.numeric(ATELESS))) %>%
  
  dplyr::mutate(ATELESS = case_when(hhea == 405010102008 & hhnum == 9 ~ 0,
                                    TRUE ~ as.numeric(ATELESS))) %>%
  
  dplyr::mutate(FEWFOOD = case_when(hhea == 405010102008 & hhnum == 61 ~ 0,
                                    TRUE ~ as.numeric(FEWFOOD))) %>%
  
  dplyr::mutate(ATELESS = case_when(hhea == 405020102038 & hhnum == 127 ~ 1,
                                    TRUE ~ as.numeric(ATELESS))) %>%
  
  dplyr::mutate(WHLDAY = case_when(hhea == 502030101002 & hhnum == 11 ~ 0,
                                    TRUE ~ as.numeric(WHLDAY))) %>%
  dplyr::mutate(RUNOUT = case_when(hhea == 503010201001 & hhnum == 49 ~ 0,
                                   TRUE ~ as.numeric(RUNOUT))) %>%
  dplyr::mutate(WHLDAY = case_when(hhea == 507030201001 & hhnum == 12 ~ 0,
                                   TRUE ~ as.numeric(WHLDAY))) %>%
  dplyr::mutate(WHLDAY = case_when(hhea == 508020302010 & hhnum == 38 ~ 0,
                                   TRUE ~ as.numeric(WHLDAY))) %>%
  dplyr::mutate(WHLDAY = case_when(hhea == 601040503006 & hhnum == 118 ~ 0,
                                   TRUE ~ as.numeric(WHLDAY))) %>%
  
  dplyr::mutate(WORRIED = case_when(hhea == 604010302019 & hhnum == 90 ~ 0,
                                    TRUE ~ as.numeric(WORRIED))) %>%
  dplyr::mutate(FEWFOOD = case_when(hhea == 611030302001 & hhnum == 92 ~ 0,
                                    TRUE ~ as.numeric(FEWFOOD))) %>%
  dplyr::mutate(ATELESS = case_when(hhea == 614010201006 & hhnum == 72 ~ 0,
                                    TRUE ~ as.numeric(ATELESS))) %>%
  
  dplyr::mutate(WORRIED = case_when(hhea == 701010101038 & hhnum == 94 ~ 1,
                                    TRUE ~ as.numeric(WORRIED))) %>%
  
  dplyr::mutate(HEALTHY = case_when(hhea == 702060202006 & hhnum == 9 ~ 1,
                                    TRUE ~ as.numeric(HEALTHY))) %>%
  
  dplyr::mutate(RUNOUT = case_when(hhea == 801010102012 & hhnum == 135 ~ 0,
                                    TRUE ~ as.numeric(RUNOUT))) %>%
  dplyr::mutate(FEWFOOD = case_when(hhea == 801030102080 & hhnum == 105 ~ 0,
                                    TRUE ~ as.numeric(FEWFOOD))) %>%
  dplyr::mutate(FEWFOOD = case_when(hhea == 806020101007 & hhnum == 81 ~ 1,
                                    TRUE ~ as.numeric(FEWFOOD))) %>%
  dplyr::mutate(WORRIED = case_when(hhea == 807010204006 & hhnum == 38 ~ 0,
                                    TRUE ~ as.numeric(WORRIED))) %>%
  dplyr::mutate(HEALTHY = case_when(hhea == 812020302020 & hhnum == 17 ~ 0,
                                    TRUE ~ as.numeric(HEALTHY))) %>%
  dplyr::mutate(HEALTHY = case_when(hhea == 813020202002 & hhnum == 34 ~ 1,
                                    TRUE ~ as.numeric(HEALTHY))) 
  
  



# HHs to remove due to lack of clear pattern
ke_hh_baseline <- 
  ke_hh_baseline %>%
  dplyr::filter(hhea != 507030201001 | hhnum != 45) %>%
  dplyr::filter(hhea != 701040101007 | hhnum != 136) %>%
  dplyr::filter(hhea != 811020702004 | hhnum != 126) %>%
  dplyr::filter(hhea != 404040201003 | hhnum != 40) %>%
  dplyr::filter(hhea != 406010301003 | hhnum != 16) %>%
  
  dplyr::filter(hhea != 418050101005 | hhnum != 22) %>%
  dplyr::filter(hhea != 502040601011 | hhnum != 27) %>%
  dplyr::filter(hhea != 601040503006 | hhnum != 32) %>%
  dplyr::filter(hhea != 602030102006 | hhnum != 115) %>%
  
  dplyr::filter(hhea != 702050101004 | hhnum != 2) %>%
  dplyr::filter(hhea != 702060202006 | hhnum != 20) %>%
  dplyr::filter(hhea != 811010502006 | hhnum != 134) %>%
  dplyr::filter(hhea != 816030103006 | hhnum != 193)
  
# Confirm there are no more missing items
ke_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  filter_at(dplyr::vars(WORRIED:WHLDAY), dplyr::any_vars(is.na(.))) %>%
  as.data.frame()

table(ke_hh_baseline$hhsize_dj)

ke_hh_baseline <- ke_hh_baseline %>%
  dplyr::filter(hhsize_dj > 0)

### 3d. MIDLINE RECODING ####

# MIDLINE
# recode FIES vars - NO response from 2 to 0
ke_hh_midline <- 
  ke_hh_midline %>% mutate_at(dplyr::vars(v301:v308),
                              dplyr::funs(case_when(
                                . == 1 ~ 1,
                                . == 2 ~ 0)
                              ))


# rename vars
ke_hh_midline <- 
  ke_hh_midline %>%
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
ke_hh_midline$wt <- ke_hh_midline$wgt_hh


# cluster and HH id
ke_hh_midline$hhea <- ke_hh_midline$hhea
# hhnum
ke_hh_midline$hhnum <- ke_hh_midline$hhnum

# psu
ke_hh_midline$psu <- haven::as_factor(ke_hh_midline$subzone)
# region
ke_hh_midline$region <- haven::as_factor(ke_hh_midline$subzone)

ke_hh_midline <- 
  ke_hh_midline %>%
  dplyr::mutate(region = case_when(region == "1. High Rainfall" ~ "High Rainfall 1 (HR1)",
                                   region == "2. Northern Arid Land" ~ "Northern Kenya (NAL)",
                                   region == "3. Semi Arid Land" ~ "Semi-Arid 2 (SA2)",
                                   region == "4. Northen Arid Land-Samburu" ~ "Northen Arid Land-Samburu"),
                region = haven::as_factor(region))
levels(ke_hh_baseline$region)
levels(ke_hh_midline$region)

# create combined weight and HH size
ke_hh_midline$wt_mem <- ke_hh_midline$wt * ke_hh_midline$hhsize_dj


# strata
ke_hh_midline$strata <- haven::as_factor(ke_hh_midline$stratum)
# survey round- midline == 2
ke_hh_midline$survey <- 2

# add background variables as factors, with levels and labels
# HH type
ke_hh_midline$genhhtype_dj <- ke_hh_midline$genhhtype_dj
# NOT IN DATASET
ke_hh_midline$genhhtype_dj <- haven::as_factor(ke_hh_midline$genhhtype_dj)

# urban - rural
ke_hh_midline$ahtype <- factor(ke_hh_midline$ahtype, levels=c(1,2), labels=c("Rural","Urban"))
# highest HH education level
# NOT IN DATASET
# ke_hh_midline$edulevel_hh_dj <- factor(ke_hh_midline$edulevel_hh2_dj))
# wealth quintile
# NOT IN DATASET. JOIN OTHRE DATASET TO ADD?

names(ke_hh_ml_wealth)
ke_hh_ml_wealth <- ke_hh_ml_wealth %>%
  dplyr::select(hhea, hhnum, awi, awiquint)

ke_hh_midline <- 
  ke_hh_midline %>%
#  dplyr::select(-awi, -awiquint) %>%
  left_join(ke_hh_ml_wealth)

ke_hh_midline$awiquint_rev <- factor(ke_hh_midline$awiquint, levels = c(1,2,3,4,5), labels = c("Poorest","Second", "Middle", "Fourth", "Wealthiest"))

ke_hh_midline$awiquint
ke_hh_midline$awiquint_rev
table(ke_hh_midline$awiquint_rev)
table(ke_hh_midline$awiquint)


# shock severity

ke_hh_midline <- 
  ke_hh_midline %>%
#  dplyr::select(-shock_sev) %>%
  left_join(ke_hh_ml_shock_sev %>% dplyr::select(hhea, hhnum, shock_sev))

head(ke_hh_midline$shock_sev)
# 
ke_hh_midline$shock_sev <- factor(ke_hh_midline$shock_sev, levels = c(1,2,3,4), labels = c("None", "Low", "Medium", "High"))
table(ke_hh_midline$shock_sev)
# No HHID?
ke_hh_midline <- 
  ke_hh_midline %>%
  mutate(hhid = row_number())

# select vars - NOT READY
ke_hh_midline <- 
  ke_hh_midline %>%
  dplyr::select(survey, v300e, hhid, hhea, hhnum, wt, psu, region, strata, ahtype,
                hhsize_dj, wt_mem,  
                genhhtype_dj, 
                awiquint_rev, shock_sev, 
                WORRIED:WHLDAY)


####### 3e. MIDLINE Missing data ###############
ke_hh_midline %>%
  dplyr::count(v300e)

# 1 respondent unavailable after callbacks made, remove 

ke_hh_midline <- ke_hh_midline %>%
  dplyr::filter(v300e == 1)



# create var counting number of NAs
ke_hh_midline <- ke_hh_midline %>%
  rowwise() %>%
  mutate(numberNA=sum(is.na(c_across(WORRIED:WHLDAY))))

ke_hh_midline %>%
  dplyr::count(numberNA)

# A tibble: 4 × 2
# Rowwise: 
# numberNA     n
# <int> <int>
# 1        0  4100
# 2        1     6
# 3        2     1
# 4        8     1


ke_hh_midline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  ungroup() %>%
  summarise(across(everything(), ~ sum(is.na(.x))))

# A tibble: 1 × 8
# WORRIED HEALTHY FEWFOOD SKIPPED ATELESS RUNOUT HUNGRY WHLDAY
# <int>   <int>   <int>   <int>   <int>  <int>  <int>  <int>
#  1       2       1       1       1      4      3      3

# produce complete list of HHs with at least 1 missing item 

ke_hh_midline %>% 
  dplyr::select(hhea, hhnum, WORRIED:WHLDAY, numberNA) %>%
  ungroup() %>%
  dplyr::filter(!complete.cases(.)) %>%
  dplyr::arrange(desc(numberNA)) %>%
  as.data.frame()

fies_prop_ml


ke_hh_midline <-
  ke_hh_midline %>%
  dplyr::mutate(RUNOUT = case_when(hhea == 1108 & hhnum == 25 ~ 0,
                                    TRUE ~ as.numeric(RUNOUT)),
                HUNGRY = case_when(hhea == 1108 & hhnum == 25 ~ 0,
                                   TRUE ~ as.numeric(HUNGRY))) %>%
  dplyr::mutate(RUNOUT = case_when(hhea == 1051 & hhnum == 19 ~ 1,
                                   TRUE ~ as.numeric(RUNOUT))) %>%
  dplyr::mutate(WHLDAY = case_when(hhea == 1053 & hhnum == 18 ~ 0,
                                   TRUE ~ as.numeric(WHLDAY))) %>%
  dplyr::mutate(HUNGRY = case_when(hhea == 1125 & hhnum == 21 ~ 0,
                                   TRUE ~ as.numeric(HUNGRY)))
                  
ke_hh_midline %>% 
  dplyr::select(hhea, hhnum, WORRIED:WHLDAY, numberNA) %>%
  ungroup() %>%
  dplyr::filter(!complete.cases(.)) %>%
  dplyr::arrange(desc(numberNA)) %>%
  as.data.frame()


ke_hh_midline <- 
  ke_hh_midline %>%
  dplyr::filter(hhea != 1114 | hhnum != 4) %>%
  dplyr::filter(hhea != 1044 | hhnum != 6) %>%
  dplyr::filter(hhea != 1106 | hhnum != 25) %>%
  dplyr::filter(hhea != 1107 | hhnum != 26) 


ke_hh_midline %>%
  dplyr::select(WORRIED:WHLDAY) %>%
  filter_at(dplyr::vars(WORRIED:WHLDAY), dplyr::any_vars(is.na(.))) %>%
  as.data.frame()

#### 3f. CREATE COMBINED from BL and ML ####

ke_hh_combined <- bind_rows(ke_hh_baseline, ke_hh_midline)





##################### 4. Rasch Modeling ####################
# generate two matrices with FIES questions only: total, baseline and midline

# BL
FIES_baseline <-
  ke_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY)
# ML
FIES_midline <-
  ke_hh_midline %>%
  dplyr::select(WORRIED:WHLDAY)



# create row/raw score var RS that sums number of yes responses across all items
ke_hh_baseline$RS = rowSums(FIES_baseline)
ke_hh_midline$RS = rowSums(FIES_midline)


# Step 4a - run RASCH model 
# NOTE: rasch models are invariant to weights. We can still use them however they are not necessary.

# baseline 

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

data.frame(res_ml$outfit.person, res_ml$XX) %>%
  dplyr::arrange(res_ml$outfit.person) %>%
  View(.)



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
res_bl$b
res_ml$b
fies_prop_bl
fies_prop_ml


data.frame(blb = res_bl$b/sd.p(res_bl$b),
           mlb = res_ml$b/sd.p(res_ml$b)) %>%
  rownames_to_column() %>%
  ggplot(., aes(x = blb, y = mlb)) +
  geom_point(size = 2, aes(color = rowname)) +
  ggrepel::geom_text_repel(aes(label = rowname)) +
  geom_abline(intercept=0, slope =1) +
  ggtitle("Comparing Scales across Rounds") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  xlab("Item Severity Score - Baseline") +
  ylab("Item Severity Score - Midline")


RM.weights::ICC.fun(res_ml$b, plot = TRUE)

eRm::plotjointICC(eRm::RM(FIES_baseline), 
                  ylab = "Probability to Respond Affirmative",
                  xlab = "Severity Parameters (ICC)",
                  main = "ICC Plot - Baseline",
                  item.subset = "all",
                  legend = T, cex = .5)


eRm::plotjointICC(eRm::RM(FIES_midline), 
                  ylab = "Probability to Respond Affirmative", 
                  xlab = "Severity Parameters (ICC)",
                  main = "ICC Plot - Midline",
                  legend = T, cex = .5)


(res_bl$b/sd.p(res_bl$b)) -  (res_ml$b/sd.p(res_ml$b))

# Step 3f - Equating across rounds (baseline and midline).
# the baseline and midline need to be put on a common scale in order
# to ensure comparison.
# identify items in common - one where scores are similar. 
common = colnames(FIES_baseline[, c(-1, -5)])

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
  y_ke_adj = adj_ml$b/sd.p(adj_ml$b)
  plot(x,y_bl, col = 1, ylim = c(-2.5,2),ylab="",xlab="")
  points(x, y_ml, col = 2)
  points(x,y_ke_adj, col = 3)
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
  "Baseline" = aggregate(ke_hh_baseline$wt, list(ke_hh_baseline$RS), FUN=sum, na.rm=TRUE)$x /sum(ke_hh_baseline$wt[!is.na(ke_hh_baseline$RS)]),
  "Midline" = aggregate(ke_hh_midline$wt, list(ke_hh_midline$RS), FUN=sum, na.rm=TRUE)$x /sum(ke_hh_midline$wt[!is.na(ke_hh_midline$RS)])
))

RS_table %>%
  as.data.frame() %>%
  rownames_to_column("Survey") %>%
  pivot_longer(V1:V9) %>%
  dplyr::mutate(name = str_replace(name, "V", "Raw score ")) %>%
  dplyr::mutate(name = str_replace_all(name, c("1"="0","2"="1","3"="2","4"="3","5"="4","6"="5","7"="6","8"="7","9"="8"))) %>% 
  ggplot(., aes(x = name, y = value, color = Survey, fill = Survey)) +
  geom_col(stat = "count", position = position_dodge()) +
 # ggthemes::theme_fivethirtyeight() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  xlab("") +
  ylab("Proportion") +
  coord_flip() +
  scale_x_discrete(limits=rev)


### Step 5 - COMPUTE PREVALENCE RATES ####

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
# columns removing WORRIED, SKIPPED, ATLESS
glob_st.m <- mean(glob_st[c(-1, -4, -5)])
glob_st.s <- sd.p(glob_st[c(-1, -4, -5)])
m.bl = mean(res_bl$b[c(-1, -4, -5)]) 
s.bl = sd.p(res_bl$b[c(-1, -4, -5)])


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

glo_probs_ml_mod_sev = 1-pnorm(newthres[1], mean = adj_ml$a, sd = adj_ml$se.a)
glo_probs_bl_mod_sev[1] = glo_probs_ml_mod_sev[1] = 0

glo_prev_bl_mod_sev = glo_probs_bl_mod_sev %*% RS_table[1,]

glo_prev_ml_adj_mod_sev = glo_probs_ml_mod_sev %*% RS_table[2,]

# check results
# baseline food insecurity - moderate+severe
glo_prev_bl_mod_sev
# midline food insecurity - moderate+severe
glo_prev_ml_adj_mod_sev


# severe FI
glo_probs_bl_sev = 1-pnorm(newthres[2], mean = res_bl$a, sd = res_bl$se.a)
glo_probs_ml_sev = 1-pnorm(newthres[2], mean = adj_ml$a, sd = adj_ml$se.a)
glo_probs_bl_sev[1] = glo_probs_ml_sev[1] =0
glo_prev_bl_sev = glo_probs_bl_sev %*% RS_table[1,]
glo_prev_ml_adj_sev = glo_probs_ml_sev %*% RS_table[2,]

# check results
# baseline food insecurity - severe
glo_prev_bl_sev
# baseline food insecurity - severe
glo_prev_ml_adj_sev
# glo_prev_ke_not_adj_sev

# create moderate only = mod+sev - sev
glo_prev_bl_mod <- glo_prev_bl_mod_sev - glo_prev_bl_sev
glo_prev_ml_adj_mod <- glo_prev_ml_adj_mod_sev - glo_prev_ml_adj_sev

glo_probs_bl_mod <- glo_probs_bl_mod_sev - glo_probs_bl_sev
glo_probs_ml_mod <- glo_probs_ml_mod_sev - glo_probs_ml_sev

# put into common object
glo_prev_bl <- c(glo_prev_bl_mod_sev, glo_prev_bl_mod, glo_prev_bl_sev)
glo_prev_ml <- c(glo_prev_ml_adj_mod_sev, glo_prev_ml_adj_mod, glo_prev_ml_adj_sev)

glo_prev_bl
glo_prev_ml


### Step 5 - assigning probability ad computing aggregate prevalence ####
# read in MOE script
library(survey)


#Attaching probabilities to each case/HH
ke_hh_baseline$prob_mod_sev = NULL
ke_hh_midline$prob_mod_sev = NULL
ke_hh_baseline$prob_mod = NULL
ke_hh_midline$prob_mod = NULL
ke_hh_baseline$prob_sev = NULL
ke_hh_midline$prob_sev = NULL


for (rs in 0:8) {
  ke_hh_baseline$prob_mod[ke_hh_baseline$RS == rs] = glo_probs_bl_mod[rs+1]
  ke_hh_midline$prob_mod[ke_hh_midline$RS == rs] =   glo_probs_ml_mod[rs+1]
  ke_hh_baseline$prob_mod_sev[ke_hh_baseline$RS == rs] = glo_probs_bl_mod_sev[rs+1]
  ke_hh_midline$prob_mod_sev[ke_hh_midline$RS == rs] =   glo_probs_ml_mod_sev[rs+1]
  ke_hh_baseline$prob_sev[ke_hh_baseline$RS == rs] = glo_probs_bl_sev[rs+1]
  ke_hh_midline$prob_sev[ke_hh_midline$RS == rs] =   glo_probs_ml_sev[rs+1]
}

table(ke_hh_baseline$prob_mod,ke_hh_baseline$RS,useNA = "ifany")

table(ke_hh_baseline$prob_mod_sev,ke_hh_baseline$RS,useNA = "ifany")

table(ke_hh_baseline$prob_sev,ke_hh_baseline$RS,useNA = "ifany")


table(ke_hh_midline$prob_mod,ke_hh_midline$RS,useNA = "ifany")

table(ke_hh_midline$prob_mod_sev,ke_hh_midline$RS,useNA = "ifany")

table(ke_hh_midline$prob_sev,ke_hh_midline$RS,useNA = "ifany")




#### Calculate aggregate prevalence levels and put in df #####

### ZOI ###
ke_hh_midline_zoi <- ke_hh_midline[ke_hh_midline$region != "Northen Arid Land-Samburu",]

table(ke_hh_midline$region)

nrow(ke_hh_midline_zoi)

nrow(ke_hh_midline)

# diff of 117

AGG_df <- data.frame(NA, nrow = 2, ncol = 17)

modsev_moe_bl_95 <- moe(ke_hh_baseline$prob_mod_sev,ke_hh_baseline$RS,ke_hh_baseline$wt * 10^6, conf.level = .95,
                        psu = ke_hh_baseline$psu, strata = ke_hh_baseline$strata)$moe * 100
mod_moe_bl_95 <- moe(ke_hh_baseline$prob_mod,ke_hh_baseline$RS,ke_hh_baseline$wt * 10^6, conf.level = .95,
                     psu = ke_hh_baseline$psu, strata = ke_hh_baseline$strata)$moe * 100
sev_moe_bl_95 <- moe(ke_hh_baseline$prob_sev,ke_hh_baseline$RS,ke_hh_baseline$wt * 10^6, conf.level = .95,
                     psu = ke_hh_baseline$psu, strata = ke_hh_baseline$strata)$moe * 100

moe_bl_95 <- c(modsev_moe_bl_95, mod_moe_bl_95, sev_moe_bl_95)

modsev_moe_ml_95 <- moe(ke_hh_midline_zoi$prob_mod_sev,ke_hh_midline_zoi$RS,ke_hh_midline_zoi$wt * 10^6, conf.level = .95,
                        psu = ke_hh_midline_zoi$psu, strata = ke_hh_midline_zoi$strata)$moe * 100
mod_moe_ml_95 <- moe(ke_hh_midline_zoi$prob_mod,ke_hh_midline_zoi$RS,ke_hh_midline_zoi$wt * 10^6, conf.level = .95,
                     psu = ke_hh_midline_zoi$psu, strata = ke_hh_midline_zoi$strata)$moe * 100
sev_moe_ml_95 <- moe(ke_hh_midline_zoi$prob_sev,ke_hh_midline_zoi$RS,ke_hh_midline_zoi$wt * 10^6, conf.level = .95,
                     psu = ke_hh_midline_zoi$psu, strata = ke_hh_midline_zoi$strata)$moe * 100


moe_ml_95 <- c(modsev_moe_ml_95, mod_moe_ml_95, sev_moe_ml_95)


# Modify ML prevalence point estimates to ZOI only (excl. Samburu)
glo_prev_ml


glo_prev_ml_adj_mod_sev_zoi <- weighted.mean(ke_hh_midline_zoi$prob_mod_sev, ke_hh_midline_zoi$wt)
glo_prev_ml_adj_mod_zoi <- weighted.mean(ke_hh_midline_zoi$prob_mod, ke_hh_midline_zoi$wt)
glo_prev_ml_adj_sev_zoi <- weighted.mean(ke_hh_midline_zoi$prob_sev, ke_hh_midline_zoi$wt)

glo_prev_ml_zoi <- c(glo_prev_ml_adj_mod_sev_zoi, glo_prev_ml_adj_mod_zoi, glo_prev_ml_adj_sev_zoi)

glo_prev_ml_zoi

AGG_df[1, c(1,2, 3)] <- glo_prev_bl * 100
AGG_df[1, c(4)] <- nrow(res_bl$XX)
AGG_df[1, c(5)] <- round(sum(ke_hh_baseline$wt * 10^6), 2)
AGG_df[1, c(6, 7, 8)] <- moe_bl_95
AGG_df[1, c(9, 10, 11)] <- (glo_prev_bl * 100) - moe_bl_95 
AGG_df[1, c(12, 13, 14)] <- (glo_prev_bl * 100) + moe_bl_95
AGG_df[1, c(15, 16, 17)] <- c(NA, NA, NA)



AGG_df[2, c(1,2, 3)] <- glo_prev_ml_zoi * 100
AGG_df[2, c(4)] <- length(ke_hh_midline_zoi$RS)
AGG_df[2, c(5)] <- round(sum(ke_hh_midline_zoi$wt * 10^6), 2)
AGG_df[2, c(6, 7, 8)] <- moe_ml_95
AGG_df[2, c(9, 10, 11)] <- (glo_prev_ml_zoi * 100) - moe_ml_95 
AGG_df[2, c(12, 13, 14)] <- (glo_prev_ml_zoi * 100) + moe_ml_95
AGG_df[2, c(15, 16, 17)] <- ifelse(((glo_prev_ml_zoi * 100) - (glo_prev_bl * 100)) > moe_ml_95, TRUE, FALSE)


colnames(AGG_df) = c("Moderate+Severe_Food_Insecurity", "Moderate_Food_Insecurity", 
                     "Severe_Food_Insecurity", "N","WN", 
                     "MSFI_MoE", "MFI_MoE","SFI_MoE",
                     "MSFI_CI_Low", "MFI_CI_Low", "SFI_CI_Low",
                     "MSFI_CI_High", "MFI_CI_High", "SFI_CI_High",
                     "MSFI_Sig", "MFI_Sig", "SFI_Sig")
rownames(AGG_df) = c("Baseline","Midline")

AGG_df



# Disaggregates - 
# ZOI
# Computing prevalence and MoEs by groups
# join BL and ML into common dataset
ke_hh_combined_zoi <- bind_rows(ke_hh_baseline, ke_hh_midline_zoi)

group1 = ke_hh_combined_zoi$survey
group1 <- factor(group1, levels = c(1,2), labels = c("Baseline","Midline")) 
# GEN HH TYPE
group4 = ke_hh_combined_zoi$genhhtype_dj
# SHOCK SEV
group5 <- ke_hh_combined_zoi$shock_sev
# AWIQUINT
group6 <- ke_hh_combined_zoi$awiquint_rev

group_list <- list(group4, group5, group6)

groups <- c(unique(as.character(group4)), unique(as.character(group5)), 
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
      prob_mods = ke_hh_combined_zoi$prob_mod[fltr]
      wt = ke_hh_combined_zoi$wt[fltr]*10^6
      rs = ke_hh_combined_zoi$RS[fltr]
      psu = ke_hh_combined_zoi$psu[fltr] 
      strata = ke_hh_combined_zoi$strata[fltr]
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
      prob_mods = ke_hh_combined_zoi$prob_sev[fltr]
      wt = ke_hh_combined_zoi$wt[fltr]*10^6
      rs = ke_hh_combined_zoi$RS[fltr]
      psu = ke_hh_combined_zoi$psu[fltr] 
      strata = ke_hh_combined_zoi$strata[fltr]
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
      prob_mods = ke_hh_combined_zoi$prob_mod_sev[fltr]
      wt = ke_hh_combined_zoi$wt[fltr]*10^6
      rs = ke_hh_combined_zoi$RS[fltr]
      psu = ke_hh_combined_zoi$psu[fltr] 
      strata = ke_hh_combined_zoi$strata[fltr]
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


mod_sev_fi_zoi <- 
  mod_sev_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(mod_sev_fi_zoi)


mod_fi_zoi <- 
  mod_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(mod_fi_zoi)



sev_fi_zoi <- 
  sev_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(sev_fi_zoi)


# rename columns for joining together

colnames(sev_fi_zoi)[-1] <- paste0("Severe_", colnames(sev_fi_zoi)[-1])

colnames(sev_fi_zoi)

colnames(mod_fi_zoi)[-1] <- paste0("Moderate_", colnames(mod_fi_zoi)[-1])

colnames(mod_fi_zoi)

table_5_1_2 <- 
  mod_fi_zoi %>% 
  left_join(sev_fi_zoi, by = "Disaggregate")

table_5_1_1 <- mod_sev_fi_zoi

# reorder rows?
table_5_1_1

table_5_1_1 <-
  table_5_1_1 %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "None", "Low", "Medium", "High")))

table_5_1_2 <-
  table_5_1_2 %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "None", "Low", "Medium", "High"))) %>%
  dplyr::select(Disaggregate, ends_with("_Baseline"), ends_with("Midline"), ends_with("_Diff"), ends_with("_Sig"))


### RFZ ####
table(ke_hh_midline$psu)
ke_hh_baseline_rfz <- ke_hh_baseline[ke_hh_baseline$psu == "2. Northern Arid Land"|ke_hh_baseline$psu == "3. Semi Arid Land",]
ke_hh_midline_rfz  <- ke_hh_midline[ke_hh_midline$psu  == "2. Northern Arid Land"|ke_hh_midline$psu  == "3. Semi Arid Land", ]

ke_hh_combined_rfz <- bind_rows(ke_hh_baseline_rfz, ke_hh_midline_rfz)

RFZ_df <- data.frame(NA, nrow = 2, ncol = 17)
# Aggregate RFZ estimates
glo_prev_bl_adj_mod_sev_rfz <- weighted.mean(ke_hh_baseline_rfz$prob_mod_sev, ke_hh_baseline_rfz$wt)
glo_prev_bl_adj_mod_rfz <- weighted.mean(ke_hh_baseline_rfz$prob_mod, ke_hh_baseline_rfz$wt)
glo_prev_bl_adj_sev_rfz <- weighted.mean(ke_hh_baseline_rfz$prob_sev, ke_hh_baseline_rfz$wt)

glo_prev_bl_rfz <- c(glo_prev_bl_adj_mod_sev_rfz, glo_prev_bl_adj_mod_rfz, glo_prev_bl_adj_sev_rfz)

glo_prev_ml_adj_mod_sev_rfz <- weighted.mean(ke_hh_midline_rfz$prob_mod_sev, ke_hh_midline_rfz$wt)
glo_prev_ml_adj_mod_rfz <- weighted.mean(ke_hh_midline_rfz$prob_mod, ke_hh_midline_rfz$wt)
glo_prev_ml_adj_sev_rfz <- weighted.mean(ke_hh_midline_rfz$prob_sev, ke_hh_midline_rfz$wt)

glo_prev_ml_rfz <- c(glo_prev_ml_adj_mod_sev_rfz, glo_prev_ml_adj_mod_rfz, glo_prev_ml_adj_sev_rfz)

glo_prev_ml_rfz
glo_prev_bl_rfz


modsev_moe_bl_95 <- moe(ke_hh_baseline_rfz$prob_mod_sev,ke_hh_baseline_rfz$RS,ke_hh_baseline_rfz$wt * 10^6, conf.level = .95,
                        psu = ke_hh_baseline_rfz$psu, strata = ke_hh_baseline_rfz$strata)$moe * 100
mod_moe_bl_95 <- moe(ke_hh_baseline_rfz$prob_mod,ke_hh_baseline_rfz$RS,ke_hh_baseline_rfz$wt * 10^6, conf.level = .95,
                     psu = ke_hh_baseline_rfz$psu, strata = ke_hh_baseline_rfz$strata)$moe * 100
sev_moe_bl_95 <- moe(ke_hh_baseline_rfz$prob_sev,ke_hh_baseline_rfz$RS,ke_hh_baseline_rfz$wt * 10^6, conf.level = .95,
                     psu = ke_hh_baseline_rfz$psu, strata = ke_hh_baseline_rfz$strata)$moe * 100

moe_bl_95 <- c(modsev_moe_bl_95, mod_moe_bl_95, sev_moe_bl_95)

modsev_moe_ml_95 <- moe(ke_hh_midline_rfz$prob_mod_sev,ke_hh_midline_rfz$RS,ke_hh_midline_rfz$wt * 10^6, conf.level = .95,
                        psu = ke_hh_midline_rfz$psu, strata = ke_hh_midline_rfz$strata)$moe * 100
mod_moe_ml_95 <- moe(ke_hh_midline_rfz$prob_mod,ke_hh_midline_rfz$RS,ke_hh_midline_rfz$wt * 10^6, conf.level = .95,
                     psu = ke_hh_midline_rfz$psu, strata = ke_hh_midline_rfz$strata)$moe * 100
sev_moe_ml_95 <- moe(ke_hh_midline_rfz$prob_sev,ke_hh_midline_rfz$RS,ke_hh_midline_rfz$wt * 10^6, conf.level = .95,
                     psu = ke_hh_midline_rfz$psu, strata = ke_hh_midline_rfz$strata)$moe * 100


moe_ml_95 <- c(modsev_moe_ml_95, mod_moe_ml_95, sev_moe_ml_95)





RFZ_df[1, c(1,2, 3)] <- glo_prev_bl_rfz * 100
RFZ_df[1, c(4)] <- nrow(ke_hh_baseline_rfz)
RFZ_df[1, c(5)] <- round(sum(ke_hh_baseline_rfz$wt * 10^6), 2)
RFZ_df[1, c(6, 7, 8)] <- moe_bl_95
RFZ_df[1, c(9, 10, 11)] <- (glo_prev_bl_rfz * 100) - moe_bl_95 
RFZ_df[1, c(12, 13, 14)] <- (glo_prev_bl_rfz * 100) + moe_bl_95
RFZ_df[1, c(15, 16, 17)] <- c(NA, NA, NA)



RFZ_df[2, c(1,2, 3)] <- glo_prev_ml_rfz * 100
RFZ_df[2, c(4)] <- length(ke_hh_midline_rfz$RS)
RFZ_df[2, c(5)] <- round(sum(ke_hh_midline_rfz$wt * 10^6), 2)
RFZ_df[2, c(6, 7, 8)] <- moe_ml_95
RFZ_df[2, c(9, 10, 11)] <- (glo_prev_ml_rfz * 100) - moe_ml_95 
RFZ_df[2, c(12, 13, 14)] <- (glo_prev_ml_rfz * 100) + moe_ml_95
RFZ_df[2, c(15, 16, 17)] <- ifelse(((glo_prev_ml_rfz * 100) - (glo_prev_bl * 100)) > moe_ml_95, TRUE, FALSE)


colnames(RFZ_df) = c("Moderate+Severe_Food_Insecurity", "Moderate_Food_Insecurity", 
                     "Severe_Food_Insecurity", "N","WN", 
                     "MSFI_MoE", "MFI_MoE","SFI_MoE",
                     "MSFI_CI_Low", "MFI_CI_Low", "SFI_CI_Low",
                     "MSFI_CI_High", "MFI_CI_High", "SFI_CI_High",
                     "MSFI_Sig", "MFI_Sig", "SFI_Sig")
rownames(RFZ_df) = c("Baseline","Midline")

RFZ_df

#Disaggregates

rm(mod_fi, sev_fi, mod_sev_fi)

# populate for rfz 
# moderate

### Create groups for RFZ
group1 = ke_hh_combined_rfz$survey
group1 <- factor(group1, levels = c(1,2), labels = c("Baseline","Midline")) 
# GEN HH TYPE
group4 = ke_hh_combined_rfz$genhhtype_dj
# SHOCK SEV
group5 <- ke_hh_combined_rfz$shock_sev
# AWIQUINT
group6 <- ke_hh_combined_rfz$awiquint_rev

group_list <- list(group4, group5, group6)

groups <- c(unique(as.character(group4)), unique(as.character(group5)), 
            unique(as.character(group6)))


mod_fi <- data.frame()

for (i in 1:length(group_list)) {
  for (round in unique(group1)) {
    for (dis in unique(groups)) {
      if(!(dis %in% group_list[[i]])) next 
      rd = round
      disag = dis
      fltr = which(group1 == round &  group_list[[i]] == dis)
      prob_mods = ke_hh_combined_rfz$prob_mod[fltr]
      wt = ke_hh_combined_rfz$wt[fltr]*10^6
      rs = ke_hh_combined_rfz$RS[fltr]
      psu = ke_hh_combined_rfz$psu[fltr] 
      strata = ke_hh_combined_rfz$strata[fltr]
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
      prob_mods = ke_hh_combined_rfz$prob_sev[fltr]
      wt = ke_hh_combined_rfz$wt[fltr]*10^6
      rs = ke_hh_combined_rfz$RS[fltr]
      psu = ke_hh_combined_rfz$psu[fltr] 
      strata = ke_hh_combined_rfz$strata[fltr]
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
      prob_mods = ke_hh_combined_rfz$prob_mod_sev[fltr]
      wt = ke_hh_combined_rfz$wt[fltr]*10^6
      rs = ke_hh_combined_rfz$RS[fltr]
      psu = ke_hh_combined_rfz$psu[fltr] 
      strata = ke_hh_combined_rfz$strata[fltr]
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


mod_sev_fi_rfz <- 
  mod_sev_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(mod_sev_fi_rfz)


mod_fi_rfz <- 
  mod_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(mod_fi_rfz)



sev_fi_rfz <- 
  sev_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(sev_fi_rfz)


# rename columns for joining together

colnames(sev_fi_rfz)[-1] <- paste0("Severe_", colnames(sev_fi_rfz)[-1])

colnames(sev_fi_rfz)

colnames(mod_fi_rfz)[-1] <- paste0("Moderate_", colnames(mod_fi_rfz)[-1])

colnames(mod_fi_rfz)


table_5_1_2_rfz <- 
  mod_fi_rfz %>% 
  left_join(sev_fi_rfz, by = "Disaggregate")

table_5_1_1_rfz <- mod_sev_fi_rfz

# reorder rows?
table_5_1_1_rfz

table_5_1_1_rfz <-
  table_5_1_1_rfz %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "None", "Low", "Medium", "High")))

table_5_1_2_rfz <-
  table_5_1_2_rfz %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "None", "Low", "Medium", "High"))) %>%
  dplyr::select(Disaggregate, ends_with("_Baseline"), ends_with("Midline"), ends_with("_Diff"), ends_with("_Sig"))


table_5_1_1_rfz$Disaggregate <- paste(table_5_1_1_rfz$Disaggregate, " - RFZ")

table_5_1_2_rfz$Disaggregate <- paste(table_5_1_2_rfz$Disaggregate, " - RFZ")




### HR1 ####
ke_hh_baseline_hr1 <- ke_hh_baseline[ke_hh_baseline$psu == "1. High Rainfall",]
ke_hh_midline_hr1  <- ke_hh_midline[ke_hh_midline$psu  == "1. High Rainfall", ]

ke_hh_combined_hr1 <- bind_rows(ke_hh_baseline_hr1, ke_hh_midline_hr1)

HR1_df <- data.frame(NA, nrow = 2, ncol = 17)
# Aggregate HR1 estimates
glo_prev_bl_adj_mod_sev_hr1 <- weighted.mean(ke_hh_baseline_hr1$prob_mod_sev, ke_hh_baseline_hr1$wt)
glo_prev_bl_adj_mod_hr1 <- weighted.mean(ke_hh_baseline_hr1$prob_mod, ke_hh_baseline_hr1$wt)
glo_prev_bl_adj_sev_hr1 <- weighted.mean(ke_hh_baseline_hr1$prob_sev, ke_hh_baseline_hr1$wt)

glo_prev_bl_hr1 <- c(glo_prev_bl_adj_mod_sev_hr1, glo_prev_bl_adj_mod_hr1, glo_prev_bl_adj_sev_hr1)

glo_prev_ml_adj_mod_sev_hr1 <- weighted.mean(ke_hh_midline_hr1$prob_mod_sev, ke_hh_midline_hr1$wt)
glo_prev_ml_adj_mod_hr1 <- weighted.mean(ke_hh_midline_hr1$prob_mod, ke_hh_midline_hr1$wt)
glo_prev_ml_adj_sev_hr1 <- weighted.mean(ke_hh_midline_hr1$prob_sev, ke_hh_midline_hr1$wt)

glo_prev_ml_hr1 <- c(glo_prev_ml_adj_mod_sev_hr1, glo_prev_ml_adj_mod_hr1, glo_prev_ml_adj_sev_hr1)

glo_prev_ml_hr1
glo_prev_bl_hr1


modsev_moe_bl_95 <- moe(ke_hh_baseline_hr1$prob_mod_sev,ke_hh_baseline_hr1$RS,ke_hh_baseline_hr1$wt * 10^6, conf.level = .95,
                        psu = ke_hh_baseline_hr1$psu, strata = ke_hh_baseline_hr1$strata)$moe * 100
mod_moe_bl_95 <- moe(ke_hh_baseline_hr1$prob_mod,ke_hh_baseline_hr1$RS,ke_hh_baseline_hr1$wt * 10^6, conf.level = .95,
                     psu = ke_hh_baseline_hr1$psu, strata = ke_hh_baseline_hr1$strata)$moe * 100
sev_moe_bl_95 <- moe(ke_hh_baseline_hr1$prob_sev,ke_hh_baseline_hr1$RS,ke_hh_baseline_hr1$wt * 10^6, conf.level = .95,
                     psu = ke_hh_baseline_hr1$psu, strata = ke_hh_baseline_hr1$strata)$moe * 100

moe_bl_95 <- c(modsev_moe_bl_95, mod_moe_bl_95, sev_moe_bl_95)

modsev_moe_ml_95 <- moe(ke_hh_midline_hr1$prob_mod_sev,ke_hh_midline_hr1$RS,ke_hh_midline_hr1$wt * 10^6, conf.level = .95,
                        psu = ke_hh_midline_hr1$psu, strata = ke_hh_midline_hr1$strata)$moe * 100
mod_moe_ml_95 <- moe(ke_hh_midline_hr1$prob_mod,ke_hh_midline_hr1$RS,ke_hh_midline_hr1$wt * 10^6, conf.level = .95,
                     psu = ke_hh_midline_hr1$psu, strata = ke_hh_midline_hr1$strata)$moe * 100
sev_moe_ml_95 <- moe(ke_hh_midline_hr1$prob_sev,ke_hh_midline_hr1$RS,ke_hh_midline_hr1$wt * 10^6, conf.level = .95,
                     psu = ke_hh_midline_hr1$psu, strata = ke_hh_midline_hr1$strata)$moe * 100


moe_ml_95 <- c(modsev_moe_ml_95, mod_moe_ml_95, sev_moe_ml_95)





HR1_df[1, c(1,2, 3)] <- glo_prev_bl_hr1 * 100
HR1_df[1, c(4)] <- nrow(ke_hh_baseline_hr1)
HR1_df[1, c(5)] <- round(sum(ke_hh_baseline_hr1$wt * 10^6), 2)
HR1_df[1, c(6, 7, 8)] <- moe_bl_95
HR1_df[1, c(9, 10, 11)] <- (glo_prev_bl_hr1 * 100) - moe_bl_95 
HR1_df[1, c(12, 13, 14)] <- (glo_prev_bl_hr1 * 100) + moe_bl_95
HR1_df[1, c(15, 16, 17)] <- c(NA, NA, NA)



HR1_df[2, c(1,2, 3)] <- glo_prev_ml_hr1 * 100
HR1_df[2, c(4)] <- length(ke_hh_midline_hr1$RS)
HR1_df[2, c(5)] <- round(sum(ke_hh_midline_hr1$wt * 10^6), 2)
HR1_df[2, c(6, 7, 8)] <- moe_ml_95
HR1_df[2, c(9, 10, 11)] <- (glo_prev_ml_hr1 * 100) - moe_ml_95 
HR1_df[2, c(12, 13, 14)] <- (glo_prev_ml_hr1 * 100) + moe_ml_95
HR1_df[2, c(15, 16, 17)] <- ifelse(((glo_prev_ml_hr1 * 100) - (glo_prev_bl * 100)) > moe_ml_95, TRUE, FALSE)


colnames(HR1_df) = c("Moderate+Severe_Food_Insecurity", "Moderate_Food_Insecurity", 
                     "Severe_Food_Insecurity", "N","WN", 
                     "MSFI_MoE", "MFI_MoE","SFI_MoE",
                     "MSFI_CI_Low", "MFI_CI_Low", "SFI_CI_Low",
                     "MSFI_CI_High", "MFI_CI_High", "SFI_CI_High",
                     "MSFI_Sig", "MFI_Sig", "SFI_Sig")
rownames(HR1_df) = c("Baseline","Midline")

HR1_df

#Disaggregates

rm(mod_fi, sev_fi, mod_sev_fi)

# populate for rfz 
# moderate

### Create groups for HR1
group1 = ke_hh_combined_hr1$survey
group1 <- factor(group1, levels = c(1,2), labels = c("Baseline","Midline")) 
# GEN HH TYPE
group4 = ke_hh_combined_hr1$genhhtype_dj
# SHOCK SEV
group5 <- ke_hh_combined_hr1$shock_sev
# AWIQUINT
group6 <- ke_hh_combined_hr1$awiquint_rev

group_list <- list(group4, group5, group6)

groups <- c(unique(as.character(group4)), unique(as.character(group5)), 
            unique(as.character(group6)))


mod_fi <- data.frame()

for (i in 1:length(group_list)) {
  for (round in unique(group1)) {
    for (dis in unique(groups)) {
      if(!(dis %in% group_list[[i]])) next 
      rd = round
      disag = dis
      fltr = which(group1 == round &  group_list[[i]] == dis)
      prob_mods = ke_hh_combined_hr1$prob_mod[fltr]
      wt = ke_hh_combined_hr1$wt[fltr]*10^6
      rs = ke_hh_combined_hr1$RS[fltr]
      psu = ke_hh_combined_hr1$psu[fltr] 
      strata = ke_hh_combined_hr1$strata[fltr]
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
      prob_mods = ke_hh_combined_hr1$prob_sev[fltr]
      wt = ke_hh_combined_hr1$wt[fltr]*10^6
      rs = ke_hh_combined_hr1$RS[fltr]
      psu = ke_hh_combined_hr1$psu[fltr] 
      strata = ke_hh_combined_hr1$strata[fltr]
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
      prob_mods = ke_hh_combined_hr1$prob_mod_sev[fltr]
      wt = ke_hh_combined_hr1$wt[fltr]*10^6
      rs = ke_hh_combined_hr1$RS[fltr]
      psu = ke_hh_combined_hr1$psu[fltr] 
      strata = ke_hh_combined_hr1$strata[fltr]
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


mod_sev_fi_hr1 <- 
  mod_sev_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(mod_sev_fi_hr1)


mod_fi_hr1 <- 
  mod_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(mod_fi_hr1)



sev_fi_hr1 <- 
  sev_fi %>%
  as_tibble() %>%
  dplyr::select(Survey_Round, Disaggregate, N, FI, MoE) %>%
  dplyr::filter(!is.na(Disaggregate)) %>%
  dplyr::mutate(N = as.numeric(N),
                FI = as.numeric(FI),
                MoE = as.numeric(MoE),
                CI_Low = round((FI - MoE), 1),
                CI_High = round((FI + MoE), 1),
                CI = paste0("(", CI_Low, ", ", CI_High, ")")) %>%
  tidyr::pivot_wider(names_sep = "_", names_from = "Survey_Round", values_from = c("N", "FI", "CI", "MoE"), id_cols = "Disaggregate") %>%
  mutate_if(is.numeric, funs(round(., 1))) %>%
  mutate(Diff = FI_Midline - FI_Baseline,
         Sig = ifelse(abs(Diff) > abs(MoE_Midline), TRUE, FALSE)) %>%
  dplyr::select(Disaggregate, FI_Baseline, CI_Baseline, N_Baseline, 
                FI_Midline, CI_Midline, N_Midline, Diff, Sig, MoE_Baseline, MoE_Midline) 

View(sev_fi_hr1)


# rename columns for joining together

colnames(sev_fi_hr1)[-1] <- paste0("Severe_", colnames(sev_fi_hr1)[-1])

colnames(sev_fi_hr1)

colnames(mod_fi_hr1)[-1] <- paste0("Moderate_", colnames(mod_fi_hr1)[-1])

colnames(mod_fi_hr1)


table_5_1_2_hr1 <- 
  mod_fi_hr1 %>% 
  left_join(sev_fi_hr1, by = "Disaggregate")

table_5_1_1_hr1 <- mod_sev_fi_hr1

# reorder rows?
table_5_1_1_hr1

table_5_1_1_hr1 <-
  table_5_1_1_hr1 %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "None", "Low", "Medium", "High")))

table_5_1_2_hr1 <-
  table_5_1_2_hr1 %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "None", "Low", "Medium", "High"))) %>%
  dplyr::select(Disaggregate, ends_with("_Baseline"), ends_with("Midline"), ends_with("_Diff"), ends_with("_Sig"))


table_5_1_1_hr1$Disaggregate <- paste(table_5_1_1_hr1$Disaggregate, " - HR1")

table_5_1_2_hr1$Disaggregate <- paste(table_5_1_2_hr1$Disaggregate, " - HR1")



#### Data wrangling - Reshape AGG_df and RFZ_df to join to table ####
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
# HR1_df
HR1_df_511_join <-
  HR1_df %>% 
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

HR1_df_512_join <-
  HR1_df %>% 
  dplyr::select(-starts_with(c("Moderate+", "MSFI")), -WN) %>%
  rownames_to_column("Period") %>%
  pivot_wider(values_from = -Period, names_from = Period)

names(HR1_df_512_join) <- 
  HR1_df_512_join %>% names() %>% 
  str_replace_all('Food_Insecurity', "FI") %>% 
  str_replace_all('MFI', "Moderate") %>%
  str_replace_all('SFI', "Severe")

HR1_df_512_join <- 
  HR1_df_512_join %>% 
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

HR1_df_511_join$Disaggregate <- paste("HR1 -", HR1_df_511_join$Disaggregate)
HR1_df_512_join$Disaggregate <- paste("HR1 -", HR1_df_512_join$Disaggregate)

# RFZ_df
RFZ_df_511_join <-
  RFZ_df %>% 
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

RFZ_df_512_join <-
  RFZ_df %>% 
  dplyr::select(-starts_with(c("Moderate+", "MSFI")), -WN) %>%
  rownames_to_column("Period") %>%
  pivot_wider(values_from = -Period, names_from = Period)

names(RFZ_df_512_join) <- 
  RFZ_df_512_join %>% names() %>% 
  str_replace_all('Food_Insecurity', "FI") %>% 
  str_replace_all('MFI', "Moderate") %>%
  str_replace_all('SFI', "Severe")

RFZ_df_512_join <- 
  RFZ_df_512_join %>% 
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

RFZ_df_511_join$Disaggregate <- paste("RFZ -", RFZ_df_511_join$Disaggregate)
RFZ_df_512_join$Disaggregate <- paste("RFZ -", RFZ_df_512_join$Disaggregate)

# Finaliz tables 
table_5_1_1 <- bind_rows(AGG_df_511_join, table_5_1_1, RFZ_df_511_join, table_5_1_1_rfz, table_5_1_1_hr1, HR1_df_511_join)
table_5_1_2 <- bind_rows(AGG_df_512_join, table_5_1_2, RFZ_df_512_join, table_5_1_2_rfz, table_5_1_2_hr1, HR1_df_512_join)

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


# calculate samburu 
modsev_samburu <- weighted.mean(ke_hh_midline$prob_mod_sev[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                                ke_hh_midline$wt[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"])
mod_samburu <- weighted.mean(ke_hh_midline$prob_mod[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                             ke_hh_midline$wt[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"])
sev_samburu <- weighted.mean(ke_hh_midline$prob_sev[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                             ke_hh_midline$wt[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"])


samburu_modsev_moe_95 <- moe(prob = ke_hh_midline$prob_mod_sev[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"],
                             rs = ke_hh_midline$RS[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                             wt = ke_hh_midline$wt[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                             conf.level = .95,
                             psu = ke_hh_midline$psu[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                             strata = ke_hh_midline$strata[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"])$moe * 100

samburu_mod_moe_95 <- moe(prob = ke_hh_midline$prob_mod[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"],
                          rs = ke_hh_midline$RS[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                          wt = ke_hh_midline$wt[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"] * 10^6, 
                          conf.level = .95,
                          psu = ke_hh_midline$psu[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                          strata = ke_hh_midline$strata[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"])$moe * 100

samburu_sev_moe_95 <- moe(prob = ke_hh_midline$prob_sev[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"],
                          rs = ke_hh_midline$RS[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                          wt =ke_hh_midline$wt[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"] * 10^6, 
                          conf.level = .95,
                          psu = ke_hh_midline$psu[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"], 
                          strata = ke_hh_midline$strata[ke_hh_midline$psu == "2. Northern Arid Land"|ke_hh_midline$psu == "3. Semi Arid Land"|ke_hh_midline$psu == "4. Northen Arid Land-Samburu"])$moe * 100



samburu_sev_95ci <- 
  paste0("(", round((sev_samburu * 100) - samburu_sev_moe_95, 1), "%, ", round((sev_samburu * 100) + samburu_sev_moe_95, 1), "%)")

samburu_mod_95ci <- paste0("(", round((mod_samburu * 100) - samburu_mod_moe_95, 1), "%, ", round((mod_samburu * 100) + samburu_mod_moe_95, 1), "%)")

samburu_modsev_95ci <- paste0("(", round((modsev_samburu * 100) - samburu_modsev_moe_95, 1), "%, ", round((modsev_samburu * 100) + samburu_modsev_moe_95, 1), "%)")

# add row for samburu
table_5_1_1[nrow(table_5_1_1)+1,] <- NA

table_5_1_2[nrow(table_5_1_2)+1,] <- NA  

table_5_1_1[nrow(table_5_1_1), 1] <- "Samburu"


table_5_1_1[nrow(table_5_1_1), 5] <- round(modsev_samburu * 100, 1)
table_5_1_1[nrow(table_5_1_1), 6] <- samburu_modsev_95ci
table_5_1_1[nrow(table_5_1_1), 7] <- nrow(ke_hh_midline %>% dplyr::filter(psu == "2. Northern Arid Land"|psu == "3. Semi Arid Land"|psu == "4. Northen Arid Land-Samburu"))

table_5_1_2[nrow(table_5_1_2), 1] <- "Samburu"

table_5_1_2[nrow(table_5_1_2), 7]  <- round(mod_samburu * 100, 1)
table_5_1_2[nrow(table_5_1_2), 8]  <- samburu_mod_95ci
table_5_1_2[nrow(table_5_1_2), 10] <- round(sev_samburu * 100, 1)
table_5_1_2[nrow(table_5_1_2), 11] <- samburu_sev_95ci

table_5_1_2[nrow(table_5_1_2), 13] <- nrow(ke_hh_midline %>% dplyr::filter(psu == "2. Northern Arid Land"|psu == "3. Semi Arid Land"|psu == "4. Northen Arid Land-Samburu"))

write.csv(table_5_1_1, "table_5_1_1.csv")
write.csv(table_5_1_2, "table_5_1_2.csv")
