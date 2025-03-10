################ UGANDA BASELINE ######################
library(haven)
library(tidyverse)
library(readxl)
library(vtable)
library(eRm)
library(RM.weights)
library(survey)
# C:\Users\N116937\ICF\SMRFS - 13. Uganda Midline\08. Data Analysis\Baseline\Analytic data
setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Uganda/Stata/Analytic")
ug_hh_baseline_import <- read_dta("FTF_P2-ZOI_Survey_Uganda_2019_household_data_analytic_REVISED_20231122.dta")
ug_ps_baseline_import <- read_dta("FTF_P2-ZOI_Survey_Uganda_2019_persons_data_analytic_REVISED_20231122.dta")

ug_hh_midline_import <- read_dta("FTF_P2-ZOI_2022_Uganda_Survey_household_data_analytic.dta")
# C:/Users/59952/ICF/SMRFS - Documents/13. Uganda Midline/08. Data Analysis/Midline/Processed_data/Analytic_data/FTF_P2-ZOI_Survey_Uganda_2022_household_data_Analytic
# ug_hh_midline_raw <- read_dta("C:/Users/59952/OneDrive - ICF/Documents/Data Analysis/Projects/Uganda Midline 2022/UGDA/FTF_P2-ZOI_Survey_Uganda_2022_household_WASH_Dwelling_data.DTA")
# C:/Users/59952/OneDrive - ICF/Documents/Data Analysis/Projects/Uganda Midline 2022/UGDA/FTF_P2-ZOI_Survey_Uganda_2022_household_WASH_Dwelling_data.DTA

# FIES
# vtable(ug_hh_midline_import)

# describe FIES vars
ug_hh_midline_import %>%
  dplyr::select(v301:v308) %>%
  psych::describe()
# View first 10 rows
ug_hh_midline_import %>%
  dplyr::distinct(hhea, hhnum, .keep_all = T) %>%
  dplyr::select(hhea, hhnum, v354, v301:v308)


# v300e & v354 - same?
ug_hh_midline_import %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::count(v300e)

ug_hh_midline_import %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::count(v354)

ug_hh_midline_import %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::group_by(v300e, v354) %>%
  count(.)
# confirmed

# view incomplete cases
ug_hh_midline_import %>%
  dplyr::select(v354, v301:v308) %>%
  dplyr::filter(!complete.cases(.))
# 12 cases, 11 are code 3 (respondent not available), 1 is code 6 (other)

# View all completed interviews with codes at v354 == 3 or v354 == 6
ug_hh_midline_import %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::filter(v354 == 3 | v354 == 6) %>%
  dplyr::select(hhea, hhnum, ahresult, v354, v301:v308)

# all NA's are either RESPONDENT NOT AVAILABLE (11) or OTHER (1)

# rows with FIES responses that are code 3 on v354
# hhea == 36, hhnum == 90
# hhea == 89, hhnum == 52


fies_prop_ml <- 
  ug_hh_midline_import %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() %>%
  prop.table(., 1) * 100

# 
# proportion responding yes to each item
round(fies_prop_ml, 2)

# examine correlation between items
# ug_hh_midline_import %>%
#   dplyr::select(v301:v308) %>%
#   psych::cor.plot()

# BASELINE
# see first 10 rows
ug_hh_baseline_import %>%
  dplyr::distinct(hhea, hhnum, .keep_all = T) %>%
  dplyr::select(hhea, hhnum, v301:v308)

# desc stats
ug_hh_baseline_import %>%
  dplyr::select(v301:v308) %>%
  psych::describe()

# incomplete cases
ug_hh_baseline_import %>%
  dplyr::select(v343, v300e, v301:v308) %>%
  dplyr::filter(!complete.cases(.)) %>%
  as.data.frame()
# lots of rows with incomplete cases

# distribution of v300e
ug_hh_baseline_import %>%
  dplyr::count(v300e)
# most or all seem to have NA code on v300e

# how many are interviews that were not completed
ug_hh_baseline_import %>%
  dplyr::count(ahresult)

# look at FIES items who have completed interviews and NA result for v300e
ug_hh_baseline_import %>%
  dplyr::filter(is.na(v300e)) %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::select(hhea, hhnum, ahresult, v300e, v301:v308)
# filtering on completed interviews and v300e == NA
# only 7 rows total

#


# Incomplete cases - all have NAs for all FIES questions. Includes 7 completed interviews, the rest are incomplete for various reasons (as shown in previous table)

ug_hh_baseline_import %>%
  dplyr::filter(is.na(v300e) & ahresult == 1) %>%
  dplyr::select(hhnum, hhea, ahresult, starts_with("v3"))

ug_hh_baseline_import %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::select(v301:v308) %>%
  psych::describe()


# proportions - unweighted
fies_prop_bl <- 
  ug_hh_baseline_import %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() %>%
  prop.table(., 1) * 100

round(fies_prop_bl, 2)
round(fies_prop_ml, 2)

# view refused
ug_hh_baseline_import %>%
  #   dplyr::filter(is.na(v300e)) %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() 

# total N on each item  
ug_hh_baseline_import %>%
  #   dplyr::filter(is.na(v300e)) %>%
  dplyr::filter(ahresult == 1) %>%
  dplyr::select(v301:v308) %>%
  pivot_longer(cols = v301:v308) %>%
  mutate(value = as_factor(value)) %>%
  dplyr::group_by(name, value) %>%
  table() %>%
  t() %>%
  colSums()

#### START ANALYSIS ##############
# function for calculating sampling errors/MOE
source("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Kenya/KenyaDA/2. Midline/X5. Jamie/R/Kenya_FIES/moe_complex survey design.R")
#create a new function to compute population standard deviation
var.p <- function(x) var(x) * (length(x)-1) / length(x)
sd.p <- function(x) sqrt(var.p(x))


# ug_hh_midline_import %>%
#   dplyr::left_join()

ug_hh_baseline <- ug_hh_baseline_import
ug_hh_midline  <- ug_hh_midline_import


# keep complete cases only
ug_hh_baseline <-
  ug_hh_baseline %>%
  dplyr::filter(v300e == 1) %>% 
  dplyr::filter(ahresult == 1)

# drop endline -
ug_hh_baseline <-
  ug_hh_baseline %>%
  dplyr::filter(ahphase != 1)

ug_hh_midline <-
  ug_hh_midline %>%
  dplyr::filter(v300e == 1) %>% 
  dplyr::filter(ahresult == 1)

# BASELINE
# convert NO replies from 2 to 0
ug_hh_baseline <- 
  ug_hh_baseline %>% 
  mutate_at(dplyr::vars(v301:v308),
            dplyr::funs(case_when(. == 1 ~ 1,
                                  . == 2 ~ 0)
                        ))


# rename FIES item variables
ug_hh_baseline <- 
  ug_hh_baseline %>%
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
ug_hh_baseline$wt <- ug_hh_baseline$wgt_hh2 / 1000000

# create combined weight and HH size
ug_hh_baseline$wt_mem <- ug_hh_baseline$wt * ug_hh_baseline$hhsize_dj

# region

ug_hh_baseline <-
  ug_hh_baseline %>%
  dplyr::filter(a03d != 1 & a03d != 9)

ug_hh_baseline$psu <- sjmisc::rec(ug_hh_baseline$a03d, rec = "2=1; 3=3;5=2; 4=4", 
                                  val.labels = c("1. Eastern", 
                                                 "2. Karamoja",
                                                 "3. Northern",
                                                 "4. Southwestern"))

ug_hh_baseline$psu <- factor(ug_hh_baseline$psu, levels=c(1,2,3,4), labels=c("East", "Karamoja", "North", "Southwest"))

# Labels:
# value    label
# 1      1. Eastern
# 2      2. Karamoja
# 3      3. Northern
# 4      4. Southwestern

# Labels:
#   value  label
# 1             1. Central
# 2                2. East
# 3               3. North
# 4           4. Southwest
# 5            5. Karamoja
# 9 9. Not in ZOI corridor

# rfz 
ug_hh_baseline$rfz <- factor(ug_hh_baseline$ftf_resil, levels= c(0,1), labels = c("Non-RFZ", "RFZ"))


# urban rural
ug_hh_baseline$ahtype <- factor(ug_hh_baseline$ahtype, levels=c(1,2), labels=c("urban","rural"))
# ward
# cluster and HH id
ug_hh_baseline$hhea <- ug_hh_baseline$hhea
ug_hh_baseline$hhnum <- ug_hh_baseline$hhnum
# strata
ug_hh_baseline$strata <- ug_hh_baseline$strata

# survey round - baseline == 1
ug_hh_baseline$survey <- 1

# genhhtype
# ug_hh_baseline$genhhtype_dj
ug_hh_baseline$genhhtype_dj <- factor(ug_hh_baseline$genhhtype_dj, levels = c(1,2,3,4), labels = c("De jure male and female adults","De jure female, no male", 
                                                         "De jure male, no female", "De jure children only")) 


# wealth quintile
ug_hh_baseline$awiquint_rev <- forcats::fct_rev(factor(as.character(ug_hh_baseline$awiquint_p2)))
ug_hh_baseline$awiquint_rev <- factor(ug_hh_baseline$awiquint_rev, levels = c(1,2,3,4,5), labels = c("Poorest","Second", "Middle", "Fourth", "Wealthiest"))


# shock severity
ug_hh_baseline$shock_sev <- factor(ug_hh_baseline$shock_sev, levels = c(1,2,3,4), labels = c("None", "Low", "Medium", "High"))
 
# shock severity - recalc

ug_hh_baseline <- 
  ug_hh_baseline %>%
  dplyr::mutate_at(dplyr::vars(starts_with(c("r10", "r11")) & ends_with("c")), 
                   dplyr::funs(ifelse(. >= 5, NA, .))) %>%
  dplyr::mutate_at(dplyr::vars(starts_with(c("r10", "r11")) & ends_with("c")), 
                   dplyr::funs(ifelse(is.na(.), 0, .))) %>%
  dplyr::mutate_at(dplyr::vars(starts_with(c("r10", "r11")) & ends_with("a")), 
                   dplyr::funs(ifelse(. == 2, 0, .))) %>%
  dplyr::mutate_at(dplyr::vars(c("r105a", "r106a",  "r107a")), 
                   dplyr::funs(ifelse(r105 == 2, 0, .))) %>% 
  #   for var r105ax r106ax  r107ax: recode X .=0 if r105==2
  dplyr::mutate_at(dplyr::vars(c("r108a", "r109a")), 
                   dplyr::funs(ifelse(r108 == 2, 0, .))) %>% 
  # for var r108ax r109ax: recode X .=0 if r108==2
  dplyr::mutate_at(dplyr::vars(starts_with(c("r10", "r11")) & ends_with("a")), dplyr::funs(ifelse(. >=3, NA, .))) %>%
  dplyr::mutate(sei_2 = 
                  (r101c*r101a) + 
                  (r102c*r102a) + 
                  (r103c*r103a) + 
                  (r104c*r104a) + 
                  (r105c*r105a) + 
                  (r106c*r106a) + 
                  (r107c*r107a) + 
                  (r108c*r108a) + 
                  (r109c*r109a) + 
                  (r110c*r110a) + 
                  (r111c*r111a) + 
                  (r112c*r112a) + 
                  (r113c*r113a) + 
                  (r114c*r114a) + 
                  (r115c*r115a) + 
                  (r116c*r116a) + 
                  (r117c*r117a) + 
                  (r118c*r118a) + 
                  (r119c*r119a)) %>%
  dplyr::mutate(shock_sev2 = cut(sei_2, breaks=c(quantile(sei_2,  probs = seq(0, 1, by=0.33333), 
                                                          na.rm=T, names=TRUE, include.lowest=T, right = TRUE, 
                                                          labels=c("1","2","3"))))) %>%
  #  dplyr::select(sei, shock_sev, starts_with(c("r10", "r11"))) %>%
  dplyr::mutate(shock_sev2 = case_when(is.na(shock_sev2) ~ "None",
                                       shock_sev2 == "(0,12]" ~ "Low",
                                       shock_sev2 == "(12,22]" ~ "Medium",
                                       shock_sev2 == "(22,61.9]" ~ "High"))




table(ug_hh_baseline$shock_sev)
table(ug_hh_baseline$shock_sev2)


# education level
ug_hh_baseline$edulevel_hh_dj <- factor(ug_hh_baseline$edulevel_hh2_dj, levels = c(1,2,3,4,5), labels = c("No education","Less than primary Y3", "Completed primary Y3", "Completed primary", "Completed A Level or Higher"))
#
#
#
#
#
#

# No HHID?
ug_hh_baseline <- 
  ug_hh_baseline %>%
  mutate(hhid = row_number())

head(ug_hh_baseline$hhid)

# select vars to keep, drop vars not needed for analysis


ug_hh_baseline <- 
  ug_hh_baseline %>%
  dplyr::select(survey, hhid, hhea, hhnum, psu, rfz, wt, hhsize_dj, wt_mem, strata, genhhtype_dj, ahtype, 
              edulevel_hh_dj, 
              awiquint_rev, shock_sev, 
              WORRIED:WHLDAY)



# MIDLINE
# recode FIES vars - NO response from 2 to 0
ug_hh_midline <- 
  ug_hh_midline %>% mutate_at(dplyr::vars(v301:v308),
                                     dplyr::funs(case_when(
                                       . == 1 ~ 1,
                                       . == 2 ~ 0)
                                     ))
# rename vars
ug_hh_midline <- 
  ug_hh_midline %>%
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
ug_hh_midline$wt <- ug_hh_midline$wgt_hh / 1000000
# create combined weight and HH size
ug_hh_midline$wt_mem <- ug_hh_midline$wt * ug_hh_midline$hhsize_dj


# cluster and HH id
ug_hh_midline$hhea <- ug_hh_midline$hhea
# hhnum
ug_hh_midline$hhnum <- ug_hh_midline$hhnum
# psu
ug_hh_midline$psu <- ug_hh_midline$c06
ug_hh_midline$psu <- factor(ug_hh_midline$psu, levels=c(1,2,3,4), labels=c("East", "Karamoja", "North", "Southwest"))

# strata
ug_hh_midline$strata <- ug_hh_midline$stratum
# survey round- midline == 2
ug_hh_midline$survey <- 2

# add background variables as factors, with levels and labels
# HH type
# ug_hh_midline$genhhtype_dj <- ug_hh_midline$genhhtype_dj
ug_hh_midline$genhhtype_dj <- factor(ug_hh_midline$genhhtype_dj, levels = c(1,2,3,4), labels = c("De jure male and female adults","De jure female, no male", 
                                                                                                   "De jure male, no female", "De jure children only")) 

# region
#
#
#
#

# urban - rural
ug_hh_midline$ahtype <- factor(ug_hh_midline$ahtype, levels=c(1,2), labels=c("urban","rural"))
# highest HH education level
ug_hh_midline$edulevel_hh_dj <- factor(ug_hh_midline$edulevel_hh2_dj, levels = c(1,2,3,4,5), labels = c("No education","Less than primary Y3", "Completed primary Y3", "Completed primary", "Completed A Level or Higher"))
# rfz
ug_hh_midline$rfz <- ug_hh_midline$c07

table(ug_hh_midline$rfz)
ug_hh_midline$rfz <- ifelse(ug_hh_midline$rfz == 2, 0, ug_hh_midline$rfz)
ug_hh_midline$rfz <- factor(ug_hh_midline$rfz, levels= c(0,1), labels = c("Non-RFZ", "RFZ"))
table(ug_hh_midline$rfz)
#
#
#
#
#

# wealth quintile
# NOT IN DATASET. JOIN OTHRE DATASET TO ADD?
# ug_hh_midline$awiquint_rev <- forcats::fct_rev(factor(as.character(ug_hh_baseline$awiquint_p2)))
# ug_hh_baseline$awiquint_rev <- factor(ug_hh_baseline$awiquint_rev, levels = c(1,2,3,4,5), labels = c("Poorest","Second", "Middle", "Fourth", "Wealthiest"))

table(ug_hh_midline$awiquint)

ug_hh_midline$awiquint_rev <- forcats::fct_rev(factor(as.character(ug_hh_midline$awiquint), 
                                levels = c(1,2,3,4,5), 
                                labels = c("Poorest","Second", "Middle", "Fourth", "Wealthiest") ) ) 

# shock severity

# ug_hh_midline <- ug_hh_midline %>%
#  left_join(ug_hh_ml_shock_sev)

ug_hh_midline$shock_sev <- factor(ug_hh_midline$shock_sev, levels = c(1,2,3,4), labels = c("None", "Low", "Medium", "High"))
table(ug_hh_midline$shock_sev)

ug_hh_midline <- 
  ug_hh_midline %>%
  dplyr::mutate_at(dplyr::vars(starts_with(c("r10", "r11")) & ends_with("c")), 
                   dplyr::funs(ifelse(. >= 5, NA, .))) %>%
  dplyr::mutate_at(dplyr::vars(starts_with(c("r10", "r11")) & ends_with("c")), 
                   dplyr::funs(ifelse(is.na(.), 0, .))) %>%
  dplyr::mutate_at(dplyr::vars(starts_with(c("r10", "r11")) & ends_with("a")), 
                   dplyr::funs(ifelse(. == 2, 0, .))) %>%
  dplyr::mutate_at(dplyr::vars(c("r105a", "r106a",  "r107a")), 
                               dplyr::funs(ifelse(r105 == 2, 0, .))) %>% 
  #   for var r105ax r106ax  r107ax: recode X .=0 if r105==2
    dplyr::mutate_at(dplyr::vars(c("r108a", "r109a")), 
                                 dplyr::funs(ifelse(r108 == 2, 0, .))) %>% 
  # for var r108ax r109ax: recode X .=0 if r108==2
  dplyr::mutate_at(dplyr::vars(starts_with(c("r10", "r11")) & ends_with("a")), dplyr::funs(ifelse(. >=3, NA, .))) %>%
  dplyr::mutate(sei_2 = 
                  (r101c*r101a) + 
                  (r102c*r102a) + 
                  (r103c*r103a) + 
                  (r104c*r104a) + 
                  (r105c*r105a) + 
                  (r106c*r106a) + 
                  (r107c*r107a) + 
                  (r108c*r108a) + 
                  (r109c*r109a) + 
                  (r110c*r110a) + 
                  (r111c*r111a) + 
                  (r112c*r112a) + 
                  (r113c*r113a) + 
                  (r114c*r114a) + 
                  (r115c*r115a) + 
                  (r116c*r116a) + 
                  (r117c*r117a) + 
                  (r118c*r118a) + 
                  (r119c*r119a)) %>%
  dplyr::mutate(shock_sev2 = cut(sei_2, breaks=c(quantile(sei_2,  probs = seq(0, 1, by=0.33333), 
                                                               na.rm=T, names=TRUE, include.lowest=T, right = TRUE, 
                                                               labels=c("1","2","3"))))) %>%
#  dplyr::select(sei, shock_sev, starts_with(c("r10", "r11"))) %>%
  dplyr::mutate(shock_sev2 = case_when(is.na(shock_sev2) ~ "None",
                                      shock_sev2 == "(0,21]" ~ "Low",
                                      shock_sev2 == "(21,32]" ~ "Medium",
                                      shock_sev2 == "(32,71]" ~ "High"))




table(ug_hh_midline$shock_sev)
table(ug_hh_midline$shock_sev2)


# No HHID?
ug_hh_midline <- 
  ug_hh_midline %>%
  mutate(hhid = row_number())


# select vars
ug_hh_midline <- 
  ug_hh_midline %>%
  dplyr::select(survey, hhid, hhea, hhnum, wt, hhsize_dj, psu, rfz, wt_mem, strata, ahtype, 
                genhhtype_dj, edulevel_hh_dj, 
                awiquint_rev, shock_sev, 
                WORRIED:WHLDAY)


# CREATE COMBINED from BL and ML
ug_hh_combined <- bind_rows(ug_hh_baseline, ug_hh_midline)

# generate two matrices with FIES questions only: total, baseline and midline

# BL
FIES_baseline <-
  ug_hh_baseline %>%
  dplyr::select(WORRIED:WHLDAY)
# ML
FIES_midline <-
  ug_hh_midline %>%
  dplyr::select(WORRIED:WHLDAY)



# create row score var RS that sums number of yes responses across all items
ug_hh_baseline$RS = rowSums(FIES_baseline)
ug_hh_midline$RS = rowSums(FIES_midline)

### STEP 3 - RUNNING RASCH MODEL ####

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
# ggplot(pca_res_variance, aes(x = Item, y= Variance)) +
#   facet_grid(Survey ~ .) +
#   geom_col() +
#   #  geom_line()  +
#   #  coord_flip() +
#   theme_fivethirtyeight() +
#   ggtitle("Screeplot of Variance") +
#   theme(axis.title = element_text(size = 10)) +
#   xlab("Item") +
#   ylab("Variance Explained")


# Step 3e - Checking stability of the scale across rounds
# {
#   x = res_bl$b/sd.p(res_bl$b)
#   y_ml = res_ml$b/sd.p(res_ml$b)
#   plot(x,y_ml, col = 1, ylim = c(-2.5,2),ylab="",xlab="")
#   points(x, y_ml, col = 2)
#   text(x+0.04,x-0.25,colnames(FIES_baseline),cex=0.6,pos=2,srt=90)
#   abline(0,1,lty = 2)
#   legend("topleft",c("baseline","midline"),pch = 1, col = c(1,2), cex = 0.75)
#   title(main = "Comparing scales across surveys")
#   }
# 

# Step 3f - Equating across rounds (baseline and midline).
# the baseline and midline need to be put on a common scale in order
# to ensure comparison.
# identify items in common - one where scores are similar. all scores are similar in UG
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
# {
#   x = res_bl$b/sd.p(res_bl$b)
#   y_bl = res_bl$b/sd.p(res_bl$b)
#   y_ml = res_ml$b/sd.p(res_ml$b)
#   y_ml_adj = adj_ml$b/sd.p(adj_ml$b)
#   plot(x,y_bl, col = 1, ylim = c(-2.5,2),ylab="",xlab="")
#   points(x, y_ml, col = 2)
#   points(x,y_ml_adj, col = 3)
#   text(x+0.04,x-0.25,colnames(FIES_baseline),cex=0.6,pos=2,srt=90)
#   abline(0,1,lty = 2)
#   legend("topleft",c("baseline","midline","midline adjusted"),pch = 1, col = c(1,2,3), cex = 0.75)
#   title(main = "Comparing scales across surveys - after adjustment")
# }


# {
#   x = res_bl$a
#   plot(x,res_ml$a,col = 1,xlab="Baseline RS severity", ylab = "Midline RS severity")
#   points(x,adj_ml$a, pch = 2, col = 2)
#   abline(0,1)
#   legend("topleft",c("Before adjustment","After adjustment"),pch=c(1,2),col=c(1,2))
#   title(main="Adjusting the severity levels associated to raw scores")
# }


# create matrix with (weighted) distributions of Raw Scores for Total, Baseline, and Midline
RS_table = t(cbind(
  "Baseline" = aggregate(ug_hh_baseline$wt, list(ug_hh_baseline$RS), FUN=sum, na.rm=TRUE)$x /sum(ug_hh_baseline$wt[!is.na(ug_hh_baseline$RS)]),
  "Midline" = aggregate(ug_hh_midline$wt, list(ug_hh_midline$RS), FUN=sum, na.rm=TRUE)$x /sum(ug_hh_midline$wt[!is.na(ug_hh_midline$RS)])
))

RS_table %>%
  as.data.frame() %>%
  rownames_to_column("Survey") %>%
  pivot_longer(V1:V9) %>%
  dplyr::mutate(name = str_replace(name, "V", "Raw score ")) %>%
  dplyr::mutate(name = str_replace_all(name, c("1"="0","2"="1","3"="2","4"="3","5"="4","6"="5","7"="6","8"="7","9"="8"))) # %>% 
 # ggplot(., aes(x = name, y = value, color = Survey, fill = Survey)) +
 # geom_col(stat = "count", position = position_dodge()) +
 # xlab("Raw Scores") +
 # ylab("Proportion") +
 # coord_flip() +
 # scale_x_discrete(limits=rev)




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
# plot(x = loc_st, y = glob_st)
# text(x = loc_st, y = glob_st, cex = 0.6, pos = 4)
# abline(0,1,lty = 2)

loc_st

glob_st

abs(glob_st - loc_st)
# standardized version of both scales
# plot(x = loc_st/sd.p(loc_st), y = glob_st/sd.p(glob_st))
# text(x = loc_st/sd.p(loc_st), y = glob_st/sd.p(glob_st),names(glob_st),cex=0.6,pos=4)
# abline(0,1,lty = 2)


# exclude Items 2 (HEALTHY), 3 (FEWFOODS) and 8 (WHLDAY) from the equating set - scores are significantly different 

# produce mean and sd for each scale among common items used in equating
# columns 2,3,5,6,7,8
glob_st.m <- mean(glob_st[c(1, 4:7)])
glob_st.s <- sd.p(glob_st[c(1, 4:7)])
m.bl = mean(res_bl$b[c(1, 4:7)]) 
s.bl = sd.p(res_bl$b[c(1, 4:7)])


# mapping the thresholds from the global scale onto the local (baseline) scale

glob_st_adj = (glob_st - glob_st.m)/glob_st.s * s.bl  + m.bl

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
# glo_prev_ml_not_adj_sev

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
ug_hh_baseline$prob_mod_sev = NULL
ug_hh_midline$prob_mod_sev = NULL
ug_hh_baseline$prob_mod = NULL
ug_hh_midline$prob_mod = NULL
ug_hh_baseline$prob_sev = NULL
ug_hh_midline$prob_sev = NULL


for (rs in 0:8) {
  ug_hh_baseline$prob_mod[ug_hh_baseline$RS == rs] = glo_probs_bl_mod[rs+1]
  ug_hh_midline$prob_mod[ug_hh_midline$RS == rs] =   glo_probs_ml_mod[rs+1]
  ug_hh_baseline$prob_mod_sev[ug_hh_baseline$RS == rs] = glo_probs_bl_mod_sev[rs+1]
  ug_hh_midline$prob_mod_sev[ug_hh_midline$RS == rs] =   glo_probs_ml_mod_sev[rs+1]
  ug_hh_baseline$prob_sev[ug_hh_baseline$RS == rs] = glo_probs_bl_sev[rs+1]
  ug_hh_midline$prob_sev[ug_hh_midline$RS == rs] =   glo_probs_ml_sev[rs+1]
}

table(ug_hh_baseline$prob_mod,ug_hh_baseline$RS,useNA = "ifany")

table(ug_hh_baseline$prob_mod_sev,ug_hh_baseline$RS,useNA = "ifany")

table(ug_hh_baseline$prob_sev,ug_hh_baseline$RS,useNA = "ifany")


table(ug_hh_midline$prob_mod,ug_hh_midline$RS,useNA = "ifany")

table(ug_hh_midline$prob_mod_sev,ug_hh_midline$RS,useNA = "ifany")

table(ug_hh_midline$prob_sev,ug_hh_midline$RS,useNA = "ifany")

# FILTER FOR RFZ ONLY - USE ONLY IF CALCULATING DISAGGREGATES IN RFZ
# ug_hh_midline <- 
#  ug_hh_midline %>%
#  dplyr::filter(rfz=="RFZ")
# ug_hh_baseline <- 
#  ug_hh_baseline %>%
#  dplyr::filter(rfz=="RFZ")
 


#### Calculate aggregate prevalence levels and put in df #####
AGG_df <- data.frame(NA, nrow = 2, ncol = 17)

modsev_moe_bl_95 <- moe(ug_hh_baseline$prob_mod_sev,ug_hh_baseline$RS,ug_hh_baseline$wt * 10^6, conf.level = .95,
                        psu = ug_hh_baseline$psu, strata = ug_hh_baseline$strata)$moe * 100
mod_moe_bl_95 <- moe(ug_hh_baseline$prob_mod,ug_hh_baseline$RS,ug_hh_baseline$wt * 10^6, conf.level = .95,
                     psu = ug_hh_baseline$psu, strata = ug_hh_baseline$strata)$moe * 100
sev_moe_bl_95 <- moe(ug_hh_baseline$prob_sev,ug_hh_baseline$RS,ug_hh_baseline$wt * 10^6, conf.level = .95,
                     psu = ug_hh_baseline$psu, strata = ug_hh_baseline$strata)$moe * 100

moe_bl_95 <- c(modsev_moe_bl_95, mod_moe_bl_95, sev_moe_bl_95)

modsev_moe_ml_95 <- moe(ug_hh_midline$prob_mod_sev,ug_hh_midline$RS,ug_hh_midline$wt * 10^6, conf.level = .95,
                        psu = ug_hh_midline$psu, strata = ug_hh_midline$strata)$moe * 100
mod_moe_ml_95 <- moe(ug_hh_midline$prob_mod,ug_hh_midline$RS,ug_hh_midline$wt * 10^6, conf.level = .95,
                     psu = ug_hh_midline$psu, strata = ug_hh_midline$strata)$moe * 100
sev_moe_ml_95 <- moe(ug_hh_midline$prob_sev,ug_hh_midline$RS,ug_hh_midline$wt * 10^6, conf.level = .95,
                     psu = ug_hh_midline$psu, strata = ug_hh_midline$strata)$moe * 100

moe_ml_95 <- c(modsev_moe_ml_95, mod_moe_ml_95, sev_moe_ml_95)


AGG_df[1, c(1,2, 3)] <- glo_prev_bl * 100
AGG_df[1, c(4)] <- nrow(res_bl$XX)
AGG_df[1, c(5)] <- round(sum(ug_hh_baseline$wt * 10^6), 2)
AGG_df[1, c(6, 7, 8)] <- moe_bl_95
AGG_df[1, c(9, 10, 11)] <- (glo_prev_bl * 100) - moe_bl_95 
AGG_df[1, c(12, 13, 14)] <- (glo_prev_bl * 100) + moe_bl_95
AGG_df[1, c(15, 16, 17)] <- c(NA, NA, NA)

AGG_df[2, c(1,2, 3)] <- glo_prev_ml * 100
AGG_df[2, c(4)] <- nrow(res_ml$XX)
AGG_df[2, c(5)] <- round(sum(ug_hh_midline$wt * 10^6), 2)
AGG_df[2, c(6, 7, 8)] <- moe_ml_95
AGG_df[2, c(9, 10, 11)] <- (glo_prev_ml * 100) - moe_ml_95 
AGG_df[2, c(12, 13, 14)] <- (glo_prev_ml * 100) + moe_ml_95
AGG_df[2, c(15, 16, 17)] <- ifelse(((glo_prev_ml * 100) - (glo_prev_bl * 100)) > moe_ml_95, "T", "F")


colnames(AGG_df) = c("Moderate+Severe_Food_Insecurity", "Moderate_Food_Insecurity", "Severe_Food_Insecurity",
                     "N","WN", 
                     "MSFI_MoE", "MFI_MoE","SFI_MoE",
                     "MSFI_CI_Low", "MFI_CI_Low", "SFI_CI_Low",
                     "MSFI_CI_High", "MFI_CI_High", "SFI_CI_High",
                     "MSFI_Sig", "MFI_Sig", "SFI_Sig")
rownames(AGG_df) = c("Baseline","Midline")

AGG_df



# Disaggregates - 
# Computing prevalence and MoEs by groups
# join BL and ML into common dataset
ug_hh_combined <- bind_rows(ug_hh_baseline, ug_hh_midline)

group1 = ug_hh_combined$survey
group1 <- factor(group1, levels = c(1,2), labels = c("Baseline","Midline")) 
# URBAN/RURAL
group2 = ug_hh_combined$ahtype
# REGION
group3 = ug_hh_combined$psu
# GEN HH TYPE
group4 = ug_hh_combined$genhhtype_dj
# SHOCK SEV
group5 <- ug_hh_combined$shock_sev
# AWIQUINT
group6 <- ug_hh_combined$awiquint_rev
# EDUCATION
group7 <- as.character(ug_hh_combined$edulevel_hh_dj)
# RFZ
group8 <- as.character(ug_hh_combined$rfz)

group_list <- list(group2, group3, group4, group5, group6, group7, group8)

groups <- c(unique(as.character(group2)), unique(as.character(group3)), 
            unique(as.character(group4)), unique(as.character(group5)), 
            unique(as.character(group6)), unique(as.character(group7)),
            unique(as.character(group8)))

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
      prob_mods = ug_hh_combined$prob_mod[fltr]
      wt = ug_hh_combined$wt[fltr]*10^6
      rs = ug_hh_combined$RS[fltr]
      psu = ug_hh_combined$psu[fltr] 
      strata = ug_hh_combined$strata[fltr]
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
      prob_mods = ug_hh_combined$prob_sev[fltr]
      wt = ug_hh_combined$wt[fltr]*10^6
      rs = ug_hh_combined$RS[fltr]
      psu = ug_hh_combined$psu[fltr] 
      strata = ug_hh_combined$strata[fltr]
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
      prob_mods = ug_hh_combined$prob_mod_sev[fltr]
      wt = ug_hh_combined$wt[fltr]*10^6
      rs = ug_hh_combined$RS[fltr]
      psu = ug_hh_combined$psu[fltr] 
      strata = ug_hh_combined$strata[fltr]
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

View(mod_sev_fi)


mod_fi <- 
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

View(mod_fi)



sev_fi <- 
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

View(sev_fi)


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
                                "urban", "rural", "None", "Low", "Medium", "High",
                                "North", "Karamoja", "Southwest", "East", "Non-RFZ", "RFZ",
                                "Completed primary Y3", "Completed primary",
                                "Completed A Level or Higher", "No education", "Less than primary Y3")))

table_5_1_2 <-
  table_5_1_2 %>%
  arrange(match(Disaggregate, c("De jure male and female adults", "De jure female, no male", 
                                "De jure male, no female", "De jure children only",
                                "Wealthiest", "Fourth", "Middle", "Second", "Poorest",
                                "urban", "rural", "None", "Low", "Medium", "High",
                                "North", "Karamoja", "Southwest", "East",  "Non-RFZ", "RFZ",
                                "Completed primary Y3", "Completed primary",
                                "Completed A Level or Higher", "No education", "Less than primary Y3"))) %>%
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

table_5_1_1 <- bind_rows(AGG_df_511_join, table_5_1_1)
table_5_1_2 <- bind_rows(AGG_df_512_join, table_5_1_2)

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


setwd("C:/Users/jparr/OneDrive/Documents/ICF/Analysis/Uganda/R/FIES/Uganda_FIES")

# 
ug_hh_combined <- ug_hh_combined %>%
  rename("xsurvey" = "survey")

write_dta(ug_hh_combined, "Uganda_BL_ML_FIES.dta")

write_csv(table_5_1_1, "table_5_1_1.csv")
write_csv(table_5_1_2, "table_5_1_2.csv")


ug_ml_survey <- srvyr::as_survey_design(ug_hh_midline, 
                                        strata = strata, 
                                        weights = wt)

ug_ml_survey %>%
  srvyr::summarize(WORRIED = srvyr::survey_mean(WORRIED, vartype = "ci"),
                   HEALTHY = srvyr::survey_mean(HEALTHY, vartype = "ci"),
                   FEWFOOD = srvyr::survey_mean(FEWFOOD, vartype = "ci"),
                   SKIPPED = srvyr::survey_mean(SKIPPED, vartype = "ci"),
                   ATELESS = srvyr::survey_mean(ATELESS, vartype = "ci"),
                   RUNOUT  = srvyr::survey_mean(RUNOUT, vartype = "ci"),
                   HUNGRY  = srvyr::survey_mean(HUNGRY, vartype = "ci"),
                   WHLDAY  = srvyr::survey_mean(WHLDAY, vartype = "ci")
                   ) %>%
  t(.)
#### calculate design effect and se for ES1 and A11 ####
moe(ug_hh_midline$prob_mod_sev,
    ug_hh_midline$RS,
    ug_hh_midline$wt * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea, 
    strata = ug_hh_midline$strata)

moe(ug_hh_midline$prob_mod,
    ug_hh_midline$RS,
    ug_hh_midline$wt * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea, 
    strata = ug_hh_midline$strata)

moe(ug_hh_midline$prob_sev,
    ug_hh_midline$RS,
    ug_hh_midline$wt * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea, 
    strata = ug_hh_midline$strata)


moe(ug_hh_midline$prob_mod_sev[ug_hh_midline$genhhtype_dj == "De jure male and female adults"],
    ug_hh_midline$RS[ug_hh_midline$genhhtype_dj == "De jure male and female adults"],
    ug_hh_midline$wt[ug_hh_midline$genhhtype_dj == "De jure male and female adults"] * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea[ug_hh_midline$genhhtype_dj == "De jure male and female adults"], 
    strata = ug_hh_midline$strata[ug_hh_midline$genhhtype_dj == "De jure male and female adults"])


moe(ug_hh_midline$prob_mod_sev[ug_hh_midline$genhhtype_dj == "De jure female, no male"],
    ug_hh_midline$RS[ug_hh_midline$genhhtype_dj == "De jure female, no male"],
    ug_hh_midline$wt[ug_hh_midline$genhhtype_dj == "De jure female, no male"] * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea[ug_hh_midline$genhhtype_dj == "De jure female, no male"], 
    strata = ug_hh_midline$strata[ug_hh_midline$genhhtype_dj == "De jure female, no male"])

moe(ug_hh_midline$prob_mod_sev[ug_hh_midline$genhhtype_dj == "De jure male, no female"],
    ug_hh_midline$RS[ug_hh_midline$genhhtype_dj == "De jure male, no female"],
    ug_hh_midline$wt[ug_hh_midline$genhhtype_dj == "De jure male, no female"] * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea[ug_hh_midline$genhhtype_dj == "De jure male, no female"], 
    strata = ug_hh_midline$strata[ug_hh_midline$genhhtype_dj == "De jure male, no female"])

moe(ug_hh_midline$prob_mod_sev[ug_hh_midline$genhhtype_dj == "De jure male, no female"],
    ug_hh_midline$RS[ug_hh_midline$genhhtype_dj == "De jure male, no female"],
    ug_hh_midline$wt[ug_hh_midline$genhhtype_dj == "De jure male, no female"] * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$psu[ug_hh_midline$genhhtype_dj == "De jure male, no female"], 
    strata = ug_hh_midline$strata[ug_hh_midline$genhhtype_dj == "De jure male, no female"])$se_s


moe(ug_hh_midline$prob_mod_sev[ug_hh_midline$rfz == "RFZ"],
    ug_hh_midline$RS[ug_hh_midline$rfz == "RFZ"],
    ug_hh_midline$wt[ug_hh_midline$rfz == "RFZ"] * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea[ug_hh_midline$rfz == "RFZ"], 
    strata = ug_hh_midline$strata[ug_hh_midline$rfz == "RFZ"])

moe(ug_hh_midline$prob_mod[ug_hh_midline$rfz == "RFZ"],
    ug_hh_midline$RS[ug_hh_midline$rfz == "RFZ"],
    ug_hh_midline$wt[ug_hh_midline$rfz == "RFZ"] * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea[ug_hh_midline$rfz == "RFZ"], 
    strata = ug_hh_midline$strata[ug_hh_midline$rfz == "RFZ"])

moe(ug_hh_midline$prob_sev[ug_hh_midline$rfz == "RFZ"],
    ug_hh_midline$RS[ug_hh_midline$rfz == "RFZ"],
    ug_hh_midline$wt[ug_hh_midline$rfz == "RFZ"] * 10^6, 
    conf.level = .95,
    psu = ug_hh_midline$hhea[ug_hh_midline$rfz == "RFZ"], 
    strata = ug_hh_midline$strata[ug_hh_midline$rfz == "RFZ"])


ug_hh_midline %>%
  count()

ug_hh_midline %>%
  group_by(genhhtype_dj) %>%
  count()

ug_hh_midline %>%
  group_by(rfz) %>%
  count()
