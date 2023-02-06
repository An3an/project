# Name: ADTTE
#
# Label: AE Time To 1st Derm. Event Analysis
#
# Input: adsl, adae

#Import packages needed
pckgs<-c("admiral","admiral.test","dplyer","lubridate","stringr","haven","tidyr","metacore","metatools","xportr")
install.packages(pckgs)

#Packages to be used
library(admiral)
library(dplyr)
library(lubridate)
library(stringr)
library(haven)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)

#Importing datasets
adsl <- read_xpt("adam/adsl.xpt")
ae <- read_xpt("sdtm/ae.xpt")

#Need ADAE complete to program, using admiral SDTM.AE in the mean time


#Creating shell of program before ADAE is complete
adsl_vars<-c("AGE","RACE","SAFFL","SEX","SITEID","RFSTDTC","STUDYID","TRT01A",
             "TRT01AN","TRTEDT","TRT01P","TRTSDT","USUBJID")

adae_vars<-c("ASTDT","USUBJID","ADT","TRTEMFL","PARAM","PARAMCD","AVAL","AGEGR1","AGEGR1N","RACEN","TRTDUR")

#Functions from admiral (just for reference):
#censor_source,convert_blanks_to_na,derive_param_tte,list_tte_source_objects,params,tte_source

#Import spec

adtte_spec <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Variables")  %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))  %>%
  filter(dataset == 'ADTTE') %>%
  filter(!variable %in% c('ADTTE','AGEGR1', 'AGEGR1N', 'RACEN')) %>%
  mutate(
    type = if_else(type == 'text','character', 'numeric')
  )

#Assigning parameters
param_lookup <- tibble::tribble(
   ~PARAMCD, ~PARAM,
   "TTDE","Time to First Dermatologic Event"
)

adsl<-adsl %>% select(all_of(adsl_vars))
adae<-adae %>% select(all_of(adae_vars))


# Get list of ADSL vars required for derivations
adsl_vars1<-vars(AGE,RACE,SAFFL,SEX,SITEID,RFSTDTC,STUDYID,TRT01A,
             TRT01AN,TRTEDT,TRT01P,TRTSDT,USUBJID)


work_adtte <- adae %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars1,
    by_vars = vars(STUDYID,USUBJID),
    filter_add = CQ01NAM == "DERMATOLOGIC EVENTS",
  )
