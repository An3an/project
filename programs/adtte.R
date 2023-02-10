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
adae <- read_xpt("adam/adae.xpt")

#where AEDECOD in
adae<-adae%>%filter(AEDECOD %in% c('APPLICATION', 'DERMATITIS', 'ERYTHEMA', 'BLISTER') )

#Need ADAE complete to program, using admiral SDTM.AE in the mean time


#Creating shell of program before ADAE is complete
adsl_vars<-c("AGE","RACE","SAFFL","SEX","SITEID","RFSTDTC","STUDYID","TRT01A",
             "TRT01AN","TRTEDT","TRT01P","TRTSDT","USUBJID","RFENDT")

#"AGEGR1" ,"AGEGR1N","RACEN","TRTDUR"add later
adae_vars<-c("ASTDT","USUBJID","TRTEMFL","STUDYID","USUBJID","SITEID","AEDECOD","AESEQ")

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

#subsetting datasets (not necessary but easier to look at when programming)
adsl<-adsl %>% select(all_of(adsl_vars))
adae<-adae %>% select(all_of(adae_vars))


# Get list of ADSL vars required for derivations
adsl_vars1<-vars(RFSTDTC,STUDYID,USUBJID,RFENDT,SITEID)

#Merge together adsl and adae
work_adtte <- adae %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars1,
    by_vars = vars(STUDYID,USUBJID,SITEID),
    #filter_add = CQ01NAM == "DERMATOLOGIC EVENTS",
  )

#time to adverse event derivation

#events
ttae <- event_source(
  dataset_name = "adae",
  filter= TRTEMFL=="Y",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "Dermatologic Event Occured",
    SRCDOM = "ADAE",
    SRCVAR = "AESTDTC",
    SRCSEQ = AESEQ
  )
)


#censors
eos <- censor_source(
  dataset_name = "adsl",
  date = RFENDT,
  set_values_to = vars(
    EVNTDESC = "Study Completion Date",
    SRCDOM = "ADSL",
    SRCVAR = "RFENDT" #might need to convet RFENDTC to date format
  )
)

#change AEDECOD to CQ01NAM once it is present in ADAE
#deriving the ttde param using censor and event done above

param_tte<-derive_param_tte(
  dataset_adsl = adsl,
  by_vars = vars(AEDECOD),
  start_date = TRTSDT,
  event_conditions = list(ttae),
  censor_conditions = list(eos),
  source_datasets = list(adsl = adsl, adae = adae),
  set_values_to = vars(
    PARAMCD = paste0("TTDE", as.numeric(as.factor(AEDECOD))),
    PARAM = paste("Time to First", AEDECOD)))

#deriving aval, remember R is case sensitive! (if you had multiple variables to derive you can do it all inside one mutate)

param_tte1<-param_tte %>% mutate(
    AVAL=ADT-STARTDT+1
)

#merge our parameters back onto original data to get all the variables we need

adtte_vars=vars(EVNTDESC,SRCDOM,SRCSEQ,CNSR,ADT,STARTDT,PARAMCD,PARAM,AVAL)

adtte_<-left_join(work_adtte,param_tte1,by=c("USUBJID","STUDYID"),multiple="all")

#using the spec to include only the variables we need

variables <- adtte_spec[['variable']]

#update to all_of when we have all the variables needed in ADSL/ADAE

adtte<-adtte_%>%select(any_of(variables))

#exporting to xpt
xportr_write(adtte, "adam/adtte.xpt")
