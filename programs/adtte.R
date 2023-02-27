# Name: ADTTE
#
# Label: AE Time To 1st Derm. Event Analysis
#
# Input: adsl, adae

#Import packages needed
# pckgs<-c("admiral","admiral.test","dplyer","lubridate","stringr","haven","tidyr","metacore","metatools","xportr")
# install.packages(pckgs)

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

#Creating variables
#AGEGR1

format_agegr1 <- function(x) {
  case_when(
    x < 65 ~ "<65",
    between(x, 65, 80) ~ "65-80",
    x > 80 ~ ">80"
    #TRUE ~ "Missing"
  )
}


#AGEGR1N

format_agegr1n <- function(x) {
  case_when(
    x == "<65" ~ 1,
    x == "65-80" ~ 2,
    x == ">80" ~ 3
    #TRUE ~ NA ##Note, missing values
  )
}

#racer1n

format_racer1n <- function(x) {
  case_when(
    x == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 6,
    x == "ASIAN" ~ 3,
    x == "BLACK OR AFRICAN AMERICAN" ~ 2,
    x == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
    x == "WHITE" ~ 1
    #TRUE ~ NA ##Note, missing values
  )
}

#put TRTP/TRTA here for now (FIX if have time)
adsl<-adsl%>%mutate(RACEN = format_racer1n(RACE),
             AGEGR1 = format_agegr1(AGE),
             AGEGR1N = format_agegr1n(AGEGR1),  RFENDT = as.Date(RFENDT, origin = "1970-01-01"))

#Functions from admiral (just for reference):
#censor_source,convert_blanks_to_na,derive_param_tte,list_tte_source_objects,params,tte_source

#Import spec

adtte_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables")  %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))  %>%
  filter(dataset == 'ADTTE') %>%

  mutate(
    type = if_else(type == 'text','character', 'numeric')
  )

#Assigning parameters
param_lookup <- tibble::tribble(
   ~PARAMCD, ~PARAM,
   "TTDE","Time to First Dermatologic Event"
)



# Get list of ADSL vars required for derivations
adsl_vars1<-vars(RFSTDTC,STUDYID,USUBJID,RFENDT,SITEID,TRTSDT,TRTEDT)

# adae_vars1<-vars(ASTDT,USUBJID,TRTEMFL,STUDYID,USUBJID,SITEID,AEDECOD,"AESEQ","CQ01NAM")


#Merge together adsl and adae to be used in param derivation
work_adtte <- adae%>%filter(TRTEMFL=="Y") %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars1,
    by_vars = vars(STUDYID,USUBJID,SITEID,TRTSDT,TRTEDT),
  )


#time to adverse event derivation

#events
ttae <- event_source(
  dataset_name = "adae",
  filter= CQ01NAM=="DERMATOLOGIC EVENTS",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "Dematologic Event Occured",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
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

#deriving the ttde param using censor and event done above

param_tte<-derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adsl = adsl, adae = adae),
  # start_date = TRTSDT,
  event_conditions = list(ttae),
  censor_conditions = list(eos),
  subject_keys = vars(USUBJID,STUDYID,SITEID),
  set_values_to = vars(
    PARAMCD = "TTDE",
    PARAM = "Time to First Dermatologic Event"))

#deriving aval, remember R is case sensitive! (if you had multiple variables to derive you can do it all inside one mutate)

param_tte1<-param_tte %>% mutate(
    AVAL=ADT-STARTDT+1
)

#merge our parameters back onto original data to get all the variables we need


adtte_vars=vars(EVNTDESC,SRCDOM,SRCSEQ,CNSR,ADT,STARTDT,PARAMCD,PARAM,AVAL,SRCVAR)



adtte_<-adsl%>%derive_vars_merged(
  dataset_add = param_tte1,
  new_vars = adtte_vars,
  by_vars = vars(STUDYID,USUBJID,SITEID),
)

variables<-adtte_spec$variable
adtte_<-convert_blanks_to_na(adtte_)

adtte<-adtte_%>%
  xportr_type(adtte_spec, "ADTTE") %>%
  xportr_label(adtte_spec, "ADTTE") %>%
  xportr_format(adtte_spec, "ADTTE")%>%
  xportr_length(adtte_spec, "ADTTE")%>%
  mutate(
    TRTSDT = as.Date(TRTSDT, origin = "1970-01-01"),
    TRTEDT = as.Date(TRTEDT, origin = "1970-01-01"),
    ADT = as.Date(ADT, origin = "1970-01-01"),
    STARTDT = as.Date(STARTDT, origin = "1970-01-01"),
    TRTP = TRT01P,
    TRTPN = TRT01PN,
    TRTDUR=TRTDURD,
    TRTA = TRT01A,
    TRTAN = TRT01AN)%>% select(all_of(variables))


adtte<-adtte%>%
  xportr_write("adam/adtte.xpt", label = "AE Time To 1st Derm. Event Analysis")

final_adtte<-read_xpt("adam/adtte.xpt")

