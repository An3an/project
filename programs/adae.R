library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)


#Reading in all the data sets needed for ADAE

adsl <- read_xpt("adam/adsl.xpt")
ae <- read_xpt("sdtm/ae.xpt")
suppae <- read_xpt("sdtm/suppae.xpt")
ex <- read_xpt("sdtm/ex.xpt")

#Converting blanks to NA if there are any blanks present in these datasets

adsl <- convert_blanks_to_na(adsl)
ae <- convert_blanks_to_na(ae)
suppae <- convert_blanks_to_na(suppae)
ex <- convert_blanks_to_na(ex)

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

table(adsl$AGE) ##Very useful!



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


# Get list of ADSL vars required for ADAE data set

adsl_vars <- vars(STUDYID, SITEID, USUBJID, TRT01A, TRT01AN, AGE
                  ,RACE, SEX, SAFFL, TRTSDT, TRTEDT)


## Deriving the Analysis Start and End Date (ASTDT, AENDT)
#Creating astdt


astdt <- derive_vars_dt(
  ae,
  new_vars_prefix = "AST",
  AESTDTC,
  highest_imputation = "Y",
  date_imputation = "first",
  flag_imputation = "auto",
  min_dates = NULL,
  max_dates = NULL,
  preserve = FALSE
)

#Creating aendt
aendt <- derive_vars_dt(
  ae,
  new_vars_prefix = "AEN",
  AEENDTC,
  highest_imputation = "n",
  date_imputation = "last", ##Check: use "last" option
  flag_imputation = "auto",
  min_dates = NULL,
  max_dates = NULL,
  preserve = FALSE
)




adae <- ae %>%
  # joining adsl to ae
    derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by = vars(STUDYID, USUBJID)
   ) %>% mutate(
      ASTDT = astdt$ASTDT,
      ASTDTF = astdt$ASTDTF,
      AENDT = aendt$AENDT
    )


#Deriving analysis start and end day (ASTDY, AENDY)

adae <- derive_vars_dy(adae, reference_date = TRTSDT,
                     source_vars = vars(ASTDT, AENDT))


#Deriving AE duration and AE duration units (ADURN, ADURU)

adae <- derive_vars_duration(
  adae,
  new_var = ADURN,
  new_var_unit = ADURU,
  start_date = ASTDT,
  end_date = AENDT,
  in_unit = "days",
 # out_unit = "days",
  floor_in = TRUE,
  add_one = TRUE,
  trunc_out = FALSE
)

##Changing "DAYS" to "DAY to be able to compare

adae['ADURU'][adae['ADURU'] == 'DAYS'] <- 'DAY'


# Derive treatment emergent analysis flag (TRTEMFL)

adae <- derive_var_trtemfl(
  adae,
  new_var = TRTEMFL,
  start_date = ASTDT,
  end_date = AENDT,
  trt_start_date = TRTSDT,
  trt_end_date = TRTEDT,
  end_window = NULL,
  ignore_time_for_trt_end = TRUE,
  initial_intensity = NULL,
  intensity = NULL
)


table(adae$TRTEMFL) ##Very useful!

###Deriving 1st Occurrence of Any AE Flag (AOCCFL)

adae <-  restrict_derivation(
      dataset = adae,
      derivation = derive_var_extreme_flag,
      args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID,ASTDT,AESEQ),
      new_var = AOCCFL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y"
  )

table(adae$AOCCFL)



###Deriving 1st Occurrence of Any AE Flag (AOCCSFL)

adae <-  restrict_derivation(
  dataset = adae,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(USUBJID,AEBODSYS),
    order = vars(AEBODSYS,ASTDT,AESEQ),
    new_var = AOCCSFL,
    mode = "first"
  ),
  filter = TRTEMFL == "Y"
)




###Deriving 1st Occurrence of Preferred Term Flag (AOCCPFL)

adae <-  restrict_derivation(
  dataset = adae,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(USUBJID,AEBODSYS,AEDECOD),
    order = vars(AEBODSYS,AEDECOD,ASTDT,AESEQ),
    new_var = AOCCPFL,
    mode = "first"
  ),
  filter = TRTEMFL == "Y"
)




###Deriving 1st Occurrence of SeriousFlag (AOCC02FL)

adae <-  restrict_derivation(
  dataset = adae,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(ASTDT,AESEQ),
    order = vars(USUBJID),
    new_var = AOCC02FL,
    mode = "first"
  ),
  filter = (TRTEMFL == "Y" &
  AESER == "Y")
)





###Deriving 1st Occurrence of Serious SOC Flag (AOCC03FL)

adae <-  restrict_derivation(
  dataset = adae,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(AEBODSYS, ASTDT,AESEQ),
    order = vars(USUBJID,AEBODSYS),
    new_var = AOCC03FL,
    mode = "first"
  ),
  filter = (TRTEMFL == "Y" &
              AESER == "Y")
)




###Deriving 1st Occurrence of Serious PT Flag (AOCC04FL	)

adae <-  restrict_derivation(
  dataset = adae,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(AEBODSYS,AEDECOD, ASTDT,AESEQ),
    order = vars(USUBJID,AEBODSYS,AEDECOD),
    new_var = AOCC04FL	,
    mode = "first"
  ),
  filter = (TRTEMFL == "Y" &
              AESER == "Y")
)




##Customized Query 01 Name derivation

`%!in%` <- Negate(`%in%`)


adae$CQ01NAM <- ifelse (((grepl("APPLICATION*", adae$AEDECOD) | grepl("*DERMATITIS*", adae$AEDECOD)
                         | grepl("*ERYTHEMA*", adae$AEDECOD) | grepl("*BLISTER*", adae$AEDECOD))|
                         (adae$AEBODSYS %in% c('SKIN AND SUBCUTANEOUS TISSUE DISORDERS')))&
                          ((-grepl("COLD SWEAT", adae$AEDECOD))| (-grepl("HYPERHIDROSIS", adae$AEDECOD))
                           | (-grepl("ALOPECIA", adae$AEDECOD)))
                         ,"DERMATOLOGIC EVENTS", NA)

###Andrea to continue with the last bit of CQ01NAM tomorrow.


#Deriving 1st Occurrence 01 Flag for CQ01 (AOCC01FL)

adae <-  restrict_derivation(
  dataset = adae,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(USUBJID, ASTDT,AESEQ),
    order = vars(USUBJID),
    new_var = AOCC01FL	,
    mode = "first"
  ),
  filter = (is.na(CQ01NAM) &
              TRTEMFL == "Y")
)




adae <- adae %>%
    mutate(
    RACEN = format_racer1n(RACE),
    AGEGR1 = format_agegr1(AGE),
    AGEGR1N = format_agegr1n(AGEGR1)

)






###Removing columns not in the compare table

to_remove <- c("DOMAIN", "AESPID", "AEBDSYCD","AEDTC", "AESTDTC", "AEENDTC",
               "AESTDY","AEENDY")

adae <- adae[ , !(names(adae) %in% to_remove)]

###Renaming TRT01A and TRT01AN to TRTA and TRTAN

colnames(adae)[c(29,30)] <- c("TRTA", "TRTAN")




xportr_write(adae, "adam/adae.xpt")


