library(haven)
library(Hmisc)
library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)
library(stringr)


#Reading in all the data sets needed for ADAE

adsl <- read_xpt("adam/adsl.xpt")
ae <- read_xpt("sdtm/ae.xpt")
suppae <- read_xpt("sdtm/suppae.xpt")
ex <- read_xpt("sdtm/ex.xpt")

#Converting blanks to NA if there are any blanks present in these data sets

adsl <- convert_blanks_to_na(adsl)
ae <- convert_blanks_to_na(ae)
suppae <- convert_blanks_to_na(suppae)
ex <- convert_blanks_to_na(ex)

# Importing specifications

adae_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables")  %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))  %>%
  filter(dataset == 'ADAE') %>%
  filter(!variable %in% c('ADAE')) %>%
  mutate(
    type = if_else(type == 'text','character', 'numeric')
  )

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


# Get list of ADSL vars required for ADAE data set

adsl_vars <- vars(STUDYID, SITEID, USUBJID, TRT01A, TRT01AN, AGE
                  ,RACE, SEX, SAFFL, TRTSDT, TRTEDT)


## Deriving the Analysis Start and End Date (ASTDT, AENDT)
#Creating astdt



astdt <- derive_vars_dt(
  ae,
  new_vars_prefix = "AST",
  AESTDTC,
  highest_imputation = "D",
  date_imputation = "first",
  flag_imputation = "auto",

)


#Creating aendt
aendt <- derive_vars_dt(
  ae,
  new_vars_prefix = "AEN",
  AEENDTC,
  highest_imputation = "n",
  date_imputation = "last", ##Check: use "last" option
  flag_imputation = "auto",

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
    AENDT = aendt$AENDT,
    RACEN = format_racer1n(RACE),
    AGEGR1 = format_agegr1(AGE),
    AGEGR1N = format_agegr1n(AGEGR1)
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


adae$ADURU <- ifelse(!is.na(adae$ASTDTF),NA,adae$ADURU)
adae$ADURN <- ifelse(!is.na(adae$ASTDTF),NA,adae$ADURN)
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

adae$TRTEMFL <- ifelse (is.na(adae$ASTDT), NA, adae$TRTEMFL)



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

#`%!in%` <- Negate(`%in%`)


adae$CQ01NAM <- ifelse ((grepl("APPLICATION*|DERMATITIS|ERYTHEMA|BLISTER", adae$AEDECOD) |
                           (adae$AEBODSYS %in% c('SKIN AND SUBCUTANEOUS TISSUE DISORDERS')))&
                          (adae$AEDECOD %nin% c('COLD SWEAT', 'HYPERHIDROSIS', 'ALOPECIA'))
                        ,"DERMATOLOGIC EVENTS", NA)




#Deriving 1st Occurrence 01 Flag for CQ01 (AOCC01FL)

adae <-  restrict_derivation(
  dataset = adae,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(USUBJID),
    order = vars(USUBJID,ASTDT,AESEQ),
    new_var = AOCC01FL,
    mode = "first"

  ),
  filter = (CQ01NAM == "DERMATOLOGIC EVENTS" & TRTEMFL == "Y" )
) %>% mutate(
  TRTA = TRT01A,
  TRTAN = TRT01AN
) %>%
  select(adae_spec$variable) %>%
  xportr_type(adae_spec, "ADAE") %>%
  xportr_label(adae_spec, "ADAE") %>%
  xportr_format(adae_spec, "ADAE") %>%
  xportr_length(adae_spec, "ADAE")


adae <- adae %>% mutate(
  TRTSDT = as.Date(TRTSDT, origin = "1970-01-01"),
  TRTEDT = as.Date(TRTEDT, origin = "1970-01-01"),
  ASTDT = as.Date(ASTDT, origin = "1970-01-01"),
  AENDT = as.Date(AENDT, origin = "1970-01-01")
) %>% xportr_write("adam/adae.xpt", label = "Adverse Events Analysis Dataset")




