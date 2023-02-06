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
    x <=65 ~ "<65",
    between(x, 65, 80) ~ "65-80",
    x > 80 ~ ">85",
    TRUE ~ "Missing"
  )
}

#AGEGR1N

format_agegr1n <- function(x) {
  case_when(
    x = "<65" ~ 1,
    x = "65-80" ~ 2,
    x = ">85" ~ 3,
    TRUE ~ "Missing"
  )
}



#racer1n

format_racer1n <- function(x) {
  case_when(
    x == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
    x == "ASIAN" ~ 2,
    x == "BLACK OR AFRICAN AMERICAN" ~ 3,
    x == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
    x == "WHITE" ~ 6,
    TRUE ~ "Missing"
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
  date_imputation = "first",
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
      ASTDY = dy$ASTDY,
      AENDT = aendt$AENDT,
      AENDY =dy$AENDY
    )



#Deriving analysis start and end day (ASTDY, AENDY)

dy <- derive_vars_dy(adae, reference_date = TRTSDT,
                     source_vars = vars(ASTDT, AENDT, TRTSDT))


adae %>% mutate(
  ASTDY = dy$ASTDY,
  AENDY =dy$AENDY
)


#Deriving AE duration and AE duration units (ADURN, ADURU)

adae <- derive_vars_duration(
  adae,
  new_var = ADURN,
  new_var_unit = ADURNU,
  start_date = ASTDT,
  end_date = AENDT,
  in_unit = "days",
  out_unit = "days",
  floor_in = TRUE,
  add_one = TRUE,
  trunc_out = FALSE
)


# Derive treatment emergent analysis flag (TRTEMFL)

treatmenrEmergentFlag <- derive_var_trtemfl(
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





xportr_write(adae, "adam/adae.xpt")
