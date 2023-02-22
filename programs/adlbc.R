# Name: ADLBC
#
# Label: Analysis Dataset Lab Blood Chemistry
#
# Input: adsl, lb, supplb

library(admiral)
#library(admiral.test) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(haven)



lb <- read_xpt("sdtm/lb.xpt")
supplb <- read_xpt("sdtm/supplb.xpt")
adsl <- read_xpt("adam/adsl.xpt")

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
    x == "ASIAN" ~ 5,
    x == "BLACK OR AFRICAN AMERICAN" ~ 2,
    x == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 4,
    x == "WHITE" ~ 1
    #TRUE ~ NA ##Note, missing values
  )
}


# Read in prepared spec file for ADLBC ----
adlbc_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables")  %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))  %>%
  filter(dataset == 'ADLBC') %>%
  mutate(
    type = if_else(type == 'text','character', 'numeric')
  )

# Handle missing values

lb <- convert_blanks_to_na(lb)
supplb <- convert_blanks_to_na(supplb)


adsl_vars <- vars(TRTSDT, TRTEDT)

adlbc_dt_dy <- lb %>%
  filter(LBCAT == "CHEMISTRY") %>%
  # Join ADSL with LB (need TRTSDT for ADY derivation)
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  ## Calculate ADT, ADY ----
derive_vars_dt(
  new_vars_prefix = "A",
  dtc = LBDTC
) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = vars(ADT))


## Add PARAMCD and PARAM, Calculate AVAL and AVALC
adlbc_paramcd <- adlbc_dt_dy %>%
mutate(
  PARAM = paste(LBTEST, LBSTRESU, sep = " "),
  PARAMCD = LBTESTCD,
  PARAMN = match(PARAM, unique(PARAM)),
  PARCAT1 = "CHEM",
  AVAL = LBSTRESN,
  AVALC = LBSTRESC,
  A1HI = LBSTNRHI,
  A1LO = LBSTNRLO,
  R2A1HI = AVAL / A1HI,
  R2A1LO = AVAL / A1LO,
  ANRLO = LBSTNRLO,
  ANRHI = LBSTNRHI,
  ALBTRVAL = pmax(LBSTRESN - (1.5 * A1HI), (0.5 * A1LO) - LBSTRESN),
)

adlbc_anrind <- adlbc_paramcd %>%
  derive_var_anrind()


## Get visit info

adlbc_visit <- adlbc_anrind %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", "")),
      TRUE ~ NA_character_
    ))
  )

# Derive baseline flags ----
adlbc_ablfl <- adlbc_visit %>%
  mutate(
    ANL01FL = if_else(is.na(AVISITN),NA_character_, 'Y'),
    ABLFL = LBBLFL
  )

## Derive baseline information ----
adlbc_base_chg <- adlbc_ablfl %>%
  # Calculate BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE
  ) %>%
  # Calculate BASEC
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = AVALC,
    new_var = BASEC
  ) %>%
  # Calculate CHG
  derive_var_chg() %>%
  # Calculate PCHG
  derive_var_pchg() %>%
  # Calculate BNRIND
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = ANRIND,
    new_var = BNRIND
  ) %>%
  # Calculate BR2A1LO
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = R2A1LO,
    new_var = BR2A1LO
  ) %>%
  # Calculate BR2A1HI
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = R2A1HI,
    new_var = BR2A1HI
  )


# Add all ADSL variables, apply spec properties, create xpt ----
adlbc <- adlbc_base_chg %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = vars(STUDYID, USUBJID)) %>%
  mutate(
    TRTA = TRT01A,
    TRTAN = TRT01AN,
    TRTP = TRT01P,
    TRTPN = TRT01PN,
    RACEN = format_racer1n(RACE),
    AGEGR1 = format_agegr1(AGE),
    AGEGR1N = format_agegr1n(AGEGR1)
  ) %>%
  select(adlbc_spec$variable) %>%
  #xportr_type(adlbc_spec, "ADLB") %>%
  xportr_label(adlbc_spec, "ADLB") %>%
  xportr_format(adlbc_spec, "ADLB") %>%
  xportr_length(adlbc_spec, "ADLB") %>%
  xportr_write("adam/adlbc.xpt", label = "Analysis Dataset Lab Blood Chemistry")








