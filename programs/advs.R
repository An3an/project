# Name: ADVS
#
# Label: Vital Signs Analysis Dataset
#
# Input: adsl, vs

library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)
library(haven)



vs <- read_xpt("sdtm/vs.xpt")
adsl <- read_xpt("adam/adsl.xpt")


# Read in prepared spec file for ADVS ----
advs_spec <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Variables")  %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))  %>%
  filter(dataset == 'ADVS') %>%
  filter(!variable %in% c('ADVS','AGEGR1', 'AGEGR1N', 'RACEN')) %>%
  mutate(
    type = if_else(type == 'text','character', 'numeric')
  )

# Handle missing values

vs <- convert_blanks_to_na(vs)



# Assign PARAMCD, PARAM, and PARAMN

param_lookup <- tibble::tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "SYSBP", "SYSBP", "Systolic Blood Pressure (mmHg)", 1,
  "DIABP", "DIABP", "Diastolic Blood Pressure (mmHg)", 2,
  "PULSE", "PULSE", "Pulse Rate (beats/min)", 3,
  "WEIGHT", "WEIGHT", "Weight (kg)", 4,
  "HEIGHT", "HEIGHT", "Height (cm)", 5,
  "TEMP", "TEMP", "Temperature (C)", 6
)



# Derivations ----

adsl_vars <- vars(TRTSDT, TRTEDT)

advs_dt_dy <- vs %>%
  # Join ADSL with VS (need TRTSDT for ADY derivation)
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  ## Calculate ADT, ADY ----
derive_vars_dt(
  new_vars_prefix = "A",
  dtc = VSDTC
) %>%
derive_vars_dy(reference_date = TRTSDT, source_vars = vars(ADT))

## Add PARAMCD and PARAM
advs_paramcd <- advs_dt_dy %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = vars(PARAMCD, PARAM, PARAMN),
    by_vars = vars(VSTESTCD),
    print_not_mapped = TRUE,
    check_type = "warning"
  ) %>%
  ## Calculate AVAL and AVALC ----
mutate(
  AVAL = VSSTRESN,
  AVALC = VSSTRESC
)


## Get visit info

advs_visit <- advs_paramcd %>%
  # Derive Timing
  mutate(
    ATPTN = VSTPTNUM,
    ATPT = VSTPT,
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
advs_btype <- advs_visit %>%
  ## Calculate BASETYPE ----
derive_var_basetype(
  basetypes = rlang::exprs(
    "LAST: AFTER LYING DOWN FOR 5 MINUTES" = ATPTN == 815,
    "LAST: AFTER STANDING FOR 1 MINUTE" = ATPTN == 816,
    "LAST: AFTER STANDING FOR 3 MINUTES" = ATPTN == 817,
    "LAST" = is.na(ATPTN)
  )
)


advs_ablfl <- advs_btype %>%
mutate(
  ANL01FL = if_else(is.na(AVISITN),NA_character_, 'Y'),
  ABLFL = VSBLFL
)

## Derive baseline information ----
advs_base_chg <- advs_ablfl %>%
  # Calculate BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = AVAL,
    new_var = BASE
  ) %>%
  # Calculate BASEC
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = AVALC,
    new_var = BASEC
  ) %>%
  # Calculate CHG
  derive_var_chg() %>%
  # Calculate PCHG
  derive_var_pchg()


# Add all ADSL variables, apply spec properties, create xpt ----
advs <- advs_base_chg %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = vars(STUDYID, USUBJID)) %>%
  mutate(
    TRTA = TRT01A,
    TRTAN = TRT01AN,
    TRTP = TRT01P,
    TRTPN = TRT01PN
  ) %>%
  select(advs_spec$variable) %>%
  xportr_type(advs_spec, "ADVS") %>%
  xportr_label(advs_spec, "ADVS") %>%
  xportr_format(advs_spec, "ADVS") %>%
  xportr_length(advs_spec, "ADVS") %>%
  xportr_write("adam/advs.xpt", label = "Vital Signs Analysis")


