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
  filter(LBCAT == "CHEMISTRY" & VISIT != 'AMBUL ECG REMOVAL' & VISIT != 'RETRIEVAL') %>%
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


# Assign PARAMCD, PARAM, and PARAMN

param_lookup <- tibble::tribble(
  ~PARAMN, ~PARAM,
  18,		"Sodium (mmol/L)",
  19,		"Potassium (mmol/L)",
  20,		"Chloride (mmol/L)",
  21,		"Bilirubin (umol/L)",
  22,		"Alkaline Phosphatase (U/L)",
  23,		"Gamma Glutamyl Transferase (U/L)",
  24,		"Alanine Aminotransferase (U/L)",
  25,		"Aspartate Aminotransferase (U/L)",
  26,		"Blood Urea Nitrogen (mmol/L)",
  27,		"Creatinine (umol/L)",
  28,		"Urate (umol/L)",
  29,		"Phosphate (mmol/L)",
  30,		"Calcium (mmol/L)",
  31,		"Glucose (mmol/L)",
  32,		"Protein (g/L)",
  33,		"Albumin (g/L)",
  34,		"Cholesterol (mmol/L)",
  35,		"Creatine Kinase (U/L)"
)



## Add PARAMCD and PARAM, Calculate AVAL and AVALC
adlbc_paramcd <- adlbc_dt_dy %>%
mutate(
  PARAM = paste(LBTEST," (", LBSTRESU,")", sep =''),
  PARAMCD = LBTESTCD,
  PARCAT1 = "CHEM",
  AVAL = LBSTRESN,
  AVALC = LBSTRESC,
  A1HI = LBSTNRHI,
  A1LO = LBSTNRLO,
  R2A1HI = AVAL / A1HI,
  R2A1LO = AVAL / A1LO,
  ANRLO = LBSTNRLO,
  ANRHI = LBSTNRHI,
  ALBTRVAL = pmax((1.5 * A1HI) - LBSTRESN, LBSTRESN - (0.5 * A1LO)),
) %>%
derive_vars_merged_lookup(
  dataset_add = param_lookup,
  new_vars = vars(PARAMN),
  by_vars = vars(PARAM),
  print_not_mapped = TRUE,
  check_type = "warning"
)

adlbc_anrind <- adlbc_paramcd %>%
  derive_var_anrind() %>%
  mutate(
    ANRIND = case_when(
      AVAL > 1.5*A1HI ~ 'H',
      AVAL <0.5*A1LO ~ 'L',
      AVAL>=0.5*A1LO & AVAL<=1.5*A1HI ~ 'N',
      is.na(AVAL) ~ 'N',
      TRUE ~ NA
    )
  )

## Get visit info

adlbc_visit <- adlbc_anrind %>%
  mutate(
    AVISIT = case_when(
      LBBLFL == "Y" ~ "Baseline",
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = as.numeric(case_when(
      LBBLFL == "Y" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", "")),
      TRUE ~ NA_character_
    ))
  )

# Derive baseline flags ----
adlbc_ablfl <- adlbc_visit %>%
  mutate(
    ABLFL = LBBLFL
  ) %>%
  # Last value in treatment visit
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(STUDYID, USUBJID, PARAMCD),
      order = vars(VISITNUM, ADT),
      new_var = AENTMTFL,
      mode = "last"
    ),
    filter = (AVISITN>0 & AVISITN<=24)
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
  mutate(
    CHG = case_when(
      ABLFL=="Y" ~ NA,
      TRUE ~ AVAL - BASE
    )
    )%>%
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
  ) %>%
  #Derive analysis flag
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(STUDYID, USUBJID, PARAMCD),
      order = vars(ALBTRVAL, desc(ADY)),
      new_var = ANL01FL,
      mode = "last"
  ),
  filter = (!is.na(ALBTRVAL) & is.na(ABLFL) & AVISITN<26)
)

adlbc_visit_end <- adlbc_base_chg %>%
  filter(!is.na(AVISITN)) %>%
  filter(ADT>=TRTSDT,AVISITN < 26, AVISITN >= 2) %>%
  group_by(USUBJID, PARAMCD) %>%
  slice_max(ADY) %>%
  # Derive Timing
  mutate(
    AVISIT ="End of Treatment",
    AVISITN = 99
  ) %>%
  ungroup()

# Add all ADSL variables, apply spec properties, create xpt ----
adlbc <- union(adlbc_base_chg,adlbc_visit_end) %>%
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
    AGEGR1N = format_agegr1n(AGEGR1),
    ADT = as.numeric(ADT) + 3653,
    TRTEDT = as.numeric(TRTEDT) + 3653,
    TRTSDT = as.numeric(TRTSDT) + 3653
  ) %>%
  select(adlbc_spec$variable) %>%
  #xportr_type(adlbc_spec, "ADLBC") %>%
  xportr_label(adlbc_spec, "ADLBC") %>%
  #xportr_format(adlbc_spec, "ADLBC") %>%
  xportr_length(adlbc_spec, "ADLBC") %>%
  xportr_write("adam/adlbc.xpt", label = "Analysis Dataset Lab Blood Chemistry")










