# Name: ADLBHY
#
# Label: Analysis Dataset Lab Hy's Law
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
adlbhy_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables")  %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))  %>%
  filter(dataset == 'ADLBHY') %>%
  mutate(
    type = if_else(type == 'text','character', 'numeric')
  )

# Handle missing values

lb <- convert_blanks_to_na(lb)
supplb <- convert_blanks_to_na(supplb)


adsl_vars <- vars(TRTSDT, TRTEDT, SUBJID)

adlbhy_dt_dy <- lb %>%
  filter(LBTESTCD == "ALT" | LBTESTCD == "AST" | LBTESTCD == "BILI") %>%
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
  1,		"Alanine Aminotransferase (U/L)",
  2,		"Aspartate Aminotransferase (U/L)",
  3,		"Bilirubin (umol/L)"
)



## Add PARAMCD and PARAM, Calculate AVAL and AVALC
adlbhy_paramcd <- adlbhy_dt_dy %>%
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
    CRIT1 = case_when(
      LBTESTCD == "ALT" ~ "R2A1HI > 1.5",
      LBTESTCD == "AST" ~ "R2A1HI > 1.5",
      LBTESTCD == "BILI" ~ "R2A1HI > 1.5"
    ),
    CRIT1FL = case_when(
      LBTESTCD == "ALT" & AVAL>1.5*LBSTNRHI ~ "Y",
      LBTESTCD == "AST" & AVAL>1.5*LBSTNRHI ~ "Y",
      LBTESTCD == "BILI" & AVAL>1.5*LBSTNRHI ~ "Y",
      !is.na(AVAL) ~ "N"
    ),
    CRIT1FN = as.numeric(factor(CRIT1FL))-1
    ) %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = vars(PARAMN),
    by_vars = vars(PARAM),
    print_not_mapped = TRUE,
    check_type = "warning"
  ) %>%
  derive_var_anrind()


## Get visit info

adlbhy_visit <- adlbhy_paramcd %>%
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
adlbhy_ablfl <- adlbhy_visit %>%
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
adlbhy_base_chg <- adlbhy_ablfl %>%
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
  # Calculate BNRIND
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = ANRIND,
    new_var = BNRIND
  )

NULL_data <- adlbhy_base_chg %>%
  select(USUBJID, PARAMCD, AVISIT, AVAL) %>%
  filter(is.na(AVAL)) %>%
  mutate(
    PARAMCD = "BILIHY",
    Miss = "missing"
  )

adlbhy_paramtyp <- adlbhy_base_chg %>%
  mutate(
    AVAL2 = AVAL,
    AVAL = as.numeric(gsub("[^0-9.]+", "", AVALC)) / A1HI
  ) %>%
  derive_param_computed(
    parameters = c("BILI"),
    by_vars = vars(STUDYID, USUBJID, SUBJID, TRTSDT,TRTEDT, ABLFL, AVISIT, AVISITN),
    analysis_value = AVAL.BILI>1.5,
    set_values_to = vars(
      PARAMCD = "BILIHY",
      PARAMN = 4,
      PARAM = "Bilirubin 1.5 x ULN",
      PARAMTYP = "DERIVED",
      PARCAT1 = "HYLAW"
    ),
    filter = !is.na(AVISIT)
  ) %>%
  derive_param_computed(
    parameters = c("ALT","AST"),
    by_vars = vars(STUDYID, USUBJID, SUBJID,TRTSDT,TRTEDT, ABLFL, AVISIT, AVISITN),
    analysis_value = AVAL.ALT>1.5 | AVAL.AST>1.5,
    set_values_to = vars(
      PARAMCD = "TRANSHY",
      PARAMN = 5,
      PARAM = "Transaminase 1.5 x ULN",
      PARAMTYP = "DERIVED",
      PARCAT1 = "HYLAW"
    ),
    filter = !is.na(AVISIT)
  ) %>%
  derive_param_computed(
    parameters = c("ALT","AST","BILI"),
    by_vars = vars(STUDYID, USUBJID, SUBJID,TRTSDT,TRTEDT, ABLFL, AVISIT, AVISITN),
    analysis_value = (AVAL.ALT>1.5 | AVAL.AST>1.5) & AVAL.BILI>1.5,
    set_values_to = vars(
      PARAMCD = "HYLAW",
      PARAMN = 6,
      PARAM = "Total Bili 1.5 x ULN and Transaminase 1.5 x ULN",
      PARAMTYP = "DERIVED",
      PARCAT1 = "HYLAW"
    ),
    filter = !is.na(AVISIT)
  )

merged_adlbhy_paramtyp <- merge(adlbhy_paramtyp,
                                NULL_data, by = c("USUBJID", "PARAMCD", "AVISIT"),
                                all.x=TRUE, all.y=TRUE) %>%
  mutate(
    AVAL = case_when(
      Miss == "missing" ~ NA_real_,
      PARAMTYP == "DERIVED" ~ AVAL.x,
      TRUE ~ AVAL2
    )
  ) %>%
  # Calculate BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE
  )%>%
  mutate(
    BASE = case_when(
      PARAMTYP == "DERIVED" & is.na(AVAL) ~ NA_real_,
      TRUE ~ BASE
    ),
    SHIFT1 = case_when(
      PARAMTYP != "DERIVED" ~ "",
      BASE == 0 & AVAL == 0 ~ "Normal to Normal",
      BASE == 0 & AVAL == 1 ~ "Normal to High",
      BASE == 1 & AVAL == 0 ~ "High to Normal",
      BASE == 1 & AVAL == 1 ~ "",
      TRUE ~ ""
    ),
    SHIFT1N = case_when(
      PARAMTYP != "DERIVED" ~ NA_real_,
      BASE == 0 & AVAL == 0 ~ 1,
      BASE == 0 & AVAL == 1 ~ 2,
      BASE == 1 & AVAL == 0 ~ 0,
      BASE == 1 & AVAL == 1 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


# Add all ADSL variables, apply spec properties, create xpt ----
adlbhy <- merged_adlbhy_paramtyp %>%
  filter(!is.na(AVISIT) & AVISITN < 26) %>%
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
  select(adlbhy_spec$variable) %>%
  xportr_type(adlbhy_spec, "ADLBHY") %>%
  xportr_label(adlbhy_spec, "ADLBHY") %>%
  mutate(
    ADT = ADT + 3653,
    TRTEDT = TRTEDT + 3653,
    TRTSDT = TRTSDT + 3653
  ) %>%
  xportr_format(adlbhy_spec, "ADLBHY") %>%
  xportr_length(adlbhy_spec, "ADLBHY") %>%
  xportr_write("adam/adlbhy.xpt", label = "Analysis Dataset Lab Hy's Law")


# message("This is a custom message that I use to remind myslef.")
# warning("This is a custom warning that I use to remind myslef.")

# Run log command just in console not in the script.
#logrx::axecute(file = "/programs/adlbhy.R", to_report = c("messages"))









