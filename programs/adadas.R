library(metacore)
library(metatools)
library(admiral.test)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


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


dm <- read_xpt("sdtm/dm.xpt")
qs <- read_xpt("sdtm/qs.xpt")
adsl <- read_xpt("adam/adsl.xpt")

qs <- convert_blanks_to_na(qs)


# Read in prepared spec file for ADVS ----
adadas_spec_2 <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables")  %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))  %>%
  filter(dataset == 'ADADAS') %>%
  mutate(type = if_else(type == 'text','character', 'numeric'))




adadas_spec <- spec_to_metacore("metadata/specs.xlsx", where_sep_sheet = FALSE) %>%
               select_dataset('ADADAS')


adadas_preds <- build_from_derived(adadas_spec,
                                 ds_list = list("DM" = dm, "ADSL" = adsl, "QS" = qs),
                                 predecessor_only = TRUE, keep = TRUE)


adadas1 <- adadas_preds %>%
  derive_vars_merged(
    dataset_add = qs,
    new_vars = vars(QSDTC, QSSTRESN, QSTEST),
    by_vars = vars(STUDYID, USUBJID, QSSEQ)
  ) %>%
  filter(
    PARAMCD %in% c(str_c("ACITM", str_pad(1:14,2, pad = "0")), "ACTOT")
  ) %>%
  # ADT
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = QSDTC
  ) %>%
  # ADY
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = vars(ADT)
  )

adadas2 <- adadas1 %>%
  mutate(
    AVISIT = case_when(
      (ADY <= 1) ~ "Baseline",
      (ADY >= 2 & ADY <= 84) ~ "Week 8",
      (ADY >= 85 & ADY <= 140) ~ "Week 16",
      (ADY > 140) ~ "Week 24",
      TRUE ~ NA_character_
    ),
    AVAL = QSSTRESN,
    PARAM = QSTEST %>% str_to_title(),
    PARAMN = as.integer(factor(PARAM, levels = unique(PARAM)))
  ) %>%
  create_var_from_codelist(adadas_spec, AVISIT, AVISITN)


adadas3 <- adadas2 %>%
  # Calculate BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE
  )


actot_expected <- tibble::tribble(
  ~PARAMCD, ~AVISITN, ~AVISIT,
  "ACTOT", 0, "Baseline",
  "ACTOT", 8, "Week 8",
  "ACTOT", 16, "Week 16",
  "ACTOT", 24, "Week 24",
)

data_locf <- derive_locf_records(
  data = adadas3,
  dataset_expected_obs = actot_expected,
  by_vars = vars(STUDYID, USUBJID, PARAMCD),
  order = vars(AVISITN, AVISIT)
) %>%
  arrange(STUDYID, USUBJID, PARAMCD, AVISITN, AVISIT)

data_locf_2 <- data_locf %>%
  group_by(STUDYID, USUBJID, PARAMCD) %>%
  arrange(STUDYID, USUBJID, PARAMCD, AVISITN, AVISIT) %>%
  fill(names(data_locf)[-which(names(data_locf) %in% c("QSBLFL", "ABLFL", "AVAL", "DTYPE"))],
       .direction = "down")  %>%
  ungroup()


aw_lookup <- tibble::tribble(
  ~AVISIT, ~AWRANGE, ~AWTARGET, ~AWLO, ~AWHI,
  "Baseline", "<=1", 1, NA_integer_, 1,
  "Week 8", "2-84", 56, 2, 84,
  "Week 16", "85-140", 112, 85, 140,
  "Week 24", ">140", 168, 141, NA_integer_
)


adadas4 <- derive_vars_merged(
  data_locf_2,
  dataset_add = aw_lookup,
  by_vars = vars(AVISIT)
) %>%
  mutate(
    AWTDIFF = abs(AWTARGET - ADY),
    AWU = "DAYS"
  )



adadas5 <- adadas4 %>%
  mutate(diff = AWTARGET - ADY) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, PARAMCD, AVISIT),
      order = vars(AWTDIFF, diff),
      new_var = ANL01FL,
      mode = "first"
    ),
    filter = !is.na(AVISIT)
  )

adadas <- adadas5 %>%
  drop_unspec_vars(adadas_spec) %>%
  xportr_type(adadas_spec_2, "ADADAS") %>%
  mutate(
    ADT = as.Date(ADT, origin = "1970-01-01"),
    TRTEDT = as.Date(TRTEDT, origin = "1970-01-01"),
    TRTSDT = as.Date(TRTSDT, origin = "1970-01-01"),
    RACEN = format_racer1n(RACE),
    CHG = if_else(!is.na(ABLFL), NA_real_, AVAL - BASE),
    PCHG = if_else(!is.na(ABLFL), NA_real_, (AVAL - BASE)/BASE*100)
  ) %>%
  order_cols(adadas_spec) %>%
  xportr_label(adadas_spec_2, "ADADAS") %>%
  xportr_format(adadas_spec_2, "ADADAS") %>%
  xportr_write("adam/adadas.xpt", label = "ADAS-Cog Analysis")







