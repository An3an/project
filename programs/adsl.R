library(metacore)
library(metatools)
library(admiral.test)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)




format_reason <- function(x) {
  case_when(
      x == "ADVERSE EVENT"  ~  "Adverse Event",
      x == "STUDY TERMINATED BY SPONSOR" ~ "Sponsor Decision",
      x == "DEATH" ~ "Death",
      x == "WITHDRAWAL BY SUBJECT" ~ "Withdrew Consent",
      x == "PHYSICIAN DECISION" ~ "Physician Decision",
      x == "PROTOCOL VIOLATION" ~ "Protocol Violation",
      x == "LOST TO FOLLOW-UP" ~ "Lost to Follow-up",
      x == "LACK OF EFFICACY" ~ "Lack of Efficacy",
      x == "PROTOCOL VIOLATION" ~ "I/E Not Met",
      TRUE ~ NA_character_
  )
}




#Randomized doses

format_dose <- function(x) {
  case_when(
    x == 'Placebo' ~ 0,
    x == 'Xanomeline High Dose' ~ 81,
    x == 'Xanomeline Low Dose' ~ 54
  )
}


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
    x == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
    x == "ASIAN" ~ 4,
    x == "BLACK OR AFRICAN AMERICAN" ~ 3,
    x == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
    x == "WHITE" ~ 6
    #TRUE ~ NA ##Note, missing values
  )
}


dm <- read_xpt("sdtm/dm.xpt")
mh <- read_xpt("sdtm/mh.xpt")
ex <- read_xpt("sdtm/ex.xpt")
ds <- read_xpt("sdtm/ds.xpt")
qs <- read_xpt("sdtm/qs.xpt")
sv <- read_xpt("sdtm/sv.xpt")
vs <- read_xpt("sdtm/vs.xpt")
sc <- read_xpt("sdtm/sc.xpt")

# Read in prepared spec file for ADLBC ----
adsl_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables")  %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))  %>%
  filter(dataset == 'ADSL') %>%
  mutate(
    type = if_else(type == 'text','character', 'numeric')
  )



adsl_1 <- dm %>%
  mutate(TRT01P = ARM, TRT01A = ARM)


ex_ext <- ex %>%
  derive_vars_dt(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dt(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN"
  )

adsl_2 <- adsl_1 %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDT),
    new_vars = vars(TRTSDT = EXSTDT),
    order = vars(EXSTDT, EXSEQ),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))),
    new_vars = vars(TRTEDT = EXENDT),
    order = vars(EXENDT, EXSEQ),
    mode = "last",
    by_vars = vars(STUDYID, USUBJID)
  )


ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

adsl_3 <- adsl_2 %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(DCDECOD = DSDECOD),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )


adsl_5 <- adsl_3 %>%
  derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC
  )

ex_CUMDOSE <- ex %>% left_join(dm, by = "USUBJID") %>%
                     mutate(
                       EXENDY = case_when(
                         is.na(EXENDY) ~ as.numeric(as.Date(RFENDTC)-as.Date(EXSTDTC)) + EXSTDY,
                         TRUE ~ EXENDY
                       )
                     ) %>%
                     group_by(USUBJID)  %>%
                     summarise(CUMDOSE = sum((EXENDY - EXSTDY + 1)*EXDOSE))


qs_efffl <- qs %>% filter(QSTESTCD == 'CIBIC' | QSTESTCD == 'ACTOT', !is.na(QSORRES), VISITNUM>3)  %>%
                   select(USUBJID, QSTESTCD, QSSTRESN) %>%
                   pivot_wider(names_from = QSTESTCD, values_from = QSSTRESN,values_fn = list) %>%
                   mutate(
                      EFFFL = if_else(CIBIC!="NULL" & ACTOT!="NULL","Y","N")
                   )


qs_MMSETOT <- qs %>% filter(QSCAT == "MINI-MENTAL STATE") %>%
                     group_by(USUBJID) %>%
                     summarise(MMSETOT = sum(QSSTRESN))


sv_max <- sv %>% left_join(dm, by = "USUBJID") %>%
          filter(RFENDTC >= SVSTDTC & VISITNUM < 13) %>%
          group_by(USUBJID) %>% summarise(MAXVISIT = max(VISITNUM))

sv_vis1 <- sv %>% filter(VISITNUM == 1) %>% select(USUBJID, SVSTDTC)  %>%
           mutate(
             VISIT1DT = as.Date(SVSTDTC)
           )


vs_blfl <- vs %>% filter((VISITNUM == 1 & VSTESTCD == "HEIGHT") |  (VISITNUM == 3 & VSTESTCD == "WEIGHT")) %>%
           select(USUBJID, VSTESTCD, VSSTRESN) %>%
           pivot_wider(names_from = VSTESTCD, values_from = VSSTRESN) %>%
           mutate(
             WEIGHTBL = round(WEIGHT, digit = 1),
             HEIGHTBL = round(HEIGHT, digit = 1),
             BMIBL = round(WEIGHTBL / ((HEIGHTBL/100)**2), digit = 1),
             BMIBLGR1 = case_when(
               BMIBL <25 & BMIBL > 0 ~ "<25",
               BMIBL <30 & BMIBL >= 25 ~ "25-<30",
               BMIBL >=30 ~ ">=30",
             )
           )

sc_EDLEVEL <- sc %>% filter(SCTESTCD == "EDLEVEL") %>% select(USUBJID, SCSTRESN) %>%
              mutate(
                EDUCLVL = SCSTRESN
              )

ds_VISNUMEN <- ds %>% filter(DSCAT == "DISPOSITION EVENT") %>%  select(USUBJID, VISITNUM, DSTERM, DSDECOD) %>%
                 mutate(
                   VISNUMEN = if_else(VISITNUM==13,12,VISITNUM)
                 )

mh_DISONSDT <- mh %>% filter(MHTERM == "ALZHEIMER'S DISEASE") %>% select(USUBJID, MHSTDTC) %>%
               mutate(
                 DISONSDT = as.Date(MHSTDTC)
               )

# Add all ADSL variables, apply spec properties, create xpt ----
adsl <- adsl_5 %>%
        left_join(sv_max, by = "USUBJID") %>%
        left_join(qs_efffl, by = "USUBJID") %>%
        left_join(vs_blfl, by = "USUBJID") %>%
        left_join(sc_EDLEVEL, by = "USUBJID") %>%
        left_join(sv_vis1, by = "USUBJID") %>%
        left_join(ds_VISNUMEN, by = "USUBJID") %>%
        left_join(qs_MMSETOT, by = "USUBJID") %>%
        left_join(ex_CUMDOSE, by = "USUBJID") %>%
        left_join(mh_DISONSDT, by = "USUBJID") %>%
  filter(ARM != "Screen Failure") %>%
  mutate(
    RFENDT = as.Date(RFENDTC),
    TRTEDT = if_else(is.na(TRTEDT), as.Date(RFENDTC),as.Date(TRTEDT)),
    TRTDURD = as.numeric(TRTEDT - TRTSDT + 1),
    RACEN = format_racer1n(RACE),
    AGEGR1 = format_agegr1(AGE),
    AGEGR1N = format_agegr1n(AGEGR1),
    TRT01PN = format_dose(TRT01P),
    TRT01AN = format_dose(TRT01A),
    SITEGR1 = case_when(
      SITEID %in% c("715", "717", "702", "706", "707", "711", "714") ~ "900",
      TRUE ~ SITEID
    ),
    AVGDD= round(CUMDOSE/TRTDURD, digit = 1),
    EFFFL = case_when(EFFFL == "Y" ~ "Y",
                      TRUE ~ "N"),
    ITTFL = ifelse( is.na(ARMCD), "N", "Y"),
    SAFFL = ifelse(ITTFL == "Y" & !is.na(TRTSDT), "Y", "N"),
    COMP16FL = ifelse(MAXVISIT >= 10, "Y", "N"),
    COMP24FL = ifelse(MAXVISIT >= 12, "Y", "N"),
    COMP8FL = ifelse(MAXVISIT >= 8, "Y", "N"),
    DCSREAS = case_when(
      DSTERM != "PROTOCOL ENTRY CRITERIA NOT MET" ~ format_reason(DSDECOD),
      TRUE ~ "I/E Not Met"
    ),
    DISCONFL = ifelse(DSDECOD != "COMPLETED", "Y", NA_character_),
    EOSSTT = ifelse(DSDECOD != "COMPLETED", "DISCONTINUED", "COMPLETED"),
    DSRAEFL = ifelse(DCSREAS == "Adverse Event", "Y", NA_character_),
    DURDIS = compute_duration(
      as.Date(DISONSDT, origin = "1970-01-01"),
      as.Date(VISIT1DT, origin = "1970-01-01"),
      in_unit = "days",
      out_unit = "months",
      floor_in = TRUE,
      add_one = TRUE,
      trunc_out = FALSE
    ),
    DURDIS = round(DURDIS, digit = 1),
    DURDSGR1 = case_when(
      DURDIS >= 12 ~ ">=12",
      DURDIS < 12  ~ "<12"
      )
  )%>%
  select(adsl_spec$variable) %>%
  #xportr_type(adsl_spec, "ADSL") %>%
  xportr_label(adsl_spec, "ADSL") %>%
  #xportr_format(adsl_spec, "ADSL") %>%
  xportr_length(adsl_spec, "ADSL") %>%
  xportr_write("adam/adsl.xpt", label = "Subject-Level Analysis Dataset")


