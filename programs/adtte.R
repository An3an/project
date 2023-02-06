#Import packages needed
pckgs<-c("admiral","admiral.test","dplyer","lubridate","stringr","haven","tidyr","metacore","metatools","xportr")
install.packages(pckgs)
library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
library(haven)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)

adsl <- read_xpt("adam/adsl.xpt")

#Need ADAE complete to program, using admiral SDTM.AE in the mean time

data("admiral_ae")
ae<-admiral_ae
