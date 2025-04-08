setwd("C:/Users/mkim255/Pre_diabetes_project_2025")

library(haven)
library(dplyr)
library(lubridate)
library(tidyr)
library(survival)
library(ggsurvfit)
library(patchwork)
library(geepack)
library(nnet)
library(data.table)

demo <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/patient.csv")
ow <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/ow.csv")
encounter <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/encounter.csv")
geog <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/geogsum.csv")
arv <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/arv_cleaned.csv")
med <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/commed.csv")
diab <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/diab_summary.csv")
htn <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/ht_summary.csv")
lipid <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/lipid_summary.csv")
egfr <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/ckd_long.csv")
ckd <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/ckd_summary.csv")
vl <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/vlsum.csv")
a1c <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/renalanddiabetes.csv")
bmi <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/bmisum.csv")


#DM observational window file
ow.dm <- ow[, c("NAID", "DMSTART_DT", "DMSTOP_DT")]
ow.dm$DMSTART_DT <- as.Date(ow.dm$DMSTART_DT)
ow.dm$DMSTOP_DT <- as.Date(ow.dm$DMSTOP_DT)

saveRDS(ow.dm, "ow.dm.rds")

ow.a1c <- ow[, c("NAID", "HGA1CSTART_DT", "HGA1CSTOP_DT")]
ow.a1c$HGA1CSTART_DT <- as.Date(ow.a1c$HGA1CSTART_DT)
ow.a1c$HGA1CSTOP_DT <- as.Date(ow.a1c$HGA1CSTOP_DT)

saveRDS(ow.a1c, "ow.a1c.rds")

#Demographics file
demo.edit <- demo

demo.edit$DEATHMONTH = ifelse(!is.na(demo.edit$DEATHYEAR) & is.na(demo.edit$DEATHMONTH), 1, demo.edit$DEATHMONTH)
demo.edit$DEATHDAY = ifelse(!is.na(demo.edit$DEATHYEAR) & !is.na(demo.edit$DEATHMONTH) & is.na(demo.edit$DEATHDAY), 1, demo.edit$DEATHDAY)
demo.edit$enroll.date = as.Date(paste(demo.edit$ENROLLYEAR, demo.edit$ENROLLMONTH, demo.edit$ENROLLDAY, sep = "-"), "%Y-%m-%d")
demo.edit$death.date = as.Date(paste(demo.edit$DEATHYEAR, demo.edit$DEATHMONTH, demo.edit$DEATHDAY, sep = "-"), "%Y-%m-%d")

demo.edit <- demo.edit %>%
  mutate(race.cat = case_when(RACE == 5 ~ "white",
                              RACE == 2 ~ "black",
                              RACE == 1 | RACE == 3 | RACE == 4 | RACE == 6 ~"other",
                              RACE == 7 ~ "missing"
  )
  )

demo.edit$COHORT_OPENDATE = as.Date(demo.edit$COHORT_OPENDATE)
demo.edit$COHORT_CLOSEDATE = as.Date(demo.edit$COHORT_CLOSEDATE)

demo.cleaned <- demo.edit[, c("NAID", "COHORT_OPENDATE", "COHORT_CLOSEDATE", "enroll.date", "death.date", "YOB", "BIRTHSEX", "race.cat", "HISPANIC")]
demo.cleaned$death.date <- ifelse(is.na(demo.cleaned$death.date), Inf, demo.cleaned$death.date)

saveRDS(demo.cleaned, "demo.cleaned.rds")

#Diabetes file
dm <- diab[diab$DM==1, c("NAID", "DM_DT")]
dm$DM_DT <- as.Date(dm$DM_DT, format = "%Y-%m-%d")
dm <- dm[!is.na(dm$DM_DT), ]

saveRDS(dm, "dm.rds") # NAID & DM_DT

#A1c file
a1c.edit <- a1c[!is.na(a1c$DAY) & !is.na(a1c$MONTH) & !is.na(a1c$YEAR) & !is.na(a1c$RESULT), ]
a1c.edit$a1c.date <- as.Date(paste(a1c.edit$YEAR, a1c.edit$MONTH, a1c.edit$DAY, sep = "-"), "%Y-%m-%d")
a1c.high <- a1c.edit[a1c.edit$RESULT >= 6.5 & a1c.edit$YEAR >= 2007, c("NAID", "a1c.date", "RESULT")]


##First high a1c confirmed within 1 year period
a1c.high.confirmed <- a1c.high %>%
  group_by(NAID) %>%
  arrange(NAID, a1c.date) %>%
  mutate(diff = lead(a1c.date) - a1c.date) %>%
  mutate(diff = ifelse(is.na(diff), Inf, diff)) %>%
  filter(diff <= 365) %>%
  slice(1) %>%
  rename(a1c.high.date.confirmed=a1c.date) %>%
  select(NAID, a1c.high.date.confirmed)

saveRDS(a1c.high.confirmed, "a1c.high.confirmed.rds")


#ART file
arv.cleaned <- arv[!is.na(arv$STARTYEAR) & !is.na(arv$STARTMONTH) & !is.na(arv$STARTDAY) & !is.na(arv$STOPYEAR) & !is.na(arv$STOPMONTH) & !is.na(arv$STOPDAY), ]

arv.cleaned$STARTDAY <- ifelse(is.na(arv.cleaned$STARTDAY), 15, arv.cleaned$STARTDAY)
arv.cleaned$STOPDAY <- ifelse(is.na(arv.cleaned$STOPDAY), 15, arv.cleaned$STOPDAY)

arv.cleaned$startdate <- as.Date(paste(arv.cleaned$STARTYEAR,arv.cleaned$STARTMONTH, arv.cleaned$STARTDAY, sep = "-"), "%Y-%m-%d")
arv.cleaned$stopdate <- as.Date(paste(arv.cleaned$STOPYEAR, arv.cleaned$STOPMONTH, arv.cleaned$STOPDAY, sep = "-"), "%Y-%m-%d")
arv.cleaned <- arv.cleaned[, c("NAID", "CLASS", "MEDNAME", "startdate", "stopdate")]

rx.nnrti <- arv.cleaned[arv.cleaned$CLASS == 7 & arv.cleaned$MEDNAME!="HX_NNRTI_TREATMENT_MED_UNKNOWN", ]
rx.pi <- arv.cleaned[arv.cleaned$CLASS == 10 & arv.cleaned$MEDNAME!="HX_PI_TREATMENT_MED_UNKNOWN", ]

rx.bic <- arv.cleaned[arv.cleaned$MEDNAME == "BICTEGRAVIR", ]
rx.dtg <- arv.cleaned[arv.cleaned$MEDNAME == "DOLUTEGRAVIR", ]
rx.evg <- arv.cleaned[arv.cleaned$MEDNAME == "ELVITEGRAVIR", ]
rx.ral <- arv.cleaned[arv.cleaned$MEDNAME == "RALTEGRAVIR", ]

rx.tdf <- arv.cleaned[arv.cleaned$MEDNAME == "TENOFOVIR", ]
rx.taf <- arv.cleaned[arv.cleaned$MEDNAME == "TENOFOVIR_TAF", ]

#Function to link ART Rx with Rx gap threshold
collapse_rx <- function(data, id_col, startdate_col, stopdate_col, rx_gap_threshold) {
  require(dplyr)
  
  collapsed_data <- data %>%
    arrange({{id_col}}, {{startdate_col}}, {{stopdate_col}}) %>%
    #Define rx.gap, create flag for collapsible groups
    mutate(rx.gap = {{startdate_col}} - lag({{stopdate_col}}),
           collapse.flag = ifelse(is.na(rx.gap) | rx.gap <= rx_gap_threshold, FALSE, TRUE),
           collapse.group = cumsum(collapse.flag)) %>%
    group_by({{id_col}}, collapse.group) %>%
    summarise(startdate=min({{startdate_col}}), stopdate=max({{stopdate_col}}), .groups="drop")
  
  return(collapsed_data)
}

rx.nnrti.col <- collapse_rx(rx.nnrti, NAID, startdate, stopdate, 30)
rx.pi.col <- collapse_rx(rx.pi, NAID, startdate, stopdate, 30)

rx.bic.col <- collapse_rx(rx.bic, NAID, startdate, stopdate, 30)
rx.dtg.col <- collapse_rx(rx.dtg, NAID, startdate, stopdate, 30)
rx.evg.col <- collapse_rx(rx.evg, NAID, startdate, stopdate, 30)
rx.ral.col <- collapse_rx(rx.ral, NAID, startdate, stopdate, 30)

rx.tdf.col <- collapse_rx(rx.tdf, NAID, startdate, stopdate, 30)
rx.taf.col <- collapse_rx(rx.taf, NAID, startdate, stopdate, 30)

saveRDS(rx.tdf.col, "rx.tdf.col.rds")
saveRDS(rx.taf.col, "rx.taf.col.rds")

rx.nnrti.col <- rx.nnrti.col %>%
  mutate(nnrti=1, pi=0) %>%
  select(-collapse.group)

rx.pi.col <- rx.pi.col %>%
  mutate(nnrti=0, pi=1) %>%
  select(-collapse.group)

rx.bic.col <- rx.bic.col %>%
  mutate(bic=1, dtg=0, evg=0, ral=0) %>%
  select(-collapse.group)

rx.dtg.col <- rx.dtg.col %>%
  mutate(bic=0, dtg=1, evg=0, ral=0) %>%
  select(-collapse.group)

rx.evg.col <- rx.evg.col %>%
  mutate(bic=0, dtg=0, evg=1, ral=0) %>%
  select(-collapse.group)

rx.ral.col <- rx.ral.col %>%
  mutate(bic=0, dtg=0, evg=0, ral=1) %>%
  select(-collapse.group)

rx.insti.all <- do.call("rbind", list(rx.bic.col, rx.dtg.col, rx.evg.col, rx.ral.col))
rx.noninsti.all <- rbind(rx.nnrti.col, rx.pi.col)

rx.insti.cleaned <- rx.insti.all %>%
  rename(rx.insti.startdate = startdate, rx.insti.stopdate = stopdate) %>%
  mutate(rx.insti.duration = rx.insti.stopdate - rx.insti.startdate) %>%
  filter(rx.insti.duration >= 30) %>%
  group_by(NAID, rx.insti.startdate) %>%
  arrange(NAID, rx.insti.startdate, rx.insti.stopdate) %>%
  slice(n()) %>%
  ungroup()

rx.noninsti.cleaned <- rx.noninsti.all %>%
  rename(rx.noninsti.startdate = startdate, rx.noninsti.stopdate = stopdate) %>%
  mutate(rx.noninsti.duration = rx.noninsti.stopdate - rx.noninsti.startdate) %>%
  filter(rx.noninsti.duration >= 30) %>%
  group_by(NAID, rx.noninsti.startdate) %>%
  arrange(NAID, rx.noninsti.startdate, rx.noninsti.stopdate) %>%
  slice(n()) %>%
  ungroup()

saveRDS(rx.insti.cleaned, "rx.insti.cleaned.rds")
saveRDS(rx.noninsti.cleaned, "rx.noninsti.cleaned.rds")

#Comorbidities file
##Treated hypertension (NA-ACCORD definition)
treated.htn <- htn[, c("NAID", "HTDX", "HTDX_DT", "HTMED", "HTMED_DT")]
treated.htn <- treated.htn[!is.na(treated.htn$HTDX) & !is.na(treated.htn$HTMED), ]
treated.htn <- treated.htn[treated.htn$HTDX == 1 & treated.htn$HTMED == 1, ]

treated.htn$htn.date <- pmax(treated.htn$HTDX_DT, treated.htn$HTMED_DT)
treated.htn <- treated.htn[, c("NAID", "htn.date")]

treated.htn$htn.date <- as.Date(treated.htn$htn.date, format = "%Y-%m-%d")

treated.htn <- treated.htn[!is.na(treated.htn$htn.date), ]

##Treated hyperlipidemia (need to edit)
treated.hld <- lipid[lipid$LIPIDMED==1, ]
treated.hld <- treated.hld[, c("NAID", "LIPIDMED_DT")]

treated.hld$LIPIDMED_DT <- as.Date(treated.hld$LIPIDMED_DT, format = "%Y-%m-%d")
treated.hld <- treated.hld[!is.na(treated.hld$LIPIDMED_DT), ]

colnames(treated.hld) <- c("NAID", "hld.date")

head(lipid)

##CKD
ckd.cleaned <- ckd[ckd$CKD60_DT !="", c("NAID", "CKD60_DT")]
ckd.cleaned$CKD60_DT <- as.Date(ckd.cleaned$CKD60_DT)
ckd.cleaned <- ckd.cleaned[ckd.cleaned$CKD60_DT >= as.Date("2000-01-01"), ]

#Comedications files
med.cleaned <- med[!is.na(med$STARTMONTH) & !is.na(med$STARTYEAR) & !is.na(med$STOPMONTH) & !is.na(med$STOPYEAR), ]

med.cleaned$STARTDAY <- ifelse(is.na(med.cleaned$STARTDAY), 15, med.cleaned$STARTDAY)
med.cleaned$STOPDAY <- ifelse(is.na(med.cleaned$STOPDAY), 15, med.cleaned$STOPDAY)

med.cleaned$startdate <- as.Date(paste(med.cleaned$STARTYEAR, med.cleaned$STARTMONTH, med.cleaned$STARTDAY, sep = "-"), "%Y-%m-%d")
med.cleaned$stopdate = as.Date(paste(med.cleaned$STOPYEAR, med.cleaned$STOPMONTH, med.cleaned$STOPDAY, sep = "-"), "%Y-%m-%d")

med.cleaned <- med.cleaned[, c("NAID", "MED_GROUP", "MEDICATIONNAME", "startdate", "stopdate")]

##rx.ac = antithrombotic agents (antiplatelet and anticoagulant)
rx.ac <- med.cleaned[med.cleaned$MED_GROUP==5, ]

rx.mh <- med.cleaned[med.cleaned$MED_GROUP==20, ]
rx.ad <- rx.mh[grepl("AMITRIPTYLINE|AMOXAPINE|Antidepressant|BUPROPION|CITALOPRAM|CLOMIPRAMINE|DESIPRAMINE|DESVENLAFAXINE|
                     DOXEPIN|DULOXETINE|ESCITALOPRAM|FLUOXETINE|FLUVOXAMINE|IMIPRAMINE|ISOCARBOXAZID|LEVOMILNACIPRAN|LOXAPINE|MAPROTILINE|
                     MILNACIPRAN|MIRTAZAPINE|MOCLOBEMIDE|NEFAZODONE|NORTRIPTYLINE|PAROXETINE|PROTRIPTYLINE|SERTRALINE|TRANYLCYPROMINE|
                     TRAZODONE|TRIMIPRAMINE|VENLAFAXINE|VILAZODONE|VORTIOXETINE", rx.mh$MEDICATIONNAME), ]

rx.ap <- rx.mh[grepl("Anti-psychotic|ARIPIPRAZOLE|ASENAPINE|BREXPIPRAZOLE|CARIPRAZINE|CHLORPROMAZINE|CLOZAPINE|FLUPHENAZINE|HALOPERIDOL|
                    LURASIDONE|METHOTRIMEPRAZINE|MOLIDONE|OLANZAPINE|PALIPERIDONE|PERPHENAZINE|PIMOZIDE|PROCHLORPERAZINE|RISPERIDONE|
                    THIORIDAZINE|THIOTHIXENE|TRIFLUOPERAZINE|ZIPRASIDONE|ZUCLOPHENTHIXOL", rx.mh$MEDICATIONNAME), ]

rx.opioid <- med.cleaned[med.cleaned$MED_GROUP==25, ]

#VL
vl.cleaned <- vl[!is.na(vl$VL_D) & !is.na(vl$VL_M) & !is.na(vl$VL_Y) & vl$VL_Y >= 2006 & !is.na(vl$VLOAD), ]
vl.cleaned$vl.date <- as.Date(paste(vl.cleaned$VL_Y, vl.cleaned$VL_M, vl.cleaned$VL_D, sep = "-"), "%Y-%m-%d")
vl.cleaned <- vl.cleaned[, c("NAID", "vl.date", "VLOAD")]

#Weight and BMI
weight.cleaned <- bmi[!is.na(bmi$WEIGHT_KG) & !is.na(bmi$YEAR) & bmi$YEAR >= 2006 & !is.na(bmi$MONTH) & !is.na(bmi$DAY), ]
weight.cleaned$weight.date <- as.Date(paste(weight.cleaned$YEAR, weight.cleaned$MONTH, weight.cleaned$DAY, sep = "-"), "%Y-%m-%d")
weight.cleaned <- weight.cleaned[, c("NAID", "WEIGHT_KG", "BMI", "weight.date")]

bmi.cleaned <- weight.cleaned[!is.na(weight.cleaned$BMI), ]


saveRDS(treated.htn, "treated.htn.rds")
saveRDS(treated.hld, "treated.hld.rds")
saveRDS(ckd.cleaned, "ckd.cleaned.rds")
saveRDS(rx.ac, "rx.ac.rds")
saveRDS(rx.ad, "rx.ad.rds")
saveRDS(rx.ap, "rx.ap.rds")
saveRDS(rx.opioid, "rx.opioid.rds")
saveRDS(vl.cleaned, "vl.cleaned.rds")
saveRDS(weight.cleaned, "weight.cleaned.rds")
saveRDS(bmi.cleaned, "bmi.cleaned.rds")


















