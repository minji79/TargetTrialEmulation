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
library(ggeasy)

#Data managed files
ow.dm <- readRDS("ow.dm.rds")
demo.cleaned <- readRDS("demo.cleaned.rds")

dm <- readRDS("dm.rds")
a1c.high.confirmed <- readRDS("a1c.high.confirmed.rds")
a1c <- read.csv("C:/Users/mkim255/Pre_diabetes_project_2025/renalanddiabetes.csv")

rx.insti.cleaned <- readRDS("rx.insti.cleaned.rds")
rx.noninsti.cleaned <- readRDS("rx.noninsti.cleaned.rds")
rx.tdf.col <- readRDS("rx.tdf.col.rds")
rx.taf.col <- readRDS("rx.taf.col.rds")

treated.htn <- readRDS("treated.htn.rds")
treated.hld <- readRDS("treated.hld.rds")
ckd.cleaned <- readRDS("ckd.cleaned.rds")

rx.ac <- readRDS("rx.ac.rds")
rx.ad <- readRDS("rx.ad.rds")
rx.ap <- readRDS("rx.ap.rds")
rx.opioid <- readRDS("rx.opioid.rds")

vl.cleaned <- readRDS("vl.cleaned.rds")
weight.cleaned <- readRDS("weight.cleaned.rds")
bmi.cleaned <- readRDS("bmi.cleaned.rds")

#Analytic dataset
enc.cleaned <- readRDS("enc.cleaned.rds")
s1 <- readRDS("s1.rds")
s1.pre.confirmed <- readRDS("s1.pre.confirmed.rds")
s4 <- readRDS("s4.rds")
s5 <- readRDS("s5.rds")
ac1 <- readRDS("ac1.rds")

cw1 <- readRDS("cw1.rds")

#Prepare encounter file
enc <- encounter[encounter$ENCOUNTERYR >= 2007 & !is.na(encounter$ENCOUNTERMTH) & !is.na(encounter$ENCOUNTERDAY) & encounter$ENCOUNTERTYPE==1, ]
enc$encounterdate <- as.Date(paste(enc$ENCOUNTERYR, enc$ENCOUNTERMTH, enc$ENCOUNTERDAY, sep = "-"), "%Y-%m-%d")
enc <- enc[, c("NAID", "ENCOUNTERYR", "encounterdate")]

#Keep encounterdates between DM observation period
enc.ow <- left_join(enc, ow.dm, by = "NAID")
enc.ow <- enc.ow[!is.na(enc.ow$DMSTART_DT) & !is.na(enc.ow$DMSTOP_DT), ]
enc.ow <- enc.ow[enc.ow$encounterdate >= enc.ow$DMSTART_DT + 365 & enc.ow$encounterdate < enc.ow$DMSTOP_DT, ]

#Merge with demographic file
#Keep encounterdates between pmax(COHORT_OPENDATE & enroll.date) and pmin(COHORT_CLOSEDATE and death.date)
enc.demo <- left_join(enc.ow, demo.cleaned, by= "NAID")
enc.demo <- enc.demo[enc.demo$encounterdate >= (pmax(enc.demo$COHORT_OPENDATE, enc.demo$enroll.date) + 365) & enc.demo$encounterdate < (pmin(enc.demo$COHORT_CLOSEDATE, enc.demo$death.date)) , ]

enc.demo <- enc.demo %>%
  mutate(age = ENCOUNTERYR - YOB) %>%
  select(-YOB)

##Identify the the last.encounter.date per observation (last visit prior to >365 days of gap)
enc.cleaned <- enc.demo %>%
  group_by(NAID) %>%
  arrange(NAID, encounterdate) %>%
  mutate(date.difference = encounterdate - lag(encounterdate)) %>%
  mutate(date.difference = ifelse(row_number() == 1, 0, date.difference)) %>%
  mutate(flag = ifelse(date.difference > 365, 1, 0)) %>%
  mutate(cum.flag = cumsum(flag)) %>%
  filter(cum.flag == 0) %>% 
  select(-flag, -date.difference, -cum.flag) %>%
  mutate(last.encounter.date = last(encounterdate)) %>%
  filter(encounterdate < last.encounter.date) %>%
  ungroup()

##Remove within 30-day visits
enc.cleaned <- enc.cleaned %>%
  group_by(NAID) %>%
  arrange(NAID, encounterdate) %>%
  mutate(diff = encounterdate - lag(encounterdate)) %>%
  filter(is.na(diff) | diff >= 30) %>%
  select(-diff) %>%
  ungroup()

enc.cleaned$enc.num <- c(1:nrow(enc.cleaned))

saveRDS(enc.cleaned, "enc.cleaned.rds")

str(enc.cleaned)

#Cohort building
s1 <- enc.cleaned %>%
  select(-DMSTART_DT, -COHORT_OPENDATE, -enroll.date)

#Merge with DM (NA-ACCORD definition) data
s1.dt <- setDT(s1)

##Exclude if DM present prior to encounterdate
s1.dt[, dm.hx :=0]
s1.dt[dm, on = .(NAID),
      dm.hx := fifelse(DM_DT <= encounterdate, 1, dm.hx),
      by = .EACHI]

s1.dt <- s1.dt[s1.dt$dm.hx==0, ] %>%
  select(-dm.hx)

s1 <- setDF(s1.dt)

##DM outcome date on the first date DM is present after the encounterdate
s1$dm.out.date=sapply(1:nrow(s1), function(i){
  date=s1$encounterdate[i]
  id.mask=dm$NAID==s1$NAID[i]
  dates.for.id=dm$DM_DT[id.mask]
  dates.in.range.for.id.mask=dates.for.id > date
  dates.in.range.for.id=dates.for.id[dates.in.range.for.id.mask]
  if(length(dates.in.range.for.id) >= 1){
    return(min(dates.in.range.for.id))
  } else {
    return(Inf)
  }
})

s1$dm.out <- ifelse(s1$dm.out.date!=Inf, 1, 0)

#Merge with A1c (two values ≥6.5 within 1 year)
s1$a1c.high.date.confirmed=sapply(1:nrow(s1), function(i){
  date=s1$encounterdate[i]
  id.mask=a1c.high.confirmed$NAID==s1$NAID[i]
  dates.for.id=a1c.high.confirmed$a1c.high.date.confirmed[id.mask]
  dates.in.range.for.id.mask=dates.for.id > date
  dates.in.range.for.id=dates.for.id[dates.in.range.for.id.mask]
  if(length(dates.in.range.for.id) >= 1){
    return(min(dates.in.range.for.id))
  } else {
    return(Inf)
  }
})

s1$a1c.high.confirmed <- ifelse(s1$a1c.high.date.confirmed!=Inf, 1, 0)

saveRDS(s1, "s1.rds")

## Define pre-diabetes patients
s1.pre2 <- setDF(s1)

## a1c.edit
a1c.edit <- a1c[!is.na(a1c$DAY) & !is.na(a1c$MONTH) & !is.na(a1c$YEAR) & !is.na(a1c$RESULT), ]
a1c.edit$a1c.date <- as.Date(paste(a1c.edit$YEAR, a1c.edit$MONTH, a1c.edit$DAY, sep = "-"), "%Y-%m-%d")
a1c.edit <- a1c.edit %>% mutate(a1c.date = as.numeric(a1c.date))

#Merge with the closest A1C date prior to the encounter date
a1c.assessment.window = 365

s1.pre$a1c.date=sapply(1:nrow(s1.pre), function(i){
  date=s1.pre$encounterdate[i]
  id.mask=a1c.edit$NAID==s1.pre$NAID[i]
  dates.for.id=a1c.edit$a1c.date[id.mask]
  dates.in.range.for.id.mask=(date - a1c.assessment.window <= dates.for.id) & (dates.for.id < date + 14)
  dates.in.range.for.id=dates.for.id[dates.in.range.for.id.mask]
  if(length(dates.in.range.for.id) >= 1){
    return(max(dates.in.range.for.id))
  } else {
    return(Inf)
  }
})


#Merge with the closest A1C value prior to the encounter date
s1.pre <- s1.pre %>% left_join(a1c.edit %>% select(NAID, RESULT, a1c.date), by=c("NAID", "a1c.date"))
s1.pre <- s1.pre %>% rename(a1c.close.date = a1c.date, a1c.close.value = RESULT)

saveRDS(s1.pre, "s1.pre.rds")

#Identify pre-dm patients
s1.pre.confirmed <- s1.pre %>% mutate(pre.dm=ifelse(a1c.close.value >= 5.7 & a1c.close.value < 6.5, 1, 0))
s1.pre.confirmed <- s1.pre.confirmed %>% filter(pre.dm ==1)

saveRDS(s1.pre.confirmed, "s1.pre.confirmed.rds")


#Merging NNRTI & PI Rx information data
##Evidence of non-INSTI Rx ≥180 days prior to encounterdate
rx.noninsti.cleaned.180 <- rx.noninsti.cleaned[rx.noninsti.cleaned$rx.noninsti.duration >=180, ]

rx.noninsti.cleaned.180$rx.noninsti.startdate <- as.numeric(rx.noninsti.cleaned.180$rx.noninsti.startdate)
rx.noninsti.cleaned.180$rx.noninsti.stopdate <- as.numeric(rx.noninsti.cleaned.180$rx.noninsti.stopdate)

s2 <- s1.pre.confirmed
s2$encounterdate <- as.numeric(s2$encounterdate)

##NNRTI & PI presence date cutoff respect to encounterdate
rx.noninsti.startdate.cutoff = 180
rx.noninsti.stopdate.cutoff = -14

s2$rx.noninsti.startdate=sapply(1:nrow(s2), function(i){
  date=s2$encounterdate[i]
  id.mask=rx.noninsti.cleaned.180$NAID==s2$NAID[i]
  dates.for.id=rx.noninsti.cleaned.180$rx.noninsti.startdate[id.mask]
  dates.in.range.for.id.mask=dates.for.id <= (date - rx.noninsti.startdate.cutoff)
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  if(length(dates.in.range) >= 1){
    return(max(dates.in.range))
  } else {
    return(NA)
  }
})

s2 <- left_join(s2, rx.noninsti.cleaned.180, by = c("NAID", "rx.noninsti.startdate"))

s3 <- s2[!is.na(s2$rx.noninsti.startdate), ]
s3 <- s3[s3$rx.noninsti.stopdate - s3$encounterdate >= rx.noninsti.stopdate.cutoff, ]

#Identify INSTI initiation at encounterdate (-14 to + 14 days) and categorize main exposure group (insti - 1 or 0)
rx.insti.cleaned$rx.insti.startdate <- as.numeric(rx.insti.cleaned$rx.insti.startdate)
rx.insti.cleaned$rx.insti.stopdate <- as.numeric(rx.insti.cleaned$rx.insti.stopdate)

rx.insti.startdate.cutoff = 14

s3$rx.insti.startdate=sapply(1:nrow(s3), function(i){
  date=s3$encounterdate[i]
  id.mask=rx.insti.cleaned$NAID==s3$NAID[i]
  dates.for.id=rx.insti.cleaned$rx.insti.startdate[id.mask]
  dates.in.range.for.id.mask=dates.for.id >= (date - rx.insti.startdate.cutoff) & dates.for.id <= (date + rx.insti.startdate.cutoff)
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  if(length(dates.in.range) >= 1){
    return(max(dates.in.range))
  } else {
    return(NA)
  }
})

s3 <- left_join(s3, rx.insti.cleaned, by = c("NAID", "rx.insti.startdate"))

s3$insti <- ifelse(!is.na(s3$rx.insti.startdate), 1, 0)

#Remove subsequent visits per id once Rx'ed INSTI
remove_rows_following_first_occurrence <- function(data, variable) {
  output=data
  index=which(data[[variable]]==1)
  if (length(index) >0) {
    output=output[1:index[1], ]
  } else {
    output=output
  }
  return(output)
}

s4 <- s3 %>%
  group_by(NAID) %>%
  arrange(NAID, enc.num) %>%
  do(remove_rows_following_first_occurrence(.,"insti")) %>%
  ungroup()

#Exclude if there is history of INSTI Rx
s4$rx.insti.hx=sapply(1:nrow(s4), function(i){
  date=s4$encounterdate[i]
  id.mask=rx.insti.cleaned$NAID==s4$NAID[i]
  dates.for.id=rx.insti.cleaned$rx.insti.startdate[id.mask]
  dates.in.range.for.id.mask=dates.for.id < (date - rx.insti.startdate.cutoff)
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  if(length(dates.in.range) >= 1){
    return(1)
  } else {
    return(0)
  }
})

s4 <- s4[s4$rx.insti.hx==0, ]

#Remove visits where NNRTI/PI stopdate exceeds INSTI stopdate for visits associated with switching to INSTI
s4$rx.insti.startdate <- ifelse(s4$insti==0, Inf, s4$rx.insti.startdate)
s4$rx.insti.stopdate <- ifelse(s4$insti==0, Inf, s4$rx.insti.stopdate)

s4 <- s4[s4$rx.insti.stopdate >= s4$rx.noninsti.stopdate, ]

#Censor dates based on switch to INSTI dates for visits prior to INSTI switch
s4 <- s4 %>%
  group_by(NAID) %>%
  arrange(NAID, encounterdate) %>%
  mutate(switch.to.insti.date = last(rx.insti.startdate))

s4$switch.to.insti.date <- ifelse(s4$insti==1, Inf, s4$switch.to.insti.date)

#Censor dates based on switch between NNRTI and PI groups after encounterdate
rx.nnrti.cleaned <- rx.noninsti.cleaned[rx.noninsti.cleaned$nnrti==1, ]
rx.pi.cleaned <- rx.noninsti.cleaned[rx.noninsti.cleaned$pi==1, ]

s4$next.rx.nnrti.startdate=sapply(1:nrow(s4), function(i){
  date=s4$encounterdate[i]
  id.mask=rx.nnrti.cleaned$NAID==s4$NAID[i]
  dates.for.id=rx.nnrti.cleaned$rx.noninsti.startdate[id.mask]
  dates.in.range.for.id.mask=dates.for.id > date
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  if(length(dates.in.range) >= 1){
    return(min(dates.in.range))
  } else {
    return(Inf)
  }
})

s4$next.rx.pi.startdate=sapply(1:nrow(s4), function(i){
  date=s4$encounterdate[i]
  id.mask=rx.pi.cleaned$NAID==s4$NAID[i]
  dates.for.id=rx.pi.cleaned$rx.noninsti.startdate[id.mask]
  dates.in.range.for.id.mask=dates.for.id > date
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  if(length(dates.in.range) >= 1){
    return(min(dates.in.range))
  } else {
    return(Inf)
  }
})

s4$switch.to.nnrti.date <- ifelse(s4$nnrti==1, Inf, s4$next.rx.nnrti.startdate)
s4$switch.to.nnrti.date <- ifelse(s4$insti==1, Inf, s4$switch.to.nnrti.date)

s4$switch.to.pi.date <- ifelse(s4$pi==1, Inf, s4$next.rx.pi.startdate)
s4$switch.to.pi.date <- ifelse(s4$insti==1, Inf, s4$switch.to.pi.date)

saveRDS(s4, "s4.rds")

#Comorbidities and comedications
setDT(treated.htn)
setDT(treated.hld)
setDT(ckd.cleaned)

setDT(rx.tdf.col)
setDT(rx.taf.col)

setDT(rx.ac)
setDT(rx.ad)
setDT(rx.ap)
setDT(rx.opioid)

s4.dt <- setDT(s4)

s4.dt[, treated.htn :=0]
s4.dt[treated.htn, on = .(NAID),
      treated.htn := fifelse(htn.date <= encounterdate, 1, treated.htn),
      by = .EACHI]

s4.dt[, treated.hld :=0]
s4.dt[treated.hld, on = .(NAID),
      treated.hld := fifelse(hld.date <= encounterdate, 1, treated.hld),
      by = .EACHI]

s4.dt[, ckd60 :=0]
s4.dt[ckd.cleaned, on = .(NAID),
      ckd60 := fifelse(CKD60_DT <= encounterdate, 1, ckd60),
      by = .EACHI]

s4.dt[, rx.ac :=0]
s4.dt[rx.ac, on = .(NAID),
      rx.ac := fifelse(startdate <= encounterdate & stopdate >= encounterdate, 1, rx.ac),
      by = .EACHI]

s4.dt[, rx.ad :=0]
s4.dt[rx.ad, on = .(NAID),
      rx.ad := fifelse(startdate <= encounterdate & stopdate >= encounterdate, 1, rx.ad),
      by = .EACHI]

s4.dt[, rx.ap :=0]
s4.dt[rx.ap, on = .(NAID),
      rx.ap := fifelse(startdate <= encounterdate & stopdate >= encounterdate, 1, rx.ap),
      by = .EACHI]

s4.dt[, rx.opioid :=0]
s4.dt[rx.opioid, on = .(NAID),
      rx.opioid := fifelse(startdate <= encounterdate & stopdate >= encounterdate, 1, rx.opioid),
      by = .EACHI]

s4.dt[, pre.tdf :=0]
s4.dt[rx.tdf.col, on = .(NAID),
      pre.tdf := fifelse(startdate <= (encounterdate - 14) & (encounterdate - 180) <= stopdate, 1, pre.tdf),
      by = .EACHI]

s4.dt[, pre.taf :=0]
s4.dt[rx.taf.col, on = .(NAID),
      pre.taf := fifelse(startdate <= (encounterdate - 14) & (encounterdate - 180) <= stopdate, 1, pre.taf),
      by = .EACHI]

s4.dt[, post.tdf :=0]
s4.dt[rx.tdf.col, on = .(NAID),
      post.tdf := fifelse(startdate <= (encounterdate + 180) & (encounterdate + 14) <= stopdate, 1, post.tdf),
      by = .EACHI]

s4.dt[, post.taf :=0]
s4.dt[rx.taf.col, on = .(NAID),
      post.taf := fifelse(startdate <= (encounterdate + 180) & (encounterdate + 14) <= stopdate, 1, post.taf),
      by = .EACHI]

s4 <- s4.dt

s4$no.taf.to.yes.taf <- ifelse(s4$pre.taf==0 & s4$post.taf==1, 1, 0)
s4$yes.taf.to.yes.taf <- ifelse(s4$pre.taf==1 & s4$post.taf==1, 1, 0)
s4$yes.taf.to.no.taf <- ifelse(s4$pre.taf==1 & s4$post.taf==0, 1, 0)
s4$no.taf.to.no.taf <- ifelse(s4$pre.taf==0 & s4$post.taf==0, 1, 0)

s4$no.tdf.to.yes.tdf <- ifelse(s4$pre.tdf==0 & s4$post.tdf==1, 1, 0)
s4$yes.tdf.to.yes.tdf <- ifelse(s4$pre.tdf==1 & s4$post.tdf==1, 1, 0)
s4$yes.tdf.to.no.tdf <- ifelse(s4$pre.tdf==1 & s4$post.tdf==0, 1, 0)
s4$no.tdf.to.no.tdf <- ifelse(s4$pre.tdf==0 & s4$post.tdf==0, 1, 0)

s5 <- s4

#VL, weight, BMI
vl.lookback.period=365
weight.lookback.period=365

s5.id = unique(s5$NAID)

#VL
vl.cleaned.by.id = lapply(s5.id, function(id) {
  id.mask=vl.cleaned$NAID==id
  vl.cleaned[id.mask, ,drop=FALSE]
})
names(vl.cleaned.by.id) = s5.id

s5$vl.suppressed=sapply(1:nrow(s5), function(i){
  date=s5$encounterdate[i]
  id=as.character(s5$NAID[i])
  dates.for.id=vl.cleaned.by.id[[id]]$vl.date
  dates.in.range.for.id.mask=dates.for.id <= date & dates.for.id >= (date - vl.lookback.period)
  if(!any(dates.in.range.for.id.mask))
    return(FALSE)
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  vls.in.range=vl.cleaned.by.id[[id]]$VLOAD[dates.in.range.for.id.mask] <=200
  max.date.mask=dates.in.range==max(dates.in.range)
  vls.in.range[max.date.mask][1]
})

#Weight
weight.cleaned.by.id = lapply(s5.id, function(id) {
  id.mask=weight.cleaned$NAID==id
  weight.cleaned[id.mask, ,drop=FALSE]
})
names(weight.cleaned.by.id) = s5.id

s5$weight=sapply(1:nrow(s5), function(i){
  date=s5$encounterdate[i]
  id=as.character(s5$NAID[i])
  dates.for.id=weight.cleaned.by.id[[id]]$weight.date
  dates.in.range.for.id.mask=dates.for.id <= date & dates.for.id >= (date - weight.lookback.period)
  if(!any(dates.in.range.for.id.mask))
    return(NA)
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  weights.in.range=weight.cleaned.by.id[[id]]$WEIGHT_KG[dates.in.range.for.id.mask]
  max.date.mask=dates.in.range==max(dates.in.range)
  weights.in.range[max.date.mask][1]
})

#BMI
bmi.cleaned.by.id = lapply(s5.id, function(id) {
  id.mask=bmi.cleaned$NAID==id
  bmi.cleaned[id.mask, ,drop=FALSE]
})
names(bmi.cleaned.by.id) = s5.id

s5$bmi=sapply(1:nrow(s5), function(i){
  date=s5$encounterdate[i]
  id=as.character(s5$NAID[i])
  dates.for.id=bmi.cleaned.by.id[[id]]$weight.date
  dates.in.range.for.id.mask=dates.for.id <= date & dates.for.id >= (date - weight.lookback.period)
  if(!any(dates.in.range.for.id.mask))
    return(NA)
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  bmis.in.range=bmi.cleaned.by.id[[id]]$BMI[dates.in.range.for.id.mask]
  max.date.mask=dates.in.range==max(dates.in.range)
  bmis.in.range[max.date.mask][1]
})

s5 <- s5 %>%
  mutate(bmi.cat = case_when(bmi < 18.5 ~ 3,
                             bmi >= 18.5 & bmi < 25 ~ 0,
                             bmi >= 25 & bmi < 30 ~ 1,
                             bmi >= 30 ~ 2,
                             is.na(bmi) ~ 4
  )
  )

#Length of NNRTI or PI use at encounterdate
s5$rx.noninsti.length.at.encounterdate = s5$encounterdate - s5$rx.noninsti.startdate

#Splines for continuous variables to prepare data for modeling
setDF(s5)

restricted.quadratic.spline <- function(values, knot.quantiles=c(0.2,0.4,0.6,0.8), name='age', na.rm=TRUE)
{
  knots = quantile(values, probs=knot.quantiles, na.rm=na.rm)
  K = length(knots)
  
  rv = data.frame(values)
  for (k in 1:(K-1))
    rv = cbind(rv, pmax(0,values-knots[k])^2-pmax(0,values-knots[K])^2)
  
  names(rv) = attr(rv, 'var_names') = paste0(name, '.sp', 1:K)
  
  rv
}

age.spline <- restricted.quadratic.spline(s5$age)
s5[names(age.spline)] = age.spline

restricted.quadratic.spline <- function(values, knot.quantiles=c(0.2,0.4,0.6,0.8), name='ENCOUNTERYR', na.rm=TRUE)
{
  knots = quantile(values, probs=knot.quantiles, na.rm=na.rm)
  K = length(knots)
  
  rv = data.frame(values)
  for (k in 1:(K-1))
    rv = cbind(rv, pmax(0,values-knots[k])^2-pmax(0,values-knots[K])^2)
  
  names(rv) = attr(rv, 'var_names') = paste0(name, '.sp', 1:K)
  
  rv
}

ENCOUNTERYR.spline <- restricted.quadratic.spline(s5$ENCOUNTERYR)
s5[names(ENCOUNTERYR.spline)] = ENCOUNTERYR.spline

restricted.quadratic.spline <- function(values, knot.quantiles=c(0.2,0.4,0.6,0.8), name='a1c.baseline', na.rm=TRUE)
{
  knots = quantile(values, probs=knot.quantiles, na.rm=na.rm)
  K = length(knots)
  
  rv = data.frame(values)
  for (k in 1:(K-1))
    rv = cbind(rv, pmax(0,values-knots[k])^2-pmax(0,values-knots[K])^2)
  
  names(rv) = attr(rv, 'var_names') = paste0(name, '.sp', 1:K)
  
  rv
}

a1c.baseline.spline <- restricted.quadratic.spline(s5$a1c.close.value)
s5[names(a1c.baseline.spline)] = a1c.baseline.spline

names(s5)


#Remove missing BMI
s5 <- s5[s5$bmi.cat != 4, ]

saveRDS(s5, "s5.rds")

## prevalence of pre-DM from 2007 to 2023
# number of DM-naive population 
s1 %>% select(NAID) %>% distinct() %>% count() # 92450
# number of pre-DM population among 92450
s1.pre.confirmed %>% select(NAID) %>% distinct() %>% count() # 18700 (20.227%)


# number of DM-naive population with eligible visits
s4 %>% select(NAID) %>% distinct() %>% count() # 

# number of pre-DM population among DM-naive population with eligible visits
s5 %>% select(NAID) %>% distinct() %>% count() # 7843

names(s5)

#Set censor date for survival analysis (2016-2023)
ac1 <- s5 %>% filter(ENCOUNTERYR >= 2016)
ac1$DMSTOP_DT <- as.numeric(ac1$DMSTOP_DT)
ac1$COHORT_CLOSEDATE <- as.numeric(ac1$COHORT_CLOSEDATE)

ac1$study.end.date <- as.numeric(as.Date("2022-12-31"))
ac1$five.year.date <- ac1$encounterdate + 365*5

ac1 <- ac1 %>%
  mutate(followup.end.date.for.dm = pmin(dm.out.date, switch.to.insti.date, DMSTOP_DT, COHORT_CLOSEDATE, death.date, last.encounter.date, study.end.date, five.year.date),
         dm.out = ifelse(dm.out.date > followup.end.date.for.dm, 0, dm.out),
         time.for.diabetes = followup.end.date.for.dm - encounterdate,
         
         switched = ifelse(switch.to.insti.date==followup.end.date.for.dm, 1, 0),
         lost = ifelse(last.encounter.date==followup.end.date.for.dm, 1, 0),
         outcome = ifelse(dm.out.date==followup.end.date.for.dm, 1, 0),
         
         followup.end.date.for.a1c = pmin(a1c.high.date.confirmed, switch.to.insti.date, DMSTOP_DT, COHORT_CLOSEDATE, death.date, last.encounter.date, study.end.date, five.year.date),
         a1c.high.confirmed = ifelse(a1c.high.date.confirmed > followup.end.date.for.a1c, 0, a1c.high.confirmed),
         time.for.a1c = followup.end.date.for.a1c - encounterdate,
         
         outcome.a1c = ifelse(a1c.high.date.confirmed==followup.end.date.for.a1c, 1, 0)
  ) %>%
  select(-dm.out.date, -a1c.high.date.confirmed, -switch.to.insti.date, -switch.to.nnrti.date, -switch.to.pi.date, -DMSTOP_DT, -COHORT_CLOSEDATE, -death.date, -last.encounter.date, -study.end.date, -five.year.date, -rx.noninsti.startdate, -rx.noninsti.stopdate, -rx.insti.startdate, -rx.insti.stopdate, -bmi)

##Derive IPTW (INSTI vs non-INSTI) using logistic regression
model <- glm(insti ~ age.sp1 + age.sp2 + age.sp3 + age.sp4 + ENCOUNTERYR.sp1 + ENCOUNTERYR.sp2 + ENCOUNTERYR.sp3 + ENCOUNTERYR.sp4 + BIRTHSEX + race.cat + rx.noninsti.length.at.encounterdate +
               bmi.cat + vl.suppressed + a1c.baseline.sp1 + a1c.baseline.sp2 + a1c.baseline.sp3 + a1c.baseline.sp4 +
               treated.htn + treated.hld + ckd60 + rx.ac + rx.ad + rx.ap + rx.opioid +
               no.taf.to.yes.taf + yes.taf.to.yes.taf + yes.taf.to.no.taf + no.tdf.to.yes.tdf + no.tdf.to.no.tdf + yes.tdf.to.no.tdf,
             data=ac1, family=binomial(link="logit"))

ac1$p.insti <- predict(model, data=ac1, type="response")
marginal.prob.insti = sum(ac1$insti)/nrow(ac1)
ac1$iptw = ifelse(ac1$insti==1, marginal.prob.insti/ac1$p.insti, (1-marginal.prob.insti)/(1-ac1$p.insti))


##Trim weights between the 1st and 99th percentiles
lower.bound <- quantile(ac1$iptw, 0.01)
upper.bound <- quantile(ac1$iptw, 0.99)

ac1$iptw.trimmed <- pmin(pmax(ac1$iptw, lower.bound), upper.bound)

##Diabetes: INSTI vs non-INSTI - weighted Cox
cox_model <- coxph(Surv(ac1$time.for.diabetes, ac1$dm.out) ~insti, data=ac1, cluster=NAID, weights = iptw.trimmed)
summary(cox_model)

##Diabetes: INSTI vs non-INSTI - IPTW incidence rates in person-years (time/365)
N.unique=length(unique(ac1$NAID[ac1$insti==1]))
N.obs=sum(ac1$insti==1)
events.raw=sum(ac1$dm.out[ac1$insti==1])
events.weighted=round(sum(ac1$dm.out[ac1$insti==1]*ac1$iptw.trimmed[ac1$insti==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$insti==1]/365*ac1$iptw.trimmed[ac1$insti==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

N.unique=length(unique(ac1$NAID[ac1$insti==0]))
N.obs=sum(ac1$insti==0)
events.raw=sum(ac1$dm.out[ac1$insti==0])
events.weighted=round(sum(ac1$dm.out[ac1$insti==0]*ac1$iptw.trimmed[ac1$insti==0]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$insti==0]/365*ac1$iptw.trimmed[ac1$insti==0])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

##Confirmed Diabetes: INSTI vs non-INSTI - weighted Cox
cox_model <- coxph(Surv(ac1$time.for.a1c, ac1$a1c.high.confirmed) ~insti, data=ac1, cluster=NAID, weights = iptw.trimmed)
summary(cox_model)


#Derive IPTW (specific INSTI vs non-INSTI) using multinomial regression
ac1$bic <- ifelse(is.na(ac1$bic), 0, ac1$bic)
ac1$dtg <- ifelse(is.na(ac1$dtg), 0, ac1$dtg)
ac1$evg <- ifelse(is.na(ac1$evg), 0, ac1$evg)
ac1$ral <- ifelse(is.na(ac1$ral), 0, ac1$ral)

ac1 <- ac1 %>%
  mutate(insti.specific=ifelse(bic==1, "BIC",
                               ifelse(dtg==1, "DTG",
                                      ifelse(evg==1, "EVG",
                                             ifelse(ral==1, "RAL",
                                                    "NONINSTI"
                                             ))))
  )

ac1$insti.specific <- factor(ac1$insti.specific)
ac1$insti.specific <- relevel(ac1$insti.specific, ref="NONINSTI")

model.insti.specific <- multinom(insti.specific ~ age.sp1 + age.sp2 + age.sp3 + age.sp4 + ENCOUNTERYR.sp1 + ENCOUNTERYR.sp2 + ENCOUNTERYR.sp3 + ENCOUNTERYR.sp4 + BIRTHSEX + race.cat + 
                                   bmi.cat + vl.suppressed + a1c.baseline.sp1 + a1c.baseline.sp2 + a1c.baseline.sp3 + a1c.baseline.sp4 +
                                   treated.htn + treated.hld + ckd60 + rx.ac + rx.ad + rx.ap + rx.opioid +
                                   no.taf.to.yes.taf + yes.taf.to.yes.taf + yes.taf.to.no.taf + no.tdf.to.yes.tdf + no.tdf.to.no.tdf + yes.tdf.to.no.tdf,
                                 data=ac1)
p.insti.specific <- predict(model.insti.specific, data=ac1, type="probs")

ac1$p.insti.spec=sapply(1:dim(ac1)[1], function(i){
  p.insti.specific[i, ac1$insti.specific[i]]
})

model.insti.specific.marginal <- multinom(insti.specific ~ 1, data=ac1)
p.insti.specific.marginal <- predict(model.insti.specific.marginal, data=ac1, type="probs")

ac1$p.insti.spec.marginal=sapply(1:dim(ac1)[1], function(i){
  p.insti.specific.marginal[i, ac1$insti.specific[i]]
})

ac1$iptw.specific = ac1$p.insti.spec.marginal/ac1$p.insti.spec

##Trim weights between the 1st and 99th percentiles
lower.bound <- quantile(ac1$iptw.specific, 0.01)
upper.bound <- quantile(ac1$iptw.specific, 0.99)

ac1$iptw.specific.trimmed <- pmin(pmax(ac1$iptw.specific, lower.bound), upper.bound)

##Diabetes: specific INSTI vs non-INSTI - weighted Cox
cox_model <- coxph(Surv(ac1$time.for.diabetes, ac1$dm.out) ~insti.specific, data=ac1, cluster=NAID, weights = iptw.specific.trimmed)
summary(cox_model)

saveRDS(ac1, "ac1.rds")

##Incidence rates for BIC IPTW
N.unique=length(unique(ac1$NAID[ac1$bic==1]))
N.obs=sum(ac1$bic==1)
events.raw=sum(ac1$dm.out[ac1$bic==1])
events.weighted=round(sum(ac1$dm.out[ac1$bic==1]*ac1$iptw.specific.trimmed[ac1$bic==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$bic==1]/365*ac1$iptw.specific.trimmed[ac1$bic==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

##Incidence rates for DTG IPTW
N.unique=length(unique(ac1$NAID[ac1$dtg==1]))
N.obs=sum(ac1$dtg==1)
events.raw=sum(ac1$dm.out[ac1$dtg==1])
events.weighted=round(sum(ac1$dm.out[ac1$dtg==1]*ac1$iptw.specific.trimmed[ac1$dtg==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$dtg==1]/365*ac1$iptw.specific.trimmed[ac1$dtg==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

##Incidence rates for EVG IPTW
N.unique=length(unique(ac1$NAID[ac1$evg==1]))
N.obs=sum(ac1$evg==1)
events.raw=sum(ac1$dm.out[ac1$evg==1])
events.weighted=round(sum(ac1$dm.out[ac1$evg==1]*ac1$iptw.specific.trimmed[ac1$evg==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$evg==1]/365*ac1$iptw.specific.trimmed[ac1$evg==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

##Incidence rates for RAL IPTW
N.unique=length(unique(ac1$NAID[ac1$ral==1]))
N.obs=sum(ac1$ral==1)
events.raw=sum(ac1$dm.out[ac1$ral==1])
events.weighted=round(sum(ac1$dm.out[ac1$ral==1]*ac1$iptw.specific.trimmed[ac1$ral==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$ral==1]/365*ac1$iptw.specific.trimmed[ac1$ral==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

#Diabetes: INSTI vs non-INSTI - Weighted CI curve
ac1s <- ac1 %>%
  mutate(insti = recode(insti, `0` = "Continued NNRTI or PI", `1` = "Switched to INSTI"))

cicurve.weighted <- survfit2(Surv(ac1s$time.for.diabetes/365, ac1s$dm.out) ~insti, data=ac1s, cluster=NAID, weights=iptw.trimmed) %>%
  ggsurvfit(type="risk", linewidth=1.3) +
  add_confidence_interval() +
  add_risktable(risktable_height = 0.33, size=6, 
                theme = list(theme_risktable_default(axis.text.y.size = 15,
                                                     plot.title.size = 15),
                             theme(plot.title = element_text(face = "bold"))),
                risktable_stats = c("{format(round(n.risk, 0), nsmall = 0)}",
                                    "{format(round(n.event, 0), nsmall = 0)}"),
                stats_label = c("N at risk",
                                "N of events")
  )+
  labs(y="Cumulative Incidence", x="Follow-up time (years)") +
  scale_x_continuous(limits=c(0.0, 5)) +
  scale_y_continuous(limits=c(0.0, 0.65)) +
  theme(legend.position = c(0.10, 0.80)) +
  ggeasy::easy_y_axis_labels_size(size=12) +
  ggeasy::easy_x_axis_labels_size(size=12) +
  ggeasy::easy_y_axis_title_size(size=18) +
  ggeasy::easy_x_axis_title_size(size=18) +
  ggeasy::easy_plot_legend_size(size=12)

cicurve.weighted
ggsave(file="C:/Users/mkim255/Pre_diabetes_project_2025/INSTI DM NA-ACCORD CI Curve IPTW.png", plot = cicurve.weighted, width=16, height=6, units="in")


#Censoring weight analysis for diabetes (main outcome): data management
cw1 <- ac1 %>%
  mutate(number.of.rows.per.enc.num=ceiling(time.for.diabetes/90)) %>%
  slice(rep(1:n(), number.of.rows.per.enc.num)) %>%
  mutate(day.in=0, day.out=90)

cw1 <- cw1 %>%
  group_by(NAID, enc.num) %>%
  mutate(day.in = day.in + (row_number() -1)*90,
         day.out = day.out + (row_number() -1)*90,
         day.out = ifelse(row_number() == n(), time.for.diabetes, day.out)) %>%
  filter(day.out!=day.in) %>%
  mutate(switched = ifelse(row_number() == n(), switched, 0),
         lost = ifelse(row_number() == n(), lost, 0),
         outcome = ifelse(row_number() == n(), outcome, 0)) %>%
  ungroup()

cw1 <- cw1 %>%
  mutate(date.in = encounterdate + day.in) %>%
  mutate(date.out = encounterdate + day.out)

cw1 %>% select(NAID, day.in, day.out, date.in, date.out, switched, lost, outcome) %>% print()

saveRDS(cw1, "cw1.rds")

#Time updated covariates
#Identify unique NAID
cw1.id = unique(cw1$NAID)

#Time updated VL, weight, BMI
vl.lookback.period=365
weight.lookback.period=365

#Updated viral load
vl.cleaned.by.id = lapply(cw1.id, function(id) {
  id.mask=vl.cleaned$NAID==id
  vl.cleaned[id.mask, ,drop=FALSE]
})
names(vl.cleaned.by.id) = cw1.id

cw1$updated.vl.suppressed=sapply(1:nrow(cw1), function(i){
  date=cw1$date.in[i]
  id=as.character(cw1$NAID[i])
  dates.for.id=vl.cleaned.by.id[[id]]$vl.date
  dates.in.range.for.id.mask=dates.for.id <= date & dates.for.id >= (date - vl.lookback.period)
  if(!any(dates.in.range.for.id.mask))
    return(FALSE)
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  vls.in.range=vl.cleaned.by.id[[id]]$VLOAD[dates.in.range.for.id.mask] <=200
  max.date.mask=dates.in.range==max(dates.in.range)
  vls.in.range[max.date.mask][1]
})

#Updated weight
#weight.cleaned.by.id = lapply(cw1.id, function(id) {
#  id.mask=weight.cleaned$NAID==id
#  weight.cleaned[id.mask, ,drop=FALSE]
#})
#names(weight.cleaned.by.id) = cw1.id

#cw1$updated.weight=sapply(1:nrow(cw1), function(i){
#  date=cw1$date.in[i]
#  id=as.character(cw1$NAID[i])
#  dates.for.id=weight.cleaned.by.id[[id]]$weight.date
#  dates.in.range.for.id.mask=dates.for.id <= date & dates.for.id >= (date - weight.lookback.period)
#  if(!any(dates.in.range.for.id.mask))
#    return(NA)
#  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
#  weights.in.range=weight.cleaned.by.id[[id]]$WEIGHT_KG[dates.in.range.for.id.mask]
#  max.date.mask=dates.in.range==max(dates.in.range)
#  weights.in.range[max.date.mask][1]
#})

#Updated BMI
bmi.cleaned.by.id = lapply(cw1.id, function(id) {
  id.mask=bmi.cleaned$NAID==id
  bmi.cleaned[id.mask, ,drop=FALSE]
})
names(bmi.cleaned.by.id) = cw1.id

cw1$updated.bmi=sapply(1:nrow(cw1), function(i){
  date=cw1$date.in[i]
  id=as.character(cw1$NAID[i])
  dates.for.id=bmi.cleaned.by.id[[id]]$weight.date
  dates.in.range.for.id.mask=dates.for.id <= date & dates.for.id >= (date - weight.lookback.period)
  if(!any(dates.in.range.for.id.mask))
    return(NA)
  dates.in.range=dates.for.id[dates.in.range.for.id.mask]
  bmis.in.range=bmi.cleaned.by.id[[id]]$BMI[dates.in.range.for.id.mask]
  max.date.mask=dates.in.range==max(dates.in.range)
  bmis.in.range[max.date.mask][1]
})

cw1 <- cw1 %>%
  mutate(updated.bmi.cat = case_when(updated.bmi < 18.5 ~ 3,
                                     updated.bmi >= 18.5 & updated.bmi < 25 ~ 0,
                                     updated.bmi >= 25 & updated.bmi < 30 ~ 1,
                                     updated.bmi >= 30 ~ 2,
                                     is.na(updated.bmi) ~ 4
  )
  )

#Time updated comorbidities and co-prescriptions using data.table 
cw1.dt <- cw1

setDT(cw1.dt)

setDT(treated.htn)
setDT(treated.hld)
setDT(ckd.cleaned)

setDT(rx.tdf.col)
setDT(rx.taf.col)

setDT(rx.ac)
setDT(rx.ad)
setDT(rx.ap)
setDT(rx.opioid)

cw1.dt[, updated.treated.htn :=0]
cw1.dt[treated.htn, on = .(NAID),
       updated.treated.htn := fifelse(htn.date <= date.in, 1, updated.treated.htn),
       by = .EACHI]

cw1.dt[, updated.treated.hld :=0]
cw1.dt[treated.hld, on = .(NAID),
       updated.treated.hld := fifelse(hld.date <= date.in, 1, updated.treated.hld),
       by = .EACHI]

cw1.dt[, updated.ckd60 :=0]
cw1.dt[ckd.cleaned, on = .(NAID),
       updated.ckd60 := fifelse(CKD60_DT <= date.in, 1, updated.ckd60),
       by = .EACHI]

cw1.dt[, updated.pre.tdf :=0]
cw1.dt[rx.tdf.col, on = .(NAID),
       updated.pre.tdf := fifelse(startdate <= (date.in - 14) & (date.in - 180) <= stopdate, 1, updated.pre.tdf),
       by = .EACHI]

cw1.dt[, updated.pre.taf :=0]
cw1.dt[rx.taf.col, on = .(NAID),
       updated.pre.taf := fifelse(startdate <= (date.in - 14) & (date.in - 180) <= stopdate, 1, updated.pre.taf),
       by = .EACHI]

cw1.dt[, updated.post.tdf :=0]
cw1.dt[rx.tdf.col, on = .(NAID),
       updated.post.tdf := fifelse(startdate <= (date.in + 180) & (date.in + 14) <= stopdate, 1, updated.post.tdf),
       by = .EACHI]

cw1.dt[, updated.post.taf :=0]
cw1.dt[rx.taf.col, on = .(NAID),
       updated.post.taf := fifelse(startdate <= (date.in + 180) & (date.in + 14) <= stopdate, 1, updated.post.taf),
       by = .EACHI]

cw1.dt[, updated.rx.ac :=0]
cw1.dt[rx.ac, on = .(NAID),
       updated.rx.ac := fifelse(startdate <= date.in & stopdate >= date.in, 1, updated.rx.ac),
       by = .EACHI]

cw1.dt[, updated.rx.ad :=0]
cw1.dt[rx.ad, on = .(NAID),
       updated.rx.ad := fifelse(startdate <= date.in & stopdate >= date.in, 1, updated.rx.ad),
       by = .EACHI]

cw1.dt[, updated.rx.ap :=0]
cw1.dt[rx.ap, on = .(NAID),
       updated.rx.ap := fifelse(startdate <= date.in & stopdate >= date.in, 1, updated.rx.ap),
       by = .EACHI]

cw1.dt[, updated.rx.opioid :=0]
cw1.dt[rx.opioid, on = .(NAID),
       updated.rx.opioid := fifelse(startdate <= date.in & stopdate >= date.in, 1, updated.rx.opioid),
       by = .EACHI]


setDF(cw1.dt)
cw1 <- cw1.dt
saveRDS(cw1, "cw1.rds")


cw1 %>% filter(NAID=="31012930") %>% select(NAID, encounterdate, day.in, day.out, treated.htn,updated.treated.htn) %>% print()

# rx.ad, updated.rx.ad

hist(s5$a1c.close.value)

##For lost IPCW
model.lost.1 <- glm(lost ~ insti + age.sp1 + age.sp2 + age.sp3 + age.sp4 + a1c.baseline.sp1 + a1c.baseline.sp2 + a1c.baseline.sp3 + a1c.baseline.sp4 + ENCOUNTERYR + BIRTHSEX + race.cat + rx.noninsti.length.at.encounterdate +
                      bmi.cat + vl.suppressed + treated.htn + treated.hld + ckd60 + rx.ac + rx.ad + rx.ap + rx.opioid +
                      no.taf.to.yes.taf + yes.taf.to.yes.taf + yes.taf.to.no.taf + no.tdf.to.yes.tdf + no.tdf.to.no.tdf + yes.tdf.to.no.tdf +
                      day.in +
                      updated.bmi.cat + updated.vl.suppressed + updated.treated.htn + updated.treated.hld + updated.ckd60 + updated.rx.ac + updated.rx.ad + updated.rx.ap + updated.rx.opioid +
                      updated.pre.tdf + updated.pre.taf + updated.post.tdf + updated.post.taf,
                    data=cw1, family=binomial(link="logit"))

model.lost.2 <- glm(lost ~ insti + day.in, data=cw1, family=binomial(link="logit"))

cw1$p.lost.1 <- predict(model.lost.1, data=cw1, type="response")
cw1$p.lost.2 <- predict(model.lost.2, data=cw1, type="response")

cw1 <- cw1 %>%
  mutate(ipcw.lost=ifelse(lost==1, p.lost.2/p.lost.1, (1-p.lost.2)/(1-p.lost.1)))

cw1 <- cw1 %>%
  group_by(NAID, enc.num) %>%
  mutate(ipcw.cumulative.lost=cumprod(ipcw.lost)) %>%
  ungroup()

##For switched IPCW: fit the logistic model for the non-INSTI group (for INSTI group, IPTW should be 1 since they do not switch)
noninsti.mask=cw1$insti==0

model.switched.1 <- glm(switched ~ age.sp1 + age.sp2 + age.sp3 + age.sp4 + a1c.baseline.sp1 + a1c.baseline.sp2 + a1c.baseline.sp3 + a1c.baseline.sp4 + ENCOUNTERYR + BIRTHSEX + race.cat + rx.noninsti.length.at.encounterdate +
                          bmi.cat + vl.suppressed + treated.htn + treated.hld + ckd60 + rx.ac + rx.ad + rx.ap + rx.opioid +
                          no.taf.to.yes.taf + yes.taf.to.yes.taf + yes.taf.to.no.taf + no.tdf.to.yes.tdf + no.tdf.to.no.tdf + yes.tdf.to.no.tdf +
                          day.in +
                          updated.bmi.cat + updated.vl.suppressed + updated.treated.htn + updated.treated.hld + updated.ckd60 + updated.rx.ac + updated.rx.ad + updated.rx.ap + updated.rx.opioid +
                          updated.pre.tdf + updated.pre.taf + updated.post.tdf + updated.post.taf,
                        data=cw1[noninsti.mask, ], family=binomial(link="logit"))

model.switched.2 <- glm(switched ~ day.in, data=cw1[noninsti.mask, ], family=binomial(link="logit"))

cw1$p.switched.1=0
cw1$p.switched.2=0

cw1$p.switched.1[noninsti.mask] <- predict(model.switched.1, data=cw1[noninsti.mask, ], type="response")
cw1$p.switched.2[noninsti.mask] <- predict(model.switched.2, data=cw1[noninsti.mask, ], type="response")

cw1 <- cw1 %>%
  mutate(ipcw.switched=ifelse(switched==1, p.switched.2/p.switched.1, (1-p.switched.2)/(1-p.switched.1)))

cw1 <- cw1 %>%
  group_by(NAID, enc.num) %>%
  mutate(ipcw.cumulative.switched=cumprod(ipcw.switched)) %>%
  ungroup()

##Multiply IPTW (for predicting INSTI exposure) * IPCW (for lost) * IPCW (for switched) and doing Coxph for hazard of diabetes
cw1$iptw.ipcw=cw1$iptw*cw1$ipcw.cumulative.lost*cw1$ipcw.cumulative.switched

#Trim
lower.bound <- quantile(cw1$iptw.ipcw, 0.01)
upper.bound <- quantile(cw1$iptw.ipcw, 0.99)

cw1$iptw.ipcw.trimmed <- pmin(pmax(cw1$iptw.ipcw, lower.bound), upper.bound)

cox_model <- coxph(Surv(cw1$day.in, cw1$day.out, cw1$outcome) ~insti, data=cw1, cluster=NAID, weights = iptw.ipcw.trimmed)
summary(cox_model)

##Multiply IPTW.specific (for predicting individual INSTI exposure) * IPCW (for lost) * IPCW (for switched) and doing Coxph for hazard of diabetes
cw1$iptw.specific.ipcw=cw1$iptw.specific*cw1$ipcw.cumulative.lost*cw1$ipcw.cumulative.switched

#Trim
lower.bound <- quantile(cw1$iptw.specific.ipcw, 0.01)
upper.bound <- quantile(cw1$iptw.specific.ipcw, 0.99)

cw1$iptw.specific.ipcw.trimmed <- pmin(pmax(cw1$iptw.specific.ipcw, lower.bound), upper.bound)

#  check missingness
install.packages("mice")
library(mice)
mice::md.pattern(cw1, plot=FALSE)

cox_model <- coxph(Surv(cw1$day.in, cw1$day.out, cw1$outcome) ~insti.specific, data=cw1, cluster=NAID, weights = iptw.specific.ipcw.trimmed)
summary(cox_model)

saveRDS(cw1, "cw1.rds")

################################
# IR with IPTW*IPCW
################################

##Diabetes: INSTI vs non-INSTI - IPTW*IPCW incidence rates in person-years (time/365)
N.unique=length(unique(cw1$NAID[cw1$insti==1]))
N.obs=sum(cw1$insti==1)
events.raw=sum(cw1$dm.out[cw1$insti==1])
events.weighted=round(sum(cw1$dm.out[cw1$insti==1]*cw1$iptw.specific.ipcw.trimmed[cw1$insti==1]))
time.at.risk=sum(cw1$time.for.diabetes[cw1$insti==1]/365*cw1$iptw.specific.ipcw.trimmed[ac1$insti==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

N.unique=length(unique(cw1$NAID[ac1$insti==0]))
N.obs=sum(cw1$insti==0)
events.raw=sum(cw1$dm.out[ac1$insti==0])
events.weighted=round(sum(ac1$dm.out[ac1$insti==0]*ac1$iptw.trimmed[ac1$insti==0]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$insti==0]/365*ac1$iptw.trimmed[ac1$insti==0])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

###########
##Incidence rates for BIC IPTW
N.unique=length(unique(ac1$NAID[ac1$bic==1]))
N.obs=sum(ac1$bic==1)
events.raw=sum(ac1$dm.out[ac1$bic==1])
events.weighted=round(sum(ac1$dm.out[ac1$bic==1]*ac1$iptw.specific.trimmed[ac1$bic==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$bic==1]/365*ac1$iptw.specific.trimmed[ac1$bic==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

##Incidence rates for DTG IPTW
N.unique=length(unique(ac1$NAID[ac1$dtg==1]))
N.obs=sum(ac1$dtg==1)
events.raw=sum(ac1$dm.out[ac1$dtg==1])
events.weighted=round(sum(ac1$dm.out[ac1$dtg==1]*ac1$iptw.specific.trimmed[ac1$dtg==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$dtg==1]/365*ac1$iptw.specific.trimmed[ac1$dtg==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

##Incidence rates for EVG IPTW
N.unique=length(unique(ac1$NAID[ac1$evg==1]))
N.obs=sum(ac1$evg==1)
events.raw=sum(ac1$dm.out[ac1$evg==1])
events.weighted=round(sum(ac1$dm.out[ac1$evg==1]*ac1$iptw.specific.trimmed[ac1$evg==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$evg==1]/365*ac1$iptw.specific.trimmed[ac1$evg==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

##Incidence rates for RAL IPTW
N.unique=length(unique(ac1$NAID[ac1$ral==1]))
N.obs=sum(ac1$ral==1)
events.raw=sum(ac1$dm.out[ac1$ral==1])
events.weighted=round(sum(ac1$dm.out[ac1$ral==1]*ac1$iptw.specific.trimmed[ac1$ral==1]))
time.at.risk=sum(ac1$time.for.diabetes[ac1$ral==1]/365*ac1$iptw.specific.trimmed[ac1$ral==1])
poisson.result <- poisson.test(x=events.weighted, T=time.at.risk, conf.level=0.95)
incidence.rate <- poisson.result$estimate
confidence.intervals <- poisson.result$conf.int

poisson.result <- data.frame(
  N.unique = N.unique,
  N.obs = N.obs,
  events.raw = events.raw,
  events.percent = events.raw/N.obs*100,
  events.weighted = events.weighted,
  Incidence.rate = incidence.rate,
  LCI = confidence.intervals[1],
  UCI = confidence.intervals[2]
)
print(poisson.result)

################################
# KM curve with IPTW*IPCW
################################

ac1s <- cw1 %>%
  mutate(insti = recode(insti, `0` = "Continued NNRTI or PI", `1` = "Switched to INSTI"))

cicurve.weighted <- survfit2(Surv(ac1s$time.for.diabetes/365, ac1s$dm.out) ~insti, data=ac1s, cluster=NAID, weights=iptw.trimmed) %>%
  ggsurvfit(type="risk", linewidth=1.3) +
  add_confidence_interval() +
  add_risktable(risktable_height = 0.33, size=6, 
                theme = list(theme_risktable_default(axis.text.y.size = 15,
                                                     plot.title.size = 15),
                             theme(plot.title = element_text(face = "bold"))),
                risktable_stats = c("{format(round(n.risk, 0), nsmall = 0)}",
                                    "{format(round(n.event, 0), nsmall = 0)}"),
                stats_label = c("N at risk",
                                "N of events")
  )+
  labs(y="Cumulative Incidence", x="Follow-up time (years)") +
  scale_x_continuous(limits=c(0.0, 5)) +
  scale_y_continuous(limits=c(0.0, 0.65)) +
  theme(legend.position = c(0.10, 0.80)) +
  ggeasy::easy_y_axis_labels_size(size=12) +
  ggeasy::easy_x_axis_labels_size(size=12) +
  ggeasy::easy_y_axis_title_size(size=18) +
  ggeasy::easy_x_axis_title_size(size=18) +
  ggeasy::easy_plot_legend_size(size=12)

cicurve.weighted
ggsave(file="C:/Users/mkim255/Pre_diabetes_project_2025/INSTI DM NA-ACCORD CI Curve IPTW.png", plot = cicurve.weighted, width=16, height=6, units="in")







##Analysis by follow up period
cw1a <- cw1 %>%
  mutate(follow.up.period = ifelse(day.in >= 720, 1, 0))

cox_model <- coxph(Surv(cw1a$day.in, cw1a$day.out, cw1a$outcome) ~insti*follow.up.period, data=cw1a, cluster=NAID, weights = iptw.ipcw.trimmed)
summary(cox_model)

cox_model <- coxph(Surv(cw1a$day.in[cw1a$follow.up.period==0], cw1a$day.out[cw1a$follow.up.period==0], cw1a$outcome[cw1a$follow.up.period==0]) ~insti, data=cw1a[cw1a$follow.up.period==0, ], cluster=NAID, weights = iptw.ipcw.trimmed)
summary(cox_model)

cox_model <- coxph(Surv(cw1a$day.in[cw1a$follow.up.period==1], cw1a$day.out[cw1a$follow.up.period==1], cw1a$outcome[cw1a$follow.up.period==1]) ~insti, data=cw1a[cw1a$follow.up.period==1, ], cluster=NAID, weights = iptw.ipcw.trimmed)
summary(cox_model)


##Interaction by year (>=2016 vs <2016)
cw1$year.group <- as.factor(ifelse(cw1$ENCOUNTERYR >= 2016, 1, 0))
cox_model <- coxph(Surv(cw1$day.in, cw1$day.out, cw1$outcome) ~ insti*year.group, data=cw1, cluster=NAID, weights = iptw.ipcw)
summary(cox_model)




