#################################################
### Analysis by follow up period
#################################################
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

#################################################
### Mediating role of weight gain - weight in the original dataset = baseline.weight
#################################################
any(is.na(cw1$weight)) # it means all individuals have baseline.weight

weight.cleaned <- readRDS("weight.cleaned.rds")
#Identify unique NAID
cw1.id = unique(cw1$NAID)

# long dataset for Weight
weight.cleaned.by.id = lapply(cw1.id, function(id) {
  id.mask=weight.cleaned$NAID==id
  weight.cleaned[id.mask, ,drop=FALSE]
})
names(weight.cleaned.by.id) = cw1.id

# post.weight up until 1 year after the index date -> cw1.weight
weight.assessment.period = 365
cw1.weight <- cw1
cw1.id <- data.frame(NAID = cw1.id)


# merge with weight.cleaned
cw1.weight.long <- do.call(rbind, lapply(1:nrow(cw1), function(i) {
  date <- cw1$encounterdate[i]  # Current encounter date
  id <- as.character(cw1$NAID[i])  # Patient ID
  
  # Check if patient ID exists in weight dataset
  if (!id %in% names(weight.cleaned.by.id)) {
    return(NULL)  # Skip if ID not found
  }
  
  # Extract available weight dates for this patient
  dates.for.id <- weight.cleaned.by.id[[id]]$weight.date
  weights.for.id <- weight.cleaned.by.id[[id]]$WEIGHT_KG
  
  # Select weights within the post-encounter period
  valid.mask <- date < dates.for.id & (date + weight.assessment.period) >= dates.for.id
  
  if (!any(valid.mask)) {
    return(NULL)  # Skip if no valid weight records
  }
  
  # Create a data frame with multiple weight entries per encounter
  data.frame(
    NAID = id,
    encounterdate = date,
    weight.date = dates.for.id[valid.mask],
    WEIGHT_KG = weights.for.id[valid.mask]
  )
}))

# Convert result to a tibble for better readability
cw1.weight.long <- as_tibble(cw1.weight.long)
cw1.weight.long$weight.date <- as.numeric(cw1.weight.long$weight.date)
cw1.weight.long$NAID <- as.numeric(cw1.weight.long$NAID)

## merge with the baseline.weight
cw1.visit <- unique(cw1[, c("NAID", "encounterdate", "weight")])
#cw1.visit <- data.frame(NAID = cw1.id)
nrow(cw1.visit) # 13338

cw1.weight.long <- cw1.weight.long %>% 
  left_join(cw1.visit, by = c("NAID", "encounterdate"))

cw1.weight.long <- cw1.weight.long %>% rename(baseline.weight=weight, post.weight=WEIGHT_KG)
head(cw1.weight.long)

# set the criteria of 5% weight gain from the baseline weight
df <- cw1.weight.long %>% 
  mutate(wr5 = baseline.weight * 1.05)
head(df)


df2 <- cw1.weight.long %>% 
  left_join(cw1, by = c("NAID", "encounterdate"))
