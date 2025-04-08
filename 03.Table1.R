# Day difference between the index clinic visit date and the latest A1c measurement 
s5 <- s5 %>% mutate(day.diff.a1c = encounterdate - a1c.close.date)

table1.1 <- s5
id <- s5 %>% select(NAID) %>% distinct()
nrow(id) # 7843

# 2016-2023
s5.2016 <- s5 %>% filter(ENCOUNTERYR >= 2016)
table1.1 <- s5.2016
id <- s5.2016 %>% select(NAID) %>% distinct()
nrow(id) # 3234


#Table 1
df=NULL
group.to.summarize=c(1, 0)
for (group in group.to.summarize){
  N.unique <- length(unique(table1.1$NAID[table1.1$insti==group]))
  N.encounters <- length(unique(table1.1$enc.num[table1.1$insti==group]))
  dm.out <- length(unique(table1.1$enc.num[table1.1$dm.out==1 & table1.1$insti==group]))
  
  nnrti <- sum(table1.1$nnrti[table1.1$insti==group])
  pi <- sum(table1.1$pi[table1.1$insti==group])
  
  #baseline A1c
  median.a1c.baseline <- median(table1.1$a1c.close.value[table1.1$insti==group])
  q1.a1c.baseline <- quantile(table1.1$a1c.close.value[table1.1$insti==group], 0.25)
  q3.a1c.baseline <- quantile(table1.1$a1c.close.value[table1.1$insti==group], 0.75)
  
  # Day difference between the index clinic visit date and the latest A1c measurement
  median.day.diff.a1c <- median(table1.1$day.diff.a1c[table1.1$insti==group])
  q1.day.diff.a1c <- quantile(table1.1$day.diff.a1c[table1.1$insti==group], 0.25)
  q3.day.diff.a1c <- quantile(table1.1$day.diff.a1c[table1.1$insti==group], 0.75)
  
  #Demographics
  median.age <- median(table1.1$age[table1.1$insti==group])
  q1.age <- quantile(table1.1$age[table1.1$insti==group], 0.25)
  q3.age <- quantile(table1.1$age[table1.1$insti==group], 0.75)
  
  birthmale <- sum(table1.1$BIRTHSEX[table1.1$insti==group]==1)
  
  black <- sum(table1.1$race.cat[table1.1$insti==group]=="black")
  white <- sum(table1.1$race.cat[table1.1$insti==group]=="white")
  other <- sum(table1.1$race.cat[table1.1$insti==group]=="other")
  missing <- sum(table1.1$race.cat[table1.1$insti==group]=="missing")
  
  year.2007.2009 <- sum(table1.1$ENCOUNTERYR[table1.1$insti==group] >=2007 & table1.1$ENCOUNTERYR[table1.1$insti==group] <= 2009)
  year.2010.2011 <- sum(table1.1$ENCOUNTERYR[table1.1$insti==group] >=2010 & table1.1$ENCOUNTERYR[table1.1$insti==group] <= 2011)
  year.2012.2013 <- sum(table1.1$ENCOUNTERYR[table1.1$insti==group] >=2012 & table1.1$ENCOUNTERYR[table1.1$insti==group] <= 2013)
  year.2014.2015 <- sum(table1.1$ENCOUNTERYR[table1.1$insti==group] >=2014 & table1.1$ENCOUNTERYR[table1.1$insti==group] <= 2015)
  year.2016.2017 <- sum(table1.1$ENCOUNTERYR[table1.1$insti==group] >=2016 & table1.1$ENCOUNTERYR[table1.1$insti==group] <= 2017)
  year.2018.2019 <- sum(table1.1$ENCOUNTERYR[table1.1$insti==group] >=2018 & table1.1$ENCOUNTERYR[table1.1$insti==group] <= 2019)
  year.2020.2021 <- sum(table1.1$ENCOUNTERYR[table1.1$insti==group] >=2020 & table1.1$ENCOUNTERYR[table1.1$insti==group] <= 2021)
  year.2022.2023 <- sum(table1.1$ENCOUNTERYR[table1.1$insti==group] >=2022 & table1.1$ENCOUNTERYR[table1.1$insti==group] <= 2023)
  
  #NNRTI or PI use duration prior to index
  median.rx.noninsti.duration.in.years <- median(as.numeric(table1.1$rx.noninsti.duration[table1.1$insti==group])/365.25, na.rm=TRUE)
  q1.rx.noninsti.duration.in.years <- quantile(as.numeric(table1.1$rx.noninsti.duration[table1.1$insti==group])/365.25, 0.25, na.rm=TRUE)
  q3.rx.noninsti.duration.in.years <- quantile(as.numeric(table1.1$rx.noninsti.duration[table1.1$insti==group])/365.25, 0.75, na.rm=TRUE)
  
  #VS
  median.weight <- median(table1.1$weight[table1.1$insti==group], na.rm=TRUE)
  q1.weight <- quantile(table1.1$weight[table1.1$insti==group], 0.25, na.rm=TRUE)
  q3.weight <- quantile(table1.1$weight[table1.1$insti==group], 0.75, na.rm=TRUE)
  
  median.bmi <- median(table1.1$bmi[table1.1$insti==group], na.rm=TRUE)
  q1.bmi <- quantile(table1.1$bmi[table1.1$insti==group], 0.25, na.rm=TRUE)
  q3.bmi <- quantile(table1.1$bmi[table1.1$insti==group], 0.75, na.rm=TRUE)
  
  underweight <- sum(table1.1$bmi.cat[table1.1$insti==group]==3, na.rm=TRUE)
  normalweight <- sum(table1.1$bmi.cat[table1.1$insti==group]==0, na.rm=TRUE)
  overweight <- sum(table1.1$bmi.cat[table1.1$insti==group]==1, na.rm=TRUE)
  obese <- sum(table1.1$bmi.cat[table1.1$insti==group]==2, na.rm=TRUE)
  missing.bmi <- sum(table1.1$bmi.cat[table1.1$insti==group]==4, na.rm=TRUE)
  
  #VL
  vl.suppressed <- sum(table1.1$vl.suppressed[table1.1$insti==group])
  
  #Comorbidities and coprescriptions
  treated.htn <- sum(table1.1$treated.htn[table1.1$insti==group])
  treated.hld <- sum(table1.1$treated.hld[table1.1$insti==group])
  ckd60 <- sum(table1.1$ckd60[table1.1$insti==group])
  
  ac <- sum(table1.1$rx.ac[table1.1$insti==group])
  ad <- sum(table1.1$rx.ad[table1.1$insti==group])
  ap <- sum(table1.1$rx.ap[table1.1$insti==group])
  opioid <- sum(table1.1$rx.opioid[table1.1$insti==group])
  
  pre.tdf <- sum(table1.1$pre.tdf[table1.1$insti==group])
  pre.taf <- sum(table1.1$pre.taf[table1.1$insti==group])
  post.tdf <- sum(table1.1$post.tdf[table1.1$insti==group])
  post.taf <- sum(table1.1$post.taf[table1.1$insti==group])
  
  no.taf.to.yes.taf <- sum(table1.1$no.taf.to.yes.taf[table1.1$insti==group])
  yes.taf.to.yes.taf <- sum(table1.1$yes.taf.to.yes.taf[table1.1$insti==group])
  yes.taf.to.no.taf <- sum(table1.1$yes.taf.to.no.taf[table1.1$insti==group])
  no.taf.to.no.taf <- sum(table1.1$no.taf.to.no.taf[table1.1$insti==group])
  no.tdf.to.yes.tdf <- sum(table1.1$no.tdf.to.yes.tdf[table1.1$insti==group])
  yes.tdf.to.yes.tdf <- sum(table1.1$yes.tdf.to.yes.tdf[table1.1$insti==group])
  yes.tdf.to.no.tdf <- sum(table1.1$yes.tdf.to.no.tdf[table1.1$insti==group])
  no.tdf.to.no.tdf <- sum(table1.1$no.tdf.to.no.tdf[table1.1$insti==group])
  
  one.column.df=c(
    N.unique=format(N.unique, big.mark = ","),
    N.encounters=format(N.encounters, big.mark = ","),
    incident.dm.without.censor=paste0(dm.out, " (", format(dm.out/N.encounters*100, digits=1, nsmall=1), "%)"),
    
    nnrti=paste0(format(nnrti, big.mark = ","), " (", format(nnrti/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    pi=paste0(format(pi, big.mark = ","), " (", format(pi/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    median.a1c.baseline=paste0(median.a1c.baseline, " (", q1.a1c.baseline, "-", q3.a1c.baseline, ")"),
    median.day.diff.a1c=paste0(median.day.diff.a1c, " (", q1.day.diff.a1c, "-", q3.day.diff.a1c, ")"),
    
    median.age=paste0(median.age, " (", q1.age, "-", q3.age, ")"),
    birthmale=paste0(format(birthmale, big.mark = ","), " (", format(birthmale/N.encounters*100, digits=1, nsmall=1), "%)"),
    
    race="race",
    black=paste0(format(black, big.mark = ","), " (", format(black/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    white=paste0(format(white, big.mark = ","), " (", format(white/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    other=paste0(format(other, big.mark = ","), " (", format(other/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    missing.race=paste0(format(missing, big.mark = ","), " (", format(missing/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    year.of.encounter="Year of encounter",
    year.2007.2009=paste0(format(year.2007.2009, big.mark = ","), " (", format(year.2007.2009/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    year.2010.2011=paste0(format(year.2010.2011, big.mark = ","), " (", format(year.2010.2011/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    year.2012.2013=paste0(format(year.2012.2013, big.mark = ","), " (", format(year.2012.2013/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    year.2014.2015=paste0(format(year.2014.2015, big.mark = ","), " (", format(year.2014.2015/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    year.2016.2017=paste0(format(year.2016.2017, big.mark = ","), " (", format(year.2016.2017/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    year.2018.2019=paste0(format(year.2018.2019, big.mark = ","), " (", format(year.2018.2019/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    year.2020.2021=paste0(format(year.2020.2021, big.mark = ","), " (", format(year.2020.2021/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    year.2022.2023=paste0(format(year.2022.2023, big.mark = ","), " (", format(year.2022.2023/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    median.rx.noninsti.duration.in.years=paste0(format(median.rx.noninsti.duration.in.years, digits=1, nsmall=1), " (", format(q1.rx.noninsti.duration.in.years, digits=1, nsmall=1), "-", format(q3.rx.noninsti.duration.in.years, digits=1, nsmall=1), ")"),
    
    median.weight=paste0(format(median.weight, digits=1, nsmall=1), " (", format(q1.weight, digits=1, nsmall=1), "-", format(q3.weight, digits=1, nsmall=1), ")"),
    
    bmi="Body mass index",
    median.bmi=paste0(format(median.bmi, digits=1, nsmall=1), " (", format(q1.bmi, digits=1, nsmall=1), "-", format(q3.bmi, digits=1, nsmall=1), ")"),
    underweight=paste0(format(underweight, big.mark = ","), " (", format(underweight/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    normalweight=paste0(format(normalweight, big.mark = ","), " (", format(normalweight/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    overweight=paste0(format(overweight, big.mark = ","), " (", format(overweight/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    obese=paste0(format(obese, big.mark = ","), " (", format(obese/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    missing.bmi=paste0(format(missing.bmi, big.mark = ","), " (", format(missing.bmi/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    vl.suppressed=paste0(format(vl.suppressed, big.mark = ","), " (", format(vl.suppressed/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    comorbidities="Comorbidities",
    treated.htn=paste0(format(treated.htn, big.mark = ","), " (", format(treated.htn/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    treated.hld=paste0(format(treated.hld, big.mark = ","), " (", format(treated.hld/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    ckd60=paste0(format(ckd60, big.mark = ","), " (", format(ckd60/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    comedications="Comedications",
    antithrombotic=paste0(format(ac, big.mark = ","), " (", format(ac/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    antidepressant=paste0(format(ad, big.mark = ","), " (", format(ad/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    antipsychotic=paste0(format(ap, big.mark = ","), " (", format(ap/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    opioid=paste0(format(opioid, big.mark = ","), " (", format(opioid/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    pre.tdf=paste0(format(pre.tdf, big.mark = ","), " (", format(pre.tdf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    post.tdf=paste0(format(post.tdf, big.mark = ","), " (", format(post.tdf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    pre.taf=paste0(format(pre.taf, big.mark = ","), " (", format(pre.taf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    post.taf=paste0(format(post.taf, big.mark = ","), " (", format(post.taf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    no.taf.to.yes.taf=paste0(format(no.taf.to.yes.taf, big.mark = ","), " (", format(no.taf.to.yes.taf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    yes.taf.to.yes.taf=paste0(format(yes.taf.to.yes.taf, big.mark = ","), " (", format(yes.taf.to.yes.taf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    yes.taf.to.no.taf=paste0(format(yes.taf.to.no.taf, big.mark = ","), " (", format(yes.taf.to.no.taf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    no.taf.to.no.taf=paste0(format(no.taf.to.no.taf, big.mark = ","), " (", format(no.taf.to.no.taf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    
    no.tdf.to.yes.tdf=paste0(format(no.tdf.to.yes.tdf, big.mark = ","), " (", format(no.tdf.to.yes.tdf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    yes.tdf.to.yes.tdf=paste0(format(yes.tdf.to.yes.tdf, big.mark = ","), " (", format(yes.tdf.to.yes.tdf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    yes.tdf.to.no.tdf=paste0(format(yes.tdf.to.no.tdf, big.mark = ","), " (", format(yes.tdf.to.no.tdf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)"),
    no.tdf.to.no.tdf=paste0(format(no.tdf.to.no.tdf, big.mark = ","), " (", format(no.tdf.to.no.tdf/N.encounters*100, digits=1, nsmall=1, big.mark = ","), "%)")
  )
  if(is.null(df)){
    df=data.frame(one.column.df)
    names(df)[1]=as.character(group)
  }
  else
    df[[as.character(group)]]=one.column.df
}

print(df)

write.csv(df, file="C:/Users/mkim255/Pre_diabetes_project_2025/PreDM Table 1 2007-2023 20240103.csv")

# histogram for the day.diff
summary(s5.2016$day.diff.a1c)

insti_labels <- c("0" = "Patients who continuing with NNRTI/PI", "1"="Patients who switching to INSTI")
ggplot(s5.2016, aes(x = day.diff.a1c/30.5)) +
  geom_histogram(fill = "white", colour = "black", bins = 24) +  # Adjusted bin count for better resolution
  facet_wrap(~ insti, scales = "free_y", labeller = as_labeller(insti_labels)) +  # Allows different y-axis limits
  theme_minimal() +  # Cleaner theme for better readability
  labs(
    title = "Distribution of Day Difference Between a1c.measure.date and index.visit.date",
    x = "Day Difference in Month",
    y = "Number of encounters (N)"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),  # Increase facet label size
    axis.text = element_text(size = 12),  # Improve axis readability
    axis.title = element_text(size = 12, face = "bold")  # Bold axis titles
  )

# table 2
head(s5.2016) #bic dtg evg ral 
insti.2016 <- s5.2016 %>% filter(insti==1)
212+234+183+10

# TAD & TDF distribution of 
bic <- insti.2016 %>% filter(bic==1)
dtg <- insti.2016 %>% filter(dtg==1)
evg <- insti.2016 %>% filter(evg==1)
ral <- insti.2016 %>% filter(ral==1)

noninst.2016 <- s5.2016 %>% filter(insti==0)
nrow(noninst.2016)

df.ral <- ral %>%
  summarise(
    total = 10,
    ini.taf = sum(no.taf.to.yes.taf==1),
    con.taf = sum(yes.taf.to.yes.taf==1),
    dis.taf = sum(yes.taf.to.no.taf==1),
    no.taf = sum(no.taf.to.no.taf==1),
    ini.tdf = sum(no.tdf.to.yes.tdf==1),
    con.tdf = sum(yes.tdf.to.yes.tdf==1),
    dis.tdf = sum(yes.tdf.to.no.tdf==1),
    no.tdf = sum(no.tdf.to.no.tdf==1),
    prop.ini.taf = ini.taf/total*100,
    prop.con.taf = con.taf/total*100,
    prop.dis.taf = dis.taf/total*100,
    prop.no.taf = no.taf/total*100,
    prop.ini.tdf = ini.tdf/total*100,
    prop.con.tdf = con.tdf/total*100,
    prop.dis.tdf = dis.tdf/total*100,
    prop.no.tdf = no.tdf/total*100
  ) %>% print()



insti.2016 %>% filter(ral==1 & dm.out ==1) %>% distinct(NAID) %>% count() %>% print()

insti <- s5 %>% filter(insti ==1) %>%
  mutate(
    mean.a1c = mean(a1c.close.value),
    sd.a1c = sd(a1c.close.value),
    median.a1c = median(a1c.close.value))

insti %>% dplyr::select(mean.a1c, sd.a1c, median.a1c) %>% print()
names(s5)










