---

title: "FHHC Yearly Report (Year 1)"

output: html_document

---

 

```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)

```

 

Load in packages

 

```{r}

library(prettyR)

library(psych)

library(lubridate)

```

 

Working directory

 

```{r}

setwd("S:/Indiana Research & Evaluation/FHHC Homelessness/Data and QPR")

base_redcap = read.csv("base.csv", header = TRUE, na.strings = c(-99, -98, -97, " "))

month6_redcap = read.csv("month6.csv", header = TRUE, na.strings = c(-99, -98, -97, " "))

SPARS_data = read.csv("FHHC.csv", header = TRUE, na.strings = c(-99, -98, -97, -1, -4, -5, -7, -9, -2, -6, -8, " "))


```
Matched Pairs in SPARS Data (base and month 6)

 

```{r}

dim(SPARS_data)

SPARS_data = SPARS_data[order(SPARS_data$ConsumerID),]

head(SPARS_data)

SPARS_data$ConsumerID = gsub("\\D", "",SPARS_data$ConsumerID)

SPARS_data$ConsumerID = as.numeric(SPARS_data$ConsumerID)

SPARS_data$InterviewDate = mdy(SPARS_data$InterviewDate)

dim(SPARS_data)

 
## Remove any interview date intake, 6-month, disharge after this date
SPARS_data = subset(SPARS_data, SPARS_data$InterviewDate < "2020-07-01")
SPARS_data$InterviewType_07 = as.numeric(SPARS_data$InterviewType_07)
dim(SPARS_data)
SPARS_base = subset(SPARS_data, SPARS_data$InterviewType_07 == 1)
dim(SPARS_base)

describe.factor(SPARS_data$InterviewType_07)

SPARS_month6 = subset(SPARS_data, SPARS_data$InterviewType_07 == 3)

dim(SPARS_month6)


write.csv(SPARS_base, "SPARS_base.csv", row.names = FALSE)
 

SPARS_wide = merge(SPARS_base, SPARS_month6, by = "ConsumerID", all.y = TRUE)

dim(SPARS_wide)
SPARS_wide$ConsumerID == SPARS_month6$ConsumerID


### Make sure this matches the current enrollments on the tracker
SPARS_data_base = subset(SPARS_data,InterviewType_07==1)
dim(SPARS_data_base)
dim(SPARS_base)

```

 

Get date from SPARs and put into REDCap month6 data (we don't have a date in REDCap month6 as it currently stands)

```{r}

SPARS_date_base = data.frame(record_id =  SPARS_base$ConsumerID,  InterviewDate = SPARS_base$InterviewDate)

 

redcap_date_base = merge(base_redcap, SPARS_date_base, by = "record_id", all.y = TRUE)

head(redcap_date_base)

dim(base_redcap)

 

#CHANGE DATE to correct year and stuff

base_redcap = subset(redcap_date_base, InterviewDate < "2020-07-01")

dim(base_redcap)

 

#CHANGE "id" and "record_id" to "id_fhhc" so you can merge based on that variable
 

colnames(base_redcap)[1] = "id_fhhc"
colnames(month6_redcap)[5] = "id_fhhc"
dim(month6_redcap)
base_redcap$id

redcap_data = merge(base_redcap, month6_redcap, by = "id_fhhc", all.y = TRUE)

head(redcap_data)

dim(redcap_data)

```

 

Obj. A: Reduce mental health symptomatology by 50% for enrollees with SMI per 6-month and discharge follow-ups.

Use PHQ-9 and GAD-7 mean scores

```{r}

#Depression base_redcap

base_redcap_depression = redcap_data[,29:37]

head(base_redcap_depression)

## number of people

dim(base_redcap_depression)

sum(is.na(base_redcap_depression))

base_redcap_depression$PHQ_9_Total = apply(base_redcap_depression, 1, sum, na.rm = TRUE)

mean(base_redcap_depression$PHQ_9_Total)

 

## Depression follow-up

month6_redcap_depression = redcap_data[,140:148]

head(month6_redcap_depression)

### number of people

dim(month6_redcap_depression)

sum(is.na(month6_redcap_depression))

month6_redcap_depression$PHQ_9_Total = apply(month6_redcap_depression, 1, sum, na.rm = TRUE)

mean(month6_redcap_depression$PHQ_9_Total)

 

#percent change

p_change_phq =  (mean(month6_redcap_depression$PHQ_9_Total)-mean(base_redcap_depression$PHQ_9_Total))/mean(base_redcap_depression$PHQ_9_Total)


 
##################################################
 

#Anxiety base_redcap

 

base_redcap_anxiety = redcap_data[,41:47]

head(base_redcap_anxiety)

 

## number of people

dim(base_redcap_anxiety)

sum(is.na(base_redcap_anxiety))

 

#base_redcapline mean score

base_redcap_anxiety$Gad_7_Total = apply(base_redcap_anxiety, 1, sum, na.rm = TRUE)

mean(base_redcap_anxiety$Gad_7_Total)

 

## Anxiety follow-up

month6_redcap_anxiety = redcap_data[,152:158]

head(month6_redcap_anxiety)

 

### number of people

dim(month6_redcap_anxiety)

sum(is.na(month6_redcap_anxiety))

 

#follow-up mean score

month6_redcap_anxiety$Gad_7_Total = apply(month6_redcap_anxiety, 1, sum, na.rm = TRUE)


anx_results = data.frame(dep_mean_base_redcap = mean(base_redcap_anxiety$Gad_7_Total), dep_mean_month6_redcapmean = mean(month6_redcap_anxiety$Gad_7_Total), n = dim(base_redcap_anxiety)[1], pchange_anx = (mean(month6_redcap_anxiety$Gad_7_Total-mean(base_redcap_anxiety$Gad_7_Total))/mean(base_redcap_anxiety$Gad_7_Total)))
round(anx_results,3)
 

phq_results = data.frame(mean_phq_base = mean(base_redcap_depression$PHQ_9_Total), mean_phq_month6 = mean(month6_redcap_depression$PHQ_9_Total), p_change_phq , n = dim(base_redcap_depression)[1])
round(phq_results,3)


```

 

Obj. B: Increase abstinence vs. past 30-day substance use among 70% of enrolled clients with SUD/COD per 6-month

and discharge follow-ups.

Drug/Alc use questions in NOMS

 

Tobacco_Use

Alcohol_Use

Cannabis_Use

 

1 = Never

2 = Once or Twice

3 = Weekly

4 = Daily or Almost Daily

```{r}

#Compare % abstaining at baseline vs. month 6

 

#TOBACCO USE at BASELINE

 
dim(SPARS_wide)
describe.factor(SPARS_wide$Tobacco_Use.x)


tob_abs_base = ifelse(SPARS_wide$Tobacco_Use.x == 1, 1 ,0) 
tob_abs_n_base = sum(tob_abs_base)

#TOBACCO USE at MONTH6


describe.factor(SPARS_wide$Tobacco_Use.y)

tob_abs_month6 = ifelse(SPARS_wide$Tobacco_Use.y == 1, 1, 0) 
tob_abs_n_month6 = sum(tob_abs_month6)

p_change_tob_abs = (mean(tob_abs_month6) -  mean(tob_abs_base)) / mean(tob_abs_base)

tob_abs_results  = round(data.frame(tob_abs_base = mean(tob_abs_base), tob_abs_n_base, tob_abs_month6 = mean(tob_abs_month6), tob_abs_n_month6, p_change_tob_abs),2) 
tob_abs_results

#AlCOHOL USE at BASELINE

describe.factor(SPARS_wide$Alcohol_Use.x)

alc_abs_base = ifelse(SPARS_wide$Alcohol_Use.x == 1, 1 ,0) 
alc_abs_n_base = sum(alc_abs_base)
#Alcohol USE at MONTH6


describe.factor(SPARS_wide$Alcohol_Use.y)

alc_abs_month6 = ifelse(SPARS_wide$Alcohol_Use.y == 1, 1, 0) 
alc_abs_n_month6 = sum(alc_abs_month6)

p_change_alc_abs = (mean(alc_abs_month6) -  mean(alc_abs_base)) / mean(alc_abs_base)

alc_abs_results  = round(data.frame(alc_abs_base = mean(alc_abs_base), alc_abs_n_base, alc_abs_month6 = mean(alc_abs_month6), alc_abs_n_month6, p_change_alc_abs),2) 
alc_abs_results

 
#Cannabis_Use USE at BASELINE

describe.factor(SPARS_wide$Cannabis_Use.x)

can_abs_base = ifelse(SPARS_wide$Cannabis_Use.x == 1, 1 ,0) 
can_abs_n_base = sum(can_abs_base)
#Cannabis_Use USE at MONTH6


describe.factor(SPARS_wide$Cannabis_Use.y)

can_abs_month6 = ifelse(SPARS_wide$Cannabis_Use.y == 1, 1, 0) 
can_abs_n_month6 = sum(can_abs_month6)

p_change_can_abs = (mean(can_abs_month6) -  mean(can_abs_base)) / mean(can_abs_base)

can_abs_results  = round(data.frame(can_abs_base = mean(can_abs_base), can_abs_n_base, can_abs_month6 = mean(can_abs_month6), can_abs_n_month6, p_change_can_abs),2) 
can_abs_results


## GSR
sub_base_mean = mean(tob_abs_results$tob_abs_base, alc_abs_results$alc_abs_base, can_abs_results$can_abs_base)
sub_base_mean

sub_month_6_mean = mean(tob_abs_results$tob_abs_month6, alc_abs_results$alc_abs_month6, can_abs_results$can_abs_month6)
sub_month_6_mean


p_change_sub = (sub_month_6_mean-sub_base_mean)/sub_base_mean
p_change_sub

sub_results = data.frame(sub_base_mean, sub_month_6_mean , p_change_sub, n = dim(SPARS_wide)[1])
sub_results
```

 

Obj. C: Reduce symptoms of trauma by 50% for enrollees who screen positive for trauma-related conditions per 6-

month and discharge follow-ups.

Use PLC-C mean scores

```{r}

#plc base_redcap


base_redcap_plc = redcap_data[,50:66]

head(base_redcap_plc)

 

## number of people

dim(base_redcap_plc)

sum(is.na(base_redcap_plc))

 

#base_redcapline mean score

base_redcap_plc$Plc_Total = apply(base_redcap_plc, 1, sum, na.rm = TRUE)

mean(base_redcap_plc$Plc_Total)

 

## PLC follow-up

month6_redcap_plc = redcap_data[,161:177]

head(month6_redcap_plc)

 

### number of people

dim(month6_redcap_plc)

sum(is.na(month6_redcap_plc))

 

#follow-up mean score

month6_redcap_plc$Plc_Total = apply(month6_redcap_plc, 1, sum, na.rm = TRUE)

mean(month6_redcap_plc$Plc_Total)

 

#percent change

 

p_change_plc = (mean(month6_redcap_plc$Plc_Total)-mean(base_redcap_plc$Plc_Total))/mean(base_redcap_plc$Plc_Total)

p_change_plc

 

 

plc_results = data.frame(plc_base = mean(base_redcap_plc$Plc_Total), plc_month6 = mean(month6_redcap_plc$Plc_Total), p_change = p_change_plc)

round(plc_results,3)

```

 

Obj. D: By 9/29/2023, reduce service/utilization costs related to SMI/COD issues (inpatient hospitalization,

emergency room visits, etc.) by 50%.

Use ER usage and Hospitalizations

Because you are summing you can using all the pre and post data so the N is the N is the matched dataset.

```{r}

#ER Visits

SPARS_wide_ER = data.frame(base_redcap_ER = SPARS_wide$TimesER.x, month6_redcap_ER = SPARS_wide$TimesER.y)

dim(SPARS_wide)
SPARS_wide_ER_complete = na.omit(SPARS_wide_ER)

#number of people

dim(SPARS_wide_ER_complete)

head(SPARS_wide_ER)

#base_redcap

sum(SPARS_wide_ER$base_redcap_ER)

#Follow-up

sum(SPARS_wide_ER$month6_redcap_ER)


er_results = data.frame(base_er = sum(SPARS_wide_ER$base_redcap_ER), month_6_er = sum(SPARS_wide_ER$month6_redcap_ER), p_change_er = (sum(SPARS_wide_ER$month6_redcap_ER)-sum(SPARS_wide_ER$base_redcap_ER))/sum(SPARS_wide_ER$base_redcap_ER))
 

SPARS_wide_ER_complete

 

#Nights Hospital MHC

SPARS_wide_hospital = data.frame(base_redcap_Hosp = SPARS_wide$NightsHospitalMHC.x, month6_redcap_Hosp = SPARS_wide$NightsHospitalMHC.y)

SPARS_wide_hospital_complete = na.omit(SPARS_wide_hospital)

 

# number of people

 

dim(SPARS_wide_hospital_complete)

head(SPARS_wide_hospital)

 

#sum base_redcapline

 

sum(SPARS_wide_hospital$base_redcap_Hosp)

 

#sum month 6

sum(SPARS_wide_hospital$month6_redcap_Hosp)

 



#sum hospitalizations for mental health and ER visits

 

#base_redcap ER and hosp

 

base_redcap_er_hosp = sum(SPARS_wide_ER$base_redcap_ER, SPARS_wide_hospital$base_redcap_Hosp)

 

#follow-up ER and hosp

 

month6_redcap_er_hosp = sum(SPARS_wide_ER$month6_redcap_ER, SPARS_wide_hospital$month6_redcap_Hosp)

 

p_change_ER_hosp = (month6_redcap_er_hosp- base_redcap_er_hosp)/base_redcap_er_hosp

p_change_ER_hosp



ER_hosp = data.frame(base_ER_hosp = base_redcap_er_hosp, month6_ER_hosp = month6_redcap_er_hosp, percentchange = p_change_ER_hosp)

ER_hosp


er_results = data.frame(base_er = sum(SPARS_wide_ER$base_redcap_ER), month_6_er = sum(SPARS_wide_ER$month6_redcap_ER), p_change_er = (sum(SPARS_wide_ER$month6_redcap_ER)-sum(SPARS_wide_ER$base_redcap_ER))/sum(SPARS_wide_ER$base_redcap_ER), n = dim(SPARS_wide)[1])

er_results

 
hosp_results = data.frame(hosp_base = sum(SPARS_wide_hospital$base_redcap_Hosp), hosp_month6 = sum(SPARS_wide_hospital$month6_redcap_Hosp), p_change_hosp = (sum(SPARS_wide_hospital$month6_redcap_Hosp)-sum(SPARS_wide_hospital$base_redcap_Hosp))/sum(SPARS_wide_hospital$base_redcap_Hosp))
hosp_results

ER_hosp = data.frame(base_ER_hosp = base_redcap_er_hosp, month6_ER_hosp = month6_redcap_er_hosp, percentchange = p_change_ER_hosp, n = dim(SPARS_wide)[1])

ER_hosp

```

 

Obj. E: Reduce past 30-day involvement with the criminal justice system (e.g., arrest, re-arrest, re-conviction) among

60% of enrollees with criminal justice histories per 6-month and discharge follow-ups.

 

Use variable "NumTimesArrested" from SPARS

```{r}
base_arrests = SPARS_wide$NumTimesArrested.x

base_arrests = na.omit(base_arrests)

 

 

month6_arrests = SPARS_wide$NumTimesArrested.y

month6_arrests = na.omit(month6_arrests)

 

 

SPARS_wide_arrests = data.frame(base_arrests = SPARS_wide$NumTimesArrested.x, month6_arrests = SPARS_wide$NumTimesArrested.y)

SPARS_wide_arrests = na.omit(SPARS_wide_arrests)

sum(is.na(SPARS_wide_arrests))

SPARS_wide_arrests

 

sum_base_arrests = sum(base_arrests)

sum_month6_arrests = sum(month6_arrests)

 

p_change_arrests = (sum_month6_arrests - sum_base_arrests)/sum_base_arrests

 

arrests = data.frame(base_arrests = sum_base_arrests, month6_arrests = sum_month6_arrests, p_change_arrests = p_change_arrests)

arrests

```

 

Obj. F: Increase client access to treatment/services, including trauma-focused services, by 90% by 9/29/2023.

 

Use NOMS variables "Svc_CaseManagement", "Svc_MentalHealth", "Svc_TreatmentPlanning" "Svc_TraumaSpecific", "Svc_Transportation", "Svc_Housing", "Svc_Employment", "Svc_Family", "Svc_ChildCare",  (asks if client has received X service since last NOMS interview)

0 = no, 1 = yes

We don't ask about these at baseline, so just show what services people are getting at 6month

```{r}

 

#we don't ask about services during baseline NOMS!

 

#report % of people receiving each service

 

#MONTH6 Trauma Services

describe.factor(SPARS_wide$Svc_CaseManagement.y)

describe.factor(SPARS_wide$Svc_MentalHealth.y)

describe.factor(SPARS_wide$Svc_TreatmentPlanning.y)

describe.factor(SPARS_wide$Svc_TraumaSpecific.y)

describe.factor(SPARS_wide$Svc_Transportation.y)

describe.factor(SPARS_wide$Svc_Housing.y)

describe.factor(SPARS_wide$Svc_Employment.y)

describe.factor(SPARS_wide$Svc_Family.y)

describe.factor(SPARS_wide$Svc_ChildCare.y)

```

 

Obj. G: Increase recovery capital among 90% of enrollees by 9/29/2023.

Use BARC-10 mean scores

```{r}

#Barc base_redcap

base_redcap_barc = base_redcap[,88:97]

base_redcap_barc = na.omit(base_redcap_barc)

head(base_redcap_barc)

 

## number of people

dim(base_redcap_barc)

base_redcap_barc = na.omit(base_redcap_barc)

sum(is.na(base_redcap_barc))

 

#base_redcap mean score

base_redcap_barc$Barc_Total = rowSums(base_redcap_barc)

mean(base_redcap_barc$Barc_Total)

mean_base_barc = mean(base_redcap_barc$Barc_Total)

 

## Barc follow-up

month6_redcap_barc = month6_redcap[,84:93]

head(month6_redcap_barc)

 

### number of people

dim(month6_redcap_barc)

month6_redcap_barc = na.omit(month6_redcap_barc)

sum(is.na(month6_redcap_barc))

 

#follow-up mean score

month6_redcap_barc$Barc_Total = rowSums(month6_redcap_barc)

mean_month6_barc = mean(month6_redcap_barc$Barc_Total)

mean_month6_barc

 

 

p_change_barc = (mean_month6_barc-mean_base_barc)/mean_base_barc

p_change_barc

 

barc = data.frame(base_barc = mean_base_barc, month6_barc = mean_month6_barc, p_change_barc = p_change_barc)

round(barc,3)
```

 

Obj. H: Achieve 80% client retention rate per 6-month and discharge follow-ups.

Number of enrollees eligible for follow-up that have been seen for 6 month (based on date calculated --> don't count people that have not yet reached reassessment window)

USE SPARS RATE
 
 

Goal IV: Increase permanent housing and other services that support recovery for clients.

 

 

Obj. A: Provide housing navigation and CES linkage to 100% of enrollees per 6-month and discharge follow-ups.

Use variable "Svc_Housing" from SPARS (asks about housing services since last NOMS interview)

```{r}

 

describe.factor(SPARS_wide$Svc_Housing.y)
61/93
 
```

 

Obj. B: Place 80% of enrollees in permanent housing by 9/29/2023.

Use participant tracker --> number housed/number enrolled


 

Obj. C: As appropriate to their ICPs, assist 100% of enrollees to identify/secure employment per 6-month and discharge follow-ups.

Use "did you receive help getting a job" question from REDCap?

 

Most people say "no" to this, so I'm not sure if employment typically isnt a main concern for our clients or what

```{r}
head(redcap_data)

month6_job_help = data.frame(need_help_job = redcap_data$did_you_receive_help_getti, get_help_job = redcap_data$job_help)
month6_job_help

month6_job_help = na.omit(month6_job_help)
month6_job_help = subset(month6_job_help, need_help_job == 1)

need_job_help = dim(month6_job_help)[1]

received_job_help = sum(month6_job_help$get_help_job)

objC_results = data.frame(need_job_help, received_job_help, percent = round(received_job_help / need_job_help,3))
objC_results
```

 

Obj. D: Increase enrollment in health insurance, Medicaid, VA, SSI/SSDI, other benefit programs by 80% for clients

in need of/eligible for these benefits by 9/29/2023.

Use Benefits Tracking in REDCap

```{r}

#benefit at base, benefit at month 6, then percent change from base to month 6

 

#MEDICAID OR MEDICARE

 

#BASELINE

medicaid_all = redcap_data[c("medicaid_or_medicare.x", "medicaid_or_medicare.y")] 
medicaid_all = na.omit(medicaid_all)
# n 
dim(medicaid_all)[1]

base_medicaid = medicaid_all$medicaid_or_medicare.x

base_medicaid = base_medicaid

sum_base_med = sum(base_medicaid)

 

#MONTH6

 

month6_medicaid = medicaid_all$medicaid_or_medicare.y

month6_medicaid = month6_medicaid

sum_month6_med = sum(month6_medicaid)

 

p_change_med = (sum_month6_med - sum_base_med)/sum_base_med

 

medicaid_or_medicare = data.frame(base = sum_base_med, month6 = sum_month6_med, percentchange = p_change_med)

 

medicaid_or_medicare

 

 

#HIP
hip_all = redcap_data[c("healthy_indiana_plan_hip.x", "healthy_indiana_plan_hip.y")] 
hip_all = na.omit(hip_all)

# n
dim(hip_all)

#BASELINE

 

base_hip = hip_all$healthy_indiana_plan_hip.x

base_hip = na.omit(base_hip)

sum_base_hip = sum(base_hip)

 

 

#MONTH6

month6_hip = hip_all$healthy_indiana_plan_hip.y

month6_hip = na.omit(month6_hip)

sum_month6_hip = sum(month6_hip)

 

p_change_hip = (sum_month6_hip - sum_base_hip)/sum_base_hip

 

hip = data.frame(base = sum_base_hip, month6 = sum_month6_hip, percentchange = p_change_hip)

 

hip

 

#SSI or SSDI

 

#BASELINE

ssi_all = redcap_data[c("ssi_or_ssdi.x", "ssi_or_ssdi.y")]
ssi_all = na.omit(ssi_all)
#n
dim(ssi_all)
base_ssi = redcap_data$ssi_or_ssdi.x

base_ssi = na.omit(base_ssi)

sum_base_ssi = sum(base_ssi)

 
length(base_ssi)
#MONTH 6

month6_ssi = redcap_data$ssi_or_ssdi.y

month6_ssi = na.omit(month6_ssi)
length(month6_ssi)

sum_month6_ssi = sum(month6_ssi)

 

 

p_change_ssi = (sum_month6_ssi - sum_base_ssi)/sum_base_ssi

 

ssi_or_ssdi = data.frame(base = sum_base_ssi, month6 = sum_month6_ssi, percentchange = p_change_ssi)

 

ssi_or_ssdi

 

 

#HUD

 

#BASELINE

hud_all =  redcap_data[c("hud_or_section_8.x", "hud_or_section_8.y")]
hud_all = na.omit(hud_all)
#n
dim(hud_all)
base_hud = redcap_data$hud_or_section_8.x

base_hud = na.omit(base_hud)

sum_base_hud = sum(base_hud)

 

#MONTH6

 

month6_hud = redcap_data$hud_or_section_8.y

month6_hud = na.omit(month6_hud)

sum_month6_hud = sum(month6_hud)

 

p_change_hud = (sum_month6_hud - sum_base_hud)/sum_base_hud

 

hud = data.frame(base = sum_base_hud, month6 = sum_month6_hud, percentchange = p_change_hud)

 

hud

 

 

#SNAP

 

#BASE

snap_all =  redcap_data[c("snap_food_stamps.x", "snap_food_stamps.y")]
snap_all = na.omit(snap_all)

#n
dim(snap_all)
base_snap = redcap_data$snap_food_stamps.x

base_snap = na.omit(base_snap)

sum_base_snap = sum(base_snap)

 

#MONTH6

month6_snap = redcap_data$snap_food_stamps.y

month6_snap = na.omit(month6_snap)

sum_month6_snap = sum(month6_snap)

 

p_change_snap = (sum_month6_snap - sum_base_snap)/sum_base_snap

 

snap = data.frame(base = sum_base_snap, month6 = sum_month6_snap, percentchange = p_change_snap)

 

snap

 

 

#PERCENT CHANGE OF ALL BENEFITS

 

benefits_percent_change = data.frame(medicaid_medicare = p_change_med, hip = p_change_hip, ssi_ssdi = p_change_ssi, hud = p_change_hud, snap = p_change_snap)

 

benefits_percent_change

```

 

Obj. E: Increased social connectedness among 80% of enrollees per 6-month and discharge follow-ups.

Use social relationships section of SPARS data

variables = Friendships, EnjoyPeople, BelongInCommunity, SupportFromFamily, SupportiveFamilyFriends, GenerallyAccomplishGoal

 

1 = Strong Disagree

2 = Disagree

4 = Agree

5 = Strongly Agree

 

Is there a better way to look at this data?

```{r}

#mean and then percent change of each variable

#do rowsum in new data set, %change over all

 

#Baseline

 

describe.factor(SPARS_wide$Friendships.x)

describe.factor(SPARS_wide$EnjoyPeople.x)

describe.factor(SPARS_wide$BelongInCommunity.x)

describe.factor(SPARS_wide$SupportFromFamily.x)

describe.factor(SPARS_wide$SupportiveFamilyFriends.x)

 

#Month6

describe.factor(SPARS_wide$Friendships.y)

describe.factor(SPARS_wide$EnjoyPeople.y)

describe.factor(SPARS_wide$BelongInCommunity.y)

describe.factor(SPARS_wide$SupportFromFamily.y)

describe.factor(SPARS_wide$SupportiveFamilyFriends.y)

 

#Friendships

Friendships = data.frame(x = SPARS_wide$Friendships.x, y = SPARS_wide$Friendships.y)

Friendships = na.omit(Friendships)

dim(Friendships)

 

BaseMean = round(mean(Friendships$x),3)

Month6Mean = round(mean(Friendships$y),3)

Friend_change = round(( Month6Mean- BaseMean)/ BaseMean,3)

 

Friend_results = data.frame(N = dim(Friendships)[1], BaseMean, Month6Mean, Friend_change)

Friend_results

mean(Friend_change)

 

#Enjoy People

Enjoy = data.frame(x = SPARS_wide$EnjoyPeople.x, y = SPARS_wide$EnjoyPeople.y)

Enjoy = na.omit(Enjoy)

dim(Enjoy)

 

BaseMean = round(mean(Enjoy$x),3)

Month6Mean = round(mean(Enjoy$y),3)

Enjoy_change = round((Month6Mean - BaseMean)/ BaseMean,3)

 

Enjoy_results = data.frame(N = dim(Enjoy)[1], BaseMean, Month6Mean, Enjoy_change)

Enjoy_results

mean(Enjoy_change)

 

#Belong in Community

Belong = data.frame(x = SPARS_wide$BelongInCommunity.x, y = SPARS_wide$BelongInCommunity.y)

Belong = na.omit(Belong)

dim(Belong)

 

BaseMean = round(mean(Belong$x),3)

Month6Mean = round(mean(Belong$y),3)

Belong_change = round((mean(Belong$y) - mean(Belong$x))/ mean(Belong$x,3))

 

Belong_results = data.frame(N = dim(Belong)[1], BaseMean, Month6Mean, Belong_change)

Belong_results

mean(Enjoy_change)

 

#Support form Family

Support = data.frame(x = SPARS_wide$SupportFromFamily.x, y = SPARS_wide$SupportFromFamily.y)

Support = na.omit(Support)

dim(Support)

 

BaseMean = round(mean(Support$x),3)

Month6Mean = round(mean(Support$y),3)

Support_change = round((Support$y - Support$x)/ Support$x,3)

 

Support_results = data.frame(N = dim(Support)[1], BaseMean, Month6Mean, Support_change)

Support_results

mean(Support_change)

 

#Supportive Family and Friends

Supportive = data.frame(x = SPARS_wide$SupportiveFamilyFriends.x, y = SPARS_wide$SupportiveFamilyFriends.y)

Supportive = na.omit(Supportive)

dim(Supportive)

 

BaseMean = round(mean(Supportive$x),3)

Month6Mean = round(mean(Supportive$y),3)

Supportive_change = round((Supportive$y - Supportive$x)/ Supportive$x,3)

 

Supportive_results = data.frame(N = dim(Supportive)[1], BaseMean, Month6Mean, Supportive_change)

Supportive_results

mean(Supportive_change)

 

#Overall

Social.x = data.frame(ID = SPARS_wide$ConsumerID, Friends.x=SPARS_wide$Friendships.x, Family.x=SPARS_wide$SupportFromFamily.x, Belong.x=SPARS_wide$BelongInCommunity.x, Supportive.x=SPARS_wide$SupportiveFamilyFriends.x, Enjoy.x=SPARS_wide$EnjoyPeople.x)

Social.x = na.omit(Social.x)

 

Social.y = data.frame(ID = SPARS_wide$ConsumerID, Friends.y=SPARS_wide$Friendships.y, Family.y=SPARS_wide$SupportFromFamily.y, Belong.y=SPARS_wide$BelongInCommunity.y, Supportive.y=SPARS_wide$SupportiveFamilyFriends.y, Enjoy.y=SPARS_wide$EnjoyPeople.y)

Social.y = na.omit(Social.y)

 

Social.x$sum = rowSums(Social.x[, -1])

Social.y$sum = rowSums(Social.y[, -1])

 

Social = merge(Social.x, Social.y, by = "ID", all.y = TRUE)

SocialBaseMean = round(mean(Social.x$sum),3)

SocialMonth3Mean = round(mean(Social.y$sum),3)

Social_p_change = round((SocialMonth3Mean - SocialBaseMean)/ SocialBaseMean,3)

 

Social_results = data.frame(N = dim(Social)[1], SocialBaseMean, SocialMonth3Mean, Social_p_change)

Social_results

```

 

Obj. F: Improve independent living skills among 80% of enrollees per 6-month and discharge follow-ups.

 

Use variable "CapableManagingHealthCareNeeds" from SPARS

 

" I feel capable of managing my health care needs..."

 

1 = On my own most of the time

2 = On my own some of the time and with support from

others some of the time

3 = With support from others most of the time

4 = Rarely or never

```{r}

#do same thing as social connectedness part

#OR use generally accomplish what I set out to do

 

#independent living

Capable = data.frame(x = SPARS_wide$CapableManagingHealthCareNeeds.x, y = SPARS_wide$CapableManagingHealthCareNeeds.y)

Supportive = na.omit(Capable)

dim(Capable)

 

BaseMean = round(mean(Capable$x),3)

Month6Mean = round(mean(Capable$y),3)

Capable_change = round((Month6Mean - BaseMean)/ BaseMean,3)

Capable_results = data.frame(N = dim(Capable)[1], BaseMean, Month6Mean, Capable_change)

Capable_results

mean(Capable_change)

 

#Generally Accomplish living

Accomplish = data.frame(x = SPARS_wide$GenerallyAccomplishGoal.x, y = SPARS_wide$GenerallyAccomplishGoal.y)

Accomplish  = na.omit(Accomplish)

dim(Accomplish)

 

BaseMean = round(mean(Accomplish$x),3)

Month6Mean = round(mean(Accomplish$y),3)

Accomplish_change = round((Month6Mean - BaseMean)/ Month6Mean,3)

Accomplish_results = data.frame(N = dim(Accomplish)[1], BaseMean, Month6Mean, Accomplish_change)

Accomplish_results

mean(Accomplish_change)

 

#Overall

Independent.x = data.frame(ID = SPARS_wide$ConsumerID, Capable.x=SPARS_wide$CapableManagingHealthCareNeeds.x, Accomplish=SPARS_wide$GenerallyAccomplishGoal.x)

Independent.x = na.omit(Independent.x)

 

Independent.y = data.frame(ID = SPARS_wide$ConsumerID, Capable.x=SPARS_wide$CapableManagingHealthCareNeeds.y, Accomplish=SPARS_wide$GenerallyAccomplishGoal.y)

Independent.y = na.omit(Independent.y)

 

Independent.x$sum = rowSums(Independent.x[, -1])

Independent.y$sum = rowSums(Independent.y[, -1])

 

Independent = merge(Independent.x, Independent.y, by = "ID", all.y = TRUE)

IndependentBaseMean = round(mean(Independent.x$sum),3)

IndependentMonth3Mean = round(mean(Independent.y$sum),3)

Independent_p_change = round((IndependentMonth3Mean -IndependentBaseMean)/ IndependentBaseMean,3)

 

Independent_results = data.frame(N = dim(Independent)[1], IndependentBaseMean, IndependentMonth3Mean, Independent_p_change)

Independent_results
```
Goal V and Spars Demos
Demos and top diagnoses
59 = F33 – Major depressive disorder, recurrent

62 = F40-F48 – Anxiety, dissociative, stress-related, somatoform and other nonpsychotic mental disorders 

57 = F31 – Bipolar disorder

1 = MALE
2 = FEMALE
3 = TRANSGENDER
4 = OTHER (SPECIFY)

11 = LESS THAN 12TH GRADE
12 = 12TH GRADE /HIGH SCHOOL DIPLOMA/ EQUIVALENT (GED)
13 = VOC/TECH DIPLOMA
14 = SOME COLLEGE OR UNIVERSITY
15 = BACHELOR'S DEGREE (BA, BS)
16 = GRADUATE WORK/GRADUATE DEGREE

2 = Age 10 to 12 years old
3 = Age 13 to 15 years old
4 = Age 16 to 25 years old
5 = Age 26 to 34 years old
6 = Age 35 to 44 years old
7 = Age 45 to 54 years old
8 = Age 55 to 64 years old
9 = Age 65 to 74 years old
10 = Age 75 to 84 years old
11 = Age 85 to 94 years old
12 = Age 95 years or older


```{r}
describe.factor(SPARS_base$Gender)

describe.factor(SPARS_base$DiagnosisOne)

describe.factor(SPARS_base$RaceWhite)
describe.factor(SPARS_base$RaceBlack)
describe.factor(SPARS_base$HispanicLatino)
describe.factor(SPARS_base$RaceAsian)


describe.factor(SPARS_base$Education)
describe.factor(SPARS_base$Agegroup)
SPARS_base$DOB = mdy(SPARS_base$DOB)
age =  (SPARS_base$InterviewDate - SPARS_base$DOB ) /365
mean(age) 
sd(age)

 44.08602+22.58065

```
Number of people with insurance
Copy the data from the insurance tab.  Then delete the year lines.
unknown means missing.
Include only those who have entered the program within the last 6 months.

1. Client did not lose insurance from enrollment to the quarter check
2. If client did not have insurance at intake, but did at the quarter check
```{r}
library(lubridate)
setwd("S:/Indiana Research & Evaluation/FHHC Homelessness/Data and QPR")
fhhc_insurance_y2_q2 = read.csv("fhhc_insurance_y2_q2.csv", header = TRUE)

### subset for those clients who have been in the program for at least six months
fhhc_insurance_y2_q2$Enrollment.Date = mdy(fhhc_insurance_y2_q2$Enrollment.Date)
fhhc_insurance_y2_q2$Enrollment.Date
fhhc_insurance_y2_q2 = subset(fhhc_insurance_y2_q2, Enrollment.Date < "2019-10-01")
dim(fhhc_insurance_y2_q2)

fhhc_insurance_y2_q2$not_lose_insurance = ifelse(fhhc_insurance_y2_q2$Insurance.at.enrollment.bin && fhhc_insurance_y2_q2$Insurance.Y.N.at.6.months == 1, 1, 0)


## Check
fhhc_insurance_y2_q2_check = fhhc_insurance_y2_q2[c("Insurance.at.enrollment.bin", "Insurance.Y.N.at.6.months", "not_lose_insurance")]
fhhc_insurance_y2_q2_check

fhhc_insurance_y2_q2$mro_improve = ifelse(fhhc_insurance_y2_q2$MRO.at.Enrollment == 0 && fhhc_insurance_y2_q2$MRO.Y.N.at.6.months == 1, 1,0)
describe.factor(fhhc_insurance_y2_q2$mro_improve)
fhhc_insurance_y2_q2

54/60

```

