---
title: "FHHC Report"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Getting the matched pairs for FHHC data need to get rid of the '' on the id
```{r}
library(prettyR)
library(psych)

#setwd("S:/Indiana Research & Evaluation/FHHC Homelessness/Data and QPR")
#base = read.csv("FHHC_base.csv", header = TRUE, na.strings = c(-99, -98, -97, " "))
#month6 = read.csv("FHHC_Month6.csv", header = TRUE, na.string = c(-99, -98, -97, " "))
#FHHC = read.csv("FHHC.csv", header = TRUE, na.string = c(-99, -98, -97, -1, -4, -5, -7, -9, -2, -6 -8, " "))

#Get matched pairs in FHHC data 

dim(FHHC)
FHHC = FHHC[order(FHHC$ConsumerID),]
FHHC$ConsumerID = gsub("\\D", "",FHHC$ConsumerID)
FHHC$ConsumerID = as.numeric(FHHC$ConsumerID)
FHHC$InterviewDate = mdy(FHHC$InterviewDate)
dim(FHHC)
FHHC =subset(FHHC, FHHC$InterviewDate < "2019/07/01")
FHHC_base = subset(FHHC, FHHC$InterviewType_07 == 1)
FHHC_Month6 = subset(FHHC, FHHC$InterviewType_07 == 3)
dim(FHHC_Month6)

FHHC_wide = merge(FHHC_base, FHHC_Month6, by = "ConsumerID", all.y = TRUE)
dim(FHHC_wide)
FHHC_wide$ConsumerID == FHHC_Month6$ConsumerID
```


Get date from SPARS / NOMS / GPRA and put into redcap base and redcap month6
Need to get the date from FHHC data for the RedCap 6month data, because we don't have it in the RedCap 6month data set.
```{r}
library(lubridate)
typeof(FHHC$ConsumerID)
date_base = data.frame(record_id =  FHHC_base$ConsumerID,  InterviewDate = FHHC_base$InterviewDate)

redcap_date_base = merge(base, date_base, by = "record_id", all.x = TRUE)
dim(redcap_date_base)
head(redcap_date_base)


###### Now six months
date_Month6 = data.frame(record_id =  FHHC_Month6$ConsumerID,  InterviewDate = FHHC_Month6$InterviewDate)
dim(date_Month6)
dim(month6)
month6$record_id = month6$id

redcap_date_Month6 = merge(month6, date_Month6, by = "record_id", all.y = TRUE)
dim(redcap_date_Month6)


redcap_data= merge(redcap_date_base, redcap_date_Month6, by = "record_id", all.y = TRUE)
dim(redcap_data)

```


Descriptives
```{r}
library(prettyR)
#1 = male; 2 = female
describe.factor(FHHC_base$Gender)

#RACE
describe.factor(FHHC_base$RaceWhite)
describe.factor(FHHC_base$RaceBlack)
describe.factor(FHHC_base$HispanicLatino)
describe.factor(FHHC_base$RaceAsian)

#12 = 12TH GRADE /HIGH SCHOOL DIPLOMA/ EQUIVALENT (GED)
#13 = VOC/TECH DIPLOMA
#14 = SOME COLLEGE OR UNIVERSITY
#15 = BACHELOR'S DEGREE (BA, BS)
#16 = GRADUATE WORK/GRADUATE DEGREE
describe.factor(FHHC_base$Education)

#4 = Age 16 to 25 years old
#5 = Age 26 to 34 years old
#6 = Age 35 to 44 years old
#7 = Age 45 to 54 years old
#8 = Age 55 to 64 years old
#9= 65 to 74 years old
#10 = Age 75 to 84 years old
#11 = Age 85 to 94 years old
#12 = Age 95 years or older
describe.factor(FHHC_base$Agegroup)
mean(FHHC_base$Agegroup)
#multiply by decimal of mean of age group

44-((44-35)*.68)

#Diagnosis FHHC$DiagnosisOne
```


Depression (mean score)
```{r}
#Depression base
base_depression = redcap_data[,27:35]
head(base_depression)
## number of people
dim(base_depression)
sum(is.na(base_depression))
base_depression$PHQ_9_Total = rowSums(base_depression)
mean(base_depression$PHQ_9_Total)

## Depression follow-up
month6_depression = redcap_data[138:146]
### number of people
dim(month6_depression)
sum(is.na(month6_depression))
month6_depression$PHQ_9_Total = rowSums(month6_depression)
mean(month6_depression$PHQ_9_Total)
```


ER Visits (count)
```{r}
#ER Visits
FHHC_wide_ER = data.frame(Base_ER = FHHC_wide$TimesER.x, Month6_ER = FHHC_wide$TimesER.y)
FHHC_wide_ER_complete = na.omit(FHHC_wide_ER)
#number of people
dim(FHHC_wide_ER_complete)
head(FHHC_wide_ER)
#Base
sum(FHHC_wide_ER$Base_ER)
#Follow-up
sum(FHHC_wide_ER$Month6_ER)

```
Hospitalizations (count)
```{r}
#Nights Hospital MHC
FHHC_wide_hospital = data.frame(Base_Hosp = FHHC_wide$NightsHospitalMHC.x, Month6_Hosp = FHHC_wide$NightsHospitalMHC.y)
FHHC_wide_hospital_complete = na.omit(FHHC_wide_hospital)

# number of people

dim(FHHC_wide_hospital_complete)
head(FHHC_wide_hospital)

#sum baseline

sum(FHHC_wide_hospital$Base_Hosp)

#sum month 6
sum(FHHC_wide_hospital$Month6_Hosp)

```

Anxiety GAD 7 (mean score)
```{r}
#Anxiety baseline

base_anxiety = base[,39:45]
head(base_anxiety)

## number of people
dim(base_anxiety)
sum(is.na(base_anxiety))

#baseline mean score
base_anxiety$Gad_7_Total = rowSums(base_anxiety)
View(base_anxiety$Gad_7_Total)
mean(base_anxiety$Gad_7_Total)

## Anxiety follow-up
month6_anxiety = month6[,40:46]
head(month6_anxiety)

### number of people
dim(month6_anxiety)
sum(is.na(month6_anxiety))

#follow-up mean score
month6_anxiety$Gad_7_Total = rowSums(month6_anxiety)
mean(month6_anxiety$Gad_7_Total)

```
Services related to SMI/COD (count)
```{r}
#sum hospitalizations for mental health and ER visits

#baseline ER and hosp

sum(FHHC_wide_ER$Base_ER, FHHC_wide_hospital$Base_Hosp)

#follow-up ER and hosp

sum(FHHC_wide_ER$Month6_ER, FHHC_wide_hospital$Month6_Hosp)


```

Alcohol (AUDIT mean score)

```{r}

#AUDIT baseline

base_audit = base[,3:12]
head(base_audit)

dim(base_audit)
base_audit = na.omit(base_audit)
sum(is.na(base_audit))

base_audit$Audit_Total = rowSums(base_audit)
View(base_audit$Audit_Total)
mean(base_audit$Audit_Total)

#AUDIT follow-up
month6_audit = month6[,4:13]
head(month6_audit)

dim(month6_audit)
sum(is.na(month6_audit))

month6_audit$Audit_Total = rowSums(month6_audit)
mean(month6_audit$Audit_Total)

```

BARC mean score (increase in recovery capital) (not all in baseline completed this measure)

```{r}

#Barc Baseline
base_barc = base[,86:95]
base_barc = na.omit(base_barc)
head(base_barc)

## number of people
dim(base_barc)
sum(is.na(base_barc))

#baseline mean score
base_barc$Barc_Total = rowSums(base_barc)
mean(base_barc$Barc_Total)

## Barc follow-up
month6_barc = month6[,81:90]
head(month6_barc)

### number of people
dim(month6_barc)
sum(is.na(month6_barc))

#follow-up mean score
month6_barc$Barc_Total = rowSums(month6_barc)
mean(month6_barc$Barc_Total)



```

PLC-C mean score decrease (lesser trauma symptoms) (if we want to use this, or we can delete this code)

```{r}

#plc base

base_plc = base[,48:64]
head(base_plc)

## number of people
dim(base_plc)
sum(is.na(base_plc))

#baseline mean score
base_plc$Plc_Total = rowSums(base_plc)
mean(base_plc$Plc_Total)

## PLC follow-up
month6_plc = month6[,49:65]
head(month6_plc)

### number of people
dim(month6_plc)
sum(is.na(month6_plc))

#follow-up mean score
month6_plc$Plc_Total = rowSums(month6_plc)
mean(month6_plc$Plc_Total)

```

