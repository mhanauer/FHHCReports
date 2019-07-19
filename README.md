---
title: "FHHC Report"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


library(prettyR)
library(psych)

#setwd("S:/Indiana Research & Evaluation/FHHC Homelessness/Data and QPR")
#base = read.csv("FHHC_base.csv", header = TRUE, na.strings = c(-99, -98, -97))
#month6 = read.csv("FHHC_Month6.csv", header = TRUE, na.string = c(-99, -98, -97))
#FHHC = read.csv("FHHC.csv", header = TRUE, na.string = c(-99, -98, -97, -1, -4, -5, -7, -9, -2, -6 -8))

#Get matched pairs in FHHC data

dim(FHHC)
FHHC = FHHC[order(FHHC$ConsumerID),]
FHHC_base = subset(FHHC, FHHC$InterviewType_07 == 1)
FHHC_Month6 = subset(FHHC, FHHC$InterviewType_07 == 3)
dim(FHHC_Month6)
FHHC_wide = merge(FHHC_base, FHHC_Month6, by = "ConsumerID", all.y = TRUE)
dim(FHHC_wide)
FHHC_wide$ConsumerID == FHHC_Month6$ConsumerID
### Subset RedCap data
library(lubridate)
#base$audit_timestamp = mdy(base$audit_timestamp)
#base$audit_timestampTest = gsub(base$audit_timestamp, "\\s", "")


redcap = merge(base, month6, by = "record_id", all.y = TRUE)
redcap$audit_timestamp.xTest = gsub(redcap$audit_timestamp.x, "\\s", "")
head(redcap)
base$audit_timestamp
dim(redcap)
head(redcap)
```
Descriptives
```{r}
library(prettyR)
#1 = male; 2 = female
describe.factor(FHHC$Gender)
describe.factor(FHHC$RaceWhite)
describe.factor(FHHC$RaceBlack)
describe.factor(FHHC$HispanicLatino)
describe.factor(FHHC$RaceAsian)

#12 = 12TH GRADE /HIGH SCHOOL DIPLOMA/ EQUIVALENT (GED)
#13 = VOC/TECH DIPLOMA
#14 = SOME COLLEGE OR UNIVERSITY
#15 = BACHELOR'S DEGREE (BA, BS)
#16 = GRADUATE WORK/GRADUATE DEGREE
describe.factor(FHHC$Education)
#4 = Age 16 to 25 years old
#5 = Age 26 to 34 years old
#6 = Age 35 to 44 years old
#7 = Age 45 to 54 years old
#8 = Age 55 to 64 years old
#9= 65 to 74 years old
#10 = Age 75 to 84 years old
#11 = Age 85 to 94 years old
#12 = Age 95 years or older
describe.factor(FHHC$Agegroup)
mean(FHHC$Agegroup)
#multiply by decimal of mean of age group

44-((44-35)*.62)

#Diagnosis FHHC$DiagnosisOne
```


Depression
```{r}
#Depression base
base_depression = base[,27:35]
## number of people
dim(base_depression)
sum(is.na(base_depression))
base_depression$PHQ_9_Total = rowSums(base_depression)
mean(base_depression$PHQ_9_Total)

## Depression follow-up
month6_depression = month6[,28:36]
### number of people
dim(month6_depression)
sum(is.na(month6_depression))
month6_depression$PHQ_9_Total = rowSums(month6_depression)
mean(month6_depression$PHQ_9_Total)
```


ER Visits
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
FHHC_wide$NightsHospitalMHC.x
```


