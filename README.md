---
title: "FHHC Report"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
FHHC
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
44-((44-35)*.75)
```
Diagnosis  FHHC$DiagnosisOne
```{r}
Diagnosis = data.frame(DiagnosisOne = FHHC$DiagnosisOne, DiagnosisTwo = FHHC$DiagnosisTwo, DiagnosisThree = FHHC$DiagnosisThree)
apply(Diagnosis, 2, function(x){describe.factor(x)})
```
Centerstone Metrics
little_interest_or_pleasur	feeling_down_depressed_or	trouble_falling_or_staying	feeling_tired_or_having_li	poor_appetite_or_overeatin	feeling_bad_about_yourself	trouble_concentrating_on_t	moving_or_speaking_so_slow	thoughts_that_you_would_be
```{r}
#Depression
depression = FHHC_redcap[,30:38]
depression = rowSums(depression)
mean(depression)
```
Substance use Audit
```{r}
audit = FHHC_redcap[,6:15]
audit = rowSums(audit)
audit = na.omit(audit)
length(audit)
mean(audit, na.rm =TRUE)
```
ER Usage
```{r}
mean(FHHC$TimesER)
```
Hospitalizations
NightsHospitalMHC
BeenHospitalizedIntegerCount
```{r}
mean(FHHC$NightsHospitalMH)
```
Anxeity
```{r}

head(FHHC_redcap)[c(42:48)]

GAD7 =  rowSums(FHHC_redcap[c(42:48)])
mean(GAD7)
```
Decrease in the number of services for mental health
```{r}
mean(FHHC$NightsHospitalMHC)
```
Average days using alcohol
```{r}
mean(FHHC$Alcohol_Use)


```





