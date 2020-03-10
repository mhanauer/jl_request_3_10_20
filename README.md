---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Load data
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/JL_Request_3_10_20")
jl_request = read.csv("JL_Request_3_10_20.csv", header = TRUE)
```
Check out vars
```{r}
library(psych)
jl_request_dat = data.frame(jl_request[,-c(1:3)])
jl_request_dat = na.omit(jl_request_dat)
dim(jl_request_dat)
jl_request_dat
jl_request_dat = apply(jl_request_dat,2, as.factor)
sum_dat = summary(jl_request_dat)
sum_dat
jl_request_dat = data.frame(jl_request_dat)
jl_request_dat
### Get meaningful names
names(jl_request_dat) = c("job", "gender", "years_service", "age", "state", "first_choice", "second_choice", "third_choice", "fourth_choice", "fifth_choice")
### Change 1,2,3,4,5 to actual values
n = dim(jl_request_dat)[1]
```
General descriptives
```{r}
sum_dat
n
```
Get the count of responses to q_33_1 by job
```{r}
count_Q33_1_job = jl_request_dat %>%
  group_by(job) %>%
  count(first_choice)
count_Q33_1_job
count_Q33_1_job$first_choice = as.numeric(count_Q33_1_job$first_choice)
write.csv(count_Q33_1_job, "count_Q33_1_job.csv", row.names = FALSE)
count_Q33_1_job = read.csv("count_Q33_1_job.csv", header = TRUE)
count_Q33_1_job$first_choice = ifelse(count_Q33_1_job$first_choice == 1, "pay", ifelse(count_Q33_1_job$first_choice == 2, "culture", ifelse(count_Q33_1_job$first_choice == 3, "benefits package", ifelse(count_Q33_1_job == 4, "retirement", ifelse(count_Q33_1_job$first_choice == 5, "pto", "retirement")))))
#mtcars[order(mpg, cyl),]

count_Q33_1_job = count_Q33_1_job[order(count_Q33_1_job$job, -count_Q33_1_job$n),]
write.csv(count_Q33_1_job, "count_Q33_1_job.csv", row.names = FALSE)
```



Do a Cramer's V for each demographic across Q_33_1 and see if there are differences
Get rid of 4 for Q_33_1
Get rid of did not prefer to say for gender only 17
```{r}
library(DescTools)
jl_request_q_33_1_dat = subset(jl_request_dat, gender != "Prefer not to disclose")
jl_request_q_33_1_dat = subset(jl_request_q_33_1_dat, Q33_1 != 4)
summary(jl_request_q_33_1_dat)

demos_dat = as.list(jl_request_dat[,1:4])
cramer_v_q_33_1_result = list()
demos_dat[[2]]
for(i in 1:length(demos_dat)){
  cramer_v_q_33_1_result[[i]] = CramerV(demos_dat[[i]], jl_request_dat$Q33_1, conf.level = .95)
}
cramer_v_q_33_1_result
### Look at age
```

