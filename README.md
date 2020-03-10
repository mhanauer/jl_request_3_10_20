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
jl_request_dat = jl_request[,-c(1:4)]
jl_request_dat = apply(jl_request_dat,2, as.factor)
sum_dat = summary(jl_request_dat)
sum_dat
jl_request_dat = data.frame(jl_request_dat)
### Get meaningful names
names(jl_request_dat)[1:4] = c("gender", "years_service", "age", "state")
jl_request_dat
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

