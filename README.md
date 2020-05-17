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
Get the count of responses to first choice by job
```{r}
n_jl_request_dat = dim(jl_request_dat)[1]
count_first_choice_job = jl_request_dat %>%
  group_by(job, first_choice) %>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))
count_first_choice_job

count_first_choice_job$first_choice = ifelse(count_first_choice_job$first_choice == 1, "pay", ifelse(count_first_choice_job$first_choice == 2, "culture", ifelse(count_first_choice_job$first_choice == 3, "benefits package", ifelse(count_first_choice_job == 4, "retirement", ifelse(count_first_choice_job$first_choice == 5, "pto", "retirment")))))

count_first_choice_job = count_first_choice_job[order(count_first_choice_job$job, -count_first_choice_job$n),]

count_first_choice_job$freq = round(count_first_choice_job$freq,2)*100
count_first_choice_job$freq = paste0(count_first_choice_job$freq, "%")
names(count_first_choice_job)[4] = "percent"
count_first_choice_job
write.csv(count_first_choice_job, "count_first_choice_job.csv", row.names = FALSE)

title_count_first_choice_job = paste0("First choice by job", " ", "n=", n_jl_request_dat)
levels(count_first_choice_job$first_choice)

### make graph
plot_count_first_choice_job = ggplot(count_first_choice_job, aes(x = first_choice,y =n, fill = job))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_count_first_choice_job, y = "Count", x = "First choice")+
  scale_y_continuous(limits = c(0,500))+
  geom_text(aes(label = count_first_choice_job$percent), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(fill = "Job")
plot_count_first_choice_job

```
First choice age
```{r}
count_first_choice_age = jl_request_dat %>%
  group_by(age, first_choice) %>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))
count_first_choice_age

count_first_choice_age$first_choice = ifelse(count_first_choice_age$first_choice == 1, "pay", ifelse(count_first_choice_age$first_choice == 2, "culture", ifelse(count_first_choice_age$first_choice == 3, "benefits package", ifelse(count_first_choice_age == 4, "retirement", ifelse(count_first_choice_age$first_choice == 5, "pto", "retirement")))))

count_first_choice_age = count_first_choice_age[order(count_first_choice_age$age, -count_first_choice_age$n),]

count_first_choice_age$freq = round(count_first_choice_age$freq,2)*100
count_first_choice_age$freq = paste0(count_first_choice_age$freq, "%")
names(count_first_choice_age)[4] = "percent"
count_first_choice_age
write.csv(count_first_choice_age, "count_first_choice_age.csv", row.names = FALSE)

title_count_first_choice_age = paste0("First choice by age", " ", "n=", n_jl_request_dat)
### make graph
plot_count_first_choice_age = ggplot(count_first_choice_age, aes(x = first_choice,y =n, fill = age))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_count_first_choice_age, y = "Count", x = "First choice")+
  scale_y_continuous(limits = c(0,500))+
  geom_text(aes(label = count_first_choice_age$percent), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(fill = "Age")
plot_count_first_choice_age


```
First choice gender
```{r}
count_first_choice_gender = jl_request_dat %>%
  group_by(gender, first_choice) %>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))
count_first_choice_gender

count_first_choice_gender$first_choice = ifelse(count_first_choice_gender$first_choice == 1, "pay", ifelse(count_first_choice_gender$first_choice == 2, "culture", ifelse(count_first_choice_gender$first_choice == 3, "benefits package", ifelse(count_first_choice_gender == 4, "retirement", ifelse(count_first_choice_gender$first_choice == 5, "pto", "retirement")))))

count_first_choice_gender = count_first_choice_gender[order(count_first_choice_gender$gender, -count_first_choice_gender$n),]

count_first_choice_gender$freq = round(count_first_choice_gender$freq,2)*100
count_first_choice_gender$freq = paste0(count_first_choice_gender$freq, "%")
names(count_first_choice_gender)[4] = "percent"
count_first_choice_gender
write.csv(count_first_choice_gender, "count_first_choice_gender.csv", row.names = FALSE)

title_count_first_choice_gender = paste0("First choice by gender", " ", "n=", n_jl_request_dat)
### make graph
plot_count_first_choice_gender = ggplot(count_first_choice_gender, aes(x = first_choice,y =n, fill = gender))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_count_first_choice_gender, y = "Count", x = "First choice")+
  scale_y_continuous(limits = c(0,1000))+
  geom_text(aes(label = count_first_choice_gender$percent), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(fill = "Gender")
plot_count_first_choice_gender


```
First choice state
```{r}
count_first_choice_state = jl_request_dat %>%
  group_by(state, first_choice) %>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))
count_first_choice_state

count_first_choice_state$first_choice = ifelse(count_first_choice_state$first_choice == 1, "pay", ifelse(count_first_choice_state$first_choice == 2, "culture", ifelse(count_first_choice_state$first_choice == 3, "benefits package", ifelse(count_first_choice_state == 4, "retirement", ifelse(count_first_choice_state$first_choice == 5, "pto", "retirement")))))

count_first_choice_state = count_first_choice_state[order(count_first_choice_state$state, -count_first_choice_state$n),]

count_first_choice_state$freq = round(count_first_choice_state$freq,2)*100
count_first_choice_state$freq = paste0(count_first_choice_state$freq, "%")
names(count_first_choice_state)[4] = "percent"
count_first_choice_state
write.csv(count_first_choice_state, "count_first_choice_state.csv", row.names = FALSE)

title_count_first_choice_state = paste0("First choice by state", " ", "n=", n_jl_request_dat)
### make graph
plot_count_first_choice_state = ggplot(count_first_choice_state, aes(x = first_choice,y =n, fill = state))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_count_first_choice_state, y = "Count", x = "First choice")+
  scale_y_continuous(limits = c(0,500))+
  geom_text(aes(label = count_first_choice_state$percent), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(fill = "State")
plot_count_first_choice_state


```
Overall
```{r}
count_first_choice = jl_request_dat %>%
  group_by(first_choice) %>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))
count_first_choice

count_first_choice$first_choice = ifelse(count_first_choice$first_choice == 1, "pay", ifelse(count_first_choice$first_choice == 2, "culture", ifelse(count_first_choice$first_choice == 3, "benefits package", ifelse(count_first_choice == 4, "retirement", ifelse(count_first_choice$first_choice == 5, "pto", "retirement")))))

count_first_choice = count_first_choice[order(-count_first_choice$n),]

count_first_choice$freq = round(count_first_choice$freq,2)*100
count_first_choice$freq = paste0(count_first_choice$freq, "%")
names(count_first_choice)[3] = "percent"
count_first_choice
write.csv(count_first_choice, "count_first_choice.csv", row.names = FALSE)

title_count_first_choice_overall = paste0("First choice by overall", " ", "n=", n_jl_request_dat)
### make graph
plot_count_first_choice_overall = ggplot(count_first_choice, aes(x = first_choice,y =n, fill = first_choice))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_count_first_choice_overall, y = "Count", x = "First choice")+
  scale_y_continuous(limits = c(0,1300))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(fill = "Overall")
plot_count_first_choice_overall

```



Do a Cramer's V for each demographic across Q_33_1 and see if there are differences
Get rid of 4 for Q_33_1
Get rid of did not prefer to say for gender only 17
```{r}
library(DescTools)
jl_request_q_33_1_dat = subset(jl_request_dat, gender != "Prefer not to disclose")
jl_request_q_33_1_dat = subset(jl_request_q_33_1_dat, first_choice != 4)
summary(jl_request_q_33_1_dat)

demos_dat = as.list(jl_request_dat[,1:4])
cramer_v_q_33_1_result = list()
for(i in 1:length(demos_dat)){
  cramer_v_q_33_1_result[[i]] = CramerV(demos_dat[[i]], jl_request_dat$first_choice, conf.level = .95)
}
cramer_v_q_33_1_result
### Look at age

```

