library(foreign)
library(ggplot2)
library(dplyr)
library(readxl)

raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)
welfare = raw_welfare
head(welfare,3)

welfare = rename(welfare, 
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_religion = h10_reg7)

class(welfare$sex)
table(welfare$sex)
welfare$sex = ifelse(welfare$sex != 1 & welfare$sex != 2, NA, welfare$sex)
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
table(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
table(is.na(welfare$income))

sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth))
welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)

age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

welfare = welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)

welfare = welfare %>% 
  mutate(age_group = ifelse(age < 20, "10",
                            ifelse(age < 30, "20",
                                   ifelse(age < 40, "30",
                                          ifelse(age < 50, "40",
                                                 ifelse(age < 60, "50",
                                                        ifelse(age < 70, "60",
                                                               ifelse(age < 80, "70", "80"))))))))
table(welfare$age_group)
qplot(welfare$age_group)

sex_income = welfare %>% 
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
table(sex_income)  
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = 'dodge') +
  scale_x_discrete(limits = c("young", "middle", "old"))

sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()

class(welfare$code_job)

library(readxl)
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)

welfare = left_join(welfare, list_job, id = "code_job")
head(welfare)
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(10)

job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>%
  summarise(mean_income = mean(income))

head(job_income)

top10 = job_income %>%
  arrange(desc(mean_income)) %>% 
  head(10)
top10
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) + 
  geom_col() + coord_flip()