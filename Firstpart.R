setwd("C://Users//zenci//Downloads")

# Libraries

library(ggplot2)
library(ltm)
library(dplyr)
library(magrittr)
library(survey)
library(SDaA)
library(tidyverse)
library(broom)
library(tidyr)

# Uploading the data

survey9 <- read.csv("survey10.csv",stringsAsFactors = T) # Cleaned Version of data-set
survey10 <- survey9 

### Data Cleaning ###

# Combining our data set

surv1 <- read.csv("Survey_Results.csv",header = T,stringsAsFactors = T) # First data-set of our survey
surv2 <- read.csv("Survey_Results2.csv",header = T,stringsAsFactors = T) # Second data-set of our survey
surv3 <- read.csv("Survey_Results3.csv",header = T,stringsAsFactors = T) # Third data-set of our survey

names(surv1) <- names(surv2) # Making sure names match before binding
names(surv3) <- names(surv2)

survey <- rbind(surv1,surv2,surv3) # Binding our data

survey <- survey[-101,] # Deleting the last observation which was a NA

# Naming the Demographic Questions

colnames(survey)[1:8] <- c("Gender","Age","Academic_Class", "Faculty",
                           "Major", "Residence", "Entry.Year", "City")

# Re-leveling Academic Class 

survey$Academic_Class <- factor(survey$Academic_Class, levels = c("Preparation School", "First-year", "Sophomore","Junior","Senior","Master","Phd"))

# Changing Likert in Satisfied Scale to 1:5

survey <- survey %>%
  mutate(X10...How.satisfied.are.you.with.the.facilities.....Classrooms.capacity.=recode(X10...How.satisfied.are.you.with.the.facilities.....Classrooms.capacity.,
                                                                                         "Very Dissatisfied"=1,"Dissatisfied"=2,"Neutral"=3,"Satisfied"=4,"Very Satisfied"=5))
survey <- survey %>%
  mutate_at(c("X10...How.satisfied.are.you.with.the.facilities.....Physical.conditions.of.the.classrooms"
              ,
              "X10...How.satisfied.are.you.with.the.facilities.....Lecture.materials.",
              
              "X10...How.satisfied.are.you.with.the.facilities.....Equipment.of.the.department.s.computer.lab",
              "X.11...How.satisfied.are.you.with.the.program.schedules.....Course.schedule",
              
              "X.11...How.satisfied.are.you.with.the.program.schedules.....Major.s.courses..attendance.policy",
              
              "X.11...How.satisfied.are.you.with.the.program.schedules.....Breaks.between.courses",
              
              "X53...How.would.you.evaluate.the.below.statements.in.terms.of.student.life......Get.training.skill.in.a.special.field",
              
              "X53...How.would.you.evaluate.the.below.statements.in.terms.of.student.life......Satisfy.self.needs",
              
              "X53...How.would.you.evaluate.the.below.statements.in.terms.of.student.life......Gain.knowledge.about.the.world..",
              
              "X54...How.would.you.evaluate.your.campus.life.satisfaction.in.terms.of.the.below.statements.....Campus.Location",
              
              "X54...How.would.you.evaluate.your.campus.life.satisfaction.in.terms.of.the.below.statements.....Facilities.in.the.Campus",
              
              "X54...How.would.you.evaluate.your.campus.life.satisfaction.in.terms.of.the.below.statements.....Safety.in.Campus",
              
              "X54...How.would.you.evaluate.your.campus.life.satisfaction.in.terms.of.the.below.statements.....On.campus.expenses",
              
              "X54...How.would.you.evaluate.your.campus.life.satisfaction.in.terms.of.the.below.statements.....Course.variability"),funs(recode(.,"Very Dissatisfied"=1,"Dissatisfied"=2,"Neutral"=3,"Satisfied"=4,"Very Satisfied"=5)))


survey <- survey %>% mutate_at(c("X32...Do.you.go.to.parties.where.you.don.t.know.anyone.",
                                 "X33...Do.you.volunteer.to.lead.a.group.or.organization.",
                                 "X41...Have.you.ever.participated.in.any.research.at.METU.",
                                 
                                 "X42...Have.you.ever.participated.in.a.scientific.study.organized.by.an.institution.or.organization.",
                                 "X43...Do.you.have.a.scientific.article.published.in.an.academic.journal.",
                                 
                                 "X44...Have.you.received.certified.training.that.will.contribute.to.your.education.outside.of.your.university.")
                               ,funs(recode(.,"No"=0,"Yes"=1)))

# Turning Course failed into 1:5 success scale

survey <- survey %>%
  mutate(X45...How.many.course.s..have.you.failed.=recode(X45...How.many.course.s..have.you.failed.,
                                                          "0"=5,"1"=4,"2"=3,"3"=2,"4+"=1))

# We had questions in our survey which were follow up questions
# Those follow up question was visible by only people who choosed "yes" answer for certain questions
# Therefore, for the participants who choosed "no" answer of the follow-up question are seen as "NA".
# We are turning those "NA" values into 0 which makes the results more precise by not losing its weight.

survey[is.na(survey)] <- 0

# Adding Columns for the Values of Social Self Efficacy, Major Satisfaction, Academic Performance, Campus Life Satisfaction

survey <- survey %<>% mutate("Major_Satisfaction_Value"=round(rowMeans(survey9[,c(9:20,23:25)]),2)) # These columns are numerical values used for predicting Major Satisfaction
survey <- survey %<>% mutate("Social_Self_Efficacy_Value"=round(rowMeans(survey9[,c(26:38,40,42,44)]),2)) # Similarly for Social-Self Efficacy
survey <- survey %<>% mutate("Academic_Performance_Value"=round(rowMeans(survey9[,c(48,53:58)]),2)) # and Academic Performance
survey <- survey %<>% mutate("Campus_Life_Satisfaction_Value"=round(rowMeans(survey9[,c(61:70)]),2)) # and Campus Life Satisfaction


### Understanding our data-set ###

# Gender Distribution

ggplot(survey)+
  geom_bar(aes(x=Gender,fill=Gender))

# Residence Distribution

ggplot(survey)+
  geom_bar(aes(Residence,fill=Residence))

# Academic Class Distribution

ggplot(survey)+
  geom_bar(aes(Academic_Class,fill=Academic_Class))

summary(survey$Academic_Class) # Checking the number of observation for each Academic Class

# Faculty Distribution

ggplot(survey)+
  geom_bar(aes(Faculty,fill=Faculty))

### Checking Cronbach Alpha For Our Factors ### 

# Cronbach Alpha for Major Satisfaction

cronbach.major <- survey[,c(9:25)] # Questions from 9 to 25 are for predicting Major Satisfaction.
cronbach.alpha(cronbach.major)
alpha.major <- cronbach.alpha(cronbach.major)$alpha

# Cronbach Alpha for Social-Self Efficacy

cronbach.social <- survey[,26:44] # Questions from 26 to 44 are for predicting Social-Self Efficacy
cronbach.alpha(cronbach.social)
alpha.social <- cronbach.alpha(cronbach.social)$alpha

# Cronbach Alpha for Academic Performance

cronbach.academic <- survey[,54:58] # Questions from 54 to 58 are for predicting Academic Performance
cronbach.alpha(cronbach.academic)
alpha.academic <- cronbach.alpha(cronbach.academic)$alpha

# Cronbach Alpha for Students' Campus Life Satisfaction

cronbach.campus <- survey[,61:70] # Questions from 61 to 70 are for predicting Campus Life Satisfaction
cronbach.alpha(cronbach.campus)
alpha.campus <- cronbach.alpha(cronbach.campus)$alpha

# Cronbach Alpha Table

alpha.table <- data.frame("Major Satisfaction"=alpha.major,"Social-Self Efficacy"=alpha.social
                          ,"Academic Performance"=alpha.academic,"Campus Life Satisfaction"=alpha.campus)
alpha.table


######## Mean Value of Campus Life Satisfaction For Demographic Variables ########

### GENDER ###

# Calculate mean campus life satisfaction 

campussat.gender <- round(tapply(survey$Campus_Life_Satisfaction_Value, survey$Gender, mean), 2)
campussat.gender

# Create table each genders campus life satisfaction

table.gender <- data.frame("Mean Value of Campus Life Satisfaction" = campussat.gender)
table.gender


### RESIDENCE ###

# Calculate mean campus life satisfaction 

campussat.residence <- round(tapply(survey$Campus_Life_Satisfaction_Value, survey$Residence, mean), 2)
campussat.residence

# Create table for each residences' campus life satisfaction

table.residence <- data.frame("Mean Value of Campus Life Satisfaction" = campussat.residence)
table.residence


### ACADEMIC CLASS ###

# Calculate mean campus life satisfaction 

campussat.academic <- round(tapply(survey$Campus_Life_Satisfaction_Value, survey$Academic_Class, mean), 2)
campussat.academic

# Create table for each academic classes'  campus life satisfaction

table.academic <- data.frame("Mean Value of Campus Life Satisfaction" = campussat.academic)
table.academic

### FACULTY ###

# Calculate mean campus life satisfaction 

campussat.faculty <- round(tapply(survey$Campus_Life_Satisfaction_Value, survey$Faculty, mean), 2)
campussat.faculty

# Create table for each faculties' campus life satisfaction

table.faculty <- data.frame("Mean Value of Campus Life Satisfaction" = campussat.faculty)
table.faculty


### ENTRY YEAR ###

# Calculate mean campus life satisfaction 

campussat.entry.year <- round(tapply(survey$Campus_Life_Satisfaction_Value, survey$Entry.Year, mean), 2)
campussat.entry.year

# Create table for each entry years'  campus life satisfaction

table.entry.year <- data.frame("Mean Value of Campus Life Satisfaction" = campussat.entry.year)
table.entry.year
