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
library(MASS)

# Uploading the data

survey9 <- read.csv("survey10.csv",stringsAsFactors = T) # Cleaned Version of data-set
survey10 <- survey9 

### Data Cleaning ###

# Combining our data set
setwd("C://Users//zenci//Downloads")
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
# Those follow up question was visible by only people who choose "yes" answer for certain questions
# Therefore, for the participants who choose "no" answer of the follow-up question are seen as "NA".
# We are turning those "NA" values into 0 which makes the results more precise by not losing its weight.

survey[is.na(survey)] <- 0

# Adding Columns for the Values of Social Self Efficacy, Major Satisfaction, Academic Performance, Campus Life Satisfaction

survey <- survey %<>% mutate("Major_Satisfaction_Value"=round(rowMeans(survey[,c(9:20,23:25)]),2)) # These columns are numerical values used for predicting Major Satisfaction
survey <- survey %<>% mutate("Social_Self_Efficacy_Value"=round(rowMeans(survey[,c(26:38,40,42,44)]),2)) # Similarly for Social-Self Efficacy
survey <- survey %<>% mutate("Academic_Performance_Value"=round(rowMeans(survey[,c(48,53:58)]),2)) # and Academic Performance
survey <- survey %<>% mutate("Campus_Life_Satisfaction_Value"=round(rowMeans(survey[,c(61:70)]),2)) # and Campus Life Satisfaction


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


####### Findings and Results ####### 

### Social-Self Efficacy ~ Residence ###

# Implementing a simple linear regression for Social-Self Efficacy and Residence

reg.residence <- lm(Social_Self_Efficacy_Value~Residence,data = survey)
summary(reg.residence) 

# As the p-value is greater than 0.05, We fail to find a significant relationship between Social Self Efficacy and Residence


# Comparing the Social Self Efficacy values for In-campus residences and Off-campus residences by t-test

t.test(Social_Self_Efficacy_Value~Residence,data = survey)

# We fail to find a difference due to the p-value of "0.07713" which is greater than 0.05.


### Social Self-Efficacy ~ Academic Performance ### 

# Hypothesis Question:

# H0: Students with 3.5 or higher Social Self Efficacy Value produce equal or lower Academic Performance than Students with lower Social Self Efficacy Value

# H1: Students with 3.5 or higher Social Self Efficacy Value produce higher Academic Performance than Students with lower Social Self Efficacy Value

social3.5_higher <- subset(survey,Social_Self_Efficacy_Value>=3.5) # sub-setting highly social people (extroverts)

social3.5_lower <- subset(survey,Social_Self_Efficacy_Value<3.5) # sub-setting medium and lower social people (introverts)

# Checking the equal variance assumption for implementing a t-test

var.test(social3.5_higher$Academic_Performance_Value,social3.5_lower$Academic_Performance_Value, alternative = "two.sided")

# Because "1" is in the 95% confidence level, by the variance test variances are equal, 
# so we assume they are coming from a population where variances are equal

t.test(social3.5_higher$Academic_Performance_Value,social3.5_lower$Academic_Performance_Value)

# Since P value is lower than 0.05 we reject null hypothesis. Students with 3.5 or higher Social Self Efficacy Value significantly 
# produce better academic performance than students with 3.5 and lower Social Self Efficacy Value


# Checking the correlation between them 

cor(survey$Social_Self_Efficacy_Value,survey$Academic_Performance_Value,method ="pearson")

# Checking to find a relationship between them by simple linear regression model

reg.social_academic <- lm(Social_Self_Efficacy_Value~Academic_Performance_Value,data = survey)
summary(reg.social_academic)

# We need to check assumptions as well

# Constant Variance Assumption for the residuals

plot(reg.social_academic) # By writing 1 we checked constant variance for residuals 
                         # By writing 2 we checked normality among our variables

# Both of the assumptions can be made due to reasonable plots. 

# Since the assumptions are met, we are implementing ANOVA

anova.social_academic <- aov(Social_Self_Efficacy_Value~Academic_Performance_Value,data =
                               survey)

summary(anova.social_academic)

# By ANOVA, we find a p-value that is highly lower than 0.05, 
# similarly by Simple Linear Regression model, we find a low p-value.

# Therefore we can conclude that there is a significant relationship between Social Self Efficacy and Academic Performance.
# Also, surprisingly enough by the correlation function we can say that this relationship is not negative as most people assume, in fact it is strongly positive 


###  Faculty ~ Social-Self Efficacy ### 

# Is there an significant relationship between Faculties and Social Self Efficacy Level 

anova.social_faculty <- aov(Social_Self_Efficacy_Value~Faculty,data=survey)
summary(anova.social_faculty)

# As p-value is greater than 0.05, we fail to conclude a significance relationship between Faculties and Social Self Efficacy Level

# Comparing Different Faculties Social Self Efficacy Level (No Differences)

test.social_faculty <- subset(survey,Faculty=="Engineering"|Faculty=="Arts and Sciences")
t.test(Social_Self_Efficacy_Value~Faculty,data = test.social_faculty)


# Stratification on Social Self Efficacy by Gender

survey$Population <- NA
survey$Population[survey$Gender == "Female"] <- 11626 # Population of Female in METU
survey$Population[survey$Gender == "Male"] <- 14825 # Population of Male in METU


# Designing our Survey Strata 

strata.data <- subset(survey,Gender=="Female" | Gender =="Male")  # Creating the strata
mydesign <- svydesign(id = ~1, strata = ~Gender, data =strata.data, fpc = ~Population)

# Mean, Conf-int, Total for Social Self Efficacy Level by Gender Strata

svymean(~Social_Self_Efficacy_Value,design = mydesign) # Estimated Average Social Self Efficacy for the Population
confint(svymean(~Social_Self_Efficacy_Value, design = mydesign)) 
svytotal(~Social_Self_Efficacy_Value, design = mydesign) # Estimated Total Social Self Efficacy for the Population
confint(svytotal(~Social_Self_Efficacy_Value, design = mydesign))

# Design Effect

svymean(~Social_Self_Efficacy_Value, design = mydesign, deff = TRUE)

# By Genders

svyby(~Social_Self_Efficacy_Value, by = ~Gender, design = mydesign, FUN = svymean) # Estimated Average Social Self Efficacy for the Population by Genders
svyby(~Social_Self_Efficacy_Value, by = ~Gender, design = mydesign, FUN = svytotal) # Estimated Total Social Self Efficacy for the Population by Genders

# Post-Stratification on Social Self Efficacy by Gender

post.strata.data <- subset(survey,Gender=="Female" | Gender =="Male")
post.strata.data$Population <- 26451 # Population of the METU

# Design

mydesign2 <- svydesign(id = ~1, data = post.strata.data, fpc = ~Population)
gender.freq <- data.frame(Gender = c("Male", "Female"),
                          Freq = c(14825,11626))
mydesign2 <- postStratify(design = mydesign2, strata = ~Gender, population = gender.freq)

# Mean and Total Value

svymean(~Social_Self_Efficacy_Value, design = mydesign2) # Estimated Average Value with Post-stratification
svytotal(~Social_Self_Efficacy_Value, design = mydesign2) # Estimated Total Value with Post-stratification

# Population Distribution Assumption Before Post-Stratification

mydesign2 <- svydesign(id = ~1, data = post.strata.data, fpc = ~Population)
tapply(weights(mydesign2), post.strata.data$Gender, sum) # Estimated Population Distribution before Post-stratification

# Population Distribution After Applying Post-Stratification

mydesign2 <- postStratify(design = mydesign2, strata = ~Gender, population = gender.freq)
tapply(weights(mydesign), post.strata.data$Gender, sum) 

# For Gender Wise Mean And Total Value (By comparison Females Variance is little bit smaller in Post-Stratification)

svyby(~Social_Self_Efficacy_Value, by = ~Gender, design = mydesign2, FUN = svymean) # Estimated Average Value for Genders with Post-stratification
svyby(~Social_Self_Efficacy_Value, by = ~Gender, design = mydesign2, FUN = svytotal) # # Estimated Total Value for Genders with Post-stratification

### Plots to Visualize Relationships ###

# Social Self Efficacy ~ Academic Class, By Residence #

ggplot(survey)+
  
  geom_point(aes(x=Social_Self_Efficacy_Value,y=Academic_Performance_Value,colour=Residence))+ # Inputting x and y values
  geom_jitter(aes(x=Social_Self_Efficacy_Value,y=Academic_Performance_Value,colour=Residence))+ # Putting additional observations without causing a change in relationship
  geom_smooth(aes(x=Social_Self_Efficacy_Value,y=Academic_Performance_Value),method =
                lm,colour="red",size=1,se=T,fill="lightblue")+ # Drawing the Regression Line with Confidence Interval
  ggtitle("Social Self Efficacy vs Academic Performance")+ # Adding Main Title
  xlab("Social Self Efficacy")+ # X-axis title
  ylab("Academic Performance")+ # Y-axis title
  facet_grid(~Academic_Class)+ # Plotting For Each Academic Class in the Same Figure
  theme_bw() # White theme

# Social-Academic ~ Faculty, By Residence #

ggplot(survey)+
  geom_point(aes(x=Social_Self_Efficacy_Value,y=Academic_Performance_Value,colour=Residence))+  # Inputting x and y values
  geom_jitter(aes(x=Social_Self_Efficacy_Value,y=Academic_Performance_Value,colour=Residence))+ # Putting additional observations without causing a change in relationship
  geom_smooth(aes(x=Social_Self_Efficacy_Value,y=Academic_Performance_Value),method =
                lm,colour="red",size=1,se=T,fill="lightblue")+ # Drawing the Regression Line with Confidence Interval
  ggtitle("Social Self Efficacy vs Academic Performance")+ # Adding Main Title
  xlab("Social Self Efficacy")+ # X-axis title
  ylab("Academic Performance")+ # Y-axis title
  facet_grid(~Faculty)+ # Plotting for Each Faculty in the Same Figure
  theme_bw() # White theme 

# Social Self Efficacy ~ Residence #

social.residence <- round(tapply(survey$Social_Self_Efficacy_Value, survey$Residence, median), 2) # Finding the median of SSE value by residence

ggplot(survey)+
  geom_boxplot(aes(x=Residence,y=Social_Self_Efficacy_Value,fill=Residence))+ # Inputting x and y values
  ggtitle("Social Self Efficacy Distribution Among Residence")+ # Adding Main Title
  ylab("Social Self Efficacy")+ # Y-axis title
  xlab("Factors")+  # X-axis title
  theme_minimal()+ 
  annotate(geom = "text", 1,social.residence[1]+0.08, label = social.residence[1], color = "red",
           size=5)+ # 1 choose which observation to put and social.residence[1]+0.08 determines where to coordinate it on y-axis and label shows what is written there
  annotate(geom = "text", 2,social.residence[2]+0.08, label = social.residence[2], color = "blue",
           size=5)+ # 2 choose which observation to put and social.residence[2]+0.08 determines where to coordinate it on y-axis and label shows what is written there
  theme(plot.title = element_text(hjust = 0.5))  # Size of the main title (Mostly for the fit in the figure)

# Social Self Efficacy ~ Academic Class # 

social.academic <- round(tapply(survey$Social_Self_Efficacy_Value, survey$Academic_Class,mean),2) # Finding SSE Mean Value by each class

ggplot(survey)+
  geom_histogram(aes(Academic_Class,Social_Self_Efficacy_Value,fill=Academic_Class),position =
                   "dodge",stat = "identity")+ # Inputting x as Class and Y as a Distribution Scale 
  ggtitle("Social Self Efficacy Distribution Among Academic Class")+ # Main Title
  ylab("Social Self Efficacy")+ # Y-axis title
  theme_bw()+
  annotate(geom = "text", 1,3.8, label = social.academic[1], color = "red",
           size=5)+ # What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text. 
  annotate(geom = "text", 2,4.6, label = social.academic[2], color = "brown",
           size=5)+ # What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  annotate(geom = "text", 3,4.04, label = social.academic[3], color = "darkgreen",
           size=5)+ # What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  annotate(geom = "text", 4,4.48, label = social.academic[4], color = "turquoise",
           size=5)+ # What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  annotate(geom = "text", 5,3.98, label = social.academic[5], color = "blue",
           size=5)+ # What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  annotate(geom = "text", 6,3.34, label = social.academic[6], color = "purple",
           size=5)+ # What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  annotate(geom = "text", 7,3.55, label = social.academic[7], color = "pink",
           size=5)+ # What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  theme(plot.title = element_text(hjust = 0.5)) # Size of the main title (Mostly for the fit in the figure)

# Social Self Efficacy ~ Faculty #

ggplot(survey)+
  geom_histogram(aes(x=Faculty,y=Social_Self_Efficacy_Value,fill=Faculty),position = "dodge",stat =
                   "identity")+ # Showing Faculties in SSE scale
  ggtitle("Social Self Efficacy Distribution Among Faculty")+ # Main title
  ylab("Social Self Efficacy")+ # Y-axis title
  theme_bw() # White theme

# Social Self Efficacy ~ Gender #

social.gender <- round(tapply(survey$Social_Self_Efficacy_Value,survey$Gender,median),2)

ggplot(survey)+
  geom_boxplot(aes(Gender,Social_Self_Efficacy_Value,fill=Gender))+ # Inputting x and y variables
  ggtitle("Social Self Efficacy Distribution Among Genders")+ # Main title
  ylab("Social Self Efficacy")+ # Y-axis title
  theme_bw()+ # White theme
  annotate(geom = "text", 1,social.gender[1]+0.08, label = social.gender[1], color = "red",
           size=5)+ # What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  annotate(geom = "text", 2,social.gender[2]+0.08, label = social.gender[2], color = "darkgreen",
           size=5)+# What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  annotate(geom = "text", 3,social.gender[3]+0.08, label = social.gender[3], color = "blue",
           size=5)+# What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  annotate(geom = "text", 4,social.gender[4]+0.08, label = social.gender[4], color = "purple",
           size=5)+# What format to add, Choosing which observation, Choosing where to put on Y-coordinate level, what to write, color of the text.
  theme(plot.title = element_text(hjust = 0.5)) # Size of the main title (Mostly for the fit in the figure)

# Box-Cox Test result is 1. Hence, variable is not changed for Social Self-Efficacy vs Academic Performance

boxcox(reg.social_academic, # lm or aov objects or formulas
       lambda = seq(-2, 2, 1/10), # Vector of values of lambda
       plotit = TRUE, # Create a plot or not # Logical. Controls if spline interpolation is used
       eps = 1/50, # Tolerance for lambda. Defaults to 0.02.
       xlab = expression(lambda), # X-axis title
       ylab = "log-Likelihood")
bc <- boxcox(reg.social_academic)
lambda <- bc$x[which.max(bc$y)]
lambda
