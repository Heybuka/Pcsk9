library(dplyr)
library(readxl)
#read in excel file

#check for missing values in dataset
which(complete.cases(comp)==FALSE)

#create new categorical variable for bwt, bwt_cat indicating low bwt = bwt <2500,
#low_bwt=1, normal_bwt=0
comp <- comp %>% mutate(bwt_cat=(ifelse(bwt<2500, 1, 0)))

#visualize loww birth weight in strata of smoking status using a bar chart
library(ggplot2)
ggplot(comp, aes(x=factor(race),fill= factor(bwt_cat)))+
        geom_bar()

#check for outliers using boxplots
comp %>% mutate(smoke_stat=ifelse(smoke==1,"smoking", "no smoking")) %>% 
        ggplot(.,aes(x=factor(smoke_stat), y=age))+
        geom_boxplot()+ xlab("smoking status")

#subset continuous variables
continuous<- comp[,c(5,7,10)]

#visualise histogram for continuous variables
lapply(continuous,function(z){ggplot(comp)+geom_histogram(aes(x=z))})
        
#presence of two outlier in age variable(0,200),filter out outliers
comp2 <- comp %>% filter(age!=0) %>% filter(age<100)

#create function calculating proportions of categorical variables
prop1 <- function(x){prop.table(table(x))}

#Subset categorical and continuous variables for trimmed dataset
categorical <- comp2[,c(1,2,6,8,9,11)]
continuous<- comp2[,c(5,7,10)]

#calculate number and proportion of categorical variables
categorical%>% sapply(table)
categorical %>% sapply(prop1)

#calculate numbers and proportions in strata of smoking
categorical %>% filter(smoke==1) %>% sapply(table)
categorical %>% filter(smoke==1) %>% sapply(prop1)

#calculate numbers and proportions in strata of non-smokers
categorical %>% filter(smoke==0) %>% sapply(table)
categorical %>% filter(smoke==0) %>% sapply(prop1)

#chisquare test for categorical variables against smoking
lapply(categorical,function(x){chisq.test(categorical$smoke,x)})

#qqplot to check for normality
comp2 %>% select(age,ppwt,wtgain) %>% lapply(qqnorm)

#calculate mean of continuous variables
sapply(continuous,mean)

#calculate mean in strata of smokers
comp2 %>% select(age, ppwt, wtgain, smoke) %>% 
        filter(smoke==1) %>% 
        sapply(mean)

#calculate mean in strata of non-smokers
comp2 %>% select(age, ppwt, wtgain, smoke) %>% 
        filter(smoke==0) %>% 
        sapply(mean)

#ttest assess the relationship between continuous covariates and smoking status
smokers <- comp2 %>% filter(smoke==1)
non_smokers <- comp2 %>% filter(smoke==0)
t.test(smokers$age, non_smokers$age)
t.test(smokers$ppwt, non_smokers$ppwt)
t.test(smokers$wtgain, non_smokers$wtgain)

#change categoricals to factors
comp2$nicu <- as.factor(comp2$nicu)
comp2$race <- as.factor(comp2$race)
comp2$gender <- as.factor(comp2$gender)
comp2$smoke <- as.factor(comp2$smoke)
comp2$ht<- as.factor(comp2$ht)
comp2$bwt_cat <- as.factor(comp2$bwt_cat)

#logistic regression of covariates on risk of bwt_cat
logcov <- function(x){summa <- summary(logreal <- glm(bwt_cat~x,data = comp2,family = "binomial"))
        or <- exp(coef(logreal))
        conf_inter <- exp(confint(logreal))
        A <- list(summa,or,conf_inter)
        return(A)
}
#conduct regression for all categorical and continuous variablesl
lapply(categorical,logcov)
lapply(continuous, logcov)

#stratify regression model by race
white <-comp2 %>% filter(race==1)
black <- comp2 %>% filter(race==2)
other <- comp2 %>% filter(race==3)

#association of smoking with low birth weight in strata of whites, odds ratio, confidence interval
white_model <- glm(bwt_cat~smoke,data = white,family = "binomial")
        summary(white_model)
        exp(coef(white_model))
        exp(confint(white_mode1))
        
#association of smoking with low birth weight in strata of blacks, odds ratio, conficence interval
black_model <- glm(bwt_cat~smoke,data = black,family = "binomial")
        summary(black_model)
        exp(coef(black_model))
        exp(confint(black_model))

#association of smoking with low birth weight in strata of other races, odds ratio, confidence interval
other_model <- glm(bwt_cat~smoke,data = other,family = "binomial")
        summary(other_model)
        exp(coef(other_model))
        exp(confint(other_model))
        
#create marginal tables
vars_1 <- comp2 %>% select(smoke, bwt_cat, race)

#convert variables to numerical
race_strata <- table(vars_1)

#import library
library(DescTools)

#Test for homogeneity of odds ratio
BreslowDayTest(race_strata)

#model 1Crude association of smoking an risk of bwt_cat, odds ratio, confidence interval
model1<- glm(bwt_cat~smoke, data = comp2,family = "binomial")
        summary(model1)
        exp(coef(model1))
        exp(confint(model1))

#model 2 adjusted for all potential confounders, odds ratio, confidence interval
model2 <- glm(bwt_cat~smoke+wtgain, data = comp2,family = "binomial")
        summary(model2)
        exp(coef(model2))
        exp(confint(model2))

#model 3, final multivariable model with interaction term, odds ratio, confidence interval
model3<- glm(bwt_cat~smoke+wtgain+race+race*smoke, data = comp2,family = "binomial")
        summary(model4)
        exp(coef(model4))
        exp(confint(model4))

