library(dplyr)
#read in datafile
prac_data <- read.csv("C:/Users/Chukwuebuka/Desktop/Research practicum/hiv_pcsk9.csv")
#change column name
colnames(prac_data)[colnames(prac_data)=="HugeBlock"]<- "Stenosis50"
colnames(prac_data)
#visualize variables
ggplot(data=prac_data,mapping=aes(x= Cholesterol,y=PCSK9))+geom_point(color="red")+
        scale_x_continuous(breaks = seq(0,300,50),limits = c(0,300))+
        scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+geom_smooth(method='lm')
go <- glm(formula = factor(stenosis) ~ PCSK9, data = prac_data, family="binomial")


#correlation
cor(x = LDL, y =PCSK9, use = "complete.obs",method = "pearson")
pcsksten <- prac_data[,c('PCSK9','Stenosis50')]

colnames(prac_data)
pcsk2na <- pcsksten[complete.cases(pcsksten),]
min(pcsk2na$PCSK9)        
median(pcsk2na$PCSK9)
pcsk2na[!max(pcsk2na$PCSK9)]
hist(pcsk2na$PCSK9,breaks = seq(0,1300,10),xlim = c(0,1300))
#test for linearity
shapiro.test(pcsk2na$PCSK9)
prac_data
#read in data 
prac_data <- read.csv("C:/Users/Chukwuebuka/Desktop/Research practicum/hiv_pcsk9.csv")
#change variable name
colnames(prac_data)[colnames(prac_data)=="HugeBlock"]<- "Stenosis50"
colnames(prac_data)[colnames(prac_data)=="htn"]<- "hypertension"
colnames(prac_data)[colnames(prac_data)=="sysBP"]<- "systolic_bp"
colnames(prac_data)[colnames(prac_data)=="diabp"]<- "diastolic_bp"
colnames(prac_data)[colnames(prac_data)=="alcYears"]<- "alcohol_years"
colnames(practm1)[colnames(practm1)=="coc"]<- "cocaine_use"
library(ggplot2)
#missing values
practm <- prac_data %>% filter(!is.na(PCSK9))
practm1 <- practm %>% filter(!is.na(stenosis))

nrow(practm1%>% filter(is.na(Triglycerides)))
summary(practm1$bmi)
#impute bmi
practm1$bmi[is.na(practm1$bmi)] <- median(practm1$bmi,na.rm = TRUE)
#impute alcohol years
practm1$alcohol_years[is.na(practm1$alcohol_years)] <- median(practm1$alcohol_years,na.rm = TRUE)
#impute cigarette years
practm1$cigYears[is.na(practm1$cigYears)] <- median(practm1$cigYears,na.rm = TRUE)
#impute cardioCRP
practm1$cardioCRP[is.na(practm1$cardioCRP)] <- median(practm1$cardioCRP,na.rm = TRUE)

#histogram of variables
var_hist<- function(x){ggplot(data=prac_data,mapping=aes(x))+
                geom_histogram(binwidth = 5)}
var_hist(prac_data$PCSK9)
var_hist(prac_data$LDL)
var_hist(prac_data$Triglycerides)
var_hist(prac_data$HDL)
var_hist(prac_data$Cholesterol)
var_hist(prac_data$alcohol_years)
#scatterplot of PCSK9 with continuous variables
scatterplot_var <- function(y){ggplot(data=prac_data, mapping=aes(x= PCSK9,y))+geom_point(color="red")+
        scale_x_continuous(breaks=seq(0,250,50), limits = c(0,250))+
        scale_y_continuous()+geom_smooth(method = 'lm')}
scatterplot_var(prac_data$LDL)
scatterplot_var(prac_data$HDL)
scatterplot_var(prac_data$Triglycerides)
scatterplot_var(prac_data$systolic_bp)
scatterplot_var(prac_data$bmi)

#boxplot of variable distribution with stenosis
ggplot(data=prac_data, mapping = aes(x = as.factor(stenosis), y = PCSK9)) + 
        geom_boxplot()+scale_y_continuous(breaks=seq(0,250,50), limits = c(0,250))

ggplot(data=prac_data, mapping = aes(x = as.factor(stenosis), y = LDL)) + 
        geom_boxplot()+scale_y_continuous(breaks=seq(0,300,50), limits = c(0,300)) 

ggplot(data=prac_data, mapping = aes(x = as.factor(stenosis), y = HDL)) + 
        geom_boxplot()+scale_y_continuous(breaks=seq(0,200,50), limits = c(0,200))

ggplot(data=prac_data, mapping = aes(x = as.factor(stenosis), y = cocaineYears)) + 
        geom_boxplot()+ylim(0,35)

#boxplot of variable distribution with obstructive stenosis
ggplot(data=prac_data, mapping = aes(x = as.factor(Stenosis50), y = PCSK9)) + 
        geom_boxplot()+scale_y_continuous(breaks=seq(0,250,50), limits = c(0,250))

ggplot(data=prac_data, mapping = aes(x = as.factor(Stenosis50), y = LDL)) + 
        geom_boxplot()+scale_y_continuous(breaks=seq(0,300,50), limits = c(0,300)) 

ggplot(data=prac_data, mapping = aes(x = as.factor(Stenosis50), y = HDL)) + 
        geom_boxplot()+scale_y_continuous(breaks=seq(0,200,50), limits = c(0,200))

ggplot(data=prac_data, mapping = aes(x = as.factor(Stenosis50), y = cocaineYears)) + 
        geom_boxplot()+ylim(0,35)
#normality test
shapiro.test(prac_data$PCSK9)
shapiro.test(prac_data$LDL)
shapiro.test(prac_data$Triglycerides)
shapiro.test(prac_data$bmi)
#summary statistics
sapply(practm1, median, na.rm=TRUE)

 sapply(practm1,IQR,na.rm=TRUE)
#aummary statistics of continous variable
summary(prac_emm,na.rm=TRUE)
#for stenosis=1 &0
practm1 %>% filter(stenosis==1)%>% summary(na.rm=TRUE)
practm1 %>% filter(stenosis==0) %>% summary(na.rm=TRUE)
#for obstructive stenosis
prac_data %>% filter(Stenosis50==1) %>% summary(na.rm=TRUE)
prac_data %>% filter(Stenosis50==0) %>% summary(na.rm=TRUE)
#function to calculate proportions
prop1 <- function(x){prop.table(table(x))}
#filter categorical variables
categorical <- prac_emm %>% dplyr::select(male,marijuana,diabetes, heroin,
                            parentHeartAttack,hypertension,cocaine_use,stenosis,alcohol,Stenosis50)
                            
Noncategorical <- prac_emm %>% dplyr::select(male,marijuana,diabetes, heroin,
                                        parentHeartAttack,hypertension, stenosis,Stenosis50)    
nrow(categorical %>% filter(cocaine_use==1))
prac_emm
nrow(categorical %>% filter(stenosis==1) %>% filter(cocaine_use==1))
sapply(categorical, prop1)
#filter categorical, stenosis=1&0
categorical %>%  filter(stenosis==1) %>% sapply(prop1)
categorical %>% filter(stenosis==0) %>% sapply(prop1)
#filter categorical, stenosis50
categorical %>%  filter(Stenosis50==1) %>% sapply(prop1)
categorical %>%  filter(Stenosis50==0) %>% sapply(prop1)

#chisquare test for categorical variables against stenosis
sapply(categorical,function(x){chisq.test(categorical$stenosis,x)})
sapply(categorical,function(x){chisq.test(categorical$Stenosis50,x)})

#wilcoxon test for continuous variables
sten1 <- practm1 %>% filter(stenosis==1)
sten0 <- practm1 %>% filter(stenosis==0)
#wiloxon tests
wilcox.test(sten1$Age,sten0$Age) 
wilcox.test(sten1$PCSK9, sten0$PCSK9)
wilcox.test(sten1$cigYears,sten0$cigYears)
wilcox.test(sten1$alcohol_years,sten0$alcohol_years)
wilcox.test(sten1$hivyears,sten0$hivyears)
wilcox.test(sten1$cocaineYears,sten0$cocaineYears)
wilcox.test(sten1$LDL,sten0$LDL)
wilcox.test(sten1$HDL,sten0$HDL)
wilcox.test(sten1$Triglycerides,sten0$Triglycerides)
wilcox.test(sten1$cardioCRP,sten0$cardioCRP)
wilcox.test(sten1$Cholesterol,sten0$Cholesterol)
wilcox.test(sten1$diastolic_bp,sten0$diastolic_bp)
wilcox.test(sten1$systolic_bp,sten0$systolic_bp)
wilcox.test(sten1$systolic_bp,sten0$systolic_bp)
wilcox.test(sten1$bmi,sten0$bmi)
wilcox.test(sten1$Endothelin_1,sten0$Endothelin_1)
wilcox.test(sten1$ART_months,sten0$ART_months)
wilcox.test(sten1$PI_months,sten0$PI_months)
#wilcoxon test for continous variables obstructive stenosis
sten501 <- prac_data %>% filter(Stenosis50==1)
sten500 <- prac_data %>% filter(Stenosis50==0)
wilcox.test(sten501$Age,sten500$Age) 
wilcox.test(sten501$cigYears,sten500$cigYears)
wilcox.test(sten501$PCSK9, sten500$PCSK9)
wilcox.test(sten501$alcohol_years,sten500$alcohol_years)
wilcox.test(sten501$hivyears,sten500$hivyears)
wilcox.test(sten501$cocaineYears,sten500$cocaineYears)
wilcox.test(sten501$LDL,sten500$LDL)
wilcox.test(sten501$HDL,sten500$HDL)
wilcox.test(sten501$Triglycerides,sten500$Triglycerides)
wilcox.test(sten501$cardioCRP,sten500$cardioCRP)
wilcox.test(sten501$Cholesterol,sten500$Cholesterol)
wilcox.test(sten501$diastolic_bp,sten500$diastolic_bp)
wilcox.test(sten501$systolic_bp,sten500$systolic_bp)
wilcox.test(sten501$systolic_bp,sten500$systolic_bp)
wilcox.test(sten501$bmi,sten500$bmi)
wilcox.test(sten501$Endothelin_1,sten500$Endothelin_1)
wilcox.test(sten501$ART_months,sten500$ART_months)
wilcox.test(sten501$PI_months,sten500$PI_months)
# Robust linear Regression analysis between covariate and pcsk9
library(robustbase)
lmage <- lmrob(data=practm1,PCSK9~Age)
summary(lmage)
confint(lmage)
lmcocaineuse <- lmrob(data=practm1, PCSK9~factor( ))
lmMale <- lmrob(data = practm1,PCSK9~factor(male))
summary(lmMale)
confint(lmMale)
lmCigyears <- lmrob(data=practm1, PCSK9~cigYears)
summary(lmCigyears)
confint(lmCigyears)
lmMar <- lmrob(data = practm1, PCSK9~marijuana)
summary(lmMar)
confint(lmMar)
lmAlc <- lmrob(data = practm1, PCSK9~alcohol_years)
summary(lmAlc)
confint(lmAlc)
lmHIV <- lmrob(data = practm1, PCSK9~hivyears)
summary(lmHIV)
confint(lmHIV)
lmCoc <- lmrob(data = practm1, PCSK9~cocaineYears)
summary(lmCoc)
confint(lmCoc)
lmLDL <- lmrob(data=practm1, PCSK9~LDL)
summary(lmLDL)
confint(lmLDL)
lmHDL <- lmrob(data = practm1, PCSK9~HDL)
summary(lmHDL)
confint(lmHDL)
lmTRI <- lmrob(data = practm1, PCSK9~Triglycerides)
summary(lmTRI)
confint(lmTRI)
lmCRP <- lmrob(data = practm1, PCSK9~cardioCRP)
summary(lmCRP)
confint(lmCRP)
lmDia <- lmrob(data = practm1, PCSK9~factor(diabetes))
summary(lmDia)
confint(lmDia)
lmHeroin <- lmrob(data=practm1, PCSK9~factor(heroin))
summary(lmHeroin)
confint(lmHeroin)
lmChol <- lmrob(dat=practm1, PCSK9~Cholesterol)
summary(lmChol)
confint(lmChol)
lmdbp <- lmrob(data = practm1, PCSK9~diastolic_bp)
summary(lmdbp)
confint(lmdbp)
lmsbp <- lmrob(data = practm1, PCSK9~systolic_bp)
summary(lmsbp)
confint(lmsbp)
lmBMI <- lmrob(data = practm1, PCSK9~bmi)
summary(lmBMI)
confint(lmBMI)
lmendo <- lmrob(data = prac_data, PCSK9~Endothelin_1)
summary(lmendo)
confint(lmendo)
lmPHA <- lmrob(data = practm1, PCSK9~factor(parentHeartAttack))
summary(lmPHA)
confint(lmPHA)
lmART <- lmrob(data = practm1, PCSK9~ART_months)
summary(lmART)
confint(lmART)
lmHyp <- lmrob(data = practm1, PCSK9~factor(hypertension))
summary(lmHyp)
confint(lmHyp)
lmPi <- lmrob(data=practm1, PCSK9~PI_months)
summary(lmPi)
confint(lmPi)

lmchrococ <- lmrob(data=prac_emm, PCSK9~(relevel(factor(cocaine_sev),ref="Never")))
summary(lmchrococ)
confint(lmchrococ)

#crude association
logreg <- glm(stenosis~PCSK9,data = practm1,family = "binomial")
summary(logreg)
exp(coef(logreg))
exp(confint(logreg))
#crude association with obstructive stenosis
logsten50 <- glm(Stenosis50~PCSK9,data = prac_emm,family = "binomial")
summary(logsten50)
exp(coef(logsten50))
exp(confint(logsten50))
#Categorize PCSK9

#effect measure modification

prac_emm <- practm1 %>% mutate(cocaine_sev=ifelse(cocaineYears>=15,'Chronic',
                                                  ifelse(cocaine_use==0,'Never','Non-chronic')))
prac_emm %>% dplyr::select(cocaine_use,cocaine_sev,cocaineYears)
           
coc_nev <- prac_emm %>% filter(cocaine_use==0)
summary(coc_nonchr$cocaineYears)
coc_use <- prac_emm %>% filter(cocaine_use==1)
colnames(prac_emm)
#OR in never cocaine use
logit_coc_nev <- glm(stenosis~PCSK9,data = coc_nev,family="binomial")
summary(logit_coc_nev)
exp(coef(logit_coc_nev))
exp(confint(logit_coc_nev))
logit_coc_nev50 <- glm(Stenosis50~PCSK9,data = coc_nev,family="binomial")
summary(logit_coc_nev50)
exp(coef(logit_coc_nev50))
exp(confint(logit_coc_nev50))

#OR in cocaine use
logit_coc_chr <- glm(stenosis~PCSK9,data = coc_use,family="binomial")
summary(logit_coc_chr)
exp(coef(logit_coc_chr))
exp(confint(logit_coc_chr))

logit_coc_chr50 <- glm(Stenosis50~PCSK9,data = coc_use,family="binomial")
summary(logit_coc_chr50)
exp(coef(logit_coc_chr50))
exp(confint(logit_coc_chr50))
#model 
logreg2<- lmrob(Cholesterol~PCSK9*chronic_coc,data = prac_emm)
summary(logreg2)
exp(coef(logreg2))

exp(coef(logreg2))


#covariates effect modification 

lmage<- rlm(data=prac_data,PCSK9~Age)
summary(lmage)
confint(lmage)
lmMaleMOD <- lm(data = prac_data,PCSK9~male)
summary(lmMale)
confint(lmMale)
lmCigyears <- lm(data=prac_data, PCSK9~cigYears)
summary(lmCigyears)
confint(lmCigyears)
library(robustbase)
#
lmageMOD0 <- lmrob(data=Chr_coc0,PCSK9~Age)
summary(lmageMOD0)
confint(lmageMOD0)
#
lmageMOD1 <- lmrob(data=Chr_coc1,PCSK9~Age)
summary(lmageMOD1)
confint(lmageMOD1)



lmMale <- lmrob(data = prac_data,PCSK9~male)
summary(lmMale)
confint(lmMale)
lmCigyears <- lmrob(data=prac_data, PCSK9~cigYears)
summary(lmCigyears)
confint(lmCigyears)
lmMar <- lmrob(data = prac_data, PCSK9~marijuana)
summary(lmMar)
confint(lmMar)
lmAlc <- lmrob(data = prac_data, PCSK9~alcohol_years)
summary(lmAlc)
confint(lmAlc)
lmHIV <- lmrob(data = prac_data, PCSK9~hivyears)
summary(lmHIV)
confint(lmHIV)
lmCoc <- lmrob(data = prac_data, PCSK9~cocaineYears)
summary(lmCoc)
confint(lmCoc)
lmLDL <- lmrob(data=prac_data, PCSK9~LDL)
summary(lmLDL)
confint(lmLDL)
lmHDL <- lmrob(data = prac_data, PCSK9~HDL)
summary(lmHDL)
confint(lmHDL)

lmTRI2a<- lmrob(data = Chr_coc0, Triglycerides~PCSK9)
summary(lmTRI2a)
confint(lmTRI2a)

lmTRI2b<- lmrob(data = Chr_coc1, Triglycerides~PCSK9)
summary(lmTRI2b)
confint(lmTRI2b)

lmCRP <- lmrob(data = prac_data, PCSK9~cardioCRP)
summary(lmCRP)
confint(lmCRP)
lmDia <- lmrob(data = prac_data, PCSK9~diabetes)
summary(lmDia)
confint(lmDia)
lmHeroin <- lmrob(data=prac_data, PCSK9~heroin)
summary(lmHeroin)
confint(lmHeroin)
lmChol <- lmrob(dat=prac_data, PCSK9~Cholesterol)
summary(lmChol)
confint(lmChol)
lmdbp <- lmrob(data = prac_data, PCSK9~diastolic_bp)
summary(lmdbp)
confint(lmdbp)
lmsbp <- lmrob(data = prac_data, PCSK9~systolic_bp)
summary(lmsbp)
confint(lmsbp)
lmBMI <- lmrob(data = prac_data, PCSK9~bmi)
summary(lmBMI)
confint(lmBMI)
lmendo <- lmrob(data = prac_data, PCSK9~Endothelin_1)
summary(lmendo)
confint(lmendo)
lmPHA <- lmrob(data = prac_data, PCSK9~parentHeartAttack)
summary(lmPHA)
confint(lmPHA)
lmART <- lmrob(data = prac_data, PCSK9~ART_months)
summary(lmART)
confint(lmART)
lmHyp <- lmrob(data = prac_data, PCSK9~factor(hypertension))
summary(lmHyp)
confint(lmHyp)
lmPi <- lmrob(data=prac_data, PCSK9~PI_months)
summary(lmPi)
confint(lmPi)








#turn pcsk9 into categorical
prac_PCSK9 <- prac_emm %>% mutate(PCSK9_cat=ifelse(PCSK9>=81,1,0))
logpcsk9 <- glm(stenosis~factor(PCSK9_cat), data = prac_PCSK9,family="binomial")
summary(logpcsk9)
exp(coef(logpcsk9))
confint(logpcsk9)
exp(confint(logpcsk9))

logpcsk950 <- glm(Stenosis50~factor(PCSK9_cat), data=prac_PCSK9, family="binomial")
summary(logpcsk950)
exp(coef(logpcsk950))
exp(confint(logpcsk950))
 
#Effect modification for categorical pcsk9
coc_nev1 <- prac_PCSK9 %>% filter(cocaine_use==0)
nrow(coc_use1 %>% filter(PCSK9_cat==1))

coc_use1 <- prac_PCSK9 %>% filter(cocaine_use==1)
colnames(prac_emm)

#EFFECT MEASURE MODIFICATION
log_EMM<- glm(stenosis~PCSK9_cat+Age+male+cocaine_use,data = prac_PCSK9,family="binomial")
summary(log_EMM)
exp(coef(log_EMM))
exp(confint(log_EMM))
log_EMM_int <- glm(stenosis~PCSK9_cat+Age+male+cocaine_use+PCSK9_cat*cocaine_use,data = prac_PCSK9,family="binomial")
 exp(coef(log_EMM_int))  
summary(log_EMM_int)
confint(log_EMM_int)
confint(exp(0.6958-0.4426))
exp(confint(log_EMM_int))
#compare models
anova(log_EMM,log_EMM_int,test = "LRT")
#OR in never cocaine use
logit_coc_nev1 <- glm(stenosis~factor(PCSK9_cat)+Age+male,data = coc_nev1,family="binomial")
summary(logit_coc_nev1)
exp(coef(logit_coc_nev1))
exp(confint(logit_coc_nev1))

logit_coc_nev501 <- glm(Stenosis50~PCSK9,data = coc_nev1,family="binomial")
summary(logit_coc_nev501)
exp(coef(logit_coc_nev501))
exp(confint(logit_coc_nev501))
#OR in cocaine use
colnames(coc_use1)

logit_coc_chr1 <- glm(stenosis~factor(PCSK9_cat)+Age+male,data = coc_use1,family="binomial")
summary(logit_coc_chr1)
exp(coef(logit_coc_chr1))
exp(confint(logit_coc_chr1))




logit_coc_nev50 <- glm(Stenosis50~PCSK9,data = coc_nev,family="binomial")
summary(logit_coc_nev50)
exp(coef(logit_coc_nev50))
exp(confint(logit_coc_nev50))


lograndom <- glm(stenosis~factor(PCSK9_cat)+Age+male+factor(PCSK9_cat)*cocaine_use,data = prac_PCSK9,family="binomial")
summary(lograndom)
exp(coef(lograndom))
exp(confint(lograndom))

lograndomcoc1<- glm(stenosis~factor(PCSK9_cat),data = prac_PCSK9coc1,family="binomial")
summary(lograndomcoc1)
exp(coef(lograndomcoc1))
exp(confint(lograndomcoc1))
 prac_PCSK9coc1 <- prac_PCSK9%>% filter(chronic_coc==1)
 
 #regression analysis of the relationships between covariates and concentration of pcsk9
 logAge <- glm(factor(PCSK9_cat)~Age,data=prac_PCSK9,family="binomial")
 summary(logAge)
 exp(coef(logAge))
 exp(confint(logAge))
 
 logMale <- glm(factor(PCSK9_cat)~factor(male),data=prac_PCSK9,family="binomial")
 summary(logMale)
 exp(coef(logMale))
 exp(confint(logMale))
 
 colnames(prac_PCSK9)
 
 logCigyears <- glm(factor(PCSK9_cat)~cigYears,data=prac_PCSK9,family="binomial")
 summary(logCigyears)
 exp(coef(logCigyears))
 exp(confint(logCigyears))
 
 logMJ <- glm(factor(PCSK9_cat)~marijuana,data=prac_PCSK9,family="binomial")
 summary(logMJ)
 exp(coef(logMJ))
 exp(confint(logMJ))
 
 logAlc <- glm(factor(PCSK9_cat)~alcohol_years,data=prac_PCSK9,family="binomial")
 summary(logAlc)
 exp(coef(logAlc))
 exp(confint(logAlc))
 
 logHIV <- glm(factor(PCSK9_cat)~hivyears,data=prac_PCSK9,family="binomial")
 summary(logHIV)
 exp(coef(logHIV))
 exp(confint(logHIV))
 
 logcocyears <- glm(factor(PCSK9_cat)~cocaineYears,data=prac_PCSK9,family="binomial")
 summary(logcocyears)
 exp(coef(logcocyears))
 exp(confint(logcocyears))
 
 logLDL <- glm(factor(PCSK9_cat)~LDL,data=prac_PCSK9,family="binomial")
 summary(logLDL)
 exp(coef(logLDL))
 exp(confint(logLDL))
 
 logHDL <- glm(factor(PCSK9_cat)~HDL,data=prac_PCSK9,family="binomial")
 summary(logHDL)
 exp(coef(logHDL))
 exp(confint(logHDL))
 
 logTri<- glm(factor(PCSK9_cat)~Triglycerides,data=prac_PCSK9,family="binomial")
 summary(logTri)
 exp(coef(logTri))
 exp(confint(logTri))
 
 logCRP <- glm(factor(PCSK9_cat)~cardioCRP,data=prac_PCSK9,family="binomial")
 summary(logCRP)
 exp(coef(logCRP))
 exp(confint(logCRP))
 
 logdiab <- glm(factor(PCSK9_cat)~diabetes,data=prac_PCSK9,family="binomial")
 summary(logdiab)
 exp(coef(logdiab))
 exp(confint(logdiab))
 
 logheroin<- glm(factor(PCSK9_cat)~heroin,data=prac_PCSK9,family="binomial")
 summary(logheroin)
 exp(coef(logheroin))
 exp(confint(logheroin))
 
 logChol<- glm(factor(PCSK9_cat)~Cholesterol,data=prac_PCSK9,family="binomial")
 summary(logChol)
 exp(coef(logChol))
 exp(confint(logChol))
 
 logDiastolic <- glm(factor(PCSK9_cat)~diastolic_bp,data=prac_PCSK9,family="binomial")
 summary(logDiastolic)
 exp(coef(logDiastolic))
 exp(confint(logDiastolic))
 
 logSystolic <- glm(factor(PCSK9_cat)~systolic_bp,data=prac_PCSK9,family="binomial")
 summary(logSystolic)
 exp(coef(logSystolic))
 exp(confint(logSystolic))
 
 logBMI <- glm(factor(PCSK9_cat)~bmi,data=prac_PCSK9,family="binomial")
 summary(logBMI)
 exp(coef(logBMI))
 exp(confint(logBMI))
 
 logHattack<- glm(factor(PCSK9_cat)~parentHeartAttack,data=prac_PCSK9,family="binomial")
 summary(logHattack)
 exp(coef(logHattack))
 exp(confint(logHattack))
 
 logART <- glm(factor(PCSK9_cat)~ART_months,data=prac_PCSK9,family="binomial")
 summary(logART)
 exp(coef(logART))
 exp(confint(logART))
 
 logHYP <- glm(factor(PCSK9_cat)~hypertension,data=prac_PCSK9,family="binomial")
 summary(logHYP)
 exp(coef(logHYP))
 exp(confint(logHYP))
 
 logPI <- glm(factor(PCSK9_cat)~PI_months,data=prac_PCSK9,family="binomial")
 summary(logPI)
 exp(coef(logPI))
 exp(confint(logPI))
 
 Logcoc_use <- glm(factor(PCSK9_cat)~cocaine_use,data=prac_PCSK9,family="binomial")
 summary(Logcoc_use)
 exp(coef(Logcoc_use))
 exp(confint(Logcoc_use))
 
 
 #Final model
 logFinal <- glm(stenosis~factor(PCSK9_cat)+male+Age,data = prac_PCSK9,family="binomial")
 summary(logFinal)
 exp(coef(logFinal))
 exp(confint(logFinal))
 colnames(prac_PCSK9)
 nrow(prac_PCSK9 %>% filter(PCSK9_cat==0))
#Regression analysis of PCSK9 with other factors(LiPID parameters)
 lmCholest <- lmrob(data=practm1, Cholesterol~PCSK9)
 summary(lmCholest)
 confint(lmCholest)
 #PCSK9 to Cholesterol(Non-cocaine)
 lmCholest0 <- lmrob(data=coc_nev, Cholesterol~PCSK9)
 summary(lmCholest0)
 confint(lmCholest0)
 #PCSK9  to Cholesterol(cocaine NOn chronic)
 lmCholest1 <- lmrob(data=coc_nonchr, Cholesterol~PCSK9)
 summary(lmCholest1)
 confint(lmCholest1)
 #PCSK9 to Cholesterol(chronic cocaine)
 lmCholest2 <- lmrob(data=coc_chr, Cholesterol~PCSK9)
 summary(lmCholest2)
 confint(lmCholest2)
 
 # PCSK9 to Triglyceride
 lmTriglyc <- lmrob(data=prac_emm, Triglycerides~PCSK9)
 summary(lmTriglyc)
 confint(lmTriglyc)
 #PCSK9 to Triglycerides(Non-cocaine)
 lmTriglyc0 <- lmrob(data=coc_nev, Triglycerides~PCSK9)
 summary(lmTriglyc0)
 confint(lmTriglyc0)
 #PCSK9  to Cholesterol(cocaine non-chronic)
 lmTriglyc1 <- lmrob(data=coc_nonchr, Triglycerides~PCSK9)
 summary(lmTriglyc1)
 confint(lmTriglyc1)
 #PCSK9  to Cholesterol(chronic cocaine)
 lmTriglyc2 <- lmrob(data=coc_chr, Triglycerides~PCSK9)
 summary(lmTriglyc2)
 confint(lmTriglyc2)
 
 # PCSK9 to HDL-C
 lmHDLC <- lmrob(data=practm1, HDL~PCSK9)
 summary(lmHDLC)
 confint(lmHDLC)
 #PCSK9 to HDL-C(Non-cocaine)
 lmHDLC0 <- lmrob(data=coc_nev, HDL~PCSK9)
 summary(lmHDLC0)
 confint(lmHDLC0)
 #PCSK9  to HDL-C(non-chronic cocaine)
 lmHDLC1 <- lmrob(data=coc_nonchr, HDL~PCSK9)
 summary(lmHDLC1)
 confint(lmHDLC1)
 #PCSK9  to HDL-C(chronic cocaine)
 lmHDLC2 <- lmrob(data=coc_chr, HDL~PCSK9)
 summary(lmHDLC2)
 confint(lmHDLC2)
 
 # PCSK9 to LDL-C
 lmLDLC <- lmrob(data=practm1, LDL~PCSK9)
 summary(lmLDLC)
 confint(lmLDLC)
 #PCSK9 to LDL-C(Non-cocaine)
 lmLDLC0 <- lmrob(data=coc_nev, LDL~PCSK9)
 summary(lmLDLC0)
 confint(lmLDLC0)
 #PCSK9  to LDL-C(non-chronic cocaine)
 lmLDLC1 <- lmrob(data=coc_nonchr, LDL~PCSK9)
 summary(lmLDLC1)
 confint(lmLDLC1)
# PCSK9  to LDL-C(chronic cocaine)
 lmLDLC2 <- lmrob(data=coc_chr, LDL~PCSK9)
 summary(lmLDLC2)
 confint(lmLDLC2)
 #Assesment of effect modification by sex
 
 prac_male <- prac_PCSK9 %>% filter(male==1)
 
prac_female <-  prac_PCSK9 %>% filter(male==0)

#OR in male 
logit_male <- glm(stenosis~factor(PCSK9_cat),data = prac_male,family="binomial")
summary(logit_male)
exp(coef(logit_male))
exp(confint(logit_male))

#OR in female
logit_female <- glm(stenosis~factor(PCSK9_cat),data = prac_female,family="binomial")
summary(logit_female)
exp(coef(logit_female))
exp(confint(logit_female))

#interaction term sex
logit_sex <- glm(stenosis~factor(PCSK9_cat)+male*factor(PCSK9_cat),data = prac_PCSK9,family="binomial")
summary(logit_sex)
exp(coef(logit_sex))
exp(confint(logit_sex))

#interaction term cocaine use
logit_cocu <- glm(stenosis~factor(PCSK9_cat)+cocaine_use+cocaine_use*factor(PCSK9_cat),data = prac_PCSK9,family="binomial")
summary(logit_cocu)
exp(coef(logit_cocu))
exp(confint(logit_cocu))
#breslow day
coc_tabs <- xtabs(~PCSK9_cat+stenosis+cocaine_use,data = prac_PCSK9)
coc_tabs
install.packages("DescTools")
BreslowDayTest(coc_tabs)

#coronary stenosis 
coronary_stenosis <- prac_PCSK9 %>% filter(stenosis==1)
nrow(coronary_stenosis %>% filter(cocaine_use==1) %>% filter(PCSK9_cat==1))
nrow(prac_PCSK9 %>% filter(cocaine_use==1) %>% filter(PCSK9_cat==1) %>% filter(stenosis==0))