dialysis <- read.csv("C:/Users/Kieran/Desktop/Stats Datasets/Semester 2 2022/Regression/Regression Assignment 2/dialysis.csv", stringsAsFactors=TRUE)


library(survival)
library(survminer)
library(gtools)
library(gtsummary)
library(dplyr)
library(tidyverse)
library(flextable)
library(mfp)


#Death is the event so censor should be death = 1, everything else = 0.
table(dialysis$stat1)
#576 died
#223 not
dialysis.d <- subset(dialysis)
#subset
dialysis.d$censor <- as.integer(ifelse(dialysis.d$stat1 == 0, 1, 0))
table(dialysis.d$censor)
#correct, 576 registered as dead, rest are censored
#1 value used as death


dialysis.d$age1 <- quantcut(dialysis.d$age, 3)
table(dialysis.d$age1)
#categorised into groups of 3
dialysis.d$y.surv <- Surv(dialysis.d$yrstotal, dialysis.d$censor)

death.fit <- survfit(y.surv ~ age1, data = dialysis.d)
ggsurvplot(death.fit, conf.int = TRUE)
#Survival.prob is clearly higher for ages 18-48, conversely prob is worst for old ages.
#prob for older ages has a severed decline within the first 5 years before being less severe.
#prob for young 18-48 is a gradual decline.

print(death.fit)
#There is not median as the lowest age category never breachs 0.5 survival probability.
#Less than 133 events

summary(death.fit, times=c(10))
#low cat: 0.58 (0.52:0.65)
#mid.cat: 0.22 (0.17:0.28)
#high.cat: 0.049 (0.028: 0.089)

death.fit2 <- survdiff(y.surv ~ age1, data = dialysis.d) #logrank
death.fit2
#significant 

death.fit3 <- survfit(y.surv ~ age1, data = dialysis.d)
surv_pvalue(death.fit3, test.for.trend = TRUE) #trend
#significant monotonic relationship with ordered categories [linear castegory test]

Cox model, age group, test for evidence of effect of age using wald.
surv.model1 <- coxph(y.surv ~ age1, data = dialysis.d)
summary(surv.model1)

tbl_regression(surv.model1) %>%
  add_global_p()
#Using Wald test, there is evidence that age(Category) has a significant effect

# COX age and diabetes
#factor diabetes
dialysis.d$diabetesf <- factor(dialysis.d$diabetes)

surv.model2 <- coxph(y.surv ~ age + diabetesf, data = dialysis.d)
tbl_regression(surv.model2) %>%
  add_global_p()

levels(dialysis.d$diabetesf) <- c("No", "Yes")


tbl_regression(surv.model2, exponentiate = TRUE,
               label = list (age ~ "Age",
                             diabetesf ~ "Diabetes")) %>%
  add_global_p() %>%
  bold_labels() %>%
  as_flex_table()


#per 1 year of age increases risk of death by 6%
#If diabetes is present then patient has 2.28 times the risk of death than someone without diabetes


#q2i
#Transplant is now the event so censor should be Dialysis = 1, everything else = 0.
dialysis.t <- subset(dialysis)
#128 dialysis cases

dialysis.t$censor <- as.integer(ifelse(dialysis.t$stat1 == 2, 1, 0))
table(dialysis.t$censor)
# age cat
dialysis.t$age.cat <- quantcut(dialysis.t$age, 3)
#new y.surv
dialysis.t$y.surv <- Surv(dialysis.t$yrstotal, dialysis.t$censor)


#data mutation, only select variables I want and factoring variables
#only need y.surv, age, gender, diabetes, age.cat
dialysis.t$gender <- factor(dialysis.t$gender)
dialysis.t$diabetes <- factor(dialysis.t$diabetes)

levels(dialysis.t$age.cat) <- c("18 - 49", "49 - 64", "64 - 85")

dialysis.t %>%
  select(y.surv, age, gender, diabetes, age.cat) %>%
  mutate(
    gender = case_when(gender == 0 ~ "Female",
                       gender == 1 ~ "Male"),
    diabetes = case_when(diabetes == 0 ~ "No",
                         diabetes == 1 ~ "Yes")
  ) -> dialysis.t2


dialysis.t2 %>%
  tbl_uvregression(
    method = coxph,
    y = y.surv,
    exponentiate = TRUE,
    label = list(age ~ "Age",
                 gender ~ "Gender",
                 diabetes ~ "Diabetes",
                 age.cat ~ "Age (Categorised)")) %>%
  bold_labels() %>%
  add_global_p() %>%
  as_flex_table()



#q2ii
colSums(is.na(dialysis.t2[1:5]))
#Just checking for missing values but no missing values so AIC is a valid model validation method

#AGE_cent
hist(dialysis.t2$age)
dialysis.t2$age_cent <- dialysis.t2$age - 54.05907
#----

#KM plot
transplant.fit <- survfit(y.surv ~ age.cat, data = dialysis.t2)
ggsurvplot(transplant.fit, conf.int = TRUE)

transplant.fit <- survfit(y.surv ~ diabetes, data = dialysis.t2)
ggsurvplot(transplant.fit, conf.int = TRUE)

transplant.fit <- survfit(y.surv ~ gender, data = dialysis.t2)
ggsurvplot(transplant.fit, conf.int = TRUE)

print(transplant.fit)


model1.1 <- coxph(y.surv ~ age + gender + diabetes + age.cat, data = dialysis.t2)
car::Anova(model1.1)
modelph <- cox.zph(model1.1)
modelph
#Age categorical confirmed to be breaching PH along with continuous


tbl_regression(model1.1, exponentiate = TRUE,
               label = list (age ~ "Age",
                             diabetes ~ "Diabetes")) %>%
  add_global_p() %>%
  bold_labels() %>%
  as_flex_table()

model1.3 <- coxph(y.surv ~ age_cent + diabetes, data = dialysis.t2)
model1.3

model1.4 <- coxph(y.surv ~ diabetes + age.cat, data = dialysis.t2)

AIC(model1.1)
AIC(model1.2)
AIC(model1.3)
AIC(model1.4)


coxph(y.surv ~ age + gender + diabetes + age.cat, data = dialysis.t2) %>%
AIC()




#AIC for age

modelage.1 <- coxph(y.surv ~ pspline(age, df=4) + diabetes, data = dialysis.t2)
modelage.2 <- coxph(y.surv ~ pspline(age, df=5) + diabetes, data = dialysis.t2)
modelage.3 <- coxph(y.surv ~ pspline(age, df=6) + diabetes, data = dialysis.t2)
modelage.4 <- coxph(y.surv ~ pspline(age, df=7) + diabetes, data = dialysis.t2)
modelage.5 <- coxph(y.surv ~ pspline(age, df=8) + diabetes, data = dialysis.t2)
modelage.6 <- coxph(y.surv ~ pspline(age, df=9) + diabetes, data = dialysis.t2)
modelage.7 <- coxph(y.surv ~ pspline(age, df=10) + diabetes, data = dialysis.t2) #best spline
modelage.8 <- coxph(y.surv ~ pspline(age, df=11) + diabetes, data = dialysis.t2)

termplot(modelage.1)
hist(dialysis.t2$age)
hist(dialysis.t2$agelog)
hist(dialysis.t2$agesqrt)

AIC(modelage.1)
AIC(modelage.2)
AIC(modelage.3)
termplot(modelage.3, term=1)
AIC(modelage.4)
termplot(modelage.4, term=1)
AIC(modelage.5)
AIC(modelage.6)
AIC(modelage.7) #best model --------------------------------------------------------------
AIC(modelage.8)

dialysis.t2$agelog <- log(dialysis.t2$age)
dialysis.t2$agesqrt <- sqrt(dialysis.t2$age)

modelage.9 <- coxph(y.surv ~ dialysis.t2$agelog + diabetes, data = dialysis.t2)
modelage.10 <- coxph(y.surv ~ dialysis.t2$agesqrt + diabetes, data = dialysis.t2)

AIC(modelage.9)
AIC(modelage.10)

dialysis.t2$age_centsquared <- dialysis.t2$age_cent^2
dialysis.t2$age_centsquared2 <- dialysis.t2$age_centsquared*dialysis.t2$age_centsquared

modelage.squared <- coxph(y.surv ~ dialysis.t2$age_cent + age_centsquared + diabetes, data = dialysis.t2) 
modelage.squared2 <- coxph(y.surv ~ dialysis.t2$age_cent + age_centsquared + age_centsquared2 + diabetes, data = dialysis.t2) 

AIC(modelage.squared)
AIC(modelage.squared2)

#fractional polynomial for age
modelage.fp <- coxph(y.surv ~ fp(age) + diabetes, data = dialysis.t2)
modelage.fp
AIC(modelage.fp)
modelph <- cox.zph(modelage.7)
modelph


#Using best spline ----------------------------------------------------------------
finalmodel.t <- coxph(y.surv ~ pspline(age, df=10) + diabetes, data = dialysis.t2) 
finalmodel.t
f.plot <- termplot(finalmodel.t, term=1, se=TRUE)
f.plot
termplot(finalmodel.t, term=1)


tbl_regression(finalmodel.t, exponentiate = TRUE,
               label = list (diabetes ~ "Diabetes")) %>%
  add_global_p() %>%
  bold_labels() %>%
  as_flex_table()

#diagnostics
modelph <- cox.zph(finalmodel.t)
modelph


finalmodel.res<- residuals(finalmodel.t, type="dfbeta")
n <- nrow(finalmodel.res)
## reshape into long format for plotting
finalmodel.res2 <- data.frame(subject=rep(rownames(finalmodel.res), 2), 
                         DFBETA=c(finalmodel.res[,1], finalmodel.res[,2]), 
                         variable=c(rep("age",n ),rep("Yes",n )))
library(car)
Boxplot(DFBETA ~ variable, data=finalmodel.res2, 
        id=list(labels=finalmodel.res2$subject, cex=0.5),
        cex.axis = 0.7)

dialysis.t2[c(391, 570), c("y.surv", "age", "diabetes")]


