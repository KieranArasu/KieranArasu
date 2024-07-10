library(ggplot2)
library(tidyverse)
library(olsrr)
library(moments) 
hosp2022 <- read.csv("C:/Users/Kieran/Desktop/Stats Datasets/Regression/Assignment 2/hosp2022.csv")

#Analysing Creatine levels.
#'[Step 1]
#data formatting,
hosp2022$sex2 <- factor(hosp2022$sex, labels = c("Male", "Female"))
hosp2022$respiratory2 <- factor(hosp2022$respiratory, labels = c("No", "Yes"))
hosp2022$cardiovascular2 <- factor(hosp2022$cardiovascular, labels = c("No", "Yes"))
hosp2022$nurse2 <- factor(hosp2022$nurse, labels = c("No", "Yes"))
hosp2022$weightc <- hosp2022$weight - 87.008

#'*CREATINE LEVELS, OUTCOME VARIABLE [DEPENDENT VARIABLE]*
#'*logCreatine*

hosp2022$logcreat <- log(hosp2022$creat)
hosp2022$logage <- log(hosp2022$age)
hosp2022$logweight <- log(hosp2022$weight)

#Quick Initial shotgun linear test via all variables
lm.initialtest <- lm(creat ~ weight + age + sex2 + respiratory2 + cardiovascular2 +
                       SBP_high + SBP_low + HR + nurse2, data = hosp2022)
summary(lm.initialtest) #Summary shows
ols_vif_tol(lm.initialtest) #VIF test
AIC(lm.initialtest)
#Initial shotgun linear test via all variables

#ALL VARIABLES CENTERED(CONT)
lm.initialtest2 <- lm(creat ~ weightc + agec + sex2 + respiratory2 + cardiovascular2 +
                       SBP_highc + HRc + nurse2 + weightc*agec + weightc*sex2, data = hosp2022)
summary(lm.initialtest2) #Summary shows
ols_vif_tol(lm.initialtest2)
AIC(lm.initialtest2)
BIC(lm.initialtest2)

#'[Step 2]
#---------------------------------------------------------------------
#'*Age*
summary(hosp2022$age)
table()
hosp2022$agecat <- cut(hosp2022$age,
                       breaks=c(40, 50, 60, 70),
                       labels=c('40-50', '51-60', '61-70'))

hosp2022$agecat2 <- cut(hosp2022$age,
                       breaks=c(40, 45, 50, 55, 60, 65, 70),
                       labels=c('40-45', '46-50', '51-55', '56-60', '61-65', '66-70'))

ggplot(data = hosp2022, mapping = aes(x = age, y = creat)) +
  geom_point(colour = "firebrick", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Age vs Creatine Levels on admission", x = "Age (Years)", y = "Creatine levels (Micromoles/L)")  #plot showing age vs creat

ggplot(data = hosp2022, mapping = aes(x = age, y = creat)) +
  geom_point(mapping = aes(color=sex2, position = "jitter")) +
  geom_smooth(method = "lm") +
  labs(title = "Age vs Creatine Levels on admission", x = "Age (Years)", y = "Creatine levels (Micromoles/L)") #plot showing age vs creat (sex coloured)

ggplot(hosp2022,aes(y=creat,x=age,color=factor(sex2)))+
  geom_point(position = "jitter")+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Age vs Creatine Levels on admission", x = "Age (Years)", y = "Creatine levels (Micromoles/L)") #plot showing male vs female [Creat vs age]


lm.age <-lm(creat ~ age, data = hosp2022)
summary(lm.age)
plot(lm.age, 1)
plot(lm.age, 2)
plot(lm.age, 5)

ggplot(data = hosp2022, mapping = aes(x = age, y = weight)) +     #age weight plot
  geom_point(colour = "firebrick", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Age vs Weight", x = "Age (Years)", y = "Weight")

summary(lm(creat ~ sex2 + age, data = hosp2022))



ggplot(data = hosp2022, mapping = aes(x = age, y = weight)) +   #age weight plot with sex
  geom_point(mapping = aes(color=sex2)) +
  geom_smooth(method = "lm") +
  labs(title = "Age vs Weight on admission", x = "Age (Years)", y = "Weight")

ggplot(hosp2022,aes(y=weight,x=age,color=factor(sex2)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Age vs Weight Levels on admission", x = "Age (Years)", y = "Weight (Kg)") #plot showing male vs female [Weight vs age]


ggplot(hosp2022,aes(y=creat,x=weight,color=age)) + #weight vs creat, age coloured
  geom_point()+ 
  stat_smooth(method="lm",se=FALSE) + 
  scale_color_gradientn(colours = rainbow(5))

summary(lm(weight ~ agec, data = hosp2022)) #check for association between age and weight
summary(lm(weight ~ age + sex2, data = hosp2022)) #check for association between age and weight adjusting for sex
summary(lm(creat ~ agec + sex2, data = hosp2022)) #age vs creat (sex adjusted) test

summary(lm(age ~ sex2, data = hosp2022))

ggplot(data = hosp2022, aes(x = sex2, y = age)) +
  geom_boxplot(fill = "azure2") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Sex and Age", x = "Sex", y = "Age")

#------------------------------------[AGE CENTERED]*
print(summary(hosp2022$age), digits = 10)
hosp2022$agec <- hosp2022$age - 55.1085



#------------------------------------
#'*Sex*
table(hosp2022$sex2)

#SUS
ggplot(data = hosp2022, aes(x = sex2, y = creat)) +
  geom_boxplot(fill = "azure2") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Sex and Creatine levels", x = "Sex", y = "Creatine levels (Micromoles/L)")

lm.sex <- lm(creat ~ sex2, data = hosp2022)
summary(lm.sex)
plot(lm.sex, 1)
plot(lm.sex, 2)
plot(lm.sex, 5)

boxplot(lm.sex$residuals ~ hosp2022$sex2,     #Residuals per group
        xlab="Sex", ylab="Residuals", main = "Residuals vs Sex Category")

ggplot(data = hosp2022, aes(x = sex2, y = weight)) +
  geom_boxplot(fill = "azure2") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Sex and Weight", x = "Sex", y = "Weight (Kg)")

ggplot(hosp2022,aes(y=creat,x=weight,color=factor(sex2)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Weight vs Creatine when adjusted for Sex", x = "Weight (Kg)", 
       y = "Creatine levels (Micromoles/L)")                               #plot showing male vs female, Creatine vs Weight

  #plot showing male vs female

summary(lm(weight ~ sex2, data = hosp2022))


ggplot(hosp2022,aes(y=creat,x=weight,color=factor(agecat)))+
  geom_point(position = "jitter")+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Weight vs Creatine when adjusted for Sex", x = "Weight (Kg)", 
       y = "Creatine levels (Micromoles/L)")                               #plot showing male vs female, Creatine vs Weight


#------------------------------------
#'*Respiratory*
#Nothing happening
ggplot(data = hosp2022, aes(x = respiratory2, y = creat)) +
  geom_boxplot(fill = "azure2") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Boxplot of Chronic Respiratory Condition and Creatine levels", x = "Presence of Chronic Respiratory Condition", y = "Creatine levels (Micromoles/L)")

summary(lm(creat ~ respiratory2, data = hosp2022))
plot(lm.resp)

summary(lm(creat ~ respiratory2 + weight + respiratory2*weight, data = hosp2022)) #interaction test

#------------------------------------
#'*Cardiovascular*
#Nothing happening
ggplot(data = hosp2022, aes(x = cardiovascular2, y = creat)) +
  geom_boxplot(fill = "lightpink") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Boxplot of Chronic Cardiovascular Condition and Creatine levels", x = "Presence of Chronic Cardiovascular Condition", y = "Creatine levels (Micromoles/L)")

ggplot(data = hosp2022, aes(x = cardiovascular2, y = logcreat)) +
  geom_boxplot(fill = "lightpink") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Boxplot of Chronic Cardiovascular Condition and Creatine levels", x = "Presence of Chronic Cardiovascular Condition", y = "log(Creatine levels)")

D
plot(lm.resp)

summary(lm(creat ~ cardiovascular2 + weight + cardiovascular2*weight, data = hosp2022)) #interaction test

#-----------------------------------
#'*Primary Explanatory Variable[WEIGHT]*
ggplot(data = hosp2022, mapping = aes(x = weight, y = creat)) +
  geom_point(colour = "coral", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)") # weight vs creat

ggplot(data = hosp2022, mapping = aes(x = weight, y = creat)) +
  geom_point(mapping = aes(color=sex2), position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)") # weight vs creat sex coloured

ggplot(data = hosp2022, mapping = aes(x = weight, y = creat)) +
  geom_point(mapping = aes(color=agecat), position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)") # weight vs creat age coloured

ggplot(hosp2022,aes(y=logcreat,x=weight,color=factor(sex2)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)")#plot showing male vs female [Creat vs Weight sex line]


ggplot(hosp2022,aes(y=creat,x=weight,color=factor(agecat)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)")#plot showing male vs female [Creat vs Weight age line]



lm.weight <- lm(creat ~ weight, data = hosp2022) #summary
plot(lm.weight, 1)
plot(lm.weight, 2)
plot(lm.weight, 5)

summary(lm.weight)

ggplot(mapping = aes(x = lm.weight$residuals, y = hosp2022$weight)) +   #RESIDUAL vs WEIGHT PLOT
  geom_point(position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Residuals vs Predictor variable (weight)", x = "Residuals", y = "Weight (kg)")

hist(lm.weight$residuals, breaks = 10)


lm.weight <- lm(creat ~ weight + sex2 + age, data = hosp2022)
plot(lm.weight, 1)
plot(lm.weight, 2)
plot(lm.weight, 5)

plot(creat~weight + age,xlab="Weight",ylab="Creat",data=hosp2022)
lines(lowess(hosp2022$weight + age ~ hosp2022$creat, f=.2), col=4,lwd=2)

#-----------------------------------
#'*Primary Explanatory Variable[WEIGHT CENTERED]*

print(summary(hosp2022$weight), digits = 10)
hosp2022$weightc <- hosp2022$weight - 87.008


ggplot(data = hosp2022, mapping = aes(x = weightc, y = creat)) +
  geom_point(colour = "coral", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)") # weight vs creat

ggplot(data = hosp2022, mapping = aes(x = weight, y = creat)) +
  geom_point(mapping = aes(color=sex2), position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)") # weight vs creat sex coloured

ggplot(data = hosp2022, mapping = aes(x = weight, y = creat)) +
  geom_point(mapping = aes(color=agecat), position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)") # weight vs creat age coloured

ggplot(hosp2022,aes(y=logcreat,x=weight,color=factor(agecat)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)")#plot showing male vs female [Creat vs Weight age line]

ggplot(hosp2022,aes(y=creat,x=weight,color=factor(agecat2)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Weight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)")#plot showing male vs female [Creat vs Weight age line]


lm.weight <- lm(creat ~ weightc, data = hosp2022) #summary
plot(lm.weight, 1)
plot(lm.weight, 2)
plot(lm.weight, 5)

summary(lm.weight)

ggplot(mapping = aes(x = lm.weight$residuals, y = hosp2022$weight)) +   #RESIDUAL vs WEIGHT PLOT
  geom_point(position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Residuals vs Predictor variable (weight)", x = "Residuals", y = "Weight (kg)")

hist(lm.weight$residuals, breaks = 10)


lm.weight <- lm(creat ~ weight + sex2 + age, data = hosp2022)
plot(lm.weight, 1)
plot(lm.weight, 2)
plot(lm.weight, 5)

plot(creat~weight + age,xlab="Weight",ylab="Creat",data=hosp2022)
lines(lowess(hosp2022$weight + age ~ hosp2022$creat, f=.2), col=4,lwd=2)
#-----------------------------------
#'*Primary Explanatory Variable[Log WEIGHT]*

ggplot(data = hosp2022, mapping = aes(x = logweight, y = creat)) +
  geom_point(colour = "coral", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "logWeight vs Creatine Levels on admission", x = "Weight (Kg)", y = "Creatine levels (Micromoles/L)") # weight vs creat

ggplot(hosp2022,aes(y=creat,x=logweight,color=factor(sex2)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Weight vs Creatine Levels on admission", x = "logWeight (Kg)", y = "Creatine levels (Micromoles/L)")#plot showing male vs female [Creat vs Weight sex line]

ggplot(hosp2022,aes(y=logcreat,x=logweight,color=factor(sex2)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Weight vs Creatine Levels on admission", x = "logWeight (Kg)", y = "Creatine levels (Micromoles/L)")#plot showing male vs female [Creat vs Weight sex line]

lm.logweight <- lm(logcreat ~ logweight +sex2, data = hosp2022)
plot(lm.logweight, 1)
plot(lm.logweight, 2)
plot(lm.logweight, 5)


#-----------------------------------
print(summary(hosp2022$SBP_high), digits = 10)
hosp2022$SBP_highc <- hosp2022$SBP_high - 148.7985

#'*SBP_high*
#Nothing happening
ggplot(data = hosp2022, mapping = aes(x = SBP_high, y = creat)) +
  geom_point(colour = "dodgerblue", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Max Systolic blood pressure vs Creatine Levels on admission", x = "Maximum Systolic blood pressure measured on admission (mmHG^-1)", y = "Creatine levels (Micromoles/L)")


summary(lm(creat ~ SBP_high, data = hosp2022))
summary(lm(creat ~ SBP_high + weight + SBP_high*weight, data = hosp2022)) #interaction test
#-----------------------------------
print(summary(hosp2022$SBP_low), digits = 10)
hosp2022$SBP_lowc <- hosp2022$SBP_low - 108.8515


#'*SBP_low*
#Nothing happening
ggplot(data = hosp2022, mapping = aes(x = SBP_low, y = creat)) +
  geom_point(colour = "firebrick", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Minimum Systolic blood pressure vs Creatine Levels on admission", x = "Minimum Systolic blood pressure measured on admission (mmHG^-1)", y = "Creatine levels (Micromoles/L)")


summary(lm(creat ~ SBP_low, data = hosp2022))

#-----------------------------------
#'*HR*
#Something happening here...
print(summary(hosp2022$HR), digits = 10)
hosp2022$HRc <- hosp2022$HR - 55.1085

ggplot(data = hosp2022, mapping = aes(x = HR, y = creat)) +
  geom_point(colour = "springgreen4", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Resting Heartrate vs Creatine Levels on admission", x = "Resting Heartrate on admission (beats/min)", y = "Creatine levels (Micromoles/L)")

ggplot(data = hosp2022, mapping = aes(x = HR, y = creat)) +
  geom_point(mapping = aes(color=sex2), position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Resting Heartrate vs Creatine Levels on admission", x = "Resting Heartrate on admission (beats/min)", y = "Creatine levels (Micromoles/L)")


lm.heart <- lm(creat ~ HR, data = hosp2022)
summary(lm.heart)
plot(lm.heart,1) # The residual versus fitted plot
plot(lm.heart,2) # The normal quantile plot
plot(lm.heart,5) # Residuals vs leverage
hist(lm.heart$residuals)

ggplot(data = hosp2022, mapping = aes(x = HR, y = weight)) +
  geom_point(colour = "springgreen4", position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Resting Heartrate vs Creatine Levels on admission", x = "Resting Heartrate on admission (beats/min)", y = "Creatine levels (Micromoles/L)")


ggplot(hosp2022,aes(y=creat,x=HR,color=factor(sex2)))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Resting Heartrate vs Creatine Levels on admission", x = "Resting Heartrate on admission (beats/min)", 
       y = "Creatine levels (Micromoles/L)")#plot showing male vs female



summary(lm(weight ~ HR, data = hosp2022))
summary(lm(creat ~ HRc + sex2, data = hosp2022))

summary(lm(weight ~ HR + sex2, data = hosp2022))


#'[plots]

ggplot(data = hosp2022, aes(x = weight, y = HR)) +
  geom_point() + 
  geom_smooth(method = "lm")


ggplot(data = hosp2022, aes(x = HR, y = creat)) +
  geom_point() + 
  geom_smooth(method = "lm")


ggplot(data = hosp2022, aes(x = HR, y = creat)) +
  geom_point(mapping = aes(color=sex2)) + 
  geom_smooth(method = "lm")
#I believe Sex is actively confounding this relation


#-----------------------------------
#'*nurse*
#Something happening here...

ggplot(data = hosp2022, aes(x = nurse2, y = creat)) +
  geom_boxplot(fill = "azure2") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Boxplot of Participant opting into nurse program and Creatine levels", x = "Participant opting into speciality nurse program", y = "Creatine levels (Micromoles/L)")

ggplot(data = hosp2022, aes(x = nurse2, y = weight)) +
  geom_boxplot(fill = "azure2") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Boxplot of Participant opting into nurse program and Weight", x = "Participant opting into speciality nurse program", y = "Weight (Kg)")

summary(lm(weight ~ nurse2, data = hosp2022)) #Check for association between weight and nurse

lm.nurse <- lm(creat ~ nurse2, data = hosp2022)
summary(lm.nurse)
plot(lm.nurse,1) # The residual versus fitted plot
plot(lm.nurse,2) # The normal quantile plot
plot(lm.nurse,5) # Residuals vs leverage

lm.nurse <- lm(creat ~ nurse2 + sex2, data = hosp2022)
summary(lm.nurse)

summary(lm(creat ~ nurse2 + sex2 + nurse2*sex2, data = hosp2022)) #Check for association between creat and nurse (When adjusting for sex)
summary(lm(creat ~ nurse2 + sex2 + age, data = hosp2022)) #Check for association between creat and nurse

ggplot(data = hosp2022, aes(x = nurse2, y = creat,color=factor(sex2))) +
  geom_boxplot(fill = "azure2") + 
  geom_smooth(method = "lm") +
  stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point") +
  labs(title = "Boxplot of Participant opting into nurse program and Creatine levels at admission", x = "Participant opting into speciality nurse program", y = "Creatine levels (Micromoles/L)")



boxplot(lm.nurse$residuals ~ hosp2022$nurse2,     #Residuals per group
        xlab="Participant opting into speciality nurse program", ylab="Residuals", main = "Residuals vs Nurse Group")

hist(lm.nurse$residuals)

#-------------------------------------------------------

lm.finalinitialtest <- lm(creat ~ weightc + agec + sex2 + HRc + nurse2, data = hosp2022)  #Initial Regression Model
summary(lm.finalinitialtest)
ols_vif_tol(lm.finalinitialtest)


lm.finalmidtest <- lm(creat ~ weightc + agec + sex2, data = hosp2022) #Post-Pruning Regression Model
summary(lm.finalmidtest)



lm.finaltest <- lm(creat ~ weightc + sex2 + agec + weightc*sex2 + weightc*agec, data = hosp2022) #Interaction (sex & age) Regression Model
summary(lm.finaltest)
ols_vif_tol(lm.finaltest)
confint(lm.finaltest)

plot(lm.finaltest,1) # The residual versus fitted plot
plot(lm.finaltest,2) # The normal quantile plot
plot(lm.finaltest,5)
hist(lm.finaltest$residuals)
lines(x, f, col = "red", lwd = 2)

ggplot(lm.finaltest, aes(x=lm.finaltest$residuals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


AIC(lm.finalinitialtest)
AIC(lm.finaltest)

BIC(lm.finalinitialtest)
BIC(lm.finaltest)
#------Other models
lm.HRtest <- lm(creat ~ weightc + agec + sex2 + HRc + nurse2 + weightc*HRc, data = hosp2022) #Interaction (HR) Regression Model
summary(lm.HRtest)
BIC(lm.initialtest)

lm.nursetest <- lm(creat ~ weightc + agec + sex2 + HRc + nurse2 + weightc*nurse2, data = hosp2022) #Interaction (HR) Regression Model
summary(lm.nursetest)

lm.logtest <- lm(logcreat ~ weightc + sex2 + agec + weightc*sex2 + weightc*agec, data = hosp2022) #Interaction (age) Regression Model
summary(lm.logtest)

plot(lm.logtest,1) # The residual versus fitted plot
plot(lm.logtest,2) # The normal quantile plot
plot(lm.logtest,5)
AIC(lm.logtest)
BIC(lm.logtest)
hist(lm.logtest$residuals)

exp(coef(lm.logtest))
(exp(coef(lm.logtest))-1)*100

lm.logtest <- lm(logcreat ~ weight + sex2 + age + weight*sex2 + weight*age, data = hosp2022) #Interaction (age) Regression Model
summary(lm.logtest)
AIC(lm.logtest)
BIC(lm.logtest)

plot(lm.logtest,1) # The residual versus fitted plot
plot(lm.logtest,2) # The normal quantile plot
plot(lm.logtest,5)
hist(lm.logtest$residuals)

#------------------------------------------------------- Prediction Tests

newhosp <- data.frame(age=55, sex2=0, weightc=87)
newhosp$sex2 <- factor(newhosp$sex2)
newhosp$prediction <- predict(lm.finaltest, newdata = newhosp)



#------------------------------------------------------- Quick Assumption Tests


plot(lm.finaltest,1) # The residual versus fitted plot
plot(lm.finaltest,2) # The normal quantile plot
plot(lm.finaltest,5) # Residuals vs leverage

boxplot(lm.finaltest$residuals ~ hosp2022$sex2,     #Residuals per group
        xlab="Participant opting into speciality nurse program", ylab="Residuals", main = "Residuals vs Nurse Group")

hist(lm.finaltest$residuals)
skewness(lm.finaltest$residuals)
shapiro.test(lm.finaltest$residuals)

#plot function----------------------------------------
func <- function (x){ggplot(data = hosp2022) +
    geom_point(mapping = aes(x = creat, y = (x))) +
    geom_smooth(mapping = aes(x = creat, y = (x)), se = FALSE)}

func(hosp2022$age)


