Assignment <- read.csv("C:/Users/Ka/Desktop/Stats Datasets/Regression/Assignment dataset.csv")

library(ggplot2)
library(magrittr)

#Variables without modification
lm.VIIRSGDP <- lm(VIIRS ~ GDP, data = Assignment)

summary(lm.VIIRSGDP)
confint(lm.VIIRSGDP)
#B0 = 0.1511, B1 = 131.268
#R2 = 0.9944, GDP accounts for 99% variability in VIIRS
#There is a 95% chance that the real value of B1 lies between 127.53


plot(Assignment$GDP,Assignment$VIIRS)
plot(VIIRS ~ GDP, data = Assignment)

#Shows linearity
ggplot(Assignment, aes(x = GDP, y = VIIRS)) +
  geom_jitter() +
  labs(title = "GDP vs VIIRS") +
    geom_smooth(method = "lm", color = "red",   se = FALSE)

plot(lm.VIIRSGDP, 1)
plot(lm.VIIRSGDP, 2)
plot(lm.VIIRSGDP, 5)
hist(lm.VIIRSGDP$residuals, breaks = 10)


#----------------------------------------------------------------------------
#log variables VIIRS and GDP
Assignment$VIIRSlog <- log(Assignment$VIIRS)
Assignment$GDPlog <- log(Assignment$GDP)

lm.VIIRSGDPlog <- lm(VIIRSlog ~ GDPlog, data = Assignment)
summary(lm.VIIRSGDPlog)
confint(lm.VIIRSGDPlog)

#B0 = 4.349, B1 = 0.82415
#0.824% increase in VIIRS for every 1% increase in GDP 

#R2 = 0.9467

#Shows linearity
ggplot(Assignment, aes(x = GDPlog, y = VIIRSlog)) +
  geom_jitter() +
  labs(title = "log(GDP) vs log(VIIRS)") +
  geom_smooth(method = "lm", color = "red",   se = FALSE)

summary(lm.VIIRSGDPlog)
plot(lm.VIIRSGDPlog, 1)
plot(lm.VIIRSGDPlog, 2)
plot(lm.VIIRSGDPlog, 5)
hist(lm.VIIRSGDPlog$residuals, breaks = 10)


#Prediction of 0.01 $B/km GDP
Prediction <- data.frame(GDPlog=log(0.01))
predict(lm.VIIRSGDPlog, Prediction, interval="confidence")
predict(lm.VIIRSGDPlog, Prediction, interval="prediction")

#----------------------------------------------------------------------------

plot(Assignment$CO2, Assignment$VIIRS)
plot(Assignment$CO2, Assignment$VIIRSlog)
Assignment$CO2log <- log(Assignment$CO2) 
plot(Assignment$CO2log, Assignment$VIIRSlog)
plot(Assignment$GDPlog, Assignment$CO2log)

ggplot(data=Assignment) +
  geom_smooth(mapping = aes(x = GDPlog, y= CO2log))

lm.Multiple <- lm(VIIRSlog ~ GDPlog + CO2log, data = Assignment)
summary(lm.Multiple)
confint(lm.Multiple)
#----------------------------------------------------------------------------

summary(lm(VIIRS ~ GDP + CO2, data = Assignment))

Assignment$CO22 <- Assignment$CO2log*2 

summary(lm(VIIRSlog ~ GDPlog + CO22, data = Assignment))


