
library(psych)
library(FNN)
library(caret)


#Examining Heartrate
Heartratedata <- read.csv("C:/Users/Kieran/Desktop/Stats Datasets/Semester 2 2022/Machine Learning/HR.csv", stringsAsFactors=TRUE)

set.seed(1510)
q1a.trC <- trainControl(
  method = "cv",     #just 1 CV, 10-fold
  number = 10)  

model1 <- train(RestingHR ~ postExerciseHR + Exercise, 
                data = Heartratedata,
                method = "lm",
                trControl = q1a.trC)
model1

#RSME = 7.17


model2 <- train(RestingHR ~ postExerciseHR + as.factor(Exercise), 
                data = Heartratedata,
                method = "lm",
                trControl = q1a.trC)
model2
#RSME = 7.22

summary(model1$finalModel)

#Better predictive ability from model2.
#This is due to Exercise being a qualitative variable, but the reason why
#the difference between the MSE's is due to how there

Heartratedata$Exercise2 <- as.factor(Heartratedata$Exercise)

#Using KNN instead of linear regression
set.seed(1510)
HR.knn.loocv <- trainControl(
                method = "LOOCV")
model.knn <- train(RestingHR ~ postExerciseHR + Exercise,
                   data = Heartratedata, method= "knn",
                   trControl = HR.knn.loocv,
                   preProcess = c("center","scale"),
                   tuneGrid = expand.grid(k=1:51))
model.knn$results
model.knn$bestTune
#17
plot(model.knn)
#17 has the lowest RMSE

#Plotting KNN Vs linear 

HR.P <- seq(50,155)
knn17.datapred1 <- knn.reg(
  train = Heartratedata[c("postExerciseHR", "Exercise")],
  y = Heartratedata$RestingHR,
  test = data.frame(postExerciseHR=HR.P, Exercise=1),
  k = 17
)    

knn17.datapred2 <- knn.reg(
  train = Heartratedata[c("postExerciseHR", "Exercise")],
  y = Heartratedata$RestingHR,
  test = data.frame(postExerciseHR=HR.P, Exercise=2),
  k = 17
)    

knn17.datapred3 <- knn.reg(
  train = Heartratedata[c("postExerciseHR", "Exercise")],
  y = Heartratedata$RestingHR,
  test = data.frame(postExerciseHR=HR.P, Exercise=3),
  k = 17
)    

summary(Heartratedata$postExerciseHR)

model1.E1 <- predict(model1, newdata = data.frame(Exercise=1, postExerciseHR=seq(50,155)))
model1.E2 <- predict(model1, newdata = data.frame(Exercise=2, postExerciseHR=seq(50,155)))
model1.E3 <- predict(model1, newdata = data.frame(Exercise=3, postExerciseHR=seq(50,155)))

#Only using Exercise = 2, range 50-155
plot(Heartratedata$postExerciseHR, Heartratedata$RestingHR, xlab = "Post-Exercise Heartrate (PEHR)", ylab = "Resting Heartrate", main = "Models plotted using predicted PEHR at 50:155 and Exercise = 2")
lines(HR.P,model1.E2, col="blue", lty=2, lwd=1.5)
lines(HR.P,knn17.datapred2$pred, col="red", lwd=1.5)
legend("topleft", legend = c("Fitted KNN", "Fitted Linear"),
       col = c("red", "blue"), lty = 1:2,lwd=1.5, cex = 0.8)


#Using All
plot(Heartratedata$postExerciseHR, Heartratedata$RestingHR)
lines(HR.P,model1.E1, col="blue", lty=2)
lines(HR.P,model1.E2, col="green", lty=2)
lines(HR.P,model1.E3, col="red", lty=2)
lines(HR.P,knn17.datapred1$pred, col="blue")
lines(HR.P,knn17.datapred2$pred, col="green")
lines(HR.P,knn17.datapred3$pred, col="red")

# ----------------------------------------------------------------
#Computing R2 of Linear model
HRdatacomplete1 <- read.csv("C:/Users/Kieran/Desktop/Stats Datasets/Semester 2 2022/Machine Learning/HR2.csv", stringsAsFactors=TRUE)

mean.PEHR <- round(mean(HRdatacomplete1$postExerciseHR, na.rm=TRUE), 2)
#91.27 Mean resting heartrate
colSums(is.na(HRdatacomplete1[1:8]))

HRdatacomplete1[is.na(HRdatacomplete1$postExerciseHR), "postExerciseHR"] <- mean.PEHR 

q2.model1 <- lm(RestingHR ~ postExerciseHR + Exercise, data = HRdatacomplete1)
summary(q2.model1)
#0.415 adj R using mean imputation as a replacement for missing entries

#Now trying prediction imputation
HRdatacomplete2 <- read.csv("C:/Users/Kieran/Desktop/Stats Datasets/Semester 2 2022/Machine Learning/HR2.csv", stringsAsFactors=TRUE)

p.model <- lm(postExerciseHR ~ RestingHR + Smoke + Sex + Exercise + Hgt + Wgt, data = HRdatacomplete2)
summary(p.model)

PredictPEHR <- round(predict(p.model),2)
HRdatacomplete2$postExerciseHR <- ifelse(is.na(HRdatacomplete2$postExerciseHR), PredictPEHR, HRdatacomplete2$postExerciseHR)

#now using "Complete model"
q2.model2 <- lm(RestingHR ~ postExerciseHR + Exercise, data = HRdatacomplete2)
summary(q2.model2)

#0.395 adj R using prediction imputation as a replacement for missing entries

#Using mean imputation seems to have provided a higher adjusted R2 which is
#indicative of a better model. The predicted values may be too precise in fitting the incomplete data and
#thus these fitted values may be overestimating or underestimating the imputed values.  Thus, I believe that
# the former model is closer to the truth.
#-----------------------------------------------------------------------------------------------------------------


#Now examining Myocardial infarction
MIData <- read.csv("C:/Users/Kieran/Desktop/Stats Datasets/Semester 2 2022/Machine Learning/heart_assignment1-1.csv", stringsAsFactors=TRUE)

#factor conversion
MIData$sex <- factor(MIData$sex, levels = c(0,1), 
                      labels = c("Female", "Male"))

MIData$cpta <- factor(MIData$cpta, levels = c(0,1), 
                     labels = c("No", "Yes"))

MIData$cpaa <- factor(MIData$cpaa, levels = c(0,1), 
                      labels = c("No", "Yes"))

MIData$cpanp <- factor(MIData$cpanp, levels = c(0,1), 
                      labels = c("No", "Yes"))

MIData$sugar <- factor(MIData$sugar, levels = c(0,1), 
                       labels = c("No", "Yes"))

MIData$STT <- factor(MIData$STT, levels = c(0,1), 
                       labels = c("No", "Yes"))

MIData$LVH <- factor(MIData$LVH, levels = c(0,1), 
                     labels = c("No", "Yes"))

MIData$angina <- factor(MIData$angina, levels = c(0,1), 
                     labels = c("No", "Yes"))

#vessels goes up to 4 not 3
MIData$vessels <- factor(MIData$vessels)

MIData$stress <- factor(MIData$stress)

#-----------------------------------------------------------------------------------------------------------------
#Confusion Matrix

q3.glm.model1 <- glm(AMI == "yes" ~ age + sex + cpta + cpaa + cpanp + bp + chol + sugar + STT + LVH + maxHR + angina + peak
                    + vessels + stress, family = binomial, data = MIData)

summary(q3.glm.model1)
mean(q3.glm.model1$residuals^2)


q3.glm.model1.pred <- predict(q3.glm.model1, 
                         type = "response")
glm.model1.AMI.class <- factor(ifelse(q3.glm.model1.pred >.5, "yes", "no"))

ConfusionMat <- table(glm.model1.AMI.class, MIData$AMI)
ConfusionMat

#-----------

#using Caret Package
Caret.ConfusionMat<- confusionMatrix(glm.model1.AMI.class, MIData$AMI)
Caret.ConfusionMat$overall[1]

#Accuracy computes to 0.84% according to caret package

n.boot <- 1000
sample.size <- dim(MIData)[1]
accuracy.boot <- NULL

set.seed(1510)
for (i in 1:n.boot) {
  id.bs <- sample.int(sample.size,   
                      sample.size,   
                      replace = TRUE)
  model.bs <- glm(formula = AMI == "yes" ~ age + sex + cpta + cpaa + cpanp + bp + chol + sugar + STT + LVH + maxHR + angina + peak
                  + vessels + stress, family = binomial, data = MIData[id.bs, ])
  pred.model.bs <- predict(model.bs, type = "response")
  pred.death.bs <- factor(ifelse(pred.model.bs >.5, "yes", "no"))
  confusion.matrix.bs <- confusionMatrix(pred.death.bs, MIData[id.bs, ]$AMI)
  accuracy.boot[i] <- confusion.matrix.bs$overall[1] #Kappa bootstrap
}
quantile(accuracy.boot, c(0.025, 0.975))

#using seed 1510,
#accuracy returns lower interval, 0.82, upper interval 0.90



#----------------------------------------------------------------------------------------------------------------------------
#expected to be lower accuracy as the model may be overfitting due to number of variables therefore when applied to other
#test sets of data, it may not be as accurate.
set.seed(1510)
MItrc <- trainControl(
              method = "cv",
              number = 10,
              classProbs = TRUE,
              summaryFunction = twoClassSummary,
              savePred = TRUE)

model.10CV <- train(AMI ~ age + sex + cpta + cpaa + cpanp + bp + chol + sugar + STT + LVH + maxHR + angina + peak
                    + vessels + stress, 
                    data = MIData, method = "glm",
                    family = "binomial",
                    trControl = MItrc,
                    metric = "ROC")
confusionMatrix(model.10CV)

#accuracy 0.807
#expected outcome.

#---------------------------------------------------------------------------------------------------------------------------
#Using ridge regression
library(glmnet)
set.seed(1510)



MI.X <- model.matrix(AMI ~ age + sex + cpta + cpaa + cpanp + bp + chol + sugar + STT + LVH + maxHR + angina + peak
                     + vessels + stress, data=MIData)[,-1]
MI.Y <- MIData[,"AMI"]

set.seed(1510)
cv.lambda <- cv.glmnet(x=MI.X, y=MI.Y, 
                       alpha = 0,
                       family = binomial)

plot(cv.lambda)
l.min <- cv.lambda$lambda.min
#lambda min = 0.04589

MI.ridge.model <- glmnet(x=MI.X, y=MI.Y,
                      alpha = 0, 
                      family = binomial,
                      lambda = l.min)
MI.ridge.model$beta
summary(MI.ridge.model)

q3.rr.model.pred <- predict(MI.ridge.model, 
                              type = "response")

#10 fold CV accuracy using alpha 0 and l min
set.seed(1510)
MI.grid <- expand.grid(alpha = 0,
                       lambda = seq(0,0.1,.0001))
#using MItrc again
MItrc <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePred = TRUE)

MI.ridge.modelCV <- train(AMI ~ age + sex + cpta + cpaa + cpanp + bp + chol + sugar + STT + LVH + maxHR + angina + peak
                          + vessels + stress, 
                          data = MIData, 
                          method = "glmnet",
                          family = "binomial",
                          trControl = MItrc,
                          metric = "ROC",
                          tuneGrid = MI.grid)

coef(MI.ridge.modelCV$finalModel,
     MI.ridge.modelCV$bestTune$lambda)

confusionMatrix(MI.ridge.modelCV)

#Accuracy of 0.8152, higher than previous fit. This could be due to overfitting as ridge regression trades variance for bias.

#Lasso fit
library(glmnet)

MI.X <- model.matrix(AMI ~ age + sex + cpta + cpaa + cpanp + bp + chol + sugar + STT + LVH + maxHR + angina + peak
                     + vessels + stress, data=MIData)[,-1]
MI.Y <- MIData[,"AMI"]

#using alpha 1 since Lasso
set.seed(1510)
cv.lambdaL <- cv.glmnet(x=MI.X, y=MI.Y, 
                       alpha = 1,
                       family = binomial)
plot(cv.lambdaL)
l.minL <- cv.lambdaL$lambda.min
l.minL
#l minL is 0.0146
set.seed(1510)
MI.lasso.model <- glmnet(x=MI.X, y=MI.Y,
                         alpha = 1, 
                         family = binomial,
                         lambda = l.minL)
MI.lasso.model$beta
#Variables chosen are cpta, STT, maxHR, Angina, peak, Vessels, stress

#stepwise backward selection
library(bestglm)
library(leaps)

MI.bkwrd <- glm(AMI == "yes" ~ age + sex + cpta + cpaa + cpanp + bp + chol + sugar + STT + LVH + maxHR + angina + peak
    + vessels + stress, family = binomial, data = MIData)
step(MI.bkwrd)
#variables chosen are STT, maxHR, Angina, peak, Vessels, stress

#the variables selected are different. (cpta)