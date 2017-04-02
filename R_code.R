install.packages("popbio")
install.packages("caret")
install.packages("pscl")
install.packages("ROCR")
install.packages("ggmap")
install.packages("corrplot")
install.packages("gmodels")
install.packages("ggplot2")
install.packages("gmodels")
install.packages("pROC")
install.packages("dplyr")
install.packages("e1071")
library(popbio)
library(caret)
library(pscl)
library(ROCR)
library(pROC)
library(ggplot2)
library(ggmap)
library(maptools)
library(plyr)
library(dplyr)
library(e1071)
library(corrplot)
library(gmodels)

# Import data
TEST4 <- read_csv("C:/Users/mingfang/Dropbox/MUSA507_Final/2016data/TEST4.csv")
TEST4_DEL <- read_csv("C:/Users/mingfang/Dropbox/MUSA507_Final/2016data/TEST4_DEL.csv")
FOOD2016 <- read_csv("C:/Users/mingfang/Dropbox/MUSA507_Final/2016data/FOOD2016.txt")
FOOD2016EDIT <- read_csv("C:/Users/mingfang/Dropbox/MUSA507_Final/2016data/FOOD2016EDIT.txt")
Reg1 <- TEST_2015_6
Reg12 <- TEST_2016_6
names(Reg1) 
names(Reg12)

# Sort data, training data from 2015

Training_15 <- data.frame(IS_ThisY_F = Reg1$IS_FAIL,IS_LAST_FAIL = Reg1$FAIL2014,EXPIRE=Reg1$LICENSE,VIOLATION=Reg1$VIOLATION,COUNT=Reg1$COUNT,NumDAYS = Reg1$NUM_DAYS,
                          JAN=Reg1$JAN,FEB=Reg1$FEB,MARCH=Reg1$MARCH,APRIL=Reg1$APRIL,MAY=Reg1$MAY,JUNE=Reg1$JUNE,
                          JULY=Reg1$JULY,AUGUST=Reg1$AUGUST,SEP=Reg1$SEP,OCTOBER=Reg1$OCTOBER,NOVERMBER=Reg1$NOVERMBER,
                          ARSON=Reg1$ARSON,
                          BURGLARY =Reg1$BURGLARY,
                          HOMICIDE=Reg1$HOMICIDE,
                          TRESPASS=Reg1$TREPASS,
                          HYEDEPARK=Reg1$HYEDEPARK,
                          WOODLAWN=Reg1$WOODLAWN,
                          GARBAGE=Reg1$GARBAGE)


# Testing data from 2016(to be updated)

Testing_16 <- data.frame(IS_ThisY_F = Reg12$IS_FAIL,IS_LAST_FAIL = Reg12$FAIL_2015,EXPIRE=Reg12$LICENSE,VIOLATION=Reg12$VIOLATION,COUNT=Reg12$COUNT,NumDAYS = Reg12$NUM_DAYS,
                         JAN=Reg12$JAN,FEB=Reg12$FEB,MARCH=Reg12$MARCH,APRIL=Reg12$APRIL,MAY=Reg12$MAY,JUNE=Reg12$JUNE,
                         JULY=Reg12$JULY,AUGUST=Reg12$AUGUST,SEP=Reg12$SEP,OCTOBER=Reg12$OCTOBER,NOVERMBER=Reg12$NOVERMBER,DECEMBER=Reg12$DECEMBER,
                         SCHOOL=Reg12$SCHOOL,DAYCARE= Reg12$DAYCARE,
                         ARSON=Reg12$ARSON,
                         BATTERY=Reg12$BATTERY,
                         ASSUALT= Reg12$ASSUALT,
                         BURGLARY =Reg12$BURGLARY,
                         NARCOTICS=Reg12$NARCOTICS,
                         MOTOR =Reg12$MOTOR,
                         HOMICIDE=Reg12$HOMICIDE,
                         LIQUO=Reg12$LIQUO,
                         TRESPASS=Reg12$TREPASS,
                         DAMAGE =Reg12$DAMAGE,
                         AVONDALE=Reg12$AVONDALE,
                         CHINATOWN=Reg12$CHINATOWN,
                         GRANDC=Reg12$GRANDC,
                         GREENTOWN=Reg12$GREENTOWN,
                         HYEDEPARK=Reg12$HYEDEPARK,
                         LRVINGPARK= Reg12$LRVINGPARK,
                         LITTLEITAL=Reg12$LITTLEITAL,
                         NORTHC=Reg12$NORTHC,
                         PORTAGE=Reg12$PORTAGE,
                         RIVERN=Reg12$RIVERN,
                         RIGERSP=Reg12$RIGERSP,
                         UPTOWN=Reg12$UPTOWN,
                         WESTLOOP=Reg12$WESTLOOP,
                         WOODLAWN=Reg12$WOODLAWN,
                         NORWOOD=Reg12$NORWOOD,
                         SANITATION=Reg12$SANITATION,GARBAGE=Reg12$GARBAGE)
names(Testing_16)

# Cross-tabulation, test the multicollinearity between variables
M <- cor(Training_15, method = "pearson")
M
#returns correlation for each variable to all the others. Here is a pretty correlation matrix plot.
#check the help for corrplot. There are a bunch of different methods for creating these graphics
corrplot(M, method = "number")

#logistic regression

lrModel <- glm(IS_ThisY_F ~ IS_LAST_FAIL + EXPIRE + VIOLATION + COUNT + JAN + 
                 FEB + MARCH + APRIL + MAY + JUNE + JULY + AUGUST + SEP + 
                 OCTOBER + NOVERMBER + BURGLARY + TRESPASS + ARSON + HOMICIDE + 
                 WOODLAWN + HYEDEPARK + GARBAGE, family="binomial"(link="logit"), data = Training_15)


lrModel <- glm(IS_ThisY_F ~ IS_LAST_FAIL + EXPIRE + VIOLATION + COUNT + JAN + 
                 FEB + MARCH + APRIL + MAY + JUNE + JULY + AUGUST + SEP + 
                 OCTOBER + NOVERMBER + BURGLARY + TRESPASS + ARSON + HOMICIDE + 
                 WOODLAWN + HYEDEPARK + GARBAGE, family="binomial"(link="logit"), data = Testing_16)


#summary
summary(lrModel)
#lets look at some 'Psuedo R^2' metrics
pR2(lrModel)

## optional: use MASS package to filter predictors
#install.packages("MASS")
library(MASS)
Valid_Pre <- stepAIC(lrModel,direction = "both")
Valid_Pre$anova


## Predict the test set probabilities using the training set model
ClassProbs1 <- predict(lrModel, Testing_16, type="response")
testProbs1 <- data.frame(IS_ThisY_F = Testing_16$IS_ThisY_F,
                         Probs = ClassProbs1)
head(testProbs1)

ClassProbs2 <- predict(lrModel, Training_15, type="response")
testProbs2 <- data.frame(IS_ThisY_F = Training_15$IS_ThisY_F,
                         Probs = ClassProbs2)




#plot the distrubtion of predictied probabilities for each binary class - 0 and 1.
adminPropsPlot1 <- ggplot(testProbs1, aes(x = Probs, fill=IS_ThisY_F)) + geom_density() +
  facet_grid(IS_ThisY_F ~ .) + xlab("Probability") 
adminPropsPlot1
##

adminPropsPlot2 <- ggplot(testProbs2, aes(x = Probs, fill=IS_ThisY_F)) + geom_density() +
  facet_grid(IS_ThisY_F ~ .) + xlab("Probability") 
adminPropsPlot2


##SENSITIVITY/SPECIFICITY/MISCLASSIFICATION/ROC ANALYSIS
### Testing 16
fit.binary = (testProbs1$Probs>=0.4)
CrossTable(fit.binary, Testing_16$IS_ThisY_F, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE)

### Training 15
fit.binary2 = (testProbs2$Probs>=0.4)
CrossTable(fit.binary2, Training_15$IS_ThisY_F, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE)


###
#how about a confusion matrix - using a command from caret

testProbs1$predClass  = ifelse(testProbs1$Probs > .4 ,1,0)
testProbs1$Correct  = ifelse(testProbs1$IS_ThisY_F == testProbs1$predClass ,1,0)
sum(testProbs1$Correct) / nrow(Testing_16)

testProbs2$predClass  = ifelse(testProbs2$Probs > .4 ,1,0)
testProbs2$Correct  = ifelse(testProbs2$IS_ThisY_F == testProbs2$predClass ,1,0)
sum(testProbs2$Correct) / nrow(Training_15)
library(heuristica)
confusionMatrix(testProbs1$Class ,testProbs1$predClass)


# ROC Curve
pred1 <- prediction( testProbs1$Probs, testProbs1$IS_ThisY_F)
perf1 <- performance(pred1,"tpr","fpr")
plot(perf1)
abline(a=0, b= 1)

######## Training 15
pred2 <- prediction( testProbs2$Probs, testProbs2$IS_ThisY_F)
perf2 <- performance(pred2,"tpr","fpr")
plot(perf2)
abline(a=0, b= 1)
########

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(perf1, pred1))
print(opt.cut(perf2, pred2))

#lets calculate 'AUC'
auc.perf = performance(pred1, measure ="auc")
auc.perf@y.values



#cross validation
ctrl <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
cvFit <- train(as.factor(IS_ThisY_F) ~ .,  data=Training_15, method="glm", family="binomial",
               trControl = ctrl)
cvFit
cvFit$results
cvFit$resample

#lets calculate the AUC in the selected neighborhood
auc.perf3 = performance(pred3, measure ="auc")
auc.perf3@y.values


WOODLAWN <- subset(Training_15,WOODLAWN==1)

ClassProbs4 <- predict(lrModel, WOODLAWN, type="response")
testProbs4 <- data.frame(IS_ThisY_F = WOODLAWN$IS_ThisY_F,
                         Probs = ClassProbs4)

pred4 <- prediction( testProbs4$Probs, testProbs4$IS_ThisY_F)
perf4 <- performance(pred4,"tpr","fpr")
plot(perf4)
abline(a=0, b= 1)

#lets calculate Hyedepark neighborhood
auc.perf4 = performance(pred4, measure ="auc")
auc.perf4@y.values



HYEDEPARK <- subset(Training_15,HYEDEPARK==1)

ClassProbs5 <- predict(lrModel, HYEDEPARK, type="response")
testProbs5 <- data.frame(IS_ThisY_F = HYEDEPARK$IS_ThisY_F,
                         Probs = ClassProbs5)

pred5 <- prediction( testProbs5$Probs, testProbs5$IS_ThisY_F)
perf5 <- performance(pred5,"tpr","fpr")
plot(perf5)
abline(a=0, b= 1)



