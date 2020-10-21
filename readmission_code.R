####### SETUP####### 
rm(list=ls())
library(tidyverse)
library(gridExtra)
library(pROC)
library(rms)
library(glmnet)
diabetes <- read.csv('~/Desktop/303final/diabetes.csv')

####### EXPLORATORY ####### 

# k<-factor(diabetes$change)
# levels(k)
# head(diabetes)
plot_readmitted <- barplot(sort(table(diabetes$readmitted), decreasing=T), 
                           xlab="Readmitted", ylab="count", main="Readmitted Plot")

#number of encounter per patient: for handling independence assumption of GLM--------
max_encounter <- diabetes %>% 
  select(patient_nbr, encounter_num) %>%
  group_by(patient_nbr) %>% 
  filter(encounter_num==max(encounter_num)) %>%
  group_by(encounter_num) %>% count()
max_encounter
max_encounter$n[1]/sum(max_encounter$n) #76.5% had 1 encounter
max_encounter$n[2]/sum(max_encounter$n) #14.6% had 2 encounters
max_encounter$n[3]/sum(max_encounter$n) #4.7% had 3 encounters
sum(max_encounter$n[4:length(max_encounter$n)])/sum(max_encounter$n) #4.2% had 4+ encounters

max_encounter_plot <-  max_encounter %>%
  ggplot(aes(x=encounter_num, y=n/71518)) + geom_bar(stat='identity') +
  labs(title="Distribution of Max # Encounters", y="Proportion of Patients", x="Max # of Encounters")
max_encounter_plot

#explore missing values---------------------------------------------------
sapply(diabetes, function(x) sum(is.na(x)))
#2273/101766 = 2% of observations had missing race values; keep column
#98569/101766 = 97% of observations had missing weight values; remove column
#49949/101766 = 49% of observations had missing medical_specialty values; remove column

#explore Unknown/NULL/Not Mapped/Not Available values
sum(diabetes$gender == "Unknown/Invalid") #only 3, keep
sum(diabetes$admission_type_id %in% c(5,6,8)) #only 10%, keep
sum(diabetes$discharge_disposition_id %in% c(18,25,26)) #only 5%, keep

#remove columns-----------------------------------------------------------
remove_col <- c("X", "admission_source_id", "payer_code", "encounter_num", 
                "weight", "medical_specialty")
diabetes <- diabetes[, !(colnames(diabetes) %in% remove_col)]

#dichotomize the response: "no readmission":0 and "readmission":1 --------
diabetes$readmitted <- ifelse(diabetes$readmitted=="NO",0,1)
diabetes$readmitted <- as.factor(diabetes$readmitted)

plot_readmitted_binary <- barplot(sort(table(diabetes$readmitted), decreasing=T), 
                                  xlab="Readmitted", ylab="count", main = "0: not readmitted, 1:readmitted")

#summary: demographics (age, gender, race) -------------------------------
demographic_plot <- diabetes %>% 
  select(race, gender, age, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Demographics Plots", y="Count")
demographic_plot

#summary: encounter (duration, # diagnoses , # lab procedures, # procedures) ----------
diabetes %>% group_by(Length.of.Stay) %>% 
  summarise(count_readmitted=sum(readmitted==1),
            prop_readmitted=count_readmitted/101766,
            count_non_readmitted=sum(readmitted==0),
            prop_non_readmitted=count_non_readmitted/101766)

encounter_plot <- diabetes %>%
  select(Length.of.Stay, number_diagnoses, num_lab_procedures, num_procedures, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Encounter Plots", y="Count")
encounter_plot

#summary: diabetes medications (change, diabetesMed) --------------------------
diabetes_medication_plot <- diabetes %>%
  select(change, diabetesMed, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Medication Plots", y="Count", 
       subtitle = "change: whether diabetes medication was changed\ndiabetesMed: whether diabetes medication was prescribed")
diabetes_medication_plot

#summary: num_medications -----------------------------------------------------
num_meds_plot <- diabetes %>%
  select(num_medications, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Medication Plot", y="Count", subtitle="number of meds prescribed in current encounter")
num_meds_plot

num_meds_box_plot <- as_tibble(diabetes) %>%
  ggplot(aes(x=readmitted, y=num_medications)) + geom_boxplot() +
  labs(title="Medication Box Plot")
num_meds_box_plot

#summary: admission (admission_type_id) ------------------------------------
admission_plot <- diabetes %>%
  select(admission_type_id, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Admission Type ID Plots", y="Count")
admission_plot

#summary: discharge (discharge_disposition_id) ------------------------
discharge_summary <- diabetes %>%
  select(discharge_disposition_id) %>%
  gather(key,value) %>% group_by(key,value) %>%
  summarise(n=n()) %>%
  mutate(proportion=n/sum(n),proportion=scales::percent(proportion)) %>% arrange(desc(n))

discharge_summary 

discharge_plot <- diabetes %>%
  select(discharge_disposition_id, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Discharge Disposition ID Plots", y="Count")
discharge_plot

#summary: past visits (outpatient, inpatient, emergency) ------------------------
past_visit_plot <- diabetes %>%
  select(number_outpatient, number_inpatient, number_emergency, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 3) +
  labs(title="Past Visit Plots", y="Count")
past_visit_plot

diabetes %>%
  select(number_outpatient, number_inpatient, number_emergency) %>%
  summary()

# a lot of small values in these covariates, breakdown by count:
table(diabetes$number_emergency) 
table(diabetes$number_inpatient)
table(diabetes$number_outpatient)

#summary: test results (A1Cresult, max_glu_serum)-------------------------------
diabetes %>% group_by(readmitted, max_glu_serum) %>% tally()
diabetes %>% group_by(readmitted, A1Cresult) %>% tally()

test_plot <- diabetes %>% 
  select(max_glu_serum, A1Cresult, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>%
  count() %>% group_by(key) %>% ungroup %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Test Result Plots", y="Count",
       subtitle = "A1Cresult: HbA1c test result\nmax_glu_serum: blood glucose level")
test_plot

#summary: drugs (23 types) -----------------------------------------------------
drugs_summary <- diabetes %>%
  select(metformin, repaglinide, nateglinide, chlorpropamide, glimepiride,
         acetohexamide, glipizide, glyburide, tolbutamide, pioglitazone,
         rosiglitazone, acarbose, miglitol, troglitazone, tolazamide,
         examide, citoglipton, insulin, glyburide.metformin, glipizide.metformin, 
         glimepiride.pioglitazone, metformin.rosiglitazone, metformin.pioglitazone) %>%
  gather(key,value) %>% group_by(key,value) %>%
  summarise(n=n()) %>%
  mutate(proportion=n/sum(n),proportion=scales::percent(proportion)) %>% arrange(desc(n))

drugs_summary 

# remove the following drugs because >= 95% of encounters had value "No" and is not a good predictor
drug_remove <- c("citoglipton", "examide", "acetohexamide", "glimepiride.pioglitazone", "metformin.pioglitazone", "metformin.rosiglitazone", "troglitazone",
  "glipizide.metformin", "tolbutamide", "miglitol", "tolazamide", "chlorpropamide", "acarbose", "nateglinide", "glyburide.metformin", "repaglinide", "glimepiride")
diabetes <- diabetes[, !(colnames(diabetes) %in% drug_remove)]

#re-categorization ---------------------------------------------------------------------------
#re-categorize admission_type_id because some categories have extremely low number of observations
diabetes$admission_type_id <- ifelse(diabetes$admission_type_id==1,"Emergency",
                                     ifelse(diabetes$admission_type_id==2,"Urgent",
                                            ifelse(diabetes$admission_type_id==3,"Elective", "Other")))
new_admission_plot <- diabetes %>%
  select(admission_type_id, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Admission Type ID Plot", y="Count")
new_admission_plot

#re-categorize discharge_disposition_id because some categories have extremely low number of observations
diabetes$discharge_disposition_id <- ifelse(diabetes$discharge_disposition_id==1, "Home",
                                            ifelse(diabetes$discharge_disposition_id %in% c(3,6), "Further Care",
                                                   ifelse(diabetes$discharge_disposition_id %in% c(2,4,5,8,9,13,14,15,16,17,22,23,24,30,27,28,29), "Institution", "Other")))
new_discharge_plot <- diabetes %>%
  select(discharge_disposition_id, readmitted) %>%
  gather(key,value,-readmitted) %>%
  group_by(key,value,readmitted) %>% count() %>%
  ungroup() %>%
  ggplot(aes(x=value, y=n, fill=readmitted)) + geom_bar(stat='identity') +
  facet_wrap(~ key, scales = 'free', ncol = 2) +
  labs(title="Discharge Disposition ID Plot", y="Count")
new_discharge_plot

#re-categorize number_emergency
diabetes$number_emergency <- ifelse(diabetes$number_emergency==0,"0",
                                     ifelse(diabetes$number_emergency %in% c(1,2,3,4,5),"1-5",
                                            ifelse(diabetes$number_emergency %in% c(6,7,8,9,10),"6-10", "11+")))
table(diabetes$number_emergency)

#re-categorize number_inpatient
diabetes$number_inpatient <- ifelse(diabetes$number_inpatient==0,"0",
                                    ifelse(diabetes$number_inpatient %in% c(1,2,3,4,5),"1-5",
                                           ifelse(diabetes$number_inpatient %in% c(6,7,8,9,10),"6-10", "11+")))
table(diabetes$number_inpatient)
#re-categorize number_outpatient
diabetes$number_outpatient <- ifelse(diabetes$number_outpatient==0,"0",
                                    ifelse(diabetes$number_outpatient %in% c(1,2,3,4,5),"1-5",
                                           ifelse(diabetes$number_outpatient %in% c(6,7,8,9,10),"6-10", "11+")))
table(diabetes$number_outpatient)


####### MODEL ####### 
set.seed(1002925800)

#create training and test set
sample_nbr <- sample(unique(diabetes$patient_nbr),size=20000)
train <- diabetes[!diabetes$patient_nbr %in% sample_nbr,]
train <- na.omit(train)
test <- diabetes[diabetes$patient_nbr %in% sample_nbr,]
test <- na.omit(test)

#fit a logistic model (initial model)-----------------------
logit.glm <- glm(readmitted ~ . -encounter_id -patient_nbr, data=train, family=binomial)
summary(logit.glm)

#model selection: stepwise AIC -----------------------
glm_step_aic <- step(logit.glm, trace=0)

#Goodness of Fit: Hosmer-Lemeshow
diabetes_train_aic <- mutate(train, predprob=predict(glm_step_aic,type='response'),
                             linpred=predict(glm_step_aic))
gdf_aic <- group_by(diabetes_train_aic, ntile(linpred,1000)) # 1000 bins
hldf_aic <- summarise(gdf_aic, y=sum(readmitted==1), ppred=mean(predprob), count=n())
hldf_aic <- mutate(hldf_aic,se.fit=sqrt(ppred*(1-ppred)/count))
hlstat_aic <- with(hldf_aic, sum((y-count*ppred)^2/(count*ppred*(1-ppred))))
c(hlstat_aic, nrow(hldf_aic))
1-pchisq(hlstat_aic,nrow(hldf_aic)-2) #not a reliable test for lack of fit as can be significant or not depending on number of bins, and there is lack of guideline this number
#will not be using the HL test going forward.

#Goodness of Fit: ROC curve
p1 <- fitted(glm_step_aic)
y1 <- train$readmitted
roc_logit1 <- roc(y1 ~ p1)
TPR <- roc_logit1$sensitivities
FPR <- 1 - roc_logit1$specificities
plot(FPR, TPR, xlim =c(0,1), ylim =c(0,1), type ='l', lty = 1, lwd = 2,col ='red', bty = "n", main="AIC Stepwise Model ROC")
abline(a = 0, b = 1, lty = 2, col ='blue')
text(0.7,0.4,label =paste("AUC = ",round(auc(roc_logit1),4)))
auc(roc_logit1) #66%

#Model Validation: cross-calibration
lrm.final1 <- lrm(formula = glm_step_aic$formula, data=train, x =TRUE, y = TRUE, model= T)
cross.calib1 <- calibrate(lrm.final1, method="crossvalidation", B=10)
plot(cross.calib1, las=1, xlab = "Predicted Probability", main="AIC Stepwise Model Validation")

#Binned residual plot
diabetes_train_aic <- mutate(train, residuals=residuals(glm_step_aic),
                             predprob=predict(glm_step_aic,type='response'),
                             linpred=predict(glm_step_aic))
gdf_aic <- group_by(diabetes_train_aic, ntile(linpred,1000))
diagdf_aic <- summarise(gdf_aic, residuals=mean(residuals),
                        linpred=mean(linpred), predprob=mean(predprob))
plot(residuals~linpred,diagdf_aic,xlab='Linear Predictor',ylab='Deviance Residuals', pch=20,main="AIC Stepwise Model Binned Residuals")
plot(residuals~predprob, diagdf_aic,xlab='Fitted Values',ylab='Deviance Residuals',pch=20,main="AIC Stepwise Model Binned Residuals")


#model selection: stepwise BIC----------------------
glm_step_bic <- step(logit.glm, k=log(length(logit.glm$fitted.values)), trace=0)

#Goodness of Fit: ROC curve
p2 <- fitted(glm_step_bic)
y <- train$readmitted
roc_logit2 <- roc(y ~ p2)
TPR <- roc_logit2$sensitivities
FPR <- 1 - roc_logit2$specificities
plot(FPR, TPR, xlim =c(0,1), ylim =c(0,1), type ='l', lty = 1, lwd = 2,col ='red', bty = "n", main="BIC Stepwise Model ROC")
abline(a = 0, b = 1, lty = 2, col ='blue')
text(0.7,0.4,label =paste("AUC = ",round(auc(roc_logit2),4)))
auc(roc_logit2) 

#Model Validation: cross-calibration
lrm.final2 <- lrm(formula = glm_step_bic$formula, data=train, x =TRUE, y = TRUE, model= T)
cross.calib2 <- calibrate(lrm.final2, method="crossvalidation", B=10)
plot(cross.calib2, las=1, xlab = "Predicted Probability", main="BIC Stepwise Model Validation")

#Binned residual plot
diabetes_train_bic <- mutate(train, residuals=residuals(glm_step_bic),
                             predprob=predict(glm_step_bic,type='response'),
                             linpred=predict(glm_step_bic))
gdf_bic <- group_by(diabetes_train_bic, ntile(linpred,1000))
diagdf_bic <- summarise(gdf_bic, residuals=mean(residuals),
                        linpred=mean(linpred), predprob=mean(predprob))
plot(residuals~linpred,diagdf_bic,xlab='Linear Predictor',ylab='Deviance Residuals', pch=20,main="BIC Stepwise Model Binned Residuals")
plot(residuals~predprob, diagdf_bic,xlab='Fitted Values',ylab='Deviance Residuals',pch=20,main="BIC Stepwise Model Binned Residuals")


#model selection: LASSO-------------
x_design <- model.matrix(readmitted ~ . -encounter_id -patient_nbr, data=train) # constructs design matrix
x_design <- as.data.frame(x_design) # converts to data frame
x_design <- x_design[,-1] # removes first column (since it is redundant b/c intercept)
x_design <- as.matrix(x_design)
y <- train$readmitted # response variable
cvfit = cv.glmnet(x_design, y, family = "binomial", type.measure = "class")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.1se")

glmnet_coef <- coef(cvfit, s="lambda.1se")
glmnet_nonzero <- which(glmnet_coef != 0)
lasso_variables <- row.names(glmnet_coef)[glmnet_nonzero]
lasso_variables <-lasso_variables[!lasso_variables %in% '(Intercept)']

#manually looked up which covariates the variables with non-zero coefficients are from
lasso_model <- glm(readmitted ~ race+age+admission_type_id+discharge_disposition_id+Length.of.Stay+num_procedures+
                     number_outpatient+number_inpatient+number_emergency+number_diagnoses+
                     metformin+insulin+change+diabetesMed, data=train, family=binomial)
summary(lasso_model)

#Goodness of Fit: ROC curve
p3 <- fitted(lasso_model)
y <- train$readmitted
roc_logit3 <- roc(y ~ p3)
TPR <- roc_logit3$sensitivities
FPR <- 1 - roc_logit3$specificities
plot(FPR, TPR, xlim =c(0,1), ylim =c(0,1), type ='l', lty = 1, lwd = 2,col ='red', bty = "n",main="LASSO Model ROC")
abline(a = 0, b = 1, lty = 2, col ='blue')
text(0.7,0.4,label =paste("AUC = ",round(auc(roc_logit3),4)))
auc(roc_logit3) 

#Model Validation: cross-calibration
lrm.final3 <- lrm(formula = lasso_model$formula, data=train, x =TRUE, y = TRUE, model= T)
cross.calib3 <- calibrate(lrm.final3, method="crossvalidation", B=10)
plot(cross.calib3, las=1, xlab = "Predicted Probability",main="LASSO Model Validation")

#Binned residual plot
diabetes_train_lasso <- mutate(train, residuals=residuals(lasso_model),
                             predprob=predict(lasso_model,type='response'),
                             linpred=predict(lasso_model))
gdf_lasso <- group_by(diabetes_train_lasso, ntile(linpred,1000))
diagdf_lasso <- summarise(gdf_lasso, residuals=mean(residuals),
                        linpred=mean(linpred), predprob=mean(predprob))
plot(residuals~linpred,diagdf_lasso,xlab='Linear Predictor',ylab='Deviance Residuals', pch=20,main="LASSO Model Binned Residuals")
plot(residuals~predprob, diagdf_lasso,xlab='Fitted Values',ylab='Deviance Residuals',pch=20,main="LASSO Model Binned Residuals")

#test set-------------------------------------------------------
glm.final <- glm_step_bic
test$pred.prob <- predict(glm.final, newdata=test, type="response")
summary(glm.final)
confint(glm.final)

roc_final <- roc(test$readmitted ~ exp(test$pred.prob))
TPR <- roc_final$sensitivities
FPR <- 1 - roc_final$specificities
plot(FPR, TPR, xlim =c(0,1), ylim =c(0,1), type ='l', lty = 1, lwd = 2,col ='red', bty = "n", main='Final Model ROC curve')
abline(a = 0, b = 1, lty = 2, col ='blue')
text(0.7,0.4,label =paste("AUC = ",round(auc(roc_final),4)))
auc(roc_final) 

#Model Validation: cross-calibration
lrm.final <- lrm(formula = glm_step_bic$formula, data=test, x =TRUE, y = TRUE, model= T)
cross.calib <- calibrate(lrm.final, method="crossvalidation", B=10)
plot(cross.calib, las=1, xlab = "Predicted Probability",main='Final Model Cross-Calibration')



#ETC-----------------
#classify cases: if the predicted probability is <0.5 then we classify as 0 (not readmitted), else 1 (readmitted)
test <- mutate(test, predout=ifelse(pred.prob<0.5,0,1))
xtabs(~readmitted+predout, test)
#3793 false positives (false readmitted)
#6785 false negatives (false non-readmitted)
(11144+6136)/(11144+3793+6785+6136) #62% classification rate, 38% misclassification
specificity <- 11144/(11144+3793) #74%, high specificity -> fewer false positives
sensitivity <- 6136/(6785+6136) #47%, okay sensitivity -> okay with false negatives



