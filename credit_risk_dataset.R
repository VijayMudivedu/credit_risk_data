# Task:
# Choose the right model to build given data. 
# Additional points will be awarded, if you do research on credit approval and adopt models accordingly.


#install.packages("VIM")
#install.packages("mice")
# install.packages("tidyverse")
# install.packages("mice")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("kernlab")
# install.packages("MASS")
# install.packages("car")
# install.packages("GGally")

library(tidyverse)
library(VIM)
library(mice)
library(caret)
library(randomForest)
library(kernlab)
library(MASS)
library(car)
library(GGally)

credit_data_df <- read.csv(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",sep = ",",header = F,check.names = TRUE,stringsAsFactors = TRUE,na.strings = "?")


head(credit_data_df)
str(credit_data_df)
summary(credit_data_df)

sapply(credit_data_df, levels)

mydata_df <- credit_data_df

#--------------------------------------------------------------
# Data Preparation
#--------------------------------------------------------------

names(mydata_df) <-
c("gender","age","debt","marital_status","bank_csutomer","education_level","ethenicity","yearsemployed","prior_default","employed","credit_score","drivers_license","citizen","zipcode","income","approved")

head(mydata_df,10)


# check NAs Missing numbers, factors
# check special characters
apply(credit_data_df, MARGIN = 2, function(x) sum(is.na(x)))
# V1  V2  V3  V4  V5  V6  V7  V8  V9 V10 V11 V12 V13 V14 V15 V16 
# 12  12   0   6   6   9   9   0   0   0   0   0   0  13   0   0 

#checking the correlation between the unknowns

vars_with_NAs <- c("V1","V2","V4","V5","V6","V7","V14") # vars with illegal chars
vars_with_no_NAs <- c("V3","V8","V9","V10","V12","V13","V15","V16")
cat_vars <- c("V1","V4","V5","V6","V7","V9","V10","V12","V13","V16")
int_vars <-c( "V2","V3","V8","V11","V14","V15")

# V1
credit_data_df %>% filter(is.na(V1)) 

# V2
credit_data_df %>% filter(is.na(V2)) 

# V4 -- # V4, V4 V6 V7,V14 missing values are apparently correlatioed. are correlated
credit_data_df %>% filter(is.na(V4))

# Correlation between V6, V7 and V14
credit_data_df %>% filter(is.na(V6)) 

# checking for patterns V3=V8=V11=0, V13= p, is the correaltion dependency of missing values
credit_data_df %>% filter((V3==0 & V8 == 0 & V11==0 & V13=="p" ))

# detecting the pair-wise correlation of 
md.pairs(credit_data_df)


#V14 missing values is covering the most of the missing values from V4,V5,V6 and V7

# Categorical variable: "V1" "V4"  "V5"  "V6"  "V7"  "V9"  "V10" "V12" "V13" "V16"
# Continuous Variables: "V2" "V3"  "V8" , Integer: "V11", "V14","V15"

# V1,V2,V4,V5,V6,V7,V14 are the missing values
  
#--------------------------------------------------------------
#imputing with MICE package
#--------------------------------------------------------------
my_data_imputed_rf <-  mice(mydata_df[,!names(mydata_df) %in% "V16" ],
                            method = "rf",m = 5)
summary(my_data_imputed_rf)

my_data_imputed_rf$imp
my_data_imputed_rf$data

mydata_rf_output <- complete(my_data_imputed_rf)

# Final Data frame with imputed output
credi_card_df_no_nas <- mydata_rf_output
credi_card_df_no_nas$label <- mydata_df$V16
str(credi_card_df_no_nas)

# check duplicates
distinct(credi_card_df_no_nas) %>% nrow()
# No duplicates found in the dataset

# Check outliers
cred_score_out <- boxplot.stats(x = credi_card_df_no_nas$credit_score,coef = 1.58)$out
income_out <- boxplot.stats(x = credi_card_df_no_nas$income,coef = 1.58)$out

# outliers as such cannot be removed as they are needed for the loan approval
final_df <- credi_card_df_no_nas


#--------------------------------------------------------------
# Exploratory Data Anaylysis
#--------------------------------------------------------------

# Income
ggplot(final_df,aes(income, fill = approved)) + geom_histogram() + 
  theme(legend.position = c(0.8,0.8),
        legend.background = element_blank())
# people with montly income lesser are rejected

# Age
ggplot(final_df,aes(age,fill = approved)) + geom_histogram(position = "dodge",binwidth = 10) + 
  theme(legend.position = c(0.8,0.8),
        legend.background = element_blank())
# Many younsters are apparently disapproved of the loan

# credit_score
ggplot(final_df,aes(credit_score,fill = approved)) + 
  geom_histogram(position = "dodge",binwidth = 10) + 
  theme(legend.position = c(0.8,0.8),
        legend.background = element_blank())
# Low credit score is often the reasons for rejection of loan

# employed
ggplot(final_df,aes(employed,fill = approved)) + geom_bar(position = "dodge") + 
  theme(legend.position = c(0.8,0.8),
        legend.background = element_blank())
# Note being employed is another major reason for rejection

# education leve

ggplot(final_df,aes(education_level,fill = approved)) + geom_bar(position = "dodge",stat = "count") + 
  theme(legend.position = c(0.8,0.8),
        legend.background = element_blank())

# not being educated also demonstrates a reason for loan rejection

# prior debt
ggplot(final_df,aes(debt,fill = approved)) + geom_histogram(position = "dodge",binwidth = 10) + 
  theme(legend.position = c(0.8,0.8),
        legend.background = element_blank())
# having no debts is more often  a reason for loan dissapproval, as no credit history is one of the reasons for loan dissapproval

# prior default
ggplot(final_df,aes(prior_default,fill = approved)) + geom_bar(position = "stack") + 
  theme(legend.position = "bottom",
        legend.background = element_blank())

# prior default is often another reason for loan rejection

# pair-wise compariseion


imp_vars <- c("gender","age","debt","marital_status","yearsemployed","prior_default","employed","credit_score","income")

ggpairs(final_df[,imp_vars])



#--------------------------------------------------------------
# split the dataset into train, validation and test variables
#--------------------------------------------------------------

set.seed(100)
indices <- sample(1:nrow(final_df), 0.6*nrow(final_df))

# training data_frame
train_df <- final_df[indices,]

# test data frame
validation_df <- final_df[-indices,]

# test indices
# indices_test <- sample(1:nrow(validation_df),size = 0.5*nrow(validation_df))
# test_df <- validation_df[-indices_test,]
# validation_df <- validation_df[indices_test,]


#--------------------------------------------------------------
# Model Building
#--------------------------------------------------------------

# creation of dummy variables
final_df_logit <- final_df
final_df_logit$approved <- ifelse(final_df_logit$approved == "+",1,0)

dummy_vars <- dummyVars(formula = ~.,data = final_df_logit,levelsOnly = FALSE,fullRank = TRUE,sep = "_")
dummy_logit_df <- as.data.frame(predict(dummy_vars,final_df_logit))

# Split the data frame in train and test
train_logit_df <- dummy_logit_df[indices,]
validat_logi_df <- dummy_logit_df[-indices,]


model_logit <-  glm(formula = approved~.,family = "binomial",data = train_logit_df)
summary(model_logit)

names(train_logit_df)

new_vars <- c("gender_b"+"age"+"debt"+"marital_status_u"+"marital_status_y"+"bank_csutomer_gg"+"education_level_c"+"education_level_cc", "education_level_d"+"education_level_e", "education_level_i", "education_level_k"+"education_level_m"+"education_level_q"+"education_level_w"+"education_level_x"+"ethenicity_dd"+"ethenicity_h" , "ethenicity_n"+"ethenicity_o"+"ethenicity_v", "yearsemployed"+"prior_default_t"+"employed_t"+"credit_score"+"drivers_license_t" ,"citizen_s"+"zipcode"+"income")


model_logit <-  glm(formula = approved ~ gender_b+age+debt+marital_status_u+marital_status_y+education_level_c+education_level_cc+ education_level_d+education_level_e+ education_level_i+ education_level_k+education_level_m+education_level_q+education_level_w+education_level_x+ethenicity_dd+ethenicity_h +ethenicity_n+ethenicity_o+ethenicity_v+ yearsemployed+prior_default_t+employed_t+credit_score+drivers_license_t +citizen_s+zipcode+income,family = "binomial",
                    data = train_logit_df)  

summary(model_logit)


model_logit_stepaic <- stepAIC(model_logit,direction = "both")

# using logistic regression
summary(model_logit_stepaic)

vif(model_logit_stepaic) # there isn't much correlaiton

#Marital status is a significant variable,
# marital_status_u   -1.750e+01  7.964e+02  -0.022 0.982465
model_logit_01 <- glm(formula = approved ~ marital_status_y + 
                        education_level_cc + education_level_e + education_level_q + 
                        education_level_x + ethenicity_h + ethenicity_n + ethenicity_v + 
                        prior_default_t + credit_score + drivers_license_t + zipcode + 
                        income, family = "binomial", data = train_logit_df)

summary(model_logit_01)

# zipcode            -0.0017729  0.0012363  -1.434 0.151565
model_logit_02 <-  glm(formula = approved ~ marital_status_y + education_level_cc + 
      education_level_e + education_level_q + education_level_x + 
      ethenicity_h + ethenicity_n + ethenicity_v + prior_default_t + 
      credit_score + drivers_license_t + income, family = "binomial", 
    data = train_logit_df)
summary(model_logit_02)

#ethenicity_h        0.6597027  0.4413851   1.495 0.135014    

model_logit_03 <-  glm(formula = approved ~ marital_status_y + education_level_cc + 
                        education_level_e + education_level_q + education_level_x + 
                        ethenicity_n + ethenicity_v + prior_default_t + 
                        credit_score + drivers_license_t + income, family = "binomial", 
                      data = train_logit_df)

summary(model_logit_03)

# ethenicity_v        0.2932651  0.3491992   0.840 0.401008 
model_logit_04 <- glm(formula = approved ~ marital_status_y + education_level_cc + 
                        education_level_e + education_level_q + education_level_x + 
                        ethenicity_n + prior_default_t + credit_score + 
                        drivers_license_t + income, family = "binomial", data = train_logit_df)

summary(model_logit_04)

# marital_status_y   -0.5384678  0.3806986  -1.414 0.157238
model_logit_04 <- glm(formula = approved ~ education_level_cc + 
                        education_level_e + education_level_q + education_level_x + 
                        ethenicity_n + prior_default_t + credit_score + drivers_license_t + 
                        income, family = "binomial", data = train_logit_df)

summary(model_logit_04)

#income              0.0003480  0.0002083   1.670 0.094864 . 
model_logit_05 <- glm(formula = approved ~ education_level_cc + education_level_e + 
                        education_level_q + education_level_x + ethenicity_n + prior_default_t + 
                        credit_score + drivers_license_t, family = "binomial", 
                      data = train_logit_df)

summary(model_logit_05)


# drivers_license_t  -0.52526    0.32884  -1.597 0.110194 
model_logit_06 <- glm(formula = approved ~ education_level_cc + education_level_e + 
                        education_level_q + education_level_x + ethenicity_n + prior_default_t + 
                        credit_score, family = "binomial", data = train_logit_df)


summary(model_logit_06)


#ethenicity_n        2.69392    1.47164   1.831  0.06717 .

model_logit_07 <- glm(formula = approved ~ education_level_cc + education_level_e + 
                        education_level_q + education_level_x + prior_default_t + 
                        credit_score, family = "binomial", data = train_logit_df)

summary(model_logit_07)

# Most important parameters to determine a loan are: Education LEvel, Prior_Default, credit_score

final_logit_model <- model_logit_07

# prediction
vars_predict <- setdiff(names(validat_logi_df),"approved")
predict_logit <- predict(final_logit_model,validat_logi_df[vars_predict],type = "response")

predict_logit_leves <- as.factor( ifelse(test = predict_logit >0.49,1,0))

confusionMatrix(predict_logit_leves,factor(validat_logi_df$approved))

# Logistic model generates a reasonably good Balancyed accuracy = 0.8563

#-------------------
# Using SVM 
#-------------------

train_control_svm <- trainControl(method = "repeatedcv",
                              repeats = 5,
                              number = 5,
                              search = "grid",
                              sampling = "smote"
                              )

tuning_grid_svm <- expand.grid(.sigma = seq(0,0.05,0.01),.C = seq(1,3,1)) 
# Sigma = non-linearity control parameter
# C = cost function paratmeter to number of misclassifcations control in SVM

# 
# kernel, c("rbfdot", "polydot", "tanhdot", "vanilladot",  : 
#             'arg' should be one of “rbfdot”, “polydot”, “tanhdot”, “vanilladot”, “laplacedot”, “besseldot”, “anovadot”, “splinedot”, “matrix”


model_svm <- ksvm(approved ~.,
                  kernel = "rbfdot", # Radial Basis Function Kernel, Polydot kernels are superior
                  data = train_df,
                  trControl = trn_cntrol_svm,
                  tuneGrid = tuning_grid_svm,
                  metric = "auc"
                  )

#attributes(model_svm)
#
vars_predict <- setdiff(names(final_df),"approved")
predicted_svm <- predict(model_svm,validation_df[vars_predict],type = "response")

# confusion matrix to check the accuracy of the model

confusionMatrix(data = predicted_svm,reference = validation_df$approved,positive = "+")

# Balanced Accuracy - AUC = 0.8762

# 
# Reference
# Prediction   -   +
#   - 122  10
# +  25 119
# 
# Accuracy : 0.8732          
# 95% CI : (0.8281, 0.9101)
# No Information Rate : 0.5326          
# P-Value [Acc > NIR] : < 2e-16         
# 
# Kappa : 0.7471          
# Mcnemar's Test P-Value : 0.01796         
# 
# Sensitivity : 0.9225          
# Specificity : 0.8299          
# Pos Pred Value : 0.8264          
# Neg Pred Value : 0.9242          
# Prevalence : 0.4674          
# Detection Rate : 0.4312          
# Detection Prevalence : 0.5217          
# Balanced Accuracy : 0.8762          
# 
# 'Positive' Class : +    


# Thus the balanced accuracy of the model is: 0.8762 is the best fit model

#-------------------
# Using RandomForest
#-------------------

# Train control

train_control_rf <- trainControl(method = "repeatedcv",
                                  repeats = 5,
                                  number = 5,
                                  search = "grid",
                                  sampling = "smote",
                                 allowParallel = TRUE)

# Tuning grid parameters of Random Forest
tuning_grid_rf <- expand.grid(.mtry = round(sqrt(ncol(train_df))),ntree = seq(100,500,100)) 

model_rf <- randomForest(approved ~.,
                         data = train_df,
                         #method = "rf",
                         trControl = train_control_rf,
                         tuneGrid = tuning_grid_rf,
                         metric = "auc")


predict_rf <- stats::predict(object = model_rf,validation_df[vars_predict],type = "response")

# Confusion Matrix
confusionMatrix(predict_rf,validation_df$approved,positive = "+")


# Random Forest model generated a superior accuracy


# Confusion Matrix and Statistics
# 
# Reference
# Prediction   -   +
#   - 129  15
# +  18 114
# 
# Accuracy : 0.8804          
# 95% CI : (0.8362, 0.9163)
# No Information Rate : 0.5326          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.7602          
# Mcnemar's Test P-Value : 0.7277          
# 
# Sensitivity : 0.8837          
# Specificity : 0.8776          
# Pos Pred Value : 0.8636          
# Neg Pred Value : 0.8958          
# Prevalence : 0.4674          
# Detection Rate : 0.4130          
# Detection Prevalence : 0.4783          
# Balanced Accuracy : 0.8806          
# 
# 'Positive' Class : +               

#------------------- Conclusion ---------------------------
# Thus of all the models the Random forest generates the best possible accuracy
# important variables are education, credit score, and prior defaults
#----------------------------------------------------------































