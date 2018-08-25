# Task: The task is pretty much simple. 
# Choose the right model to build given data. 
# Additional points will be awarded, if you do research on credit approval and adopt models accordingly.


library(tidyverse)
#install.packages("VIM")
library(VIM)
#install.packages("mice")
library(mice)

credit_data_df <- read.csv(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",sep = ",",header = F,check.names = TRUE,stringsAsFactors = TRUE,na.strings = "?")


head(credit_data_df)
str(credit_data_df)
summary(credit_data_df)

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

mydata_df <- credit_data_df

# Categorical variable: "V1" "V4"  "V5"  "V6"  "V7"  "V9"  "V10" "V12" "V13" "V16"
# Continuous Variables: "V2" "V3"  "V8" , Integer: "V11", "V14","V15"

# V1,V2,V4,V5,V6,V7,V14 are the missing values
  
# imputing using kNN
my_data_nas_imputed_knn <- VIM::kNN(mydata_df,k=7,imp_var = TRUE)

summary(my_data_nas_imputed_knn[vars_with_NAs])

my_data_nas_imputed_knn[my_data_nas_imputed_knn[,"V2_imp"] == TRUE,][c("V1","V2","V4","V5","V6","V7","V14")]


#imputing with MICE package
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

distinct(credi_card_df_no_nas,V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15) %>% nrow()
# No duplicates found in the dataset

# Check outliers
v2_out <- boxplot.stats(x = credi_card_df_no_nas$V2,coef = 1.58)$out
v3_out <- boxplot.stats(x = credi_card_df_no_nas$V3,coef = 1.58)$out
v8_out <- boxplot.stats(x = credi_card_df_no_nas$V8,coef = 1.58)$out
v14_out <- boxplot.stats(x = credi_card_df_no_nas$V14,coef = 1.58)$out
v15_out <- boxplot.stats(x = credi_card_df_no_nas$V15,coef = 1.58)$out

# remove outliers
final_df <- credi_card_df_no_nas[-which(credi_card_df_no_nas$V2 %in% v2_out),]
final_df <- final_df[-which(credi_card_df_no_nas$V3 %in% v3_out),]
final_df <- final_df[-which(credi_card_df_no_nas$V8 %in% v8_out),]
final_df <- final_df[-which(credi_card_df_no_nas$V14 %in% v14_out),]
final_df <- final_df[-which(credi_card_df_no_nas$V15 %in% v15_out),]

str(final_df)

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
indices_test <- sample(1:nrow(validation_df),size = 0.5*nrow(validation_df))
test_df <- validation_df[-indices_test,]
validation_df <- validation_df[indices_test,]


#--------------------------------------------------------------
# Model Building
#--------------------------------------------------------------

# Random Forest
library(caret)
library(randomForest)
library(kernlab)

# Using SVM to tune

train_control <- trainControl(method = "repeatedcv",
                              repeats = 5,
                              number = 5,
                              search = "grid",
                              sampling = "smote"
                              )

  








































