library(readxl)
library(tidyverse)
library(magrittr)
library(caret)
library(randomForest)
#-------------------------------------------------------------------------------
df <- read_csv("Data/ML_DataSet.csv")

names(df) %>% sort()

# Outcome 1
table(df$admission_flag)
# Outcome 2
table(df$death)


# Check missing value
sapply(df, function(x){sum(is.na(x))})

# Replace admissions_los = 0 if they were not admitted
df$admissions_los[is.na(df$admissions_los)] <- 0


table(df_ad$plan_type)
table(df_ad$rub_desc)
table(df_ad$covid_wave)


df %<>% mutate(admission_flag = ifelse(admission_flag == 0, "No", "Yes"),
               death = ifelse(death == 0, "No", "Yes"),
               plan_type1 = ifelse(plan_type == "Executive/Comprehensive", 1, 0),
               plan_type2 = ifelse(plan_type == "Keycare", 1, 0),
               plan_type3 = ifelse(plan_type == "Priority/Saver", 1, 0),
               plan_type4 = ifelse(plan_type == "Unknown", 1, 0),
               rub_desc1 = ifelse(rub_desc == "1 - Healthy Users", 1, 0),
               rub_desc2 = ifelse(rub_desc == "2 - Low Users", 1, 0),
               rub_desc3 = ifelse(rub_desc == "3 - Moderate Users", 1, 0),
               rub_desc4 = ifelse(rub_desc == "4 - High Users", 1, 0),
               rub_desc5 = ifelse(rub_desc == "5 - Very High Users", 1, 0),
               covid_wave1 = ifelse(rub_desc == "post wave 1", 1, 0),
               covid_wave2 = ifelse(rub_desc == "wave 1", 1, 0),
               covid_wave3 = ifelse(rub_desc == "wave 2", 1, 0),
               gender = ifelse(gender == "F", 1, 0))

#---------- for admission_flag
#===============================================================================

df_ad <- df %>% select(admission_flag, age, gender, plan_type1, plan_type2, plan_type3, plan_type4,
           rub_desc1, rub_desc2, rub_desc3, rub_desc4, rub_desc5, covid_wave1,
           covid_wave2, covid_wave3, hiv, diabetes, hypertension, 
           hypercholesterolaemia, congestive_cardiac_failure, depression, hypothyroidism, 
           glaucoma, chronic_renal_disease, ischaemic_heart_disease, oncology, copd,
           tuberculosis, reinfection_flag, no_of_comobs)


sapply(df_ad, function(x){sum(is.na(x))})
glimpse(df_ad)


# Check near zero variance variables ==> should remove
nzv <- nearZeroVar(df_ad)
names(df_ad)[nzv]

# [1] "plan_type4"                 "covid_wave1"                "covid_wave2"               
# [4] "covid_wave3"                "hiv"                        "congestive_cardiac_failure"
# [7] "hypothyroidism"             "glaucoma"                   "chronic_renal_disease"     
# [10] "ischaemic_heart_disease"    "oncology"                   "tuberculosis"              
# [13] "reinfection_flag"  

#---------- Split data
set.seed(123)

trainingRows <- createDataPartition(df_ad$admission_flag, p = 0.8, list = F)

train_ad <- df_ad[trainingRows,] # Training dataset
test_ad <- df_ad[-trainingRows,] # Testing dataset

dim(train_ad)
dim(test_ad)


table(train_ad$admission_flag)
table(test_ad$admission_flag)

#---------- Set up 
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "grid")

#---------- Random forest 
# library(doParallel)
# cores <- 8
# registerDoParallel(cores = cores)


# # Grid search setting
RFtunegrid <- expand.grid(.mtry = 2:5) # Set as 3 only since it run too slow :)))

set.seed(123)
rf_model_ad <- train(admission_flag ~ age + gender + plan_type1 + plan_type2 + plan_type3 +
                     rub_desc1 + rub_desc2 + rub_desc3 + rub_desc4 + rub_desc5 + diabetes + hypertension + 
                     hypercholesterolaemia + depression + copd,
                     data = train_ad, 
                     method = "rf", metric = "ROC",
                     preProcess= c("center", "scale"),
                     tuneGrid = RFtunegrid, trControl = fitControl)


rf_model_ad

# extracting variable importance
rf_imp_ad <- varImp(rf_model_ad, scale = FALSE)
rf_imp_ad <- rf_imp_ad$importance
rf_gini_ad <- data.frame(Variables = row.names(rf_imp_ad), 
                      MeanDecreaseGini = rf_imp_ad$Overall)


gc()















