library(readxl)
library(tidyverse)
library(magrittr)
library(caret)

#-------------------------------------------------------------------------------
df <- read_csv("ML_DataSet.csv")

# x <- read_csv("test.csv") take names of predictors
names(df) %>% sort()

# Outcome 1
table(df$admission_flag)
# Outcome 2
table(df$death)


# Choose potential predictors

poten_pred <- names(x)

# Data set with all potential predictors
df %<>% select(admission_flag, death, poten_pred, -c(pregnancy, bmi_level))
# Check missing value
sapply(df, function(x){sum(is.na(x))})

# Replace admissions_los = 0 if they were not admitted
df$admissions_los[is.na(df$admissions_los)] <- 0


df %<>% mutate(admission_flag = ifelse(admission_flag == 0, "No", "Yes"),
               death = ifelse(death == 0, "No", "Yes"))

#-------------------------------------------------------------------------------
#---------- for admission_flag
# Split data
set.seed(123)

trainingRows <- createDataPartition(df$admission_flag, p = 0.8, list = F)

train_ad <- df[trainingRows,]
test_ad <- df[-trainingRows,]

dim(train_ad)
dim(test_ad)


table(train_ad$admission_flag)
table(test_ad$admission_flag)

# K-NN, Tree, and logistic
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

# Random forest 
RFtunegrid <- expand.grid(mtry = 1:5)

set.seed(123)
rf_model_ad <- train(admission_flag ~ age + gender + plan_type + rub_desc + hiv + diabetes + hypertension + 
                      hypercholesterolaemia+ congestive_cardiac_failure + depression+ hypothyroidism + 
                      glaucoma + chronic_renal_disease + ischaemic_heart_disease + oncology + copd+
                      tuberculosis + reinfection_flag + covid_wave,
                  data = train_ad, 
                  method = "rf", metric = "ROC",
                  tuneGrid = RFtunegrid, trControl = fitControl)

rf_model

# extracting variable importance
rf_imp_ad <- varImp(rf_model_ad, scale = FALSE)
rf_imp_ad <- rf_imp_ad$importance
rf_gini_ad <- data.frame(Variables = row.names(rf_imp_ad), 
                      MeanDecreaseGini = rf_imp_ad$Overall)


















