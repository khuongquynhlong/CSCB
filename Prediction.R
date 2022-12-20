library(readxl)
library(tidyverse)
library(magrittr)
library(caret)
library(randomForest)
library(pROC)
library(classifierplots)

#----------  Set up theme
#===============================================================================
mytheme <- function(...) {
    theme_minimal() +
        theme(
            plot.title = element_text(size = 14,color = "grey10",  face = "bold", hjust = 0.5),
            plot.subtitle = element_text(face = "italic", color = "gray10", size = 14),
            plot.caption = element_text(face = "italic", size = 14, color = "gray10"),
            axis.line = element_line(linetype = "solid"),
            axis.text.x = element_text(color = "gray10", size = 14),
            axis.text.y = element_text(color = "gray10", size = 14),
            # axis.ticks = element_blank(),
            axis.title.x = element_text(color = "gray10", size = 14),
            axis.title.y = element_text(color = "gray10", size = 14),
            # panel.grid.minor = element_blank(),
            # panel.grid.major = element_blank(),
            plot.background = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            legend.title = element_text(size = 14, face = "bold"),
            legend.direction = "horizontal",
            legend.position = "top",
            legend.background = element_rect(fill = NA, color = NA),
            legend.text = element_text(size = 14),
            legend.key.width = unit(4, "line"),
            strip.text = element_text(size = 14, face = "bold"),
            strip.background = element_rect(fill = NA, color = NA)
        )
}


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
#===============================================================================
# library(doParallel)
# cores <- 8
# registerDoParallel(cores = cores)

# # Grid search setting
RFtunegrid <- expand.grid(.mtry = 2:5)

set.seed(123)
rf_model_ad <- train(admission_flag ~ age + gender + plan_type1 + plan_type2 + plan_type3 +
                     rub_desc1 + rub_desc2 + rub_desc3 + rub_desc4 + rub_desc5 + diabetes + hypertension + 
                     hypercholesterolaemia + depression + copd,
                     data = train_ad, 
                     method = "rf", metric = "ROC",
                     preProcess= c("center", "scale"),
                     tuneGrid = RFtunegrid, trControl = fitControl)


rf_model_ad

plot(rf_model_ad)

# extracting variable importance
rf_imp_ad <- varImp(rf_model_ad, scale = FALSE)
rf_imp_ad <- rf_imp_ad$importance
rf_gini_ad <- data.frame(Variables = row.names(rf_imp_ad), 
                      MeanDecreaseGini = rf_imp_ad$Overall)

rf_gini_ad %<>% mutate(Variables = case_when(Variables == "plan_type1" ~ "Executive/Comprehensive",
                                          Variables == "plan_type2" ~ "Keycare",
                                          Variables == "plan_type3" ~ "Priority/Saver",
                                          Variables == "rub_desc1" ~ "Healthy Users",
                                          Variables == "rub_desc2" ~ "Low Users",
                                          Variables == "rub_desc3" ~ "Moderate Users",
                                          Variables == "rub_desc4" ~ "High Users",
                                          Variables == "rub_desc5" ~ "Very High Users",
                                          Variables == "age" ~ "Age",
                                          Variables == "gender" ~ "Gender",
                                          Variables == "diabetes" ~ "Diabetes",
                                          Variables == "hypertension" ~ "Hypertension",
                                          Variables == "hypercholesterolaemia" ~ "Hypercholesterolaemia",
                                          Variables == "depression" ~ "Depression",
                                          Variables == "copd" ~ "COPD"))

rf_gini_ad %>% ggplot(aes(x=reorder(Variables, MeanDecreaseGini), 
             y=MeanDecreaseGini)) +
    geom_col(alpha = 0.8, show.legend = F, fill = "navy", color = "white") +
    coord_flip() + 
    theme(legend.position="none") +
    labs(x = NULL, y = "Gini Score") +
    mytheme() -> gini

png("Figures/F1_Gini.png", units="in", width = 12, height = 6, res = 300)
gini
dev.off()

# Internal error
#-------------------------------
rf_pred_prob_train <- predict(rf_model_ad, train_ad, type = "prob")[[2]]
rf_pred_class_train <- predict(rf_model_ad, train_ad)


train_ad_com <- cbind(train_ad, rf_pred_prob_train, rf_pred_class_train) %>% 
    mutate(admission_flag = as.factor(admission_flag),
           admission_flag_01 = ifelse(admission_flag == "Yes", 1, 0))


rf_cm_train <- confusionMatrix(train_ad_com$admission_flag, 
                               train_ad_com$rf_pred_class_train,
                               positive = "Yes")
rf_cm_train

# AUC
rf_roc_train <- roc_plot(train_ad_com$admission_flag_01, train_ad_com$rf_pred_prob_train) + 
    mytheme() +
    labs(title = "ROC - Random forest: Training Dataset")
    
# External error
#-------------------------------
rf_pred_prob_test <- predict(rf_model_ad, test_ad, type = "prob")[[2]]
rf_pred_class_test <- predict(rf_model_ad, test_ad)


test_ad_com <- cbind(test_ad, rf_pred_prob_test, rf_pred_class_test) %>% 
    mutate(admission_flag = as.factor(admission_flag),
           admission_flag_01 = ifelse(admission_flag == "Yes", 1, 0))


rf_cm_test <- confusionMatrix(test_ad_com$admission_flag, 
                              test_ad_com$rf_pred_class_test,
                              positive = "Yes")
rf_cm_test

# AUC

rf_roc_test <- roc_plot(test_ad_com$admission_flag_01, test_ad_com$rf_pred_prob_test) + 
    mytheme() +
    geom_line(color = "navy", size = 2) +
    labs(title = "ROC - Random forest: Testing Dataset")


png("Figures/F2_Roc_rf.png", units="in", width = 14, height = 6, res = 300)
gridExtra::grid.arrange(rf_roc_train, rf_roc_test, ncol = 2)
dev.off()



#----------KNN 
#===============================================================================
set.seed(123)
knn_model_ad <- train(admission_flag ~ age + gender + plan_type1 + plan_type2 + plan_type3 +
                      rub_desc1 + rub_desc2 + rub_desc3 + rub_desc4 + rub_desc5 + 
                      diabetes + hypertension + hypercholesterolaemia + depression + copd,
                  data = train_ad,
                  method = "knn", 				
                  preProc = c("center","scale"),
                  metric="ROC",
                  trControl= fitControl)
knn_model_ad

plot(knn_model_ad)

# Internal error
#-------------------------------
knn_pred_prob_train <- predict(knn_model_ad, train_ad, type = "prob")[[2]]
knn_pred_class_train <- predict(knn_model_ad, train_ad)


train_ad_com <- cbind(train_ad_com, knn_pred_prob_train, knn_pred_class_train)

knn_cm_train <- confusionMatrix(train_ad_com$admission_flag, 
                               train_ad_com$knn_pred_class_train,
                               positive = "Yes")
knn_cm_train

# AUC
knn_roc_train <- roc_plot(train_ad_com$admission_flag_01, train_ad_com$knn_pred_prob_train) + 
    mytheme() +
    labs(title = "ROC - KNN: Training Dataset")

# External error
#-------------------------------
knn_pred_prob_test <- predict(knn_model_ad, test_ad, type = "prob")[[2]]
knn_pred_class_test <- predict(knn_model_ad, test_ad)


test_ad_com <- cbind(test_ad_com, knn_pred_prob_test, knn_pred_class_test)

knn_cm_test <- confusionMatrix(test_ad_com$admission_flag, 
                              test_ad_com$knn_pred_class_test,
                              positive = "Yes")
knn_cm_test

# AUC

knn_roc_test <- roc_plot(test_ad_com$admission_flag_01, test_ad_com$knn_pred_prob_test) + 
    mytheme() +
    geom_line(color = "navy", size = 2) +
    labs(title = "ROC - KNN: Testing Dataset")


png("Figures/F3_Roc_knn.png", units="in", width = 14, height = 6, res = 300)
gridExtra::grid.arrange(knn_roc_train, knn_roc_test, ncol = 2)
dev.off()



#---------- Logisctic regresion 
#===============================================================================
set.seed(123)
logit_model_ad <- train(admission_flag ~ age + gender + plan_type1 + plan_type2 + plan_type3 +
                      rub_desc1 + rub_desc2 + rub_desc3 + rub_desc4 + rub_desc5 + 
                      diabetes + hypertension + hypercholesterolaemia + depression + copd,
                  data = train_ad,
                  method = "glm", 
                  metric="ROC",
                  trControl= fitControl)
logit_model_ad



# Internal error
#-------------------------------
logit_pred_prob_train <- predict(logit_model_ad, train_ad, type = "prob")[[2]]
logit_pred_class_train <- predict(logit_model_ad, train_ad)


train_ad_com <- cbind(train_ad_com, logit_pred_prob_train, logit_pred_class_train)

logit_cm_train <- confusionMatrix(train_ad_com$admission_flag, 
                                train_ad_com$logit_pred_class_train,
                                positive = "Yes")
logit_cm_train

# AUC
logit_roc_train <- roc_plot(train_ad_com$admission_flag_01, train_ad_com$logit_pred_prob_train) + 
    mytheme() +
    labs(title = "ROC - Logistic: Training Dataset")

# External error
#-------------------------------
logit_pred_prob_test <- predict(logit_model_ad, test_ad, type = "prob")[[2]]
logit_pred_class_test <- predict(logit_model_ad, test_ad)


test_ad_com <- cbind(test_ad_com, logit_pred_prob_test, logit_pred_class_test)

logit_cm_test <- confusionMatrix(test_ad_com$admission_flag, 
                               test_ad_com$logit_pred_class_test,
                               positive = "Yes")
logit_cm_test

# AUC

logit_roc_test <- roc_plot(test_ad_com$admission_flag_01, test_ad_com$logit_pred_prob_test) + 
    mytheme() +
    geom_line(color = "navy", size = 2) +
    labs(title = "ROC - Logistic: Testing Dataset")


png("Figures/F4_Roc_logit.png", units="in", width = 14, height = 6, res = 300)
gridExtra::grid.arrange(logit_roc_train, logit_roc_test, ncol = 2)
dev.off()





png("Figures/F2_Roc_all.png", units="in", width = 14, height = 12, res = 300)
gridExtra::grid.arrange(rf_roc_train, rf_roc_test,
                        knn_roc_train, knn_roc_test, 
                        logit_roc_train, logit_roc_test, ncol = 2)
dev.off()


# saveRDS(train_ad_com, "Data/train_ad_com.RDS")
# saveRDS(test_ad_com, "Data/test_ad_com.RDS")


