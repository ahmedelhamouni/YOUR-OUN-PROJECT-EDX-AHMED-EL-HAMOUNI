##################################
# PH125.9x Data Science
#Your Own Project Capstone Project
# AHMED EL HAMOUNI
#################################

###################################################
# ******* DOWNLOADING AND PREPARATION DATA *******#
###################################################

# the installing of packages 


if(!require(readr)) install.packages('readr') else library(readr)
if(!require(tidyverse)) install.packages('tidyverse') else library(tidyverse)
if(!require(dplyr)) install.packages('dplyr') else library(dplyr)
if(!require(caret)) install.packages('caret') else library(caret)
if(!require(corrplot)) install.packages('corrplot') else library(corrplot)
if(!require(ggplot2)) install.packages('ggplot2') else library(ggplot2)
if(!require(rpart)) install.packages('rpart') else library(rpart)
if(!require(rpart.plot)) install.packages('rpart.plot') else library(rpart.plot)
if(!require(reshape2)) install.packages('reshape2') else library(reshape2)

## OR OTHER CODE
 


# your oun project capstone:
# download the dataset
# https://github.com/ahmedelhamouni/EL-HAMOUNI-AHMED-Your-Own-Project-edx-/blob/main/diabetes.csv




diabetes <- data.frame(read_csv("diabetes.csv"))

# dimension of the data frame
data.frame(tibble("Rows"=dim(diabetes)[1],
                  "Columns"=dim(diabetes)[2])) %>% 
  knitr::kable()




# statistical description 
summary(diabetes[1:4]) %>% knitr::kable()
summary(diabetes[5:8]) %>% knitr::kable()

str(diabetes) 

# outcome display
diabetes %>% group_by(Outcome) %>% 
  summarise(N=n()*100/dim(diabetes)[1]) %>%
  ggplot(aes(x=Outcome,y=N,fill=Outcome))+ 
  geom_text(aes(label=paste(round(N,1),"%"),vjust=-0.25,fontface='bold'))+
  geom_bar(stat = 'identity',color='black') +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "") +
  scale_x_discrete(limits=c(0,1)) +
  ggtitle("Proportions of outcomes")
  

# OR THIS CODE FOR OUTCOME DISPLAY 
diabetes %>%
  group_by(Outcome) %>%
  summarise(N = n() * 100 / nrow(diabetes)) %>%
  ggplot(aes(x = Outcome, y = N, fill = Outcome)) +
  geom_text(aes(label = paste(round(N, 1), "%"), vjust = -0.25, fontface = 'bold')) +
  geom_bar(stat = 'identity', color = 'black') +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "") +
  scale_x_discrete(limits = c(0, 1)) +
  ggtitle("Proportions of outcomes")


# distribution of each variable
ggplot(melt(diabetes),aes(x=value,fill=variable)) + 
  geom_density() + 
  facet_wrap(~variable,scale="free") + 
  theme(legend.position = "") + 
  ggtitle("Density plot for every variables")



# 2 SD WAY BY OTHER CODE

ggplot(melt(diabetes), aes(x = value, fill = variable)) +
  geom_density() +
  facet_wrap(~ variable, scale = "free") +
  theme(legend.position = "") +
  ggtitle("Density plot for every variables")

# correlation plot
diabetes_abb <- diabetes
diabetes_abb_names <- c("P","G","BP","ST","I","BMI","DPF","A","OC")
colnames(diabetes_abb) <- diabetes_abb

cor_diabetes <- cor(diabetes_abb,method=c("spearman"))
corrplot(cor_diabetes, 
         method = 'square', 
         diag = FALSE,
         tl.pos = "d",
         main="Correlation matrix")


# 2 SD WAY BY OTHER CODE correlation plot
diabetes_abb <- diabetes
diabetes_abb_names <- c("P", "G", "BP", "ST", "I", "BMI", "DPF", "A", "OC")
colnames(diabetes_abb) <- diabetes_abb_names

cor_diabetes <- cor(diabetes_abb, method = "spearman")
corrplot::corrplot(cor_diabetes,
                   method = "square",
                   diag = FALSE,
                   tl.pos = "d",
                   main = "Correlation matrix")

# train/test sets creation
X <- diabetes %>% select(-Outcome)
y <- as.factor(diabetes$Outcome)
## Validation set
test_validation <- createDataPartition(y, times = 1, p = 0.9, list = FALSE)
validation <- diabetes[-test_validation, ]
test_index <- createDataPartition(y, times = 1, p = 0.8, list = FALSE)
## Train & Test set
train <- diabetes[test_index, ]
test <- diabetes[-test_index, ]

# Outcome as factor
train$Outcome <- as.factor(train$Outcome)
test$Outcome <- as.factor(test$Outcome)
validation$Outcome <- as.factor(validation$Outcome)


# -- Try of a first basic model

# fitting
fit <- train(Outcome~.,
             data=train,
             method="glm")

# predict
pred <- predict(fit,test)

# confusion matrix
cm <- confusionMatrix(pred,test$Outcome,mode="everything",positive="1")

# accuracy
cm[3]$overall[1]
# f1-score
cm[4]$byClass[7]



# Model creation 
model_result <- function(method){
  
  # cross-validation control
  ctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3)
                       
  # fitting
  fit <- train(Outcome~.,
               data=train,
               trControl = ctrl,
               method=method)

  # --- TEST ---
  # predict
  pred_test <- predict(fit,test)
  
  # confusion matrix
  cm_test <- confusionMatrix(pred_test,test$Outcome,mode="everything",positive="1")
  
 # accuracy
  accuracy_test <- round(cm_test[3]$overall[1],3)
  # f1-score
  f1_score_test <- round(cm_test[4]$byClass[7],3)
   
  # --- TRAIN ---
  # predict
  pred_train <- predict(fit,train)
  
  # confusion matrix
  cm_train <- confusionMatrix(pred_train,train$Outcome,mode="everything",positive="1")
  
  # accuracy
  accuracy_train <- round(cm_train[3]$overall[1],3)
  # f1-score
  f1_score_train <- round(cm_train[4]$byClass[7],3)
  
  
  # tibble with results
  result <- tibble("model"=method,
                   "train_acc"=accuracy_train,
                   "test_acc"=accuracy_test,
                   "acc_diff"=round(abs(accuracy_train-accuracy_test),5),
                   "train_f1"=f1_score_train,
                   "test_f1"=f1_score_test,
                   "f1_diff"=round(abs(f1_score_train-f1_score_test),5)
                   )
  return(result)
}

# list of models
models <- c("svmLinear","rf","glm","glmboost","lda","rpart")


# scores of every models
final <- data.frame()
i <- 0
for (model in models){
  print(paste("fitting :",model))
  score <- data.frame(model_result(model))
  final <- bind_rows(final,score)
  i <- i+1
  print(paste("[",i,"/",length(models),"] |", model, "fitted"))
}

# model results (ordered by accuracy on test set)
final <- final %>% mutate(ratio = abs(test_acc/(acc_diff+0.2)))
final_table <- final[order(desc(final$ratio)),] %>% knitr::kable()
print(final_table)


# prune = no -> so we can see which mstop is the best
accuracies <- list()
f1_scores <- list()
mstops <- list()
iterations <- seq(50,500,10)

ctrl <- trainControl(method = "repeatedcv",number = 10,repeats = 3)
for (i in iterations){
  grid = expand.grid(mstop = i,
                      prune="no")
  fit <- train(Outcome~.,
                  data=train,
                  trControl=ctrl,
                  method="glmboost",
                  tuneGrid=grid)
  pred <- predict(fit,test)
  
  cm <- confusionMatrix(pred,test$Outcome,mode="everything",positive="1")
  
  mstops <- append(mstops,i)
  accuracies <- append(accuracies,accuracy_test <- cm[3]$overall[1])
  f1_scores <- append(f1_scores,accuracy_test <- cm[4]$byClass[7])
  print(paste(round((i-min(iterations))*100/(max(iterations)-min(iterations))),"% completed"))
}

# tuning plot
mstops_tuning <- data.frame(tibble("mstop"=mstops,"accuracy"=accuracies,"f1_score"=f1_scores))
mstops_tuning <- as.data.frame(lapply(mstops_tuning, unlist))
mstops_tuning <- mstops_tuning %>% mutate(ratio = (accuracy*f1_score))
ggplot(mstops_tuning,aes(x=mstop,y=ratio)) + 
  geom_line() +
  ggtitle(paste("Ratio per mstop values. Max for mstop =",
                mstops_tuning$mstop[which.max(mstops_tuning$ratio)]))

# Final model testing
grid_opt <- expand.grid(mstop = mstops_tuning$mstop[which.max(mstops_tuning$ratio)],
                       prune = 'no')

fit_opt <- train(Outcome~.,
                 data=train,
                 trControl=ctrl,
                 method="glmboost",
                 tuneGrid=grid_opt) 

pred_opt <- predict(fit_opt,test)

cm_opt <- confusionMatrix(pred_opt,
                          test$Outcome,
                          mode="everything",
                          positive="1")

glm_opt_result <- data.frame(tibble("model"    = "glmBoost",
                                    "accuracy" = round(cm_opt[3]$overall[1],3),
                                    "f1_score" = round(cm_opt[4]$byClass[7],3)))
print(glm_opt_result)



# SVM
accuracies <- list()
f1_scores <- list()
c <- list()

iterations <- seq(0.01,2,0.02)

ctrl <- trainControl(method = "repeatedcv",number = 10,repeats = 3)
for (i in iterations){
  grid = expand.grid(C = i)
  fit <- train(Outcome~.,
               data=train,
               trControl=ctrl,
               method="svmLinear",
               tuneGrid=grid)
  pred <- predict(fit,test)
  
  cm <- confusionMatrix(pred,test$Outcome,mode="everything",positive="1")
  
  c <- append(c,i)
  accuracies <- append(accuracies,accuracy_test <- cm[3]$overall[1])
  f1_scores <- append(f1_scores,accuracy_test <- cm[4]$byClass[7])
  print(paste(round((i-min(iterations))*100/(max(iterations)-min(iterations))),"% completed"))
}

c_tuning <- data.frame(tibble("C"=c,"accuracy"=accuracies,"f1_score"=f1_scores))
c_tuning <- as.data.frame(lapply(c_tuning, unlist))
c_tuning <- c_tuning %>% mutate(ratio = (accuracy*f1_score))
ggplot(c_tuning,aes(x=C,y=ratio)) + 
  geom_line() +
  ggtitle(paste("Ratio per C values. Max for C = ",
                c_tuning$C[which.max(c_tuning$ratio)]))

grid_opt <- expand.grid(C = c_tuning$C[which.max(c_tuning$ratio)])

fit_opt <- train(Outcome~.,
                 data=train,
                 trControl=ctrl,
                 method="svmLinear",
                 tuneGrid=grid_opt) 

pred_opt <- predict(fit_opt,test)

cm_opt <- confusionMatrix(pred_opt,
                          test$Outcome,
                          mode="everything",
                          positive="1")

print(paste("accuracy :",round(cm_opt[3]$overall[1],3),"F1-score :",round(cm_opt[4]$byClass[7],3)))

svm_opt_result <- data.frame(tibble("model"    = "svmLinear",
                                    "accuracy" = round(cm_opt[3]$overall[1],3),
                                    "f1_score" = round(cm_opt[4]$byClass[7],3)))
print(svm_opt_result)


# svm tuning
svm_fit <- train(Outcome~., data=train,trControl=ctrl,method="svmLinear")
svm_predict <- predict(svm_fit,validation)
svm_cm <- confusionMatrix(svm_predict,validation$Outcome,
                          mode="everything",
                          positive="1")
print(svm_cm)


# final results
final_results <- bind_rows(glm_opt_result,svm_opt_result) %>% knitr::kable()
print(final_results)

# Bonus : decision tree
tree <- rpart(Outcome~.,data=train)
rpart.plot(tree, 
           type = 5, 
           extra = 100,
           box.palette = "GnRd",
           main="Decision tree")


importances <- data.frame(tree$variable.importance)
feature_importance <- data.frame(tibble("feature"=rownames(importances),
                                        "importance"=importances$tree.variable.importance))

feature_importance <- feature_importance[order(desc(feature_importance$importance)),]


# feature importance plot
feature_importance %>% 
  arrange(desc(importance)) %>%
  mutate(feature=factor(feature,levels=feature)) %>%
  ggplot(aes(y=importance,x=feature,fill=feature)) + 
  geom_col() + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle=45
                                   ,hjust=1))+
  ggtitle("Features importance")
