# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(mlbench)

# Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav")

gss_tbl<- gss_tbl  %>%
  #  all missing, don’t know, inapplicable, not answered has been marked as NAs already
  #  filter NAs in the number of working hours last week column
  filter(is.na(HRS1) != T) %>%
  # rename the HRS1 column
  rename(workhours = HRS1) %>%
  # select columns with <0.75 NAs 
  select(where(~mean(is.na(.)) < 0.75)) %>%
  mutate(workhours = as.numeric(workhours))

# visualization 
gss_tbl %>% 
  ggplot(aes(workhours)) +
  geom_freqpoly()

# Analysis
# create train test split
# Determine row to split on: split
split <- round(nrow(gss_tbl) * 0.75)

# Create training data
gss_train <- gss_tbl[1:split, ]

# Create testing data
gss_test <- gss_tbl[(split + 1):nrow(gss_tbl), ]

# create model list
algo <- c("lm", "glmnet", "ranger", "xgbTree")

# create custom gridsearch function
grids <- function(x) {
if(x == "lm") {
  tuneGrid = NULL
} else if(x=="glmnet") {
  tuneGrid = expand.grid(
    alpha=0:1,
    lambda=seq(0.0001,1,length=20)
  )
} else if(x=="ranger"){
  
    tuneGrid = data.frame(
      .mtry = c(2,3,7),
      .splitrule = "variance",
      .min.node.size = 5
    )
} else  {
    tuneGrid = expand.grid(
      nrounds = 100,
      # scale_pos_weight = 0.32, # uncommenting this line leads to the error
      eta = c(0.01, 0.001, 0.0001),
      max_depth = c(2, 4, 6, 8),
      gamma = 0, 
      subsample = 1,
      min_child_weight = c(1, 2, 3), 
      colsample_bytree = 1
    )
  }
}

# set empty table template
table1_tbl <- c()

# fit models
for(i in algo) {
  set.seed(12)
  model <- train(
    workhours~.,
    gss_train,
    method = i,
    na.action = na.pass,
    tuneGrid = grids(i),
    preProcess = c("center","scale","medianImpute"),
    trControl=trainControl(method="cv",number=10, verboseIter=T)
  )
  
  # Publication
  # extract highest R2 from CV    
  cv_rsq <- max(model$results$Rsquared)
  # calculate R2 for the testing data
  ho_rsq <- cor(predict(model, gss_test,na.action = na.pass),gss_test$workhours)^2
  # combine algorithm names, cv_rsq, ho_rsq into a table
  table <- cbind(i,str_remove(format(round(cv_rsq,2),nsmall=2),"^0"),str_remove(format(round(ho_rsq,2),nsmall=2),"^0"))
  table1_tbl  <- rbind(table1_tbl ,table)
  colnames(table1_tbl) <- c("algo","cv_rsq","ho_rsq")
}

table1_tbl

# Q1: R-squared values vary across different models. Elastic net performs the best in both cross validation and holdout data, whilst linear regression performs the worst. This is because different models differ mathematically and also use different regulization methods.
# Q2: In general, the models perform better on the holdout sample, which implies that the models do not overfit and predict unseen data well.
# Q3: I will choose Elastic net as they perform the best on both training and testing data. No tradeoffs. 
