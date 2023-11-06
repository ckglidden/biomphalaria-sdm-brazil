### code to test model performance using spatial CV for the B. tenagophila model (SA aggregate def of urban)


#---------------------------------------#
#gradient boosted regression tree       #
#---------------------------------------#


#------------------------------------------------------
# Set up
#------------------------------------------------------

library(xgboost) 
library(rBayesianOptimization)
library(caret)
library(rsample) #to split stratified data
library(dplyr)


#------------------------------------------------------
#read in final data
#------------------------------------------------------

analysis_data_final <- readRDS("tenagophila_AR_brt_data_oct12.rds")

rows <- sample(nrow(analysis_data_final))
analysis_data_final_v2 <- analysis_data_final[rows,]

#----------------
#model performance with spatial - cv

brt_performance <- data.frame(model = rep("BRT", 5),
                              fold_id = 1:5,
                              auc = rep(NA, 5),
                              sensitivity = rep(NA, 5),
                              specificity = rep(NA, 5),
                              presence = rep(NA, 5),
                              background = rep(NA, 5))

for(j in 1:5){
  tryCatch({

    train <- analysis_data_final_v2[analysis_data_final_v2$fold != j,]; train <- train[, -2]
    test  <- analysis_data_final_v2[analysis_data_final_v2$fold == j,]; test <- test[, -2]

    scale_weight = round((as.data.frame(table(train$presence))[1,2] / as.data.frame(table(train$presence))[2,2]), 0)
    
    brt_performance[j, "fold_id"] <- j
    brt_performance[j, "presence"] <- as.data.frame(table(train$presence))[2,2]
    brt_performance[j, "background"] <- as.data.frame(table(train$presence))[1,2]
    
    train <- as.matrix(train)
    d_train <- xgb.DMatrix(train[,-1], label=train[,1])
    
    test <- as.matrix(test)
    d_test <- xgb.DMatrix(test[,-1], label=test[,1])
    
    #------------------------------------------------------
    # Write function for parameter selection function as the engine of Bayesian optimization
    #------------------------------------------------------
    ntrees.max = 200
    xgb_cv_bayes <- function(eta, max.depth, min.child.weight, subsample, colsample_bytree, gamma) {
      cv <- xgb.cv(params = list(booster = "gbtree",
                                 eta = eta,
                                 max_depth = max.depth,
                                 min_child_weight = min.child.weight,
                                 subsample = subsample,
                                 colsample_bytree = colsample_bytree,
                                 gamma = 0,
                                 objective = "binary:logistic",
                                 eval_metric = "logloss",
                                 seed = 25),
                   data = d_train,
                   nrounds = ntrees.max,
                   nfold = 5, #this then uses 5 fold CV within this function
                   early_stopping_rounds = 10,
                   scale_pos_weight = scale_weight,
                   verbose = T)
      list(Score = -unlist(cv$evaluation_log[cv$best_iteration, "test_logloss_mean"]), # Ensure score is negative, since optimization maximizes
           Pred = cv$pred,
           cb.print.evaluation(period = 1))
    }
    
    #------------------------------------------------------
    # Acquire optimal parameters with Bayesian optimization (maximization function) via the R package "rBayesianOptimization"
    #------------------------------------------------------
    best_params <- BayesianOptimization(xgb_cv_bayes,
                                        bounds = list(eta = c(0.01, 0.3),
                                                      max.depth = c(2L, 10L),
                                                      min.child.weight = c(1L, 15L),
                                                      subsample = c(0.6, 1),
                                                      colsample_bytree = c(0.6, 1)),
                                        init_grid_dt = NULL,
                                        init_points = 10,
                                        n_iter = 40,
                                        acq = "ucb",
                                        kappa = 3,
                                        eps = 1.5,
                                        verbose = T)
    
    #------------------------------------------------------
    # Using the tuned hyperparameters, run a second cross-validation to acquire nrounds
    #------------------------------------------------------
    xgb_cv <- xgb.cv(params = best_params,
                     data = d_train,
                     nrounds = ntrees.max,
                     nfold = 5,
                     scale_pos_weight = scale_weight,
                     early_stopping_rounds = 10,
                     objective = "binary:logistic",
                     eval_metric = "logloss",
                     verbose = T)
    
    best_params$nrounds <- xgb_cv$best_ntreelimit
    
    #------------------------------------------------------
    # Check evaluation log, to see that testing and training errors are declining -- need to make a data frame to save this
    # Ensure that optimized hyper parameters are within the pre specified bounds
    #------------------------------------------------------
    
    # parameter_table[1,j] <- xgb_cv$evaluation_log$test_logloss_mean[1]
    # parameter_table[2,1] <- xgb_cv$evaluation_log$test_logloss_mean[nrow(xgb_cv$evaluation_log)]
    # 
    # parameter_table[3,j] <- xgb_cv$params$Best_Par[1] %>% round(4)
    # parameter_table[4,j] <- xgb_cv$params$Best_Par[2] %>% round(4)
    # parameter_table[5,j] <- xgb_cv$params$Best_Par[3] %>% round(4)
    # parameter_table[6,j] <- xgb_cv$params$Best_Par[4] %>% round(4)
    # parameter_table[7,j] <- xgb_cv$params$Best_Par[5] %>% round(4)
    # parameter_table[8,j] <- xgb_cv$params$Best_Par[6] %>% round(4)
    # parameter_table[9,j] <- best_params$nrounds
    
    #------------------------------------------------------
    # Run the full xgb model with the suite of optimal parameters
    #------------------------------------------------------
    watchlist <- list(train = d_train, test = d_test)
    xgb.fit <- xgboost(data = d_train,
                       eta = best_params$Best_Par[1],
                       max_depth = best_params$Best_Par[2],
                       min_child_weight = best_params$Best_Par[3],
                       subsample = best_params$Best_Par[4],
                       colsample_bytree = best_params$Best_Par[5],
                       gamma = 0,
                       nrounds = best_params$nrounds,
                       scale_pos_weight = scale_weight, #(330/165)
                       objective = "binary:logistic",
                       eval_metric = "logloss")
    
    ###prediction test
    brt_performance[j, "fold_id"] <- j
    
    xgbpred <- predict(xgb.fit, d_test); true_vals <- as.data.frame(test); true_vals <- true_vals$presence
    
    auc <- pROC::roc(response=true_vals, predictor=xgbpred, levels=c(0,1), auc = TRUE, plot = FALSE)
    brt_performance[j, "auc"] <- auc$auc
    
    threshold <- pROC::coords(auc, "best", ret = "threshold")
    CM <- confusionMatrix(as.factor(ifelse(xgbpred >= threshold$threshold, 1, 0)), as.factor(true_vals), positive = "1")
    CM_table <- as.data.frame(CM[[4]])
    brt_performance[j, "sensitivity"] <- CM_table[1,]
    brt_performance[j, "specificity"] <- CM_table[2,]

  }, error=function(e){})
}


write.csv(brt_performance, "tenagophila_brt_model_performance_AR.csv")





















