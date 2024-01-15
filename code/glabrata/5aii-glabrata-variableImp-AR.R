### B. glabrata model code for bootstrapping for VIP and PDPs

##-------------------------------------------##
## set wd & load libraries                   ##
##-------------------------------------------##
setwd("biomphalaria-sdm-brazil") # set path based on where you save it on local machine

library(xgboost) 
library(rBayesianOptimization)
library(caret)
library(rsample) #to split stratified data
library(dplyr)
library(SHAPforxgboost)
library(pdp)


#------------------------#
#read in final data.     #
#------------------------#
analysis_data <- readRDS("clean-data/glabrata/glabrata_AR_brt_data_oct12.rds")

#-------------------------------#
# set up df for loop output     #
#-------------------------------#
means_df <- c()
pd_df_all <- c()

#-------------------------------#
# boosted regression tree       #
#-------------------------------#

for(i in 1:25){
  set.seed(i*1226)
  
  #shuffle before creating kfolds
  rows <- sample(nrow(analysis_data))
  analysis_data_v2 <- analysis_data[rows,]
  feature_data <- analysis_data_v2[,-2] #remove fold id
  
  ##create test & train split
  train_index <- createDataPartition(feature_data$presence, p = .8, list = FALSE)
  train <- feature_data[train_index,]; train_matrix <- as.matrix(train); d_train_matrix <- xgb.DMatrix(train_matrix[,-1], label= train_matrix[,1])

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
                 data = d_train_matrix,
                 nrounds = ntrees.max,
                 nfold = 5, #this then uses 5 fold CV within this function
                 early_stopping_rounds = 10,
                 scale_pos_weight = 2,
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
                   data = d_train_matrix,
                   nrounds = ntrees.max,
                   nfold = 5,
                   scale_pos_weight = 2,
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
  #watchlist <- list(train = d_train_matrix, test = d_test_matrix)
  xgb.fit <- xgboost(data = d_train_matrix,
                     eta = best_params$Best_Par[1],
                     max_depth = best_params$Best_Par[2],
                     min_child_weight = best_params$Best_Par[3],
                     subsample = best_params$Best_Par[4],
                     colsample_bytree = best_params$Best_Par[5],
                     gamma = 0,
                     nrounds = best_params$nrounds,
                     scale_pos_weight = 2,
                     objective = "binary:logistic",
                     eval_metric = "logloss")

  
  # To return the SHAP values and ranked features by mean|SHAP|
  shap_values <- shap.values(xgb_model = xgb.fit, X_train = train_matrix[,-1])
  
  # The ranked features by mean |SHAP| for feature importance
  means <- as.data.frame(shap_values$mean_shap_score) #this gets global feature importance - this is the mean SHAP value per feature
  means$Feature <- rownames(means) #this just makes sure that the SHAP values is associated with each feature 
  means$iter <- i #saving iteration to potentially aggregate and plot better
  means_df <- rbind(means_df, means)
  
  names_list <- names(analysis_data)[3:ncol(analysis_data)]
  
  #univariate pdps
     pd_df = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c('variable', 'value','yhat'))),
                       row.names = NULL, stringsAsFactors=F)
     
     for (k in 1:length(names_list)) { #loop through each variable
       
         output <- as.data.frame(pdp::partial(xgb.fit, pred.var = names_list[k], prob = TRUE, train = train[-1]))
         
         loop_pdp_df <- data.frame(matrix(vector(), nrow(output), 4,
                                      dimnames=list(c(), c('variable', 'value','yhat','iter'))), stringsAsFactors=F,
                               row.names=NULL)
         
         loop_pdp_df$variable <- names_list[k]
         loop_pdp_df$value <- output[[1]]
         loop_pdp_df$yhat <- output[[2]]
       loop_pdp_df$iter <- i
         
         pd_df <- rbind(pd_df, loop_pdp_df)
         
       }
       
       pd_df_all <- rbind(pd_df_all, pd_df)
  
      
}


write.csv(pd_df_all, "output/glabrata/glabrata_brt_pd_df_bootstrapping_AR.csv") 
write.csv(means_df, "output/glabrata/glabrata_brt_shap_importance_bootstrapping_AR.csv")



