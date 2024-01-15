### B. straminea model code for estimating Brazil wide model predictions for the historical, current, and counterfacutal distributions

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
analysis_data <- readRDS("clean-data/straminea/straminea_AR_brt_data_oct12.rds")
observed_1992 <- readRDS('raw-data/env-rasters/prediction-map-rds/sdm_data_1992.rds')
observed_2017 <- readRDS('raw-data/env-rasters/prediction-map-rds/sdm_data_2017.rds')
climate_counterfactual <- readRDS('raw-data/env-rasters/prediction-map-rds/sdm_climate_counterfactual.rds')
urban_counterfactual <- readRDS('raw-data/env-rasters/prediction-map-rds/sdm_urban_counterfactual.rds')

#-------------------------------#
# boosted regression tree.      #
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

  
  observed_1992_pred <- cbind(observed_1992, predict(xgb.fit, as.matrix(observed_1992[,  xgb.fit$feature_names]))); names(observed_1992_pred)[ncol(observed_1992_pred)] <- "predictions"
  observed_2017_pred <- cbind(observed_2017, predict(xgb.fit, as.matrix(observed_2017[,  xgb.fit$feature_names]))); names(observed_2017_pred)[ncol(observed_2017_pred)] <- "predictions"
  climate_counterfactual_pred <- cbind(climate_counterfactual, predict(xgb.fit, as.matrix(climate_counterfactual[,  xgb.fit$feature_names]))); names(climate_counterfactual_pred)[ncol(climate_counterfactual_pred)] <- "predictions"
  urban_counterfactual_pred <- cbind(urban_counterfactual, predict(xgb.fit, as.matrix(urban_counterfactual[,  xgb.fit$feature_names]))); names(urban_counterfactual_pred)[ncol(urban_counterfactual_pred)] <- "predictions"
  
  observed_1992_pred_raster <- rasterFromXYZ(observed_1992_pred[,c("x", "y", "predictions")])
  observed_2017_pred_raster <- rasterFromXYZ(observed_2017_pred[,c("x", "y", "predictions")])
  climate_counterfactual_pred_raster <- rasterFromXYZ(climate_counterfactual_pred[,c("x", "y", "predictions")])
  urban_counterfactual_pred_raster <- rasterFromXYZ(urban_counterfactual_pred[,c("x", "y", "predictions")])
  
  hist_path <- paste0('output/straminea/geoTiffs/hist_raster_',i,'.tif')
  current_path <- paste0('output/straminea/geoTiffs/current_raster_',i,'.tif')
  climate_path <- paste0('output/straminea/geoTiffs/climate_counterfactual_',i,'.tif')
  urban_path <- paste0('geoTiffs/urban_counterfactual_',i,'.tif')
  
  writeRaster(observed_1992_pred_raster, hist_path)
  writeRaster(observed_2017_pred_raster, current_path)
  writeRaster(climate_counterfactual_pred_raster, climate_path)
  writeRaster(urban_counterfactual_pred_raster, urban_path)
  
  
}




