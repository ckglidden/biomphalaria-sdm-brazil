## summary statistics and figures for B. glabrata model performance and trait profiles (variable contribution and PDPs)

setwd('/Users/cglidden/Desktop/Restore/schisto_project/schisto-brazil-sdm-repo')

##ckglidden
library(ggplot2); library(pROC); library(xgboost); library(rBayesianOptimization); library(stringr); library(dplyr); library(tidyr)


# palette for vimp & pdps
colors <- c("red3", "dodgerblue", "forestgreen", "goldenrod1")

# palettes for maps
# pal0 <- viridis::viridis(7, option = "D", direction = -1)
# pal <- c("grey93", "grey93", pal0[2:7])
# pal2.0 <- paletteer_c("ggthemes::Red-Blue Diverging", 20) #hcl_palettes(palette = "Blue-Red 2") #carto_pal(7, "TealRose")
# pal2 <- c(pal2.0[1:8], "grey93", "grey93", pal2.0[13:20])
# pal3 <- viridis::viridis(10, option = "C")

# replacement string for interpreting variables
rep_str = c('high_urban_gradient' = 'dense urb gradient', 'un_urban_gradient' = 'urban gradient', 'ar_urban_gradient' = 'urban gradient',
            'ag_mosiac' = 'agricultural mosiac', 'temp_crops' = 'temporary crops', 'waterRecur' = 'water reoccurrence',
            'waterOccur' = 'water occurrence', 'riverDist' = 'dist to river', 
            'bio12' = 'annual precip', 'bio13' = 'precip wettest month', 'bio14' = 'precip driest month', 'bio15' = 'precip seasonality', 'bio16' = 'precip wettest qt',
            'bio2' = 'diurnal temp range', 'bio3' = 'isothermality', 'bio8' = 'temp wettest qt', 'bio9' = 'temp driest qt',
            'bulk' = 'soil bulk density', 'pH' = 'soil pH', 'soilCarbon' = 'soil carbon content', 'soilWater' = 'soil water content',
            'sand' = 'soil sand content', 'clay' = 'soil clay content')

var_type_str = c('high_urban_gradient' = 'land-use', 'un_urban_gradient' = 'land-use', 'ar_urban_gradient' = 'land-use',
                 'ag_mosiac' = 'land-use', 'temp_crops' = 'land-use', 
                 'waterRecur' = 'hydrology', 'waterOccur' = 'hydrology', 'riverDist' = 'hydrology', 'aspect' = 'hydrology', 'hnd' = 'hydrology', 'upa' = 'hydrology',
                 'bio12' = 'climate', 'bio13' = 'climate', 'bio14' = 'climate', 'bio15' = 'climate', 'bio16' = 'climate',
                 'bio2' = 'climate', 'bio3' = 'climate', 'bio8' = 'climate', 'bio9' = 'climate', 'elevation' = 'climate',
                 'bulk' = 'soil property', 'pH' = 'soil property', 'soilCarbon' = 'soil property', 'soil water' = 'soil property', 'clay' = 'soil property', 
                 'sand' = 'soil property', 'soilWater' = 'soil property')

##--------------------------------------##
## spatial cross-validation auc         ##
##--------------------------------------##
n <- 5

####### UN definition of urban
spatial_cv_auc_un <- read.csv('output/glabrata/glabrata_brt_model_performance_UN.csv')

un_model_performance <- data.frame(metric = c('AUC', 'sensitivity', 'specificity'),
                                   mean = rep(NA, 3),
                                   lower_ci = rep(NA, 3),
                                   upper_ci = rep(NA, 3),
                                   min = rep(NA, 3),
                                   max = rep(NA, 3))

un_model_performance[1, 2] <- mean(spatial_cv_auc_un$auc) # 0.85
error1 <- qt(0.975,df=n-1)*sd(spatial_cv_auc_un$auc)/sqrt(n)
un_model_performance[1, 3] <- mean(spatial_cv_auc_un$auc) - error1 # 0.80
un_model_performance[1, 4] <- mean(spatial_cv_auc_un$auc) + error1 # 0.91
un_model_performance[1, 5] <- min(spatial_cv_auc_un$auc) # 0.79
un_model_performance[1, 6] <- max(spatial_cv_auc_un$auc) # 0.90

un_model_performance[2, 2] <- mean(spatial_cv_auc_un$sensitivity) # 0.86
error2 <- qt(0.975,df=n-1)*sd(spatial_cv_auc_un$sensitivity)/sqrt(n)
un_model_performance[2, 3] <- mean(spatial_cv_auc_un$sensitivity) - error2 # 0.75
un_model_performance[2, 4] <- mean(spatial_cv_auc_un$sensitivity) + error2 # 0.98
un_model_performance[2, 5] <- min(spatial_cv_auc_un$sensitivity) # 0.76
un_model_performance[2, 6] <- max(spatial_cv_auc_un$sensitivity) # 1.0

un_model_performance[3, 2] <- mean(spatial_cv_auc_un$specificity) # 0.77
error3 <- qt(0.975,df=n-1)*sd(spatial_cv_auc_un$specificity)/sqrt(n)
un_model_performance[3, 3] <- mean(spatial_cv_auc_un$specificity) - error3 # 0.67
un_model_performance[3, 4] <- mean(spatial_cv_auc_un$specificity) + error3 # 0.87
un_model_performance[3, 5] <- min(spatial_cv_auc_un$specificity) # 0.67
un_model_performance[3, 6] <- max(spatial_cv_auc_un$specificity) # 0.85

####### rough aggregation of South America definitions of urban
spatial_cv_auc_ar <- read.csv('output/glabrata/glabrata_brt_model_performance_AR.csv')

ar_model_performance <- data.frame(metric = c('AUC', 'sensitivity', 'specificity'),
                                   mean = rep(NA, 3),
                                   lower_ci = rep(NA, 3),
                                   upper_ci = rep(NA, 3),
                                   min = rep(NA, 3),
                                   max = rep(NA, 3))

ar_model_performance[1, 2] <- mean(spatial_cv_auc_ar$auc) # 0.83
error1 <- qt(0.975,df=n-1)*sd(spatial_cv_auc_ar$auc)/sqrt(n)
ar_model_performance[1, 3] <- mean(spatial_cv_auc_ar$auc) - error1 # 0.77
ar_model_performance[1, 4] <- mean(spatial_cv_auc_ar$auc) + error1 # 0.89
ar_model_performance[1, 5] <- min(spatial_cv_auc_ar$auc) # 0.77
ar_model_performance[1, 6] <- max(spatial_cv_auc_ar$auc) # 0.88

ar_model_performance[2, 2] <- mean(spatial_cv_auc_ar$sensitivity) # 0.87
error2 <- qt(0.975,df=n-1)*sd(spatial_cv_auc_ar$sensitivity)/sqrt(n)
ar_model_performance[2, 3] <- mean(spatial_cv_auc_ar$sensitivity) - error2 # 0.73
ar_model_performance[2, 4] <- mean(spatial_cv_auc_ar$sensitivity) + error2 # 1.0
ar_model_performance[2, 5] <- min(spatial_cv_auc_ar$sensitivity) # 0.70
ar_model_performance[2, 6] <- max(spatial_cv_auc_ar$sensitivity) # 1.0

ar_model_performance[3, 2] <- mean(spatial_cv_auc_ar$specificity) # 0.74
error3 <- qt(0.975,df=n-1)*sd(spatial_cv_auc_ar$specificity)/sqrt(n)
ar_model_performance[3, 3] <- mean(spatial_cv_auc_ar$specificity) - error3 # 0.67
ar_model_performance[3, 4] <- mean(spatial_cv_auc_ar$specificity) + error3 # 0.81
ar_model_performance[3, 5] <- min(spatial_cv_auc_ar$specificity) # 0.68
ar_model_performance[3, 6] <- max(spatial_cv_auc_ar$specificity) # 0.80

##--------------------------------------##
## variable contribution.               ##
##--------------------------------------##

######################
##### UN definition
brt_importance_un <- read.csv("output/glabrata/glabrata_brt_shap_importance_bootstrapping_UN.csv")
names(brt_importance_un)[2] <- "Importance"

brt_mean_importance_un <- aggregate(Importance ~ Feature, data = brt_importance_un, 
                                 FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                                     min = min(x, na.rm = TRUE),
                                                     lower_q = quantile(x, 0.025),
                                                     upper_q = quantile(x, 0.975)))
brt_mean_importance_un <- do.call(data.frame, brt_mean_importance_un)

brt_mean_importance_un <- brt_mean_importance_un %>% 
  mutate(variable_type = str_replace_all(Feature, var_type_str),
    Feature = str_replace_all(Feature, rep_str)) 

glabrata_importance_plot_un <- ggplot(brt_mean_importance_un, 
                                    aes(x = reorder(Feature, Importance.lower_q.2.5.), y = Importance.mean, color = variable_type)) +
  xlab('feature') + ylab('mean contribution to predictions') + 
  geom_pointrange(aes(ymin = Importance.lower_q.2.5., ymax = Importance.upper_q.97.5.), position = "dodge", linewidth = 0.8, size = 0.25) +
  scale_color_manual(values = colors, name = "variable type") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = c(0.6, 0.2),
        legend.background=element_blank(),
        text = element_text(size=22), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))

# supplement figure
ggsave("output/glabrata/glabrata_UN_shap_plot_oct13.pdf", 
       glabrata_importance_plot_un, units = "in", width = 5, height = 6,  dpi = 600)

#################################
####### rough aggregation of South America definitions of urban
brt_importance_ar <- read.csv("output/glabrata/glabrata_brt_shap_importance_bootstrapping_AR.csv")
names(brt_importance_ar)[2] <- "Importance"

brt_mean_importance_ar <- aggregate(Importance ~ Feature, data = brt_importance_ar, 
                                    FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                                        min = min(x, na.rm = TRUE),
                                                        lower_q = quantile(x, 0.025),
                                                        upper_q = quantile(x, 0.975)))
brt_mean_importance_ar <- do.call(data.frame, brt_mean_importance_ar)


brt_mean_importance_ar <- brt_mean_importance_ar %>% 
  mutate(variable_type = str_replace_all(Feature, var_type_str),
         Feature = str_replace_all(Feature, rep_str)) 

glabrata_importance_plot_ar <- ggplot(brt_mean_importance_ar, 
                                      aes(x = reorder(Feature, Importance.lower_q.2.5.), y = Importance.mean, color = variable_type)) +
  geom_pointrange(aes(ymin = Importance.lower_q.2.5., ymax = Importance.upper_q.97.5.), position = "dodge", linewidth = 0.8, size = 0.25) +
  xlab('feature') + ylab('mean contribution to predictions') +
  scale_color_manual(values = colors, name = "variable type") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = c(0.6, 0.2),
        legend.background=element_blank(),
        text = element_text(size=12))

# supplement figure
ggsave("output/glabrata/glabrata_AR_shap_plot_oct13.pdf", 
       glabrata_importance_plot_ar, units = "in", width = 5, height = 6,  dpi = 600)

##--------------------------------------##
## partial dependence plots             ##
##--------------------------------------##
voi <- c("annual precip", "precip wettest month", "precip driest month", "precip seasonality", "precip wettest qt",
         "diurnal temp range", "isothermality", "temp wettest qt", "temp driest qt",
         "dense urb gradient", "urban gradient", "agricultural mosiac", "temporary crops")

#####################
##### UN definition
pdp_df_un <- read.csv("output/glabrata/glabrata_brt_pd_df_bootstrapping_UN.csv")

pdp_df_un <- pdp_df_un %>% 
  mutate(variable_type = str_replace_all(variable, var_type_str),
         variable = str_replace_all(variable, rep_str))

pdp_df_un_voi <- pdp_df_un[pdp_df_un$variable %in% voi,] # put this in above function

# pdp_df_un_voi$variable_ordered <- factor(pdp_df_un_voi$variable, levels= c("annual precip", "recruitment warmest month", "recruitment mean temp", "recruitment_range",
#                                                                      "dist high density urban", "dist to urban"))

# maybe separate land-use & climate
pdp_glabrata_un_plot <- ggplot(pdp_df_un_voi, aes(x=value, y=yhat, color = variable_type)) +
  stat_smooth(aes(group=iter), color='lightgrey', method='loess', size=0.5, se=FALSE) + 
  stat_smooth(method='loess', size=2, se=FALSE) + 
  scale_color_manual(values = c("red3", "forestgreen"), name = "variable type") +
  facet_wrap(~fct_relevel(variable), scales = "free", ncol = 3) +
  theme_bw() + 
  ylab("probability") +
  theme(legend.position = "none",
        legend.background=element_blank(),
        text = element_text(size=10), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.text = element_text(size=10))

# supplement
ggsave("output/glabrata/glabrata_pdp_UN_plot_oct13.pdf", pdp_glabrata_un_plot,  dpi = 600)

###################################
####### rough aggregation of South America definitions of urban
pdp_df_ar <- read.csv("output/glabrata/glabrata_brt_pd_df_bootstrapping_AR.csv")

pdp_df_ar <- pdp_df_ar %>% 
  mutate(variable_type = str_replace_all(variable, var_type_str),
         variable = str_replace_all(variable, rep_str))

pdp_df_ar_voi <- pdp_df_ar[pdp_df_ar$variable %in% voi,] # use this in above function

# pdp_df_ar_voi$variable_ordered <- factor(pdp_df_ar_voi$variable, levels= c("annual precip", "recruitment warmest month", "recruitment mean temp", "recruitment_range",
#                                                                      "dist high density urban", "dist to urban"))

pdp_glabrata_ar_plot <- ggplot(pdp_df_ar_voi, aes(x=value, y=yhat, color = variable_type)) +
  stat_smooth(aes(group=iter), color='lightgrey', method='loess', size=0.5, se=FALSE) + 
  stat_smooth(method='loess', size=2, se=FALSE) + 
  scale_color_manual(values = c("red3", "forestgreen"), name = "variable type") +
  facet_wrap(~fct_relevel(variable), scales = "free", ncol = 3) +
  theme_bw() + 
  ylab("probability") +
  theme(legend.position = "none",
        legend.background=element_blank(),
        text = element_text(size=10), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.text = element_text(size=10))

# supplement
ggsave("output/glabrata/glabrata_pdp_AR_plot_oct13.pdf", pdp_glabrata_ar_plot,  dpi = 600)

##--------------------------------------##
## main text VIMP & PDP combined        ##
##--------------------------------------##

# make sure aesthetics match

main_glabrata_importance_plot_ar <- glabrata_importance_plot_ar +
  # ggtitle("a. variable importance") +
  theme(legend.position = "none",
        legend.background=element_blank(),
        text = element_text(size=9), 
        axis.text.x = element_text(size=8, angle = 30),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),
        strip.text = element_text(size=9))

main_voi <- c("precip seasonality", "precip driest month", "urban gradient", "isothermality", "precip wettest qt", "dense urb gradient")
main_pdp_df_ar_voi <- pdp_df_ar[pdp_df_ar$variable %in% main_voi,] %>%
  mutate(variable=fct_relevel(variable, c("precip seasonality", "precip driest month", "urban gradient", "isothermality", "precip wettest qt", "dense urb gradient"))) %>%
  arrange(variable)

pdp_glabrata_ar_main_plot <- ggplot(main_pdp_df_ar_voi, aes(x=value, y=yhat, color = variable_type)) +
  stat_smooth(aes(group=iter), color='lightgrey', method='loess', size=0.5, se=FALSE) + 
  stat_smooth(method='loess', size=2, se=FALSE) + 
  scale_color_manual(values = c("red3", "forestgreen"), name = "variable type") +
  facet_wrap(~fct_relevel(variable), scales = "free", ncol = 2) +
  theme_minimal() + 
  ylab("probability") +
 # ggtitle("b. partial dependence") +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.position = "none",
        legend.background=element_blank(),
        text = element_text(size=9), 
        axis.text.x = element_text(size=8, angle = 30),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),
        strip.text = element_text(size=8))

final_g <- cowplot::plot_grid(
  main_glabrata_importance_plot_ar, pdp_glabrata_ar_main_plot,
  labels = "auto",
  label_size = 12,
  label_x = 0, label_y = 1,
  rel_widths = c(1, 1.5)
)

ggsave("output/glabrata/glabrata_vimp_pdp_AR_main_plot_oct17.pdf", 
       width = 6, height = 4, units = "in",
       final_g,  dpi = 600)

##--------------------------------------##
## hind-casting UN model                ##
##--------------------------------------##
full_data <- readRDS("clean-data/glabrata/glabrata_UN_brt_data_oct12.rds")
test_data <- readRDS("clean-data/glabrata/glabrata_UN_test_data_oct18.rds")
test_data$presence <- as.numeric(test_data$presence)

#train model with all data
train <- as.matrix(full_data[-2])
d_train <- xgb.DMatrix(train[,-1], label=train[,1])

test_data <- as.matrix(test_data)
d_test <- xgb.DMatrix(test_data[,-26], label=test_data[,26])

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
                 data = d_train,
                 nrounds = ntrees.max,
                 nfold = 5,
                 scale_pos_weight = 2,
                 early_stopping_rounds = 10,
                 objective = "binary:logistic",
                 eval_metric = "logloss",
                 verbose = T)

best_params$nrounds <- xgb_cv$best_ntreelimit


#------------------------------------------------------
# Run the full xgb model with the suite of optimal parameters
#------------------------------------------------------
xgb.fit <- xgboost(data = d_train,
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

###prediction test

xgbpred <- predict(xgb.fit, d_test); true_vals <- as.data.frame(test_data); true_vals <- true_vals$presence

auc <- pROC::roc(response=true_vals, predictor=xgbpred, levels=c(0,1), auc = TRUE, plot = FALSE)
auc$auc # 0.84
threshold <- pROC::coords(auc, "best", ret = "threshold")
CM <- caret::confusionMatrix(as.factor(ifelse(xgbpred >= threshold$threshold, 1, 0)), as.factor(true_vals), positive = "1")
CM_table <- as.data.frame(CM[[4]])
CM_table[1,] #0.85
CM_table[2,] # 0.74

##--------------------------------------##
## hind-casting AR model                ##
##--------------------------------------##
full_data <- readRDS("clean-data/glabrata/glabrata_AR_brt_data_oct12.rds")
test_data <- readRDS("clean-data/glabrata/glabrata_AR_test_data_oct18.rds")
test_data$presence <- as.numeric(test_data$presence)

#train model with all data
train <- as.matrix(full_data[-2])
d_train <- xgb.DMatrix(train[,-1], label=train[,1])

test_data <- as.matrix(test_data)
d_test <- xgb.DMatrix(test_data[,-26], label=test_data[,26])

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
                 data = d_train,
                 nrounds = ntrees.max,
                 nfold = 5,
                 scale_pos_weight = 2,
                 early_stopping_rounds = 10,
                 objective = "binary:logistic",
                 eval_metric = "logloss",
                 verbose = T)

best_params$nrounds <- xgb_cv$best_ntreelimit


#------------------------------------------------------
# Run the full xgb model with the suite of optimal parameters
#------------------------------------------------------
xgb.fit <- xgboost(data = d_train,
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

###prediction test

xgbpred <- predict(xgb.fit, d_test); true_vals <- as.data.frame(test_data); true_vals <- true_vals$presence

auc <- pROC::roc(response=true_vals, predictor=xgbpred, levels=c(0,1), auc = TRUE, plot = FALSE)
auc$auc # 0.89
threshold <- pROC::coords(auc, "best", ret = "threshold")
CM <- caret::confusionMatrix(as.factor(ifelse(xgbpred >= threshold$threshold, 1, 0)), as.factor(true_vals), positive = "1")
CM_table <- as.data.frame(CM[[4]])
CM_table[1,] # 0.925
CM_table[2,] # 0.749



























######################### NEED TO CLEAN THIS UP
####bivariate pdps

#bivariate pdps - unique to each species
pdp_bivariate1 <- pdp::partial(xgb.fit, prob = TRUE, train = full_data[-c(1,2)],
                               pred.var = c("distance_int_pop","bio14"), chull= TRUE)

pdp_bivariate2 <- pdp::partial(xgb.fit, prob = TRUE, train = full_data[-c(1,2)],
                              pred.var = c("distance_int_pop","bio15"), chull = TRUE)

bio14_urban <- ggplot(pdp_bivariate1, aes(distance_int_pop/1000, bio14, fill = yhat)) +
  #stat_contour(geom="polygon", aes(fill=stat(level))) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  xlab("distance to urban (km)") +
  ylab("precip driest month") +
  theme_bw() +
  labs(fill = "prob") +
  theme(legend.position = "bottom",
        legend.background=element_blank(),
        legend.text = element_text(size=8), 
        text = element_text(size=8), 
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        strip.text = element_text(size=8),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.7, 'cm'))

bio15_urban <- ggplot(pdp_bivariate2, aes(distance_int_pop/1000, bio15, fill = yhat)) +
  #stat_contour(geom="polygon", aes(fill=stat(level))) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  xlab("dist to urban (km)") +
  ylab("precip seasonality") +
  theme_bw() +
  labs(fill = "prob") +
  theme(legend.position = "bottom",
        legend.background=element_blank(),
        legend.text = element_text(size=8),
        text = element_text(size=8),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        strip.text = element_text(size=8),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.7, 'cm'))

pdp_urban <- ggplot(subset(pdp_df_voi, variable == "dist to urban"), aes(x=value/1000, y=yhat)) +
  stat_smooth(aes(group=iter), color='lightgrey', method='loess', size=0.5, se=FALSE) + 
  stat_smooth(method='loess', size=2, se=FALSE, color = "forestgreen") + 
  theme_bw() + 
  ylab("probability") +
  xlab("dist to urban (km)") +
  theme(legend.position = "none",
        legend.background=element_blank(),
        text = element_text(size=6), 
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        strip.text = element_text(size=8))

pdp_bio14 <- ggplot(subset(pdp_df_voi, variable == "precip driest month"), aes(x=value, y=yhat)) +
  stat_smooth(aes(group=iter), color='lightgrey', method='loess', size=0.5, se=FALSE) + 
  stat_smooth(method='loess', size=2, se=FALSE, color = "red3") + 
  theme_bw() + 
  ylab("probability") +
  xlab("precip driest month") +
  theme(legend.position = "none",
        legend.background=element_blank(),
        text = element_text(size=8), 
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        strip.text = element_text(size=8))

pdp_bio15 <- ggplot(subset(pdp_df_voi, variable == "precip seasonality"), aes(x=value, y=yhat)) +
  stat_smooth(aes(group=iter), color='lightgrey', method='loess', size=0.5, se=FALSE) + 
  stat_smooth(method='loess', size=2, se=FALSE, color = "red3") + 
  theme_bw() + 
  ylab("probability") +
  xlab("precip seasonality") +
  theme(legend.position = "none",
        legend.background=element_blank(),
        text = element_text(size=8), 
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        strip.text = element_text(size=8))


##### might need to use cowplot
all_plots <- gridExtra::arrangeGrob(pdp_urban, pdp_bio14, pdp_bio15, bio14_urban, bio15_urban, ncol = 5)

ggsave("../output/glabrata/glabrata_pdp_interactions_may1.pdf", 
       height = 2.15, width = 8,
       all_plots, units = "in", dpi = 600)


############################################################################################
############################################################################################
##PREDICTION MAPS - UNMASKED
############################################################################################
############################################################################################


##############################################################
##################################FULL PREDICTED DISTRIBUTIONS
#############################################################

########## 1990 ##########

hist_data <- list.files(path="../output/glabrata/unmasked/historical", pattern="tif", all.files=FALSE, full.names=TRUE,recursive=TRUE)
hist_e <- raster::stack(hist_data)

#get average
hist_glabrata_raster <- calc(hist_e, fun = mean, na.rm = T)
crs(hist_glabrata_raster) <- sp::CRS('+init=EPSG:4326')

#get sd
hist_sd_glabrata_raster <- calc(hist_e, fun = sd, na.rm = T)
crs(hist_sd_glabrata_raster) <- sp::CRS('+init=EPSG:4326')

#save rasters
outfile <- writeRaster(hist_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/mean_brt_1990_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)
outfile <- writeRaster(hist_sd_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/sd_brt_1990_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)

# read in file if need to plot again
hist_glabrata_raster <- raster("../../../output/glabrata/aggregates/mean_brt_1990_glabrata_prediction.tif")


################# COUNTERFACTUALS
########## 2017 ##########

#read in rasters
cur_data <- list.files(path="../output/glabrata/unmasked/current", pattern="tif", all.files=FALSE, full.names=TRUE,recursive=TRUE)
cur_e <- raster::stack(cur_data)

#get average
cur_glabrata_raster <- calc(cur_e, fun = mean, na.rm = T)
crs(cur_glabrata_raster) <- sp::CRS('+init=EPSG:4326')

#get sd
cur_sd_glabrata_raster <- calc(cur_e, fun = sd, na.rm = T)
crs(cur_sd_glabrata_raster) <- sp::CRS('+init=EPSG:4326')

#save rasters
outfile <- writeRaster(cur_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/mean_brt_2017_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)
outfile <- writeRaster(cur_sd_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/sd_brt_2017_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)

#read if need to plot again
cur_glabrata_raster <- raster("../../../output/glabrata/aggregates/mean_brt_2017_glabrata_prediction.tif")

####### 2017 - 1990 difference #######

####### ACTUAL RISK & DISTRIBUTION MAPS ########
pdf("../output/glabrata/unmasked/aggregates/glabrata_all_contemp_maps.pdf") 
par(bty = 'n', mfrow = c(2,2), mar = c(0.5,1,1,2)) 

plot(hist_glabrata_raster, col = pal, 
     zlim = c(0,1), main = "a. 1993 distribution", axes = F, frame.plot=FALSE) 

plot(cur_glabrata_raster, col = pal, 
     zlim = c(0,1), main = "b. 2017 distribution", axes = F, frame.plot=FALSE) 

plot(difference_raster, zlim = c(-0.65, 0.65),
     col = pal2, main = "c. change in distribution", axes = F, frame.plot=FALSE) 

dev.off()

##############################################################
##################################Drivers of change         #
#############################################################


#########################
###### Hold temp constant
#########################

#read in rasters
# temp_data <- list.files(path="../output/glabrata/unmasked/aggregates/temp_constant", pattern="tif", all.files=FALSE, full.names=TRUE, recursive = TRUE)
# temp_e <- raster::stack(temp_data)
# 
# #get average
# temp_glabrata_raster <- calc(temp_e, fun = mean, na.rm = T)
# crs(temp_glabrata_raster) <- sp::CRS('+init=EPSG:4326')
# 
# #get sd
# temp_sd_glabrata_raster <- calc(temp_e, fun = sd, na.rm = T)
# crs(temp_sd_glabrata_raster) <- sp::CRS('+init=EPSG:4326')
# 
# #save rasters
# outfile <- writeRaster(temp_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/mean_temp_constant_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)
# outfile <- writeRaster(temp_sd_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/sd_temp_constant_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)
# 
# ##########risk map
# bin_temp_gl <- temp_glabrata_raster
# bin_temp_gl[bin_temp_gl > 0.5] <- 1
# bin_temp_gl[bin_temp_gl <= 0.5] <- 0
# 
# temp_gl_risk <- pop_2017*bin_temp_gl
# crs(temp_gl_risk) <- sp::CRS('+init=EPSG:4326')
# 
# outfile <- writeRaster(temp_gl_risk, filename='../output/glabrata/unmasked/aggregates/glabrata_exp_risk_temp_con.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)
# 
# 
# ##########################
# ##### Hold precip constant
# ##########################
# 
# #read in rasters
# precip_data <- list.files(path="../output/glabrata/unmasked/aggregates/precip_constant", pattern="tif", all.files=FALSE, full.names=TRUE, recursive = TRUE)
# precip_e <- raster::stack(precip_data)
# 
# #get average
# precip_glabrata_raster <- calc(precip_e, fun = mean, na.rm = T)
# crs(precip_glabrata_raster) <- sp::CRS('+init=EPSG:4326')
# 
# #get sd
# precip_sd_glabrata_raster <- calc(precip_e, fun = sd, na.rm = T)
# crs(precip_sd_glabrata_raster) <- sp::CRS('+init=EPSG:4326')
# 
# #save rasters
# outfile <- writeRaster(precip_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/unmasked/aggregates/mean_precip_constant_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)
# outfile <- writeRaster(precip_sd_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/sd_precip_constant_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)


##########################
##### Hold climate constant
##########################

#read in rasters
climate_data <- list.files(path="../output/glabrata/unmasked/climate_constant", pattern="tif", all.files=FALSE, full.names=TRUE, recursive = TRUE)
climate_e <- raster::stack(climate_data)

#get average
climate_glabrata_raster <- calc(climate_e, fun = mean, na.rm = T)
crs(climate_glabrata_raster) <- sp::CRS('+init=EPSG:4326')

#get sd
climate_sd_glabrata_raster <- calc(climate_e, fun = sd, na.rm = T)
crs(climate_sd_glabrata_raster) <- sp::CRS('+init=EPSG:4326')

#save rasters
outfile <- writeRaster(climate_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/mean_climate_constant_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)
outfile <- writeRaster(climate_sd_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/sd_climate_constant_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)

climate_glabrata_raster <- raster("../../../output/glabrata/aggregates/mean_climate_constant_glabrata_prediction.tif")
  
#######################
##### Hold urbanization
#######################

#read in rasters
urban_data <- list.files(path="../output/glabrata/unmasked/urban_constant", pattern="tif", all.files=FALSE, full.names=TRUE, recursive = TRUE)
urban_e <- raster::stack(urban_data)

#get average
urban_glabrata_raster <- calc(urban_e, fun = mean, na.rm = T)
crs(urban_glabrata_raster) <- sp::CRS('+init=EPSG:4326')

#get sd
urban_sd_glabrata_raster <- calc(urban_e, fun = sd, na.rm = T)
crs(urban_sd_glabrata_raster) <- sp::CRS('+init=EPSG:4326')

#save rasters
outfile <- writeRaster(urban_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/mean_urban_constant_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)
outfile <- writeRaster(urban_sd_glabrata_raster, filename='../output/glabrata/unmasked/aggregates/sd_urban_constant_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)

# read in again for figure
urban_glabrata_raster <- raster("../../../output/glabrata/aggregates/mean_urban_constant_glabrata_prediction.tif")
  
########################################################################
#################Distribution Plot Comparing Drivers of Change         #
########################################################################

# pdf("/Users/carolineglidden/Documents/GitHub/schisto-snails-brazil-distribution/output/glabrata/unmasked/aggregates/glabrata_drivers_distribution.pdf") 
# par(bty = 'n', mfrow = c(4,2), mar = c(0.5,1,1,2)) 
# 
# plot(hist_glabrata_raster, col = pal, 
#      zlim = c(0,1), main = "a. 1990 distribution", axes = F, frame.plot=FALSE) 
# 
# plot(cur_glabrata_raster, col = pal, 
#      zlim = c(0,1), main = "b. 2017 distribution", axes = F, frame.plot=FALSE) 
# 
# 
# plot(temp_glabrata_raster, col = pal, 
#      zlim = c(0,1), main = "c. 2017 but temp constant", axes = F, frame.plot=FALSE) 
# 
# temp_const_dif <- cur_glabrata_raster - temp_glabrata_raster
# plot(temp_const_dif, col = pal2, 
#      zlim = c(-0.4, 0.4), main = "d. diff (b - c)", axes = F, frame.plot=FALSE) 
# 
# 
# plot(precip_glabrata_raster, col = pal, 
#      zlim = c(0,1), main = "e. 2017 but precip constant", axes = F, frame.plot=FALSE) 
# 
# precip_const_dif <- cur_glabrata_raster - precip_glabrata_raster
# plot(precip_const_dif, col = pal2, 
#      zlim = c(-0.4, 0.4), main = "f. diff (b - e)", axes = F, frame.plot=FALSE) 
# 
# plot(urban_glabrata_raster, col = pal, 
#      zlim = c(0,1), main = "g. 2017 but pop constant", axes = F, frame.plot=FALSE) 
# 
# urban_const_dif <- cur_glabrata_raster - urban_glabrata_raster
# plot(urban_const_dif, col = pal2, 
#      zlim = c(-0.4, 0.4), main = "h. diff (b - g)", axes = F, frame.plot=FALSE) 
# dev.off()

##
pdf("../output/glabrata/unmasked/aggregates/glabrata_drivers_distribution_climate_constant.pdf") 
par(bty = 'n', mfrow = c(3,2), mar = c(0.5,1,1,2)) 

plot(hist_glabrata_raster, col = pal, 
     zlim = c(0,1), main = "a. 1993 distribution", axes = F, frame.plot=FALSE) 

plot(cur_glabrata_raster, col = pal, 
     zlim = c(0,1), main = "b. 2017 distribution", axes = F, frame.plot=FALSE) 

plot(climate_glabrata_raster, col = pal, 
     zlim = c(0,1), main = "e. 2017 but climate constant", axes = F, frame.plot=FALSE) 

climate_const_dif <- cur_glabrata_raster - climate_glabrata_raster
plot(climate_const_dif, col = pal2, 
     zlim = c(-0.65, 0.65), main = "f. diff (b - e)", axes = F, frame.plot=FALSE) 

plot(urban_glabrata_raster, col = pal, 
     zlim = c(0,1), main = "g. 2017 but pop constant", axes = F, frame.plot=FALSE) 

urban_const_dif <- cur_glabrata_raster - urban_glabrata_raster
plot(urban_const_dif, col = pal2, 
     zlim = c(-0.65, 0.65), main = "h. diff (b - g)", axes = F, frame.plot=FALSE) 
dev.off()

# pdf("../output/glabrata/unmasked/aggregates/axis.pdf") 
# plot(cur_glabrata_raster, col = pal, 
#      zlim = c(0,1), main = "b. 2017 distribution", axes = F, frame.plot=FALSE) 
# dev.off()
# 
# pdf("../output/glabrata/unmasked/aggregates/axis2.pdf") 
# plot(climate_const_dif, col = pal2, 
#      zlim = c(-0.65, 0.65), main = "f. diff (b - e)", axes = F, frame.plot=FALSE) 
# dev.off()

## diff figure
# read in most populous cities
major_cities <- read.csv("../../raw_data/feature_collections/metro_areas/fifteen_populous_cites.csv")
major_cities$color <- ifelse(major_cities$direction == "increase", "darkblue", "darkred")

pdf("../../../output/glabrata/aggregates/glabrata_drivers_difference.pdf") 
par(bty = 'n', mfrow = c(1, 2), mar = c(0.5,1,1,2)) 

# plot(difference_raster, zlim = c(-0.65, 0.65),
#     col = pal2, main = "a. observed difference", axes = F, frame.plot=FALSE) 

climate_const_dif <- cur_glabrata_raster - climate_glabrata_raster
plot(climate_const_dif, col = pal2, 
     zlim = c(-0.65, 0.65), main = "a. climate constant", axes = F, frame.plot=FALSE) 

urban_const_dif <- cur_glabrata_raster - urban_glabrata_raster
plot(urban_const_dif, col = pal2, 
     zlim = c(-0.65, 0.65), main = "b. urban constant", axes = F, frame.plot=FALSE) 
points(major_cities$long, major_cities$lat, col = major_cities$color, pch = 16)

dev.off()



###### 100km examples

## maceio
par(mfrow = c(2,2))
maceio_1993 <- raster("../output/glabrata/unmasked/aggregates/city_square/maceio_1993_glabrata_prediction.tif")
plot(maceio_1993)

maceio_2017 <- raster("../output/glabrata/unmasked/aggregates/city_square/maceio_2017_glabrata_prediction.tif")
plot(maceio_2017)

maceio_climate <- raster("../output/glabrata/unmasked/aggregates/city_square/maceio_climate_glabrata_prediction.tif")
plot(maceio_climate)

maceio_urban <- raster("../output/glabrata/unmasked/aggregates/city_square/maceio_urban_glabrata_prediction.tif")
plot(maceio_urban)

plot(maceio_2017 - maceio_urban)
plot(maceio_2017 - maceio_climate)

## recife
par(mfrow = c(2,2))
recife_1993 <- raster("../output/glabrata/unmasked/aggregates/city_square/recife_1993_glabrata_prediction.tif")
plot(recife_1993)

recife_2017 <- raster("../../../output/glabrata/aggregates/city_square/recife_2017_glabrata_prediction.tif")
plot(recife_2017)

recife_climate <- raster("../output/glabrata/unmasked/aggregates/city_square/recife_climate_glabrata_prediction.tif")
plot(recife_climate)

recife_urban <- raster("../../../output/glabrata/aggregates/city_square/recife_urban_glabrata_prediction.tif")
plot(recife_urban)

#recife cities
recife_cities <- read.csv("../../raw_data/feature_collections/metro_areas/recife_cities_gr75k.csv")

#diff raster 
diff <- recife_2017 - recife_urban
outfile <- writeRaster(diff, filename='../../../output/glabrata/aggregates/city_square/mean_urban_change_recife_glabrata_prediction.tif', format="GTiff",options=c("INTERLEAVE=BAND","COMPRESS=LZW"), overwrite=TRUE)

# read in most populous muncipalities
pdf("../../../output/glabrata/aggregates/city_square/glabrata_difference_recife.pdf") 
par(mfrow = c(1,1), bty = 'n')
plot(recife_2017 - recife_urban, 
     zlim = c(-0.4, 0.4),
     axes = F, frame.plot=FALSE,
     col = pal2)
points(-34.88133, -8.057343, pch = 15, cex = 3, col = "darkgreen")
dev.off()

## diff cost mapping
# diff_distance <- raster("../../../output/glabrata/aggregates/city_square/diff_distance_1993_2017.tif")
# diff_distance <- crop(diff_distance, recife_urban)
# diff_distance <- diff_distance / 1000

# plot(diff_distance, 
     # zlim = c(-0.4, 0.4),
#     axes = F, frame.plot=FALSE,
#     col = rev(pal2))

## ourinhos
par(mfrow = c(2,2))
ourinhos_1993 <- raster("../output/glabrata/unmasked/aggregates/city_square/ourinhos_1993_glabrata_prediction.tif")
plot(ourinhos_1993)

ourinhos_2017 <- raster("../output/glabrata/unmasked/aggregates/city_square/ourinhos_2017_glabrata_prediction.tif")
plot(ourinhos_2017)

ourinhos_climate <- raster("../output/glabrata/unmasked/aggregates/city_square/ourinhos_climate_glabrata_prediction.tif")
plot(ourinhos_climate)

ourinhos_urban <- raster("../output/glabrata/unmasked/aggregates/city_square/ourinhos_urban_glabrata_prediction.tif")
plot(ourinhos_urban)

pdf("../output/glabrata/unmasked/aggregates/city_square/glabrata_difference_ourinhos.pdf") 
par(mfrow = c(1,1), bty = 'n')
plot(ourinhos_2017 - ourinhos_urban, 
     zlim = c(-0.4, 0.4),
     axes = F, frame.plot=FALSE,
     col = pal2)
dev.off()
