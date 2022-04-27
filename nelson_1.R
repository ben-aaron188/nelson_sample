###############################################################################
###############################################################################
### NELSON sample size estimator for supervised machine learning problems
###
###
###############################################################################
rm(list = ls())

source('/Users/bennettkleinberg/GitHub/nelson_sample/nelson_sim_utils.R')
source('/Users/bennettkleinberg/GitHub/nelson_sample/nelson_learning_utils.R')



#TODO:
# feature strength via log-odds reversion DONE
# feature strength prior information DONE
# print output statement
# evaluate simulation-to-empirical match DONE
# add slots to class with meta var
# add weight to small values in sampling (lognormal?)
# role of intercept

###############################################################################
###############################################################################
###############################################################################


p1 = make_pseudo_population(feat_n = 50
                            , population_size = 1e5
                            , feat_dist = 'uniform'
                            , feat_strength_unit = 'prop'
                            , min = 0.01
                            , max = 0.30
                            , seed = 123)


run_ml_model_gen = function(population
                          , n
                          , model = 'rf'
                          , train_prop = 0.80
                          , cv_folds = 5
                          , cv_reps = 5
                          , seed){
  
  set.seed(seed)
  if(n == 'baseline'){
    data_ml = sample_from_population(population = population
                                     , n = nrow(population)
                                     , seed = (seed*42))
  } else {
    data_ml = sample_from_population(population = population
                                     , n = n
                                     , seed = (seed*42))
  }
  
  
  data_ml$y = as.factor(make.names(data_ml$y))
  data_ml = as.data.frame(data_ml)
  
  train_idx = createDataPartition(y = data_ml$y
                                  , p = train_prop
                                  , list = FALSE)
  
  train_data = data_ml[train_idx, ]
  test_data = data_ml[-train_idx,]
  
  train_ctrl = trainControl(method="repeatedcv"
                            , number = cv_folds
                            , repeats = cv_reps
                            , selectionFunction = "oneSE"
                            , classProbs = T
                            , summaryFunction = twoClassSummary
                            , savePredictions = F)
  
  train_model = train(y ~ .
                      , data = train_data
                      , method = model
                      , metric="ROC"
                      , trControl = train_ctrl
                      #, preProcess = c('nzv')
  )
  
  pred = predict(train_model, test_data[, -c(1)])
  probs = predict(train_model, test_data[, -c(1)], type='prob')[,1]
  
  confusion_matrix = confusionMatrix(pred, test_data$y)
  
  return(confusion_matrix)
  
}

nlsn_flow_fwd = function(population = p1
                         , train_prop = 0.8
                         , cv_folds = 4
                         , cv_reps = 1
                         , seed = 32
                         , iter_max = 10
                         , n_min = 20
                         , n_max = 120
                         , n_steps = 20
                         ){

  cmat_baseline = run_ml_model_gen(population = population
                            , n = 'baseline'
                            , train_prop = train_prop
                            , cv_folds = cv_folds
                            , cv_reps = cv_reps
                            , seed = seed)
  
  baseline_metric = get_metric_from_confmat(cmat_baseline)
  
  
  list_meta = list()
  base_seed = 58239*seed
  
  steps = seq(n_min, n_max, n_steps)
  
  for (i in 1:length(steps)){
    
    print(i)
    
    cmat_list = list()
    for(j in 1:iter_max){
      print(j)
      
      new_seed = base_seed+steps[i]+j
      
      print(new_seed)
      
      temp_cmat = run_ml_model_gen(population = population
                                   , n = steps[i]
                                   , train_prop = train_prop
                                   , cv_folds = cv_folds
                                   , cv_reps = cv_reps
                                   , seed = new_seed)
      
      metric = get_metric_from_confmat(temp_cmat)
      
      df_ = data.frame(metric = metric
                       , n = steps[i]
                       , iter = j)
      
      cmat_list[[j]] = df_
    }
    
    list_to_df = rbindlist(cmat_list)
    list_meta[[i]] = list_to_df
    
  }
  
  results_dt = rbindlist(list_meta)
  results_dt[, metric_delta := (baseline_metric - metric)]
  
  return(results_dt)
}



nlsn_analyse = function(nlsn_results_obj
                        , CI_level = 0.99){
  
  nlsn_aggr = nlsn_results_obj[, .('M' = mean(metric_delta), 'SD' = sd(metric_delta), 'iter' = .N), n]
  nlsn_aggr[, SE := SD/sqrt(iter)]
  crit_z = qnorm((1-CI_level),lower.tail=FALSE)
  nlsn_aggr[, CI_lo := M-crit_z*SE]
  nlsn_aggr[, CI_hi := M+crit_z*SE]
  
  return(nlsn_aggr)
  
}


nlsn_results = nlsn_flow_fwd(n_min = 50, n_max = 1000, n_steps = 50, iter_max = 20)
nlsn_results_2 = nlsn_analyse(nlsn_results
                              , CI_level = 0.90)

stability_corridor = 0.05
plot(M ~ n
     , data = nlsn_results_2
     , col=4
     , ylim=c(-0.5, 0.5)
     , ylab='Metric delta'
     , xlab='Sample size'
     , main=""
     , pch=19
     , cex=0.8
     , xaxt='n'
     , yaxt='n'
     , panel.first = grid())
lines(CI_lo ~ n, data = nlsn_results_2, col='red')
lines(CI_hi ~ n, data = nlsn_results_2, col='red')
axis(1, at = format(round(seq(0, 1000, 50), 2), nsmall=2), las=1)
axis(2, at = format(round(seq(-0.5, 0.5, .1), 2), nsmall=2), las=1)
abline(h=stability_corridor, col='black', lty=2)
abline(h=-stability_corridor, col='black', lty=2)

plot(V1 ~ n, data = results_mean)





# META FUNCTION
## params
### - nfeatures
### - strengths: weak, moderate, strong
### - distr.: uniform, normal
### model
### balance
### split
### metric

##### STEPS within
# 1. simulate pop DONE
# 2. obtain baseline performance (metric!)  ONCE!
# 3. increase n successively (store cmat)
# 3a. performance improvement: stop once criterion is met
# 4. assessment with corridor (from cmats)


# VERSIONS
## user: simply user-based input
## dev: full control